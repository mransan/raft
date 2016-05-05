
open Raft_pb

module State            = Raft_helper.State
module Follower         = Raft_helper.Follower
module Candidate        = Raft_helper.Candidate
module Leader           = Raft_helper.Leader
module Configuration    = Raft_helper.Configuration
module Timeout_event    = Raft_helper.Timeout_event 

type time = float 
  
type message_to_send = Raft_pb.message * int 
  
let rec keep_first_n l = function
  | 0 -> []
  | n -> 
    begin match l with 
    | hd::tl -> hd::(keep_first_n tl (n - 1))
    | []     -> [] 
    end

let make_append_entries prev_log_index (cache:rev_log_cache) state =  
  let max = state.configuration.max_nb_logs_per_message in

  let cache = 
    if Raft_helper.Rev_log_cache.contains_next_of prev_log_index cache
    then 
      Raft_helper.Rev_log_cache.sub prev_log_index cache
    else 
      Raft_helper.Rev_log_cache.make prev_log_index state.log
  in 
  
  let rev_log_entries = keep_first_n cache.rev_log_entries max in 

  let request = {
    leader_term = state.current_term;
    leader_id = state.id;
    prev_log_index; 
    prev_log_term = cache.prev_term;
    rev_log_entries;
    leader_commit = state.commit_index;
  } in
  (cache, request)

  (*
let compute_append_entries state ({indices; _ } as leader_state) = 
  let leader_state, msgs_to_send = List.fold_left (fun (leader_state, msgs_to_send) server_index 
  *)

let make_append_entries_for_server state {indices} receiver_id = 

  let indices, req = List.fold_left (fun (indices, req) ({server_id; next_index; cache;_ } as index) -> 
    if server_id = receiver_id
    then 
      let cache, req = make_append_entries (next_index -1) cache state in 
      ({index with cache}::indices , Some req)
    else 
      (index::indices, req)
  ) ([], None) indices
  in

  match req with
  | None -> failwith "[Raft_logic] Invalid receiver_id"
  | Some req -> 
    ({indices}, req)

(* This helper function create heartbeat requests for 
 * all the servers with past deadlines. 
 *)
let make_heartbeat_requests state ({indices} as leader_state) now = 
  let servers_to_send = List.fold_left (fun acc {server_id; heartbeat_deadline;} ->
      if now >= heartbeat_deadline
      then server_id::acc
      else acc
    ) [] indices
  in 

  List.fold_left (fun (leader_state, msgs_to_send) {server_id; cache=_; _ } -> 
    if List.mem server_id servers_to_send 
    then 
      let leader_state, msg_to_send = 
        let leader_state, r = make_append_entries_for_server state leader_state server_id in
        leader_state, (Append_entries_request r, server_id)
      in
      (leader_state, msg_to_send::msgs_to_send)
      
    else 
      (leader_state, msgs_to_send)
  ) (leader_state, []) leader_state.indices 


(* Helper function to update the heartbeat deadline for the servers
 * that we are sending messages to. 
 *)
let record_requests_sent configuration leader_state msgs_to_send now = 
  List.fold_left (fun leader_state (_, server_id) -> 
    Raft_helper.Leader.record_request_sent 
        ~server_id ~now ~configuration leader_state
  ) leader_state msgs_to_send
  

(** {2 Request Vote} *) 

module Request_vote = struct 
  
  let make state = 
    let index, term  = State.last_log_index_and_term state in
    {
      candidate_term = state.current_term; 
      candidate_id = state.id; 
      candidate_last_log_index = index;
      candidate_last_log_term = term;
    }

  let handle_request state request now =
  
    let {candidate_id; candidate_term; candidate_last_log_index;_} = request in 

    let make_response state vote_granted = 
      {voter_term = state.current_term; voter_id = state.id ; vote_granted; }
    in 

    if candidate_term < state.current_term 
    then 
      (* 
       * This request is coming from a candidate lagging behind ... 
       * no vote for him.
       *
       *)
      (state, make_response state false)
    else 
      let state = 
        (* 
         * Enforce invariant that if this server is lagging behind 
         * it must convert to a follower and update to that term.
         *
         *)
        if candidate_term > state.current_term
        then Follower.become ~term:candidate_term ~now state 
        else state
      in
      let last_log_index = State.last_log_index state in 
      if candidate_last_log_index < last_log_index
      then 
        (* 
         * Enforce the safety constraint by denying vote if this server
         * last log is more recent than the candidate one.
         *
         *)
        (state, make_response state false)
      else
        let role = state.role in 
        match role with
        | Follower {voted_for = None; _} ->
          (* 
           * This server has never voted before, candidate is getting the vote
           *
           * In accordance to the `Rules for Servers`, the follower must
           * reset its election deadline when granting its vote.
           *
           *)
          
          let {configuration = {election_timeout; _ }; _} = state in  
          let state ={state with 
            role = Follower {
              voted_for         = Some candidate_id; 
              current_leader    = None;
              election_deadline = now +. election_timeout; 
            } 
          } in  
          (state, make_response state true)
           
        | Follower {voted_for = Some id} when id = candidate_id -> 
          (* 
           * This server has already voted for that candidate ... reminding him
           *
           *)
          (state, make_response state true)

        | _ -> 
          (* 
           * Server has previously voted for another candidate or 
           * itself
           *
           *)
          (state, make_response state false)
  
  let handle_response state response now = 
  
    let {current_term; role; configuration; _ } = state in 
    let {voter_term; vote_granted} = response in 

    if voter_term > current_term 
    then 
      (* 
       * Enforce invariant that if this server is lagging behind 
       * it must convert to a follower and update to the latest term.
       *
       *)
      let state = Follower.become ~term:voter_term ~now state in 
      (state, [])
    else 
  
      match role, vote_granted  with
      | Candidate ({vote_count; election_deadline} as candidate_state) , true -> 
        let has_majority = vote_count >= (configuration.nb_of_server / 2) in 
        if  has_majority
        then 
          (* 
           * By reaching a majority of votes, the 
           * candidate is now the new leader
           *
           *)
          let state = Leader.become state now in 
          
          (* 
           * As a new leader, the server must send Append entries request
           * to the other servers to both establish its leadership and
           * start synching its log with the others. 
           *)
          begin match state.role with
          | Leader {indices} -> (
            let indices, msgs_to_send = List.fold_left (fun (indices, msgs_to_send) ({server_id; next_index; cache } as index) ->
                
                let prev_log_index = next_index - 1 in 
                let cache, req  = make_append_entries prev_log_index cache state in 
                let msg_to_send = (Append_entries_request req, server_id) in 
                ({index with cache;}::indices, msg_to_send::msgs_to_send)

              ) ([], []) indices
            in 
            ({state with role = Leader {indices}}, msgs_to_send)
          )
          | _ -> assert(false)
          end 
        else 
          (* Candidate has a new vote but not yet reached the majority
           *)
          let new_state = {state with 
            role = Candidate (Candidate.increment_vote_count candidate_state);
          } in 
          (new_state, [])

      | Candidate _ , false
        (* The vote was denied, the election keeps on going until
         * its deadline. 
         *)

      | Follower _ , _ 
      | Leader   _ , _ -> (state, [])
        (* If the server is either Follower or Leader, it means that 
         * it has changed role in between the time it sent the 
         * [RequestVote] request and this response. 
         * This response can therefore safely be ignored and the server 
         * keeps its current state.
         *)

end (* Request_vote *)

(** {2 Append Entries} *) 

module Append_entries = struct 
  
  let make state receiver_id = 
    match state.role with
    | Leader leader_state -> 
      let leader_state, req = make_append_entries_for_server state leader_state receiver_id in 
      ({state with role = Leader leader_state}, Some req)
    | _ -> (state, None) 

  let handle_request state request now = 
    
    let {leader_term; leader_commit; leader_id;} = request in  
    
    let make_response state result = 
      {receiver_term = state.current_term; receiver_id = state.id; result;}
    in 
    
    if leader_term < state.current_term 
    then 
      (* This request is coming from a leader lagging behind...
       *)
      
      (state, make_response state Term_failure)
  
    else 
      (* This request is coming from a legetimate leader, 
       * let's ensure that this server is a follower.
       *)
      let state  = Follower.become ~current_leader:leader_id ~term:leader_term ~now state in  

      (* Next step is to handle the log entries from the leader.
       * 
       * The algorithm search for the [prev log entry] that the leader 
       * sent. 
       *)
      let {prev_log_index; prev_log_term; rev_log_entries; leader_commit} = request in 
  
      (* This functions merges the request log entries with the 
       * current log of the server. The current log is first split by 
       * the caller into: 
       *
       * - [pre_logs] all the logs prior to (and including) [prev_log_index].
       * - [rev_post_logs] all the log entry in the server future of the
       *   [prev_log_index]. This list is expected to be in reverse order. 
       *
       * This function will then merge the 2 list of logs by adding all the 
       * common logs first, then if either one is empty or an entry differs
       * then the entries from the leader will override the one in the server. 
       *
       * This merging is necessary because message can be delivered out of 
       * order. (See [test.ml] for a code example). This means that this
       * server might receive an outdated append query from its leader.
       *) 
      let merge_log_entries state rev_post_logs pre_logs  =  

        let rec aux count log = function
          | [], [] -> (count, log) 
          | ({index=i1;term=t1;data} as e)::tl1, {index=i2;term=t2; _ }::tl2 -> 
            if i1 = i2 && t1 = t2 
            then aux (count + 1) (e::log) (tl1, tl2)
            else aux (count + 1) (e::log) (tl1, []) 
          | hd::tl, []
          | [], hd::tl -> aux (count + 1) (hd::log) (tl, []) 
        in 

        let (log_size, log) = aux (state.log_size - (List.length rev_post_logs)) pre_logs (rev_log_entries, rev_post_logs) in
        let state = {state with log ; log_size; } in 
        let receiver_last_log_index = State.last_log_index state in 
        let state = 
          (* Update this server commit index based on value sent from 
           * the leader. 
           * Note that it is not guaranteed that the leader has sent
           * all the log until it commit index so the min value 
           * is taken.
           *)
          if leader_commit > state.commit_index
          then {state with
            commit_index = min leader_commit receiver_last_log_index 
          } 
          else state   
        in 
        let response = make_response state (Success {receiver_last_log_index; }) in 
        (state, response)
      in

      let make_log_failure_with_latest_log = function
        | [] -> Log_failure {
          receiver_last_log_index = 0;
          receiver_last_log_term = 0;
        }
        | {index; term; _ }::_ -> Log_failure {
          receiver_last_log_index = index;
          receiver_last_log_term = term;
        } 
      in
  
      let rec aux post_logs = function 
        | [] -> 
          if prev_log_index = 0
          then 
            (* [case 0] No previous log were ever inserted
             *)
            merge_log_entries state post_logs []
          else 
            (* [case 1] The prev_log_index is not found in the state log. 
             * This server is lagging behind. 
             *)
            (state, make_response state (make_log_failure_with_latest_log []))
  
        | ({index; term; _ }::tl as log) when index = prev_log_index && 
                                               term = prev_log_term -> 
          (* [case 2] The prev_log_index matches the leader, all is good, 
           * let's append all the new logs. 
           *)
          merge_log_entries state post_logs log 
  
        | {index; _ }::log when index = prev_log_index -> 
          (* [case 3] The prev_log_index is inconstent with the leader. 
           * This conflict is resolved by discarding it along with all 
           * following log entries. 
           * As far as the leader is concerned it's like [case 1] now. 
           *)
          let new_state = {state with log} in 
          (new_state, make_response state (make_log_failure_with_latest_log log))

        |  hd::tl -> aux (hd::post_logs) tl  

      in 
      match state.log with
      | {index; _}::tl when prev_log_index > index -> 
        (* 
         * This is the case when a new [Leader] which has more log entries
         * than this server sends a first [Append_entries_request]. It initializes 
         * its belief of thie server [last_log_index] to its own [last_log_index]. 
         * 
         * However this server does not have as many log entries. 
         *
         * In such a case, we send failure right away. 
         *)
        (state, make_response state (make_log_failure_with_latest_log state.log))

      | _ -> aux [] state.log 
  
  let handle_response state ({receiver_term; _ } as response) now = 
    
    let {receiver_term; receiver_id; result} = response in 
    
    if receiver_term > state.current_term
    then 
      (* Enforce invariant that if the server is lagging behind 
       * it must convert to a follower and update to that term.
       *)
      let state = Follower.become ~term:receiver_term ~now state in 
      (state, [])
  
    else 
      match state.role with
      | Follower _
      | Candidate _ -> (state, []) 

      | Leader leader_state ->

        let leader_state = 
          Leader.record_response_received ~server_id:receiver_id leader_state 
        in 

        begin match result with
        | Success {receiver_last_log_index} -> 
          (* Log entries were successfully inserted by the receiver... 
           *   
           * let's update our leader state about that receiver
           *)

          let configuration = state.configuration in 

          let leader_state, nb_of_replications = Leader.update_receiver_last_log_index 
            ~server_id:receiver_id 
            ~log_index:receiver_last_log_index 
            leader_state 
          in

          let commit_index = 
            (* 
             * Check if the received log entry from has reached 
             * a majority of server. 
             * Note that we need to add `+1` simply to count this 
             * server (ie leader) which does not update its next/match
             *
             *)
            if Configuration.is_majority configuration (nb_of_replications + 1) && 
               receiver_last_log_index > state.commit_index
            then receiver_last_log_index
            else state.commit_index
          in  
          let state = {state with commit_index} in 

          (* 
           * We now compute the next messages to send.
           *
           * First we see if the server that we just receive the response
           * from has missing entries. This is needed because we might have 
           * previously throttled the number of log sent due to the 
           * max_nb_message parameter in the configuration. 
           *
           *)
          let leader_state, req1 = 
            make_append_entries_for_server state leader_state receiver_id
          in 

          let msg1 = match req1.rev_log_entries with
            | [] -> []
            | _  -> [(Append_entries_request req1, receiver_id)]
          in 

          (* 
           * Second we check that if we others servers are past their 
           * heartbeat deadlines. 
           *
           * The reason we compute those requests here rather than 
           * waiting for a heartbeat timeout is that the server 
           * event loop might be continuously receiving responses from 
           * servers and the [Timeout] event never triggered. 
           *
           * Furthermore at this stage we know that we are a leader 
           * and therefore expected to send heartbeat requests.
           *
           *)
          let leader_state, msg2 = make_heartbeat_requests state leader_state now in 

          let msgs = msg1 @ msg2 in 

          let leader_state = 
            record_requests_sent configuration leader_state msgs now 
              (* 
               * We assume the msgs will be sent and therefore the 
               * heartbeat deadline of all servers which we send messages
               * to need to be updated.
               *
               *)
          in

          let state = {state with role = Leader leader_state} in

          (state, msgs)

      | Log_failure log_failure -> 
        (* 
         * The receiver log is not matching this server current belief. 
         * If a leader this server should decrement the next 
         * log index and retry the append. 
         *
         *)
        let configuration = state.configuration in 
        let leader_state = Leader.decrement_next_index ~log_failure ~server_id:receiver_id state leader_state in 
        let leader_state, req = make_append_entries_for_server state leader_state receiver_id in
        let msgs_to_send = [(Append_entries_request req, receiver_id)] in 
        let leader_state = record_requests_sent configuration leader_state msgs_to_send now in  
        let state  = {state with role = Leader leader_state} in 
        (state,msgs_to_send)

      | Term_failure  -> assert(false)
        (* 
         * This should have already been detected when comparing the receiver_term with 
         * the [state.current_term]. 
         *)
      
      end (* match result *) 


end (* Append_entries *)

module Message = struct

  let handle_message state message now = 
    match message with
    | Request_vote_request ({candidate_id; _ } as r) -> 
      let state, response = Request_vote.handle_request state r now in  
      (state, [(Request_vote_response response, candidate_id)])
    
    | Append_entries_request ({leader_id; _ } as r) -> 
      let state, response = Append_entries.handle_request state r now in  
      (state, [(Append_entries_response response, leader_id)])

    | Request_vote_response r ->
      Request_vote.handle_response state r now 
    
    | Append_entries_response r ->
      Append_entries.handle_response state r now 

  (* Iterates over all the other server ids. (ie the ones different 
   * from the state id).
   *)
  let fold_over_servers f e0 {id; configuration = {nb_of_server;_ }; _ } =  
    let rec aux acc = function 
      | -1 -> acc 
      | server_id  -> 
        let next = server_id - 1 in 
        if server_id = id 
        then aux acc next 
        else aux (f acc server_id) next 
    in
    aux e0 (nb_of_server - 1)

  let request_vote_for_all ({id; configuration = {nb_of_server;_ }; _ } as state) = 
    fold_over_servers (fun acc server_id ->
      let request = Request_vote.make state  in 
      (Request_vote_request request, server_id) :: acc
    ) [] state 
  
  let handle_new_election_timeout state now = 
    let state = Raft_helper.Candidate.become ~now state in 
    let msgs  = request_vote_for_all state in 
    (state, msgs)
  
  let handle_heartbeat_timeout ({role; configuration; _ } as state) now = 
    match state.role with
    | Leader leader_state -> 
      let leader_state, msgs_to_send = make_heartbeat_requests state leader_state now in 
      let leader_state = record_requests_sent 
        configuration leader_state msgs_to_send now 
      in 
      let state = {state with role = Leader leader_state} in 
      (state, msgs_to_send)
    | _ -> 
      (state, [])

  type new_log_response = 
    | Appended of Raft_pb.state * message_to_send list
    | Forward_to_leader of int 
    | Delay 

  let handle_add_log_entries state datas now = 
    match state.role with
    | Follower {current_leader = None ; _ }
    | Candidate _ -> 
      Delay 
      (* Server in the middle of an election with no [Leader] 
       * agreed upon yet 
       *)
    
    | Follower {current_leader = Some leader_id; _ } -> 
      Forward_to_leader leader_id 
      (* The [Leader] should be the one centralizing all the 
       * new log entries. 
       *)

    | Leader _ -> 
      let state = Leader.add_logs datas state in 
      begin match state.role with
      | Follower  _ | Candidate _ -> assert(false) 
        (* We don't expect the [Leader.add_log] functions to 
         * change the role. 
         *)

      | Leader ({indices} as leader_state) -> 

        let leader_state, msgs_to_send = List.fold_left (fun (leader_state, msgs) receiver_connection -> 
          let {
            server_id; 
            outstanding_request; _ } = receiver_connection
          in 

          if outstanding_request
          then (leader_state, msgs)
            (* We do not send new log entries to server which already 
             * have requests sent to them. 
             *)

          else 
            let leader_state, req = make_append_entries_for_server state leader_state server_id in
            let msgs = (Append_entries_request req, server_id)::msgs in
            (leader_state, msgs) 

        ) (leader_state, []) indices in 

        let leader_state = record_requests_sent 
          state.configuration leader_state msgs_to_send now 
        in 
        let state = {state with role = Leader leader_state } in 
        Appended (state, msgs_to_send) 

      end (* match state.role *) 

end (* Message *)  
