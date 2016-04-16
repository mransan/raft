
open Raft_pb

module State            = Raft_helper.State
module Follower         = Raft_helper.Follower
module Candidate        = Raft_helper.Candidate
module Leader           = Raft_helper.Leader
module Configuration    = Raft_helper.Configuration
module Follow_up_action = Raft_helper.Follow_up_action 

type time = float 
  
type message_to_send = Raft_pb.message * int 
  
(* Helper function to collect all the log entries
 * up until (and including) the log with given 
 * index. 
 *)
let collect_log_since_index last_index log max_nb_message = 

  let rec keep_first_n l = function
    | 0 -> []
    | n -> 
      begin match l with 
      | hd::tl -> hd::(keep_first_n tl (n - 1))
      | [] -> assert(false)
      end
  in 

  (*
   * In the case of a last_index because a lot lower than the 
   * latest log index then the linear operation 
   * below is really expensive. 
   * 
   * TODO
   * An optimization could be to keep track for each server of that
   * [reverse log entries] value and only use the actual log value
   * when we are done sending all the reverse log entries.
   *
   *)

  let rec aux count rev_log_entries = function
    | [] -> 
      if last_index = 0
      then 
        if count > max_nb_message
        then
          (0, keep_first_n rev_log_entries max_nb_message) 
        else
          (0, rev_log_entries)
      else 
        failwith "[Raft_logic] Internal error invalid log index"

    | ({index;term;data = _ } as entry)::tl -> 
      if index = last_index 
      then 
        if count > max_nb_message
        then
          (term, keep_first_n rev_log_entries max_nb_message) 
        else 
          (term, rev_log_entries)
      else 
        aux (count + 1) (entry::rev_log_entries) tl  
  in 
  aux 0 [] log 

let make_append_entries prev_log_index state =  
  let max_nb_message = state.configuration.max_nb_message in
  let (prev_log_term, rev_log_entries) = 
    collect_log_since_index prev_log_index state.log max_nb_message in  

  {
    leader_term = state.current_term;
    leader_id = state.id;
    prev_log_index; 
    prev_log_term;
    rev_log_entries;
    leader_commit = state.commit_index;
  }

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
      (* This request is coming from a candidate lagging behind ... 
       * no vote for him.
       *)
      (state, make_response state false, Follow_up_action.default state now)
    else 
      let state = 
        (* Enforce invariant that if this server is lagging behind 
         * it must convert to a follower and update to that term.
         *)
        if candidate_term > state.current_term
        then Follower.become ~term:candidate_term ~now state 
        else state
      in
      let last_log_index = State.last_log_index state in 
      if candidate_last_log_index < last_log_index
      then 
        (* Enforce the safety constraint by denying vote if this server
         * last log is more recent than the candidate one.
         *)
        (state, make_response state false, Follow_up_action.default state now)
      else
        let role = state.role in 
        match role with
        | Follower {voted_for = None; _} ->
          (* This server has never voted before, candidate is getting the vote
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
          (state, make_response state true, Follow_up_action.default state now)
           

        | Follower {voted_for = Some id} when id = candidate_id -> 
          (* This server has already voted for that candidate ... reminding him
           *)
          (state, make_response state true, Follow_up_action.default state now)

        | _ -> 
          (* Server has previously voted for another candidate or 
           * itself
           *)
          (state, make_response state false, Follow_up_action.default state now) 
  
  let handle_response state response now = 
  
    let {current_term; role; configuration; _ } = state in 
    let {voter_term; vote_granted} = response in 

    if voter_term > current_term 
    then 
      (* Enforce invariant that if this server is lagging behind 
       * it must convert to a follower and update to the latest term.
       *)
      let state = Follower.become ~term:voter_term ~now state in 
      (state, [], Follow_up_action.default state now) 
    else 
  
      match role, vote_granted  with
      | Candidate ({vote_count; election_deadline} as candidate_state) , true -> 
        let has_majority = vote_count >= (configuration.nb_of_server / 2) in 
        if  has_majority
        then 
          (* Candidate is the new leader
           *)
          let state = Leader.become state now in 
          
          (* As a new leader, the server must send Append entries request
             to the other servers to both establish its leadership and
             start synching its log with the others. 
           *)
          let msgs = begin match state.role with
            | Leader {next_index; _ } -> (
              List.map (fun {server_id; server_log_index} ->
                
                let prev_log_index = server_log_index - 1 in 
                let req = make_append_entries prev_log_index state in 
                let msg = Append_entries_request req in 
                (msg, server_id)
              ) next_index 
            )
            | _ -> failwith "Programmatic error" 
          end 
          in

          let hearbeat_timeout = configuration.hearbeat_timeout in 
          let action = Follow_up_action.make_heartbeat_wait hearbeat_timeout in 

          (state, msgs, action)  
        else 
          (* Candidate has a new vote but not yet reached the majority
           *)
          let new_state = {state with 
            role = Candidate (Candidate.increment_vote_count candidate_state);
          } in 
          (new_state, [], Follow_up_action.existing_election_wait election_deadline now) 

      | Candidate {election_deadline; _}, false ->
        (state, [], Follow_up_action.existing_election_wait election_deadline now)
        (* The vote was denied, the election keeps on going until
         * its deadline. 
         *)

      | Follower _ , _ 
      | Leader   _ , _ -> (state, [], Follow_up_action.default state now)
        (* If the server is either Follower or Leader, it means that 
         * it has changed role in between the time it sent the 
         * [RequestVote] request and this response. 
         * This response can therefore safely be ignored and the server 
         * keeps its current state.
         *)

end (* Request_vote *)

(** {2 Append Entries} *) 

module Append_entries = struct 
  

  let make_of_leader_state state {next_index; _ } receiver_id = 

    let is_receiver ({server_id; _ }:server_index) = 
      server_id = receiver_id
    in 

    match List.find is_receiver next_index with
    | {server_log_index; _ } -> (
      let prev_log_index = server_log_index - 1 in 
      make_append_entries prev_log_index state 
    ) 
    | exception Not_found -> 
      failwith "[Raft_logic] Invalid receiver id"

  let make state receiver_id = 
    match state.role with
    | Leader leader_state -> Some (make_of_leader_state state leader_state receiver_id) 
    | _ -> None 

  let handle_request state request now = 
    
    let {leader_term; leader_commit; leader_id;} = request in  
    
    let make_response state result = 
      {receiver_term = state.current_term; receiver_id = state.id; result;}
    in 
    
    
    if leader_term < state.current_term 
    then 
      (* This request is coming from a leader lagging behind...
       *)
      
      (state, make_response state Failure, Follow_up_action.default state now)
  
    else 
      (* This request is coming from a legetimate leader, 
       * let's ensure that this server is a follower.
       *)
      let state  = Follower.become ~current_leader:leader_id ~term:leader_term ~now state in  
      let new_election_wait_action = Follow_up_action.default state now in 

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
        (state, response, new_election_wait_action)
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
            (state, make_response state Failure, new_election_wait_action) 
  
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
          (new_state, make_response state Failure, new_election_wait_action)
        |  hd::tl -> aux (hd::post_logs) tl 
  
      in 
      aux [] state.log 
  
  let handle_response state ({receiver_term; _ } as response) now = 
    
    let {receiver_term; receiver_id; result} = response in 
    
    if receiver_term > state.current_term
    then 
      (* Enforce invariant that if the server is lagging behind 
       * it must convert to a follower and update to that term.
       *)
      let state = Follower.become ~term:receiver_term ~now state in 
      (state, [], Follow_up_action.default state now) 
  
    else 
      match result with
      | Success {receiver_last_log_index} -> 
        (* Log entries were successfully inserted by the receiver... 
         * let's update our leader state about that receiver
         *)

        begin match state.role with
        | Leader leader_state ->

          let configuration = state.configuration in 

          let leader_state, nb = Leader.update_receiver_last_log_index 
            ~server_id:receiver_id 
            ~log_index:receiver_last_log_index 
            leader_state 
          in


          let commit_index = 
            (* Check if the received log entry from has reached 
             * a majority of server. 
             * Note that we need to add `+1` simply to count this 
             * server (ie leader) which does not update its next/match
             *)
            if Configuration.is_majority configuration (nb + 1)
            then receiver_last_log_index
            else state.commit_index
          in  
          
          let state = {state with commit_index; role = Leader leader_state} in
          let req = make_append_entries receiver_last_log_index state in 

          let msg = match req.rev_log_entries with
            | [] -> []
            | _  -> [(Append_entries_request req, receiver_id)]
          in 

          let action = Follow_up_action.default state now in 
          (state, msg, action)

        | _ -> (state, [], Follow_up_action.default state now)

        end (* match state.role *)

      | Failure ->
        (* The receiver log is not matching this server current belief. 
         * If a leader this server should decrement the next 
         * log index and retry the append. 
         *)
        begin match state.role with
        | Leader leader_state ->
          let leader_state = Leader.decrement_next_index ~server_id:receiver_id leader_state in 
          let state  = {state with role = Leader leader_state} in 
          let msg    = Append_entries_request (
            make_of_leader_state state leader_state receiver_id
          ) in 
          let action = Follow_up_action.default state now in 
          (state, [(msg, receiver_id)], action)
        | _ ->
          (state, [], Follow_up_action.default state now)
        end 

end (* Append_entries *)

module Message = struct


  let handle_message state message now = 
    match message with
    | Request_vote_request ({candidate_id; _ } as r) -> 
      let state, response, action = Request_vote.handle_request state r now in  
      (state, [(Request_vote_response response, candidate_id)], action)
    
    | Append_entries_request ({leader_id; _ } as r) -> 
      let state, response, action = Append_entries.handle_request state r now in  
      (state, [(Append_entries_response response, leader_id)], action) 

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

  let append_entries_request_for_all ({id; configuration = {nb_of_server;_ }; _ } as state) = 
    fold_over_servers (fun acc server_id -> 
      match Append_entries.make state server_id with
      | Some request -> ((Append_entries_request request, server_id) ::acc) 
      | None         -> acc
    ) [] state 

  let request_vote_for_all ({id; configuration = {nb_of_server;_ }; _ } as state) = 
    fold_over_servers (fun acc server_id ->
      let request = Request_vote.make state  in 
      (Request_vote_request request, server_id) :: acc
    ) [] state 
  
  let handle_new_election_timeout state now = 
    let state = Raft_helper.Candidate.become ~now state in 
    let msgs  = request_vote_for_all state in 
    let action = Follow_up_action.default state now in 
    (state, msgs, action)
  
  let handle_heartbeat_timeout ({role; configuration; _ } as state) now = 
    match state.role with
    | Leader leader_state -> 
      let msgs = append_entries_request_for_all state in 
      let leader_state = List.fold_left (fun leader_state (_, server_id) -> 
        fst @@ Raft_helper.Leader.update_receiver_deadline 
            ~server_id ~now ~configuration leader_state
      ) leader_state msgs
      in 
      let hearbeat_timeout = configuration.hearbeat_timeout in 
      let action = Follow_up_action.make_heartbeat_wait hearbeat_timeout in 
      let state = {state with role = Leader leader_state} in 
      (state, msgs, action) 
    | _ -> 
      (state, [], Follow_up_action.default state now)
        
end (* Message *)  
