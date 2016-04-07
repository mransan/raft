
open Raft_pb

module State            = Raft_helper.State
module Follower         = Raft_helper.Follower
module Candidate        = Raft_helper.Candidate
module Leader           = Raft_helper.Leader
module Configuration    = Raft_helper.Configuration
module Follow_up_action = Raft_helper.Follow_up_action 

type time = float 

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
        then Follower.become ~term:candidate_term state 
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
        | Follower {voted_for = None} ->
          (* This server has never voted before, candidate is getting the vote
           *)
          
          let state ={state with 
            role = Follower {
              voted_for = Some candidate_id; 
              current_leader = None;
            } 
          } in  
          (state, make_response state true, Follow_up_action.new_election_wait state)

        | Follower {voted_for = Some id} when id = candidate_id -> 
          (* This server has already voted for that candidate ... reminding him
           *)
          (state, make_response state true, Follow_up_action.new_election_wait state)

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
      let state = Follower.become ~term:voter_term state in 
      (state, Follow_up_action.new_election_wait state) 
    else 
  
      match role, vote_granted  with
      | Candidate ({vote_count; election_deadline} as candidate_state) , true -> 
        let has_majority = vote_count >= (configuration.nb_of_server / 2) in 
        if  has_majority
        then 
          (* Candidate is the new leader
           *)
          (Leader.become state now, Act_as_new_leader)  
        else 
          (* Candidate has a new vote but not yet reached the majority
           *)
          let new_state = {state with 
            role = Candidate (Candidate.increment_vote_count candidate_state);
          } in 
          (new_state, Follow_up_action.existing_election_wait election_deadline now) 

      | Candidate {election_deadline; _}, false ->
        (state, Follow_up_action.existing_election_wait election_deadline now)
        (* The vote was denied, the election keeps on going until
         * its deadline. 
         *)

      | Follower _ , _ -> (state, Follow_up_action.new_election_wait state) 
      | Leader   _ , _ -> (state, Follow_up_action.default state now)
        (* If the server is either Follower or Leader, it means that 
         * it has changed role in between the time it sent the 
         * [RequestVote] request and this response. 
         * This response can therefore safely be ignored and the server 
         * keeps its current state.
         *)

end (* Request_vote *)

(** {2 Append Entries} *) 

module Append_entries = struct 
  
  (* Helper function to collect all the log entries
   * up until (and including) the log with given 
   * index. 
   *)
  let collect_log_since_index last_index log = 
    let rec aux log_entries = function
       | [] -> 
         if last_index = 0
         then (0, List.rev log_entries) 
         else failwith "[Raft_logic] Internal error invalid log index"

       | ({index;term;data = _ } as entry)::tl -> 
         if index = last_index 
         then (term, List.rev log_entries)
         else aux (entry::log_entries) tl  
    in 
    aux [] log 

  let make state receiver_id = 

    match state.role with
    | Leader {next_index; match_index = _ } -> ( 
      
      let is_receiver ({server_id; _ }:server_index) = 
        server_id = receiver_id
      in 
      match List.find is_receiver next_index with
      | {server_log_index; _ } -> (

        let prev_log_index = server_log_index - 1 in 

        let (prev_log_term, log_entries) = 
          collect_log_since_index prev_log_index state.log in  

        Some {
          leader_term = state.current_term;
          leader_id = state.id;
          prev_log_index; 
          prev_log_term;
          log_entries;
          leader_commit = state.commit_index;
        }
      ) 
      | exception Not_found -> 
        failwith "[Raft_logic] Invalid receiver id"
    )
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
      let state  = Follower.become ~current_leader:leader_id ~term:leader_term state in  
      let new_election_wait_action = Follow_up_action.new_election_wait state in 

      (* Next step is to handle the log entries from the leader.
       * 
       * The algorithm search for the [prev log entry] that the leader 
       * sent. 
       *)
      let {prev_log_index; prev_log_term; log_entries; leader_commit} = request in 
  
      let module Helper = struct 
        
        (* This functions merges the request log entries with the 
         * current log of the server. The current log is first split by 
         * the caller into: 
         *
         * - [pre_logs] all the logs prior to (and including) [prev_log_index].
         *
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

          let rev_log_entries = List.rev log_entries in

          let rec aux log = function
            | [], [] -> log 
            | ({index=i1;term=t1;data} as e)::tl1, {index=i2;term=t2; _ }::tl2 -> 
              if i1 = i2 && t1 = t2 
              then aux (e::log) (tl1, tl2)
              else aux (e::log) (tl1, []) 
            | hd::tl, []
            | [], hd::tl -> aux (hd::log) (tl, []) 
          in 

          let state = {state with 
            log = aux pre_logs (rev_log_entries, rev_post_logs)
          } in 
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
  
      end (* Helper *) in 
  
      let rec aux post_logs = function 
        | [] -> 
          if prev_log_index = 0
          then 
            (* [case 0] No previous log were ever inserted
             *)
            Helper.merge_log_entries state post_logs []
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
          Helper.merge_log_entries state post_logs log 
  
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
      let state = Follower.become ~term:receiver_term state in 
      (state, Follow_up_action.new_election_wait state) 
  
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

          let leader_state, min_heartbeat_timeout = Leader.update_receiver_deadline 
            ~server_id:receiver_id
            ~now 
            ~configuration
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
          let action = Follow_up_action.make_heartbeat_wait min_heartbeat_timeout in 
          (state, action)

        | _ -> (state, Follow_up_action.default state now)

        end (* match state.role *)

      | Failure ->
        (* The receiver log is not matching this server current belief. 
         * If a leader this server should decrement the next 
         * log index and retry the append. 
         *)
        begin match state.role with
        | Leader leader_state ->
          let leader_state = Leader.decrement_next_index ~server_id:receiver_id leader_state in 
          let state = {state with role = Leader leader_state} in 
          let action = Retry_append {server_id = receiver_id} in  
          (state, action)
        | _ ->
          (state, Follow_up_action.default state now)
        end 

end (* Append_entries *)
