
open Raft_pb

module Follower  = Raft_helper.Follower
module Candidate = Raft_helper.Candidate
module Leader    = Raft_helper.Leader


(** {2 Request Vote} *) 

module Request_vote = struct 

  let handle_request state {candidate_id; candidate_term} = 
  
    let make_response state vote_granted = 
      {voter_term = state.current_term; voter_id = state.id ; vote_granted; }
    in 
  
    if candidate_term < state.current_term 
    then 
      (* This request is coming from a candidate lagging behind ... no vote for him.
       *)
      (state, make_response state false)
    else 
      let state = 
        (* Enforce invariant that if this server is lagging behind 
           it must convert to a follower and update to that term.
         *)
        if candidate_term > state.current_term
        then Follower.make state candidate_term  
        else state
      in
      let role = state.role in 
      match role with
      | Follower {voted_for = None} ->
        (* This server has never voted before, candidate is getting the vote
         *)
        ({state with role = Follower {voted_for = Some candidate_id}}, make_response state true) 
      | Follower {voted_for = Some id} when id = candidate_id -> 
        (* This server has already voted for that candidate ... reminding him
         *)
        (state, make_response state true)
      | _ -> 
        (* Server has previously voted for another candidate or 
           itself
         *)
        (state, make_response state false) 
  
  let handle_response state response = 
  
    let {current_term; role; configuration; _ } = state in 
    let {voter_term; vote_granted} = response in 

    if voter_term > current_term 
    then 
      (* Enforce invariant that if this server is lagging behind 
         it must convert to a follower and update to the latest term.
       *)
      (Follower.make state voter_term, Nothing_to_do) 
    else 
  
      match role, vote_granted  with
      | Candidate ({vote_count;_ } as candidate_state) , true -> 
        let has_majority = vote_count >= (configuration.nb_of_server / 2) in 
        if  has_majority
        then 
          (* Candidate is the new leader
           *)
          (Leader.make state, Act_as_new_leader)  
        else 
          (* Candidate has a new vote but not yet reached the majority
           *)
          let new_state = {state with 
            role = Candidate (Candidate.increment_vote_count candidate_state);
          } in 
          (new_state, Nothing_to_do) 
      | _ -> 
        (* The server has changed role in between the time it sent the 
           [RequestVote] request and this response. 
           This response can therefore safely be ignored and the server 
           keeps its current state.
         *)
        (state, Nothing_to_do) 

end (* Request_vote *)

(** {2 Append Entries} *) 

module Append_entries = struct 

  let handle_request state request = 
    
    let make_response state result = 
      {receiver_term = state.current_term; receiver_id = state.id; result; }
    in 
    
    if request.leader_term < state.current_term 
    then 
      (* This request is coming from a leader lagging behind...
       *)
      (state, make_response state Failure)
  
    else 
      (* This request is coming from a legetimate leader, 
         let's ensure that this server is a follower.
       *)
      let state = Follower.make state request.leader_term in  
  
      (* Next step is to handle the log entries from the leader.
         
         The algorithm search for the [prev log entry] tha the leader 
         sent. 
       *)
      let {prev_log_index; prev_log_term; log_entries; _} = request in 
  
      let module Helper = struct 
        
        let insert_log_entries state old =  
          let state = {state with log = log_entries @ old } in  
          let receiver_last_log_index = match state.log with
            | [] -> (-1) 
            | {index; _ }::_ -> index 
          in 
          (state, make_response state (Success {receiver_last_log_index}))
  
      end in 
  
      let rec aux = function 
        | [] -> 
          if prev_log_index = -1
          then 
            (* [case 0] No previous log were ever inserted
             *)
            Helper.insert_log_entries state []
          else 
            (* [case 1] The prev_log_index is not found in the state log. This server is lagging
               behind. 
             *)
            (state, make_response state Failure) 
  
        | ({index; term; _ }::tl as log) when index = prev_log_index && term = prev_log_term -> 
          (* [case 2] The prev_log_index matches the leader, all is good, let's append all the 
             new logs. 
           *)
           (* TODO implement the [leaderCommit] logic (item 5)
            *)
          Helper.insert_log_entries state log 
  
        | {index; _ }::log when index = prev_log_index -> 
          (* [case 3] The prev_log_index is inconstent with the leader. This conflict is resolved
             by discarding it along with all following log entries. As far as the leader is concerned
             it's like [case 1] now. 
           *)
          let new_state = {state with log} in 
          (new_state, make_response state Failure)
        |  _::tl -> aux tl 
  
      in 
      aux state.log 
  
  let handle_response state ({receiver_term; _ } as response) = 
    
    let {receiver_term; receiver_id; result} = response in 
    
    if receiver_term > state.current_term
    then 
      (* Enforce invariant that if the server is lagging behind 
         it must convert to a follower and update to that term.
       *)
      (Follower.make state receiver_term, Nothing_to_do) 
  
    else 
      match result with
      | Success {receiver_last_log_index} -> 
        (* Log entries were successfully inserted by the receiver... let's update
           our leader state about that receiver
         *)
        let new_state = Leader.update_receiver_last_log_index state receiver_id receiver_last_log_index in
        (new_state, Nothing_to_do)
  
      | Failure ->
        (* The receiver log is not matching this server current belief. As a leader
           this server should decrement the next log index and retry the append. 
         *)
        let new_state = Leader.decrement_next_index state receiver_id in 
        let next_action = 
          if Leader.is_leader state
          then Retry_append {server_id  = receiver_id} 
          else Nothing_to_do
        in 
        (new_state, next_action)
end (* Append_entries *)
