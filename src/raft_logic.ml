
open Raft_pb

module State     = Raft_helper.State
module Follower  = Raft_helper.Follower
module Candidate = Raft_helper.Candidate
module Leader    = Raft_helper.Leader
module Configuration = Raft_helper.Configuration

(** {2 Request Vote} *) 

module Request_vote = struct 
  
  let make state = 
    let index, term  = State.last_log_index_and_term state 
    in
    {
      candidate_term = state.current_term; 
      candidate_id = state.id; 
      candidate_last_log_index = index;
      candidate_last_log_term = term;
    }

  let handle_request state request =
  
    let {candidate_id; candidate_term; candidate_last_log_index;_} = request in 

    let make_response state vote_granted = 
      {voter_term = state.current_term; voter_id = state.id ; vote_granted; }
    in 
  
    if candidate_term < state.current_term 
    then 
      (* This request is coming from a candidate lagging behind ... 
         no vote for him.
       *)
      (state, make_response state false)
    else 
      let state = 
        (* Enforce invariant that if this server is lagging behind 
           it must convert to a follower and update to that term.
         *)
        if candidate_term > state.current_term
        then Follower.become ~term:candidate_term state 
        else state
      in
      let last_log_index = State.last_log_index state in 
      if candidate_last_log_index < last_log_index
      then 
        (* Enforce the safety constraint by denying vote if this server
           last log is more recent than the candidate one.
         *)
        (state, make_response state false)
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
          (state, make_response state true) 

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
      (Follower.become ~term:voter_term state, Nothing_to_do) 
    else 
  
      match role, vote_granted  with
      | Candidate ({vote_count;_ } as candidate_state) , true -> 
        let has_majority = vote_count >= (configuration.nb_of_server / 2) in 
        if  has_majority
        then 
          (* Candidate is the new leader
           *)
          (Leader.become state, Act_as_new_leader)  
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
  
  let make state receiver_id = 

    match state.role with
    | Leader {next_index; match_index = _ } -> ( 
      let {server_log_index;_ } = List.nth next_index receiver_id in 
      let prev_log_index = server_log_index - 1 in 

      let (prev_log_term, log_entries) = 
        let rec aux log_entries = function
           | [] -> 
             if prev_log_index = 0
             then (0, List.rev log_entries) 
             else failwith @@ Printf.sprintf 
               ("Invalid next index for receiver(%i) in server(%i), " ^^ 
                "prev_log_index(%i)") receiver_id state.id prev_log_index

           | ({index;term;data = _ } as entry)::tl -> 
             if index = prev_log_index
             then (term, List.rev log_entries)
             else aux (entry::log_entries) tl  
        in 
        aux [] state.log
      in

      Some {
        leader_term = state.current_term;
        leader_id = state.id;
        prev_log_index; 
        prev_log_term;
        log_entries;
        leader_commit = state.commit_index;
      }
    )
    | _ -> None 

  let handle_request state request = 
    
    let {leader_term; leader_commit; leader_id;} = request in  
    
    let make_response state result = 
      {receiver_term = state.current_term; receiver_id = state.id; result;}
    in 
    
    if leader_term < state.current_term 
    then 
      (* This request is coming from a leader lagging behind...
       *)
      (state, make_response state Failure)
  
    else 
      (* This request is coming from a legetimate leader, 
         let's ensure that this server is a follower.
       *)
      let state = Follower.become ~current_leader:leader_id ~term:leader_term state in  
  
      (* Next step is to handle the log entries from the leader.
         
         The algorithm search for the [prev log entry] that the leader 
         sent. 
       *)
      let {prev_log_index; prev_log_term; log_entries; leader_commit} = request in 
  
      let module Helper = struct 
        
        let insert_log_entries state old =  
          let state = {state with log = log_entries @ old } in  
          let receiver_last_log_index = State.last_log_index state in 
          let state = 
            (* Update this server commit index based on value sent from 
               the leader. 
               Note that it is not guaranteed that the leader has sent
               all the log until it commit index so the min value 
               is taken.
             *)
            if leader_commit > state.commit_index
            then {state with
              commit_index = min leader_commit receiver_last_log_index 
            } 
            else state   
          in 
          (state, make_response state (Success {receiver_last_log_index}))
  
      end in 
  
      let rec aux = function 
        | [] -> 
          if prev_log_index = 0
          then 
            (* [case 0] No previous log were ever inserted
             *)
            Helper.insert_log_entries state []
          else 
            (* [case 1] The prev_log_index is not found in the state log. 
               This server is lagging behind. 
             *)
            (state, make_response state Failure) 
  
        | ({index; term; _ }::tl as log) when index = prev_log_index && 
                                               term = prev_log_term -> 
          (* [case 2] The prev_log_index matches the leader, all is good, 
             let's append all the new logs. 
           *)
           (* TODO implement the [leaderCommit] logic (item 5)
            *)
          Helper.insert_log_entries state log 
  
        | {index; _ }::log when index = prev_log_index -> 
          (* [case 3] The prev_log_index is inconstent with the leader. 
             This conflict is resolved by discarding it along with all 
             following log entries. 
             As far as the leader is concerned it's like [case 1] now. 
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
      (Follower.become ~term:receiver_term state, Nothing_to_do) 
  
    else 
      match result with
      | Success {receiver_last_log_index} -> 
        (* Log entries were successfully inserted by the receiver... 
           let's update our leader state about that receiver
         *)
        begin match state.role with
        | Leader leader_state ->
          let leader_state, nb = Leader.update_receiver_last_log_index 
            ~server_id:receiver_id ~log_index:receiver_last_log_index leader_state 
          in
          let commit_index = 
            (* Check if the received log entry from has reached 
               a majority of server. 
               Note that we need to add `+1` simply to count this 
               server (ie leader) which does not update its next/match
             *)
            if Configuration.is_majority state.configuration (nb + 1)
            then receiver_last_log_index
            else state.commit_index
          in  
          
          ({state with commit_index; role = Leader leader_state}, Nothing_to_do)

        | _ -> (state, Nothing_to_do)
        end

      | Failure ->
        (* The receiver log is not matching this server current belief. 
           If a leader this server should decrement the next 
           log index and retry the append. 
         *)
        let new_state = Leader.decrement_next_index state receiver_id in 
        let next_action = 
          if State.is_leader state
          then Retry_append {server_id  = receiver_id} 
          else Nothing_to_do
        in 
        (new_state, next_action)

end (* Append_entries *)
