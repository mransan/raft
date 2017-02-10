open Raft_pb 

module Log= Raft_log
module State = Raft_state

module Follower = struct 

  let create ~configuration ~now ~id () = 
    let {election_timeout = t ; election_timeout_range = r; _ } = configuration in 
    let timeout = t +. (Random.float r -. (r /. 2.)) in
    {
      State.current_term = 0; 
      id; 
      log = Log.empty;
      commit_index = 0; 
      role = State.(Follower {
        voted_for = None; 
        current_leader = None;
        election_deadline = now +.  timeout 
      });  
      configuration; 
    }

  let become ?current_leader ~term ~now state = 
    let {State.configuration = {election_timeout = t; election_timeout_range = r; _}; _} = state in 
    let election_deadline  = now +. t +. (Random.float r -. (r /. 2.)) in

    let role = match state.State.role with
      | State.Follower follower_state -> 
        let voted_for = 
          if term = state.State.current_term 
          then follower_state.State.voted_for 
          else None 
        in 
        State.Follower {
          State.current_leader; 
          State.election_deadline;
          State.voted_for;
        }
      | State.Candidate _ when state.State.current_term = term -> 
        State.Follower {
          State.voted_for = Some state.State.id;
          State.current_leader; 
          State.election_deadline; 
        }
      | _ -> State.Follower {
        State.voted_for = None;
        State.current_leader; 
        State.election_deadline;
      }
    in 
    { state with State.current_term = term; role } 

end 

module Candidate = struct 

  let become ~now state = 
    let {election_timeout = t; election_timeout_range = r; _ } = state.State.configuration in 
    let timeout = t +. (Random.float r -. (r /. 2.)) in
    let role = State.(Candidate {
      vote_count = 1; 
      election_deadline = now +. timeout;
    }) in 
    {state with 
     State.role;
     State.current_term = state.State.current_term + 1; 
    } 

  let increment_vote_count ({State.vote_count; _ } as candidate_state) = 
    {candidate_state with State.vote_count = vote_count + 1}

end 

module Leader = struct 

  let become state now = 

    let last_log_index = Log.last_log_index state.State.log in
    
    let {nb_of_server; hearbeat_timeout; _} = state.State.configuration in 
  
    let rec aux followers = function
      | (-1) -> followers
      |  i   -> 
        if i = state.State.id
        then 
          aux followers (i -1)
        else 
          let next_index = last_log_index + 1 in 
          let match_index = 0  in 
          let unsent_entries = [] in 
            (* The cache is expected to be empty... which is fine since it will
             * get filled at when a new log entry will be
             * added. 
             *)

          let follower:State.follower_info = {
            State.server_id = i; 
            next_index; 
            match_index; 
            unsent_entries; 
            outstanding_request = false;
            heartbeat_deadline = now +. hearbeat_timeout;
              (* 
               * Here the expectation is that after becoming a leader
               * the client application will send a message to all the receivers
               * and therefore the heartbeat_deadline is set 
               * to [now + timeout] rather than [now].
               *
               *)
          } in 
          aux (follower::followers) (i - 1)
    in 
    let followers = aux [] (nb_of_server - 1) in 
    
    {state with State.role = State.(Leader followers)}


  (*
   * Reusable function to update the index of a particular
   * receiver id. 
   *)  
  let update_index ~receiver_id ~f leader_state = 

    List.map (fun follower -> 
      if follower.State.server_id = receiver_id
      then (f follower)
      else follower
    ) leader_state 

  let update_receiver_last_log_index ~receiver_id ~log_index leader_state = 

    let leader_state = update_index ~receiver_id ~f:(fun index -> 
      let {State.next_index; match_index; _} = index in 
      let upd_next_index = log_index + 1 in 
      let upd_match_index = log_index in 
      if upd_match_index > match_index && upd_next_index > next_index
      then 
        {index with State.next_index = log_index + 1; match_index = log_index}
      else
        (*
         * It is possible to receive out of order responses from the other 
         * raft servers. 
         *
         * In such a case we don't want to decrement the next index of the server
         * since the server is expected to never remove previously saved log entries. 
         *) 
        index
    ) leader_state
    in  

    (* Calculate the number of server which also have replicated that 
       log entry
     *)
    let nb_of_replications = List.fold_left (fun n {State.match_index; _ } -> 
      if match_index >= log_index 
      then n + 1 
      else n
    ) 0 leader_state in
    
    (leader_state, nb_of_replications) 

  let decrement_next_index ~log_failure ~receiver_id state leader_state = 
    let {receiver_last_log_index; _ }= log_failure in 

    let latest_log_index = Log.last_log_index state.State.log  in 

    assert(receiver_last_log_index <= latest_log_index);  
      (* 
       * This is an invariant. The server cannot have replicated more logs
       * than the Leader.
       * 
       * However due to message re-ordering it is possible to receive a [LogFailure] 
       * with the receiver_last_log_index equal to the latest_log_index. 
       *
       * Consider the following scenario
       * - [Leader] Append_entry prev_index = x rev_log_entries [x+1]
       * - [Server] receives the request and correctly replicate x + 1 
       * - !! RESPONSE IS LOST !!
       * - [Leader] Append_entry prev_index = x rev_log_entries [x+1]
       * - [Server] return a failure since it has replicated x + 1 and cannot 
       *   remove that log entry since it is coming from the current term leader.
       *)
    update_index ~receiver_id ~f:(fun index -> 
      {index with 
       State.next_index = receiver_last_log_index + 1; 
       State.match_index = receiver_last_log_index}
    )  leader_state

  let record_response_received ~receiver_id leader_state = 
    
    update_index 
      ~receiver_id
      ~f:(fun index ->
        {index with State.outstanding_request = false;}
      ) 
      leader_state
    
  let min_heartbeat_timout ~now followers = 

    let min_heartbeat_deadline = List.fold_left (fun min_deadline f -> 
      let {State.heartbeat_deadline; _ } = f in
        if heartbeat_deadline < min_deadline
        then heartbeat_deadline
        else min_deadline
      ) max_float followers
    in 
    min_heartbeat_deadline -. now 

end (* Leader *) 
