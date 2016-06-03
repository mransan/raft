open Raft_pb 
open Raft_state

module Log= Raft_log
module State = Raft_state

module Follower = struct 

  let create ~configuration ~now ~id () = 
    let {election_timeout = t ; election_timeout_range = r; _ } = configuration in 
    let timeout = t +. (Random.float r -. (r /. 2.)) in
    {
      id; 
      current_term = 0; 
      log = Log.empty;
      commit_index = 0; 
      role = Follower {
        voted_for = None; 
        current_leader = None;
        election_deadline = now +.  timeout 
      };  
      configuration; 
    }

  let become ?current_leader ~term ~now state = 
    let {configuration = {election_timeout = t; election_timeout_range = r; _}; _} = state in 
    let election_deadline  = now +. t +. (Random.float r -. (r /. 2.)) in

    let role = match state.role with
      | Follower follower_state -> Follower {follower_state with
        current_leader; 
        election_deadline;
      }
      | Candidate _ when state.current_term = term -> 
        Follower {
          voted_for = Some state.id;
          current_leader; 
          election_deadline; 
        }
      | _ -> Follower {
        voted_for = None;
        current_leader; 
        election_deadline;
      }
    in 
    { state with current_term = term; role } 

end 

module Candidate = struct 

  let become ~now state = 
    let {election_timeout = t; election_timeout_range = r; _ } = state.configuration in 
    let timeout = t +. (Random.float r -. (r /. 2.)) in
    let candidate_state = {
      vote_count = 1; 
      election_deadline = now +. timeout;
    } in 
    {state with 
     role = Candidate candidate_state; 
     current_term = state.current_term + 1; 
    } 

  let increment_vote_count ({vote_count; _ } as candidate_state) = 
    {candidate_state with vote_count = vote_count + 1}

end 

module Leader = struct 

  let become state now = 

    let last_log_index = Log.last_log_index state.log in
    
    let {nb_of_server; hearbeat_timeout} = state.configuration in 
  
    let rec aux indices = function
      | (-1) -> indices
      |  i   -> 
        if i = state.id
        then 
          aux indices (i -1)
        else 
          let next_index = last_log_index + 1 in 
          let match_index = 0  in 
          let unsent_entries = [] in 
            (* The cache is expected to be empty... which is fine since it will
             * get filled at when a new log entry will be
             * added. 
             *)

          let index:server_index = {
            server_id = i; 
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
          aux (index::indices) (i - 1)
    in 
    let indices = aux [] (nb_of_server - 1) in 
    
    {state with role = Leader {indices}}


  (*
   * Reusable function to update the index of a particular
   * receiver id. 
   *)  
  let update_index ~receiver_id ~f leader_state = 

    let indices = List.map (fun index -> 
      if index.server_id = receiver_id
      then (f index)
      else index
    ) leader_state.indices in

    {indices}

  let update_receiver_last_log_index ~receiver_id ~log_index leader_state = 

    let leader_state = update_index ~receiver_id ~f:(fun index ->
      {index with next_index = log_index + 1; match_index = log_index}
    ) leader_state
    in  

    (* Calculate the number of server which also have replicated that 
       log entry
     *)
    let nb_of_replications = List.fold_left (fun n {match_index; _ } -> 
      if match_index >= log_index 
      then n + 1 
      else n
    ) 0 leader_state.indices in 
    
    (leader_state, nb_of_replications) 

  let decrement_next_index ~log_failure ~receiver_id state leader_state = 
    let {receiver_last_log_index} = log_failure in 

    let latest_log_index, latest_log_term = Log.last_log_index_and_term state.log  in 

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
       next_index = receiver_last_log_index + 1; 
       match_index = receiver_last_log_index}
    )  leader_state

  let record_response_received ~receiver_id leader_state = 
    
    update_index 
      ~receiver_id
      ~f:(fun index ->
        {index with outstanding_request = false;}
      ) 
      leader_state
    
  let min_heartbeat_timout ~now {indices} = 

    let min_heartbeat_deadline = List.fold_left (fun min_deadline {heartbeat_deadline; _ } -> 
        if heartbeat_deadline < min_deadline
        then heartbeat_deadline
        else min_deadline
      ) max_float indices
    in 
    min_heartbeat_deadline -. now 

end (* Leader *) 
