open Raft_pb

module State = struct 

  let last_log_index_and_term {log; _ } = 
    match log with
    | {index;term}::_ -> (index, term) 
    | [] -> (0, 0)

  let last_log_index state = 
    fst @@ last_log_index_and_term state
  
  let is_follower {role; _} = 
    match role with 
    | Follower _ -> true 
    | _ -> false 

  let is_candidate {role; _ } = 
    match role with 
    | Candidate _ -> true 
    | _ -> false 
  
  let is_leader {role; _ } = 
    match role with 
    | Leader _ -> true 
    | _ -> false 

end 

module Follower = struct 

  let create ?current_leader ?current_term:(current_term = 0) ?voted_for ?log:(log = []) ~configuration ~now ~id () = 
    let {election_timeout = t ; election_timeout_range = r; _ } = configuration in 
    let timeout = t +. (Random.float r -. (r /. 2.)) in
    {
      id; 
      current_term; 
      log;
      log_size = List.length log;
      commit_index = 0; 
      role = Follower {
        voted_for; 
        current_leader; 
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

    let last_log_index = State.last_log_index state in
    
    let {nb_of_server; hearbeat_timeout} = state.configuration in 
  
    let rec aux ((next_index, match_index, receiver_connections) as acc) = function
      | (-1) -> acc
      |  i   -> 
        if i = state.id
        then 
          aux (next_index, match_index, receiver_connections) (i -1)
        else 
          let next      = {server_id = i; server_log_index = last_log_index + 1} in
          let match_    = {server_id = i; server_log_index = 0 } in 
          let connection= {
            server_id = i; 
            heartbeat_deadline = now +. hearbeat_timeout;
            outstanding_request = false;
          } in 
            (* 
             * Here the expectation is that after becoming a leader
             * the client application will send a message to all the receivers
             * and therefore the heartbeat_deadline is set 
             * to [now + timeout] rather than [now].
             *
             *)
          let next_index = next :: next_index in 
          let match_index = match_ :: match_index  in 
          let receiver_connections = connection :: receiver_connections in
          aux (next_index, match_index, receiver_connections) (i - 1)
    in 
    let next_index, match_index, receiver_connections = aux ([], [], []) (nb_of_server - 1) in 
    
    {state with 
      role = Leader {
        next_index; 
        match_index; 
        receiver_connections;
     }}


  let find_server_log_index server_index receiver_id = 
    let is_server = fun ({server_id; _ }:server_index) -> 
      server_id = receiver_id
    in 
    begin match List.find is_server server_index with
    | {server_log_index; _ } -> Some server_log_index
    | exception Not_found -> None 
    end 

  let next_index_for_receiver {role; _ } receiver_id = 
    match role with
    | Leader {next_index; _ } -> find_server_log_index next_index receiver_id 
    | _ -> None 
  
  let match_index_for_receiver {role; _ } receiver_id = 
    match role with
    | Leader {match_index; _ } -> find_server_log_index match_index receiver_id 
    | _ -> None 

  let add_log data state = 
    let last_log_index = State.last_log_index state in
    {state with 
      log = {
        index = last_log_index + 1;
        term = state.current_term; 
        data; 
      }::state.log;
      log_size = state.log_size + 1;
    }
   
  let update_receiver_last_log_index ~server_id ~log_index leader_state = 
    let receiver_id = server_id in 
    if log_index = 0
    then (leader_state, 0)
    else 
      let {next_index; match_index; } = leader_state in 
      let update_server_index_value server_index v = 
        List.map (fun ({server_id; server_log_index = _ } as s) -> 
          if server_id = receiver_id 
          then {s with server_log_index = v}
          else s  
        ) server_index 
      in 

      let next_index = update_server_index_value next_index (log_index + 1) in 
      let match_index = update_server_index_value match_index log_index in 

      (* Calculate the number of server which also have replicated that 
         log entry
       *)
      let nb_of_replications = List.fold_left (fun n {server_log_index; _ } -> 
        if server_log_index >= log_index 
        then n + 1 
        else n
      ) 0 match_index in 
      
      ({leader_state with next_index; match_index}, nb_of_replications) 


  let update_receiver_connection ~receiver_id ~f leader_state = 
    let {receiver_connections;_ } = leader_state in 

    let receiver_connections = List.fold_left (fun acc receiver_connection -> 
      if receiver_connection.server_id = receiver_id
      then (f receiver_connection)::acc
      else receiver_connection::acc
    ) []  receiver_connections in

    {leader_state with receiver_connections}

  let record_request_sent ~server_id ~now ~configuration leader_state = 

    update_receiver_connection 
      ~receiver_id:server_id 
      ~f:(fun receiver_connection ->
        {receiver_connection with
          heartbeat_deadline =  configuration.hearbeat_timeout +. now; 
          outstanding_request = true;
        }
      ) 
      leader_state

  let record_response_received ~server_id leader_state = 
    
    update_receiver_connection 
      ~receiver_id:server_id 
      ~f:(fun receiver_connection ->
        
        {receiver_connection with outstanding_request = false;}
      ) 
      leader_state
    
  let min_heartbeat_timout ~now {receiver_connections; _ } = 

    let min_heartbeat_deadline = List.fold_left (fun min_deadline {heartbeat_deadline; _ } -> 
        if heartbeat_deadline < min_deadline
        then heartbeat_deadline
        else min_deadline
      ) max_float receiver_connections
    in 
    min_heartbeat_deadline -. now 

  let decrement_next_index ~server_id ({next_index; _ } as leader_state) = 
    let receiver_id = server_id in 
    let next_index = List.map (fun ({server_id; server_log_index; } as s) -> 
      if server_id = receiver_id
      then {s with server_log_index = server_log_index - 1} 
      else s
    ) next_index in
    {leader_state with next_index }

end 

module Configuration = struct

  let is_majority {nb_of_server; _} nb = 
    nb > (nb_of_server / 2) 

end 

module Timeout_event = struct

  let existing_election_wait election_deadline now = 
    {
      timeout      = election_deadline -. now; 
      timeout_type = New_leader_election; 
    }
  
  let make_heartbeat_wait timeout = 
    {
      timeout = timeout; 
      timeout_type = Heartbeat;
    }

  let next state now = 
    match state.role with
    | Follower {election_deadline; _} ->
      existing_election_wait election_deadline  now  
    | Leader leader_state -> 
      make_heartbeat_wait (Leader.min_heartbeat_timout ~now leader_state)
    | Candidate {election_deadline; _ } -> 
      existing_election_wait election_deadline  now  

end 
