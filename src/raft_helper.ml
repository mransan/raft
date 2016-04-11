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

  let create ?current_leader ?current_term:(current_term = 0) ?voted_for ?log:(log = []) ~configuration ~id () = 
    {
      id; 
      current_term; 
      log;
      log_size = List.length log;
      commit_index = 0; 
      last_applied = 0; 
      role = Follower {voted_for; current_leader};  
      configuration; 
    }

  let become ?current_leader ~term state = { state with 
    role = Follower {
      voted_for = None; 
      current_leader; 
    }; 
    current_term = term; 
  }

end 

module Candidate = struct 

  let become ~now state = 
    let {election_timeout; _ } = state.configuration in 
    let candidate_state = {
      vote_count = 1; 
      election_deadline = now +. election_timeout
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
  
    let rec aux ((next_index, match_index, receiver_heartbeats) as acc) = function
      | (-1) -> acc
      |  i   -> 
        if i = state.id
        then 
          aux (next_index, match_index, receiver_heartbeats) (i -1)
        else 
          let next      = {server_id = i; server_log_index = last_log_index + 1} in
          let match_    = {server_id = i; server_log_index = 0 } in 
          let heartbeat = {server_id = i; heartbeat_deadline = now +. hearbeat_timeout} in 
          aux (next::next_index, match_::match_index, heartbeat::receiver_heartbeats) (i - 1)
    in 
    let next_index, match_index, receiver_heartbeats = aux ([], [], []) (nb_of_server - 1) in 
    
    {state with role = Leader {next_index; match_index; receiver_heartbeats}}


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

  let update_receiver_deadline ~server_id ~now ~configuration leader_state = 

    let {receiver_heartbeats;_ } = leader_state in 
    let receiver_id = server_id in 

    let (receiver_heartbeats, min_deadline) = List.fold_left (fun  acc receiver_heartbeat -> 

      let receiver_heartbeats, min_deadline = acc in 
      let {heartbeat_deadline; server_id}  = receiver_heartbeat in 

      let receiver_heartbeat = 
        if server_id = receiver_id
        then {receiver_heartbeat with heartbeat_deadline = configuration.hearbeat_timeout +. now} 
        else  receiver_heartbeat 
      in 
      
      let min_deadline = min min_deadline receiver_heartbeat.heartbeat_deadline in 

      (receiver_heartbeat::receiver_heartbeats, min_deadline) 

    ) ([], max_float) receiver_heartbeats in 

    ({leader_state with receiver_heartbeats}, (min_deadline -. now)) 


  let min_heartbeat_timout ~now {receiver_heartbeats; _ } = 

    let min_heartbeat_deadline = List.fold_left (fun min_deadline {heartbeat_deadline; _ } -> 
        if heartbeat_deadline < min_deadline
        then heartbeat_deadline
        else min_deadline
      ) max_float receiver_heartbeats
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

module Follow_up_action = struct

  let new_election_wait state = 
    Wait_for_rpc {
      timeout      = state.configuration.election_timeout; 
      timeout_type = New_leader_election; 
    } 

  let existing_election_wait election_deadline now = 
    Wait_for_rpc {
      timeout      = election_deadline -. now; 
      timeout_type = New_leader_election; 
    }
  
  let make_heartbeat_wait timeout = 
    Wait_for_rpc {
      timeout = timeout; 
      timeout_type = Heartbeat;
    }

  let default state now = 
    match state.role with
    | Follower _ -> new_election_wait state 
    | Leader leader_state -> 
      make_heartbeat_wait (Leader.min_heartbeat_timout ~now leader_state)
    | Candidate {election_deadline; _ } -> 
      existing_election_wait election_deadline  now  

end 
