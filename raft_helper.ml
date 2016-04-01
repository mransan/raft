open Raft_pb

module Follower = struct 

  let make state term = 
    {state with role = Follower {voted_for = None} ; current_term = term; }

end 

module Candidate = struct 

  let make state now = 
    let {election_timeout; _ } = state.configuration in 
    let candidate_state = {
      vote_count = 1; 
      election_deadline = now +. election_timeout
    } in 
    {state with role = Candidate candidate_state; current_term = state.current_term + 1; } 

  let increment_vote_count ({vote_count; _ } as candidate_state) = 
    {candidate_state with vote_count = vote_count + 1}

end 

module Leader = struct 

  let make state  = 

    let last_log_index = match state.log with
      | [] -> -1 
      | {index; _ }::_ -> index 
    in 
    
    let {nb_of_server; _ } =  state.configuration in 
  
    let rec aux ((next_index, match_index) as acc) = function
      | (-1) -> acc
      |  i   -> 
         let next   = {server_id = i; server_log_index = last_log_index + 1} in 
         let match_ = {server_id = i; server_log_index = 0 } in 
         aux (next::next_index, match_::match_index) (i -1)
    in 
    let next_index, match_index = aux ([], []) (nb_of_server - 1) in 
    
    {state with role = Leader {next_index; match_index}}

  let add_log data state = 
    let last_log_index = match state.log with
      | [] -> -1 
      | {index; _ }::_ -> index 
    in 
    {state with 
      log = {
        index = last_log_index + 1;
        term = state.current_term; 
        data; 
      }::state.log
    }
   
  let is_leader {role; _ } = 
    match role with 
    | Leader _ -> true 
    | _ -> false

  let update_receiver_last_log_index state receiver_id last_log_index = 
    if last_log_index = -1 
    then state 
    else 
      match state.role with
      | Leader {next_index; match_index; } -> 
  
        let update_server_index_value server_index v = 
          List.map (fun ({server_id; server_log_index = _ } as s) -> 
            if server_id = receiver_id 
            then {s with server_log_index = v}
            else s  
          ) server_index 
        in 
  
        {state with role = Leader {
          next_index  = update_server_index_value next_index (last_log_index + 1); 
          match_index = update_server_index_value match_index last_log_index; 
        }} 
      | _ -> 
        state
  
  let decrement_next_index state receiver_id = 
    match state.role with
    | Leader {next_index; match_index} -> 
      let next_index = List.map (fun ({server_id; server_log_index; } as s) -> 
        if server_id = receiver_id
        then {s with server_log_index = server_log_index - 1} 
        else s
      ) next_index in
      {state with role = Leader {next_index; match_index}}
    | _ -> 
      state
end 
