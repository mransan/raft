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

  let make ?current_leader state term = { state with 
    role = Follower {
      voted_for = None; 
      current_leader; 
    }; 
    current_term = term; 
  }

end 

module Candidate = struct 

  let make state now = 
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

  let make state  = 

    let last_log_index = State.last_log_index state in
    
    let {nb_of_server; _ } =  state.configuration in 
  
    let rec aux ((next_index, match_index) as acc) = function
      | (-1) -> acc
      |  i   -> 
         let next   = {server_id = i; server_log_index = last_log_index + 1} in
         let match_ = {server_id = i; server_log_index = 0 } in 
         aux (next::next_index, match_::match_index) (i - 1)
    in 
    let next_index, match_index = aux ([], []) (nb_of_server - 1) in 
    
    {state with role = Leader {next_index; match_index}}

  let next_index_for_receiver {role; _ } receiver_id = 
    match role with
    | Leader {next_index; _ } -> 
        let {server_log_index; _ } = List.find (fun ({server_id; _ }:server_index) -> 
          server_id = receiver_id
        ) next_index in 
        Some server_log_index
    | _ -> None 

  let add_log data state = 
    let last_log_index = State.last_log_index state in
    {state with 
      log = {
        index = last_log_index + 1;
        term = state.current_term; 
        data; 
      }::state.log
    }
   
  let update_receiver_last_log_index leader_state receiver_id last_log_index = 
    if last_log_index = 0
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

      let next_index = update_server_index_value next_index (last_log_index + 1) in 
      let match_index = update_server_index_value match_index last_log_index in 

      (* Calculate the number of server which also have replicated that 
         log entry
       *)
      let nb = List.fold_left (fun nb {server_log_index; _ } -> 
        if server_log_index >= last_log_index
        then nb + 1 
        else nb
      ) 0 match_index in 
      
      ({next_index; match_index}, nb) 
  
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

module Configuration = struct

  let is_majority {nb_of_server; _} nb = 
    nb > (nb_of_server / 2) 

end 
