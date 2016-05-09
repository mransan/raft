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

module Rev_log_cache = struct 

  type local_cache = Raft_pb.log_interval

  type global_cache = Raft_pb.log_interval list 

  let  size = 100_000 

  let make since log = 
    
    let last_index = 
      match log with
      | {index;_}::_ -> index
      | _ -> 0
    in

    let rec aux rev_log_entries = function
      | [] ->
        if since = 0 
        then 
          {prev_index =0; prev_term =0; rev_log_entries; last_index}
        else 
          failwith "[Raft_logic] Internal error invalid log index"
  
      | {index; term; _ }::tl when index = since -> 
        {prev_index = index; prev_term = term; rev_log_entries; last_index}
  
      | hd::tl -> aux (hd::rev_log_entries) tl  
    in
    aux [] log 

  (* 
   * Return the latest log entry index stored in the given 
   * local cache. 
   *
   *)
  let last_cached_index = function 
    | [] -> 0 
    | {last_index; _}::_ -> last_index

  let update_global_cache state = 
    let gc    = state.global_cache in 
    let since = last_cached_index gc in 
    match state.log with 
    | [] -> state
    | {index;_}::tl when (index - since) > size -> 
      let entry = make since state.log in 
      {state with global_cache = entry::gc}
    | _ -> state 
  
  (*
   * Returns true if the local cache contains at least one 
   * next logs after [i]
   *)
  let contains_next_of i {last_index; prev_index;_ } = 
    prev_index <= i && i < last_index

  (*
   * Returns the sub (ie subset) local cache starting a the given
   * [since] index. 
   *
   *)
  let sub since ({prev_index; rev_log_entries; _} as t) = 
    if since = prev_index 
    then t 
    else 
      let rec aux = function
        | [] ->
          failwith "[Raft_logic] Internal error invalid log index"
          (* 
           * The caller should have called [contains_next_of] 
           * to ensure that this cache contains data next to [since].
           *)

        | {index; term; _}::tl when index = since ->
          {t with prev_index = index; prev_term = term; rev_log_entries = tl; } 

        | _::tl ->
          aux tl 
      in 
      aux rev_log_entries 
  
  let find i t = 
    let f = fun {prev_index; _} -> i >= prev_index in 
    sub i @@ match List.find f t with 
      | x -> x
      | exception Not_found -> 
        failwith "[Raft_logic] Internal error find previous local cache"

  let update_local_cache since log local_cache t = 
    match log with
    | [] -> {
      prev_index = 0; 
      prev_term = 0;
      rev_log_entries = [];
      last_index = 0;
    }
    | {index; _}::_ -> 

        (* First check if it's in the local 
         * cache. 
         *)
        if contains_next_of since local_cache
        then
          sub since local_cache
        else 
          (* Now the data can either be in the global
           * caches or not. 
           *
           * If the [since] index is greater than the 
           * last cached entry then it's not in the global
           * cache. 
           *)
          if since >= (last_cached_index t)
          then  
            make since log 
          else 
            find since t 


end (* Rev_log_cache *) 

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
      global_cache = []; 
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
  
    let rec aux indices = function
      | (-1) -> indices
      |  i   -> 
        if i = state.id
        then 
          aux indices (i -1)
        else 
          let next_index = last_log_index + 1 in 
          let match_index = 0 in 
          let local_cache = Rev_log_cache.make last_log_index state.log in

          let index:server_index = {
            server_id = i; 
            next_index; 
            match_index; 
            local_cache; 
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


  let find_server_index indices receiver_id = 
    let is_server = fun ({server_id; _ }:server_index) -> 
      server_id = receiver_id
    in 
    begin match List.find is_server indices with
    | x -> Some x 
    | exception Not_found -> None 
    end 

  let add_logs datas state = 

    let rec aux term last_log_index log log_size = function
      | [] -> (log, log_size)  
      | data::tl -> 
        let last_log_index = last_log_index + 1 in 
        let log = {
          index = last_log_index;
          term;
          data;
        } :: log in 
        aux term last_log_index log (log_size + 1) tl 
    in 

    let log, log_size = 
      let term = state.current_term in 
      let last_log_index = State.last_log_index state in 
      let log = state.log in 
      let log_size = state.log_size in 

      aux term last_log_index log log_size datas 
    in 

    {state with log; log_size; }
    
  let update_receiver_last_log_index ~server_id ~log_index leader_state = 
    let receiver_id = server_id in 
    if log_index = 0
    then (leader_state, 0)
    else 
      let {indices} = leader_state in 

      let indices = 
        List.map (fun ({server_id; next_index = _ } as s) -> 
          if server_id = receiver_id 
          then {s with next_index = log_index + 1; match_index = log_index; }
          else s  
        ) indices
      in 

      (* Calculate the number of server which also have replicated that 
         log entry
       *)
      let nb_of_replications = List.fold_left (fun n {match_index; _ } -> 
        if match_index >= log_index 
        then n + 1 
        else n
      ) 0 indices in 
      
      ({indices}, nb_of_replications) 


  let update_index ~receiver_id ~f leader_state = 
    let {indices} = leader_state in 

    let indices = List.fold_left (fun indices index -> 
      if index.server_id = receiver_id
      then (f index)::indices
      else index::indices
    ) [] indices in

    {indices}

  let record_response_received ~server_id leader_state = 
    
    update_index 
      ~receiver_id:server_id 
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

  let decrement_next_index ~log_failure ~server_id state {indices; _ } = 
    let receiver_id = server_id in 
    let {receiver_last_log_index ; receiver_last_log_term } = log_failure in 

    let latest_log_index, latest_log_term = match state.log with
      | [] -> (0, 0) 
      | {index; term; data = _} :: _ -> (index, term)
    in

    assert(receiver_last_log_index < latest_log_index);  
      (* 
       * This is an invariant. When receiving the [Append_entries]
       * request, in case of [Log_failure] the server is responsible
       * to find the earlier log entry to synchronize with the [Leader]. 
       *
       * We could handle this case as well. 
       *)

    (* 
     * Next step is to make sur that both the [receiver_last_log_index]
     * and [receiver_last_log_term] are matching an entry in the [Leader] 
     * log.   
     *
     * In the case it is then it means that the log entry sent 
     * by the receiver is the good common log entry to synchronize the [Leader]
     * and this [Follower]. 
     *
     * In the case there is no match then we jump back to a previous term to 
     * find a entry to synchronize upon.
     *)
    let receiver_last_log_index = 
      let rec aux = function 
        | [] -> 0 
        | {index; term; _}::tl -> 
          if index > receiver_last_log_index
          then aux tl 
          else 
            if term = receiver_last_log_term
            then 
              (* all good we verify the server has the matching 
               * log
               *) 
              receiver_last_log_index
            else 
              (* Same index but different term, in this case, 
               * let's just go back to the last index of the previous term
               *)
              let rec aux = function 
                | [] -> 0 
                | {index; term; _}::tl when term <> receiver_last_log_term -> index
                | _::tl -> aux tl 
              in 
              aux tl 
      in
      aux state.log
    in

    let indices = List.map (fun ({server_id; next_index; } as s) -> 
      if server_id = receiver_id
      then {s with next_index = receiver_last_log_index + 1} 
      else s
    ) indices in

    {indices}

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
