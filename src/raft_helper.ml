open Raft_pb

module Rev_log_cache = struct 

  type local_cache = Raft_pb.log_interval
  
  let make_expanded entries = 
    Expanded {entries}

  let make ?until ~since log = 
    
    let last_index, log = 
      match until with
      | None -> 
        let last_index = 
          match log with
          | {index;_}::_ -> index
          | _ -> 0
        in
        (last_index, log) 

      | Some until -> 
        let rec aux = function
          | ({index; _ }::tl) as log when index = until -> log  
          | _::tl -> aux tl 
          | [] -> []
        in
        (until, aux log) 
    in 

    let rec aux rev_log_entries = function
      | [] ->
        if since = 0 
        then 
          {
            prev_index =0; 
            prev_term =0; 
            rev_log_entries = make_expanded rev_log_entries; 
            last_index
          }
        else begin  
          Printf.eprintf "[Raft_logic] Internal error invalid log index\n%!";
          failwith "[Raft_logic] Internal error invalid log index"
        end
  
      | {index; term; _ }::tl when index = since -> 
        {
          prev_index = index; 
          prev_term = term; 
          rev_log_entries = make_expanded rev_log_entries; 
          last_index
        }
  
      | hd::tl -> aux (hd::rev_log_entries) tl  
    in
    aux [] log 

  type global_cache = Raft_pb.log_interval_rope option

  (* 
   * Return the latest log entry index stored in the given 
   * local cache. 
   *
   *)
  let last_cached_index = function 
    | Interval {last_index; _} -> last_index 
    | Append   {last_index; _} -> last_index
  
  let update_global_cache state = 

    let gc    = state.global_cache in 
    let since = match gc with
      | None -> 0 
      | Some gc -> last_cached_index gc 
    in 
  
    (* We can only cache the logs which are commited
     *)

    let commit_index = state.commit_index in 
    if commit_index - since < state.configuration.log_interval_size
    then state
    else 

      (* 
       * The cache threshold is reached, let's 
       * append to the cache a new log interval with 
       * all the logs since the last cache update.
       *)

      let new_interval = Interval (make ~until:commit_index ~since state.log) in 
      let last_index   = last_cached_index new_interval in 

      let rope_height = function
        | Interval _ -> 0 
        | Append  {height; _} -> height 
      in 

      let rec add = function 
        | Interval x -> Append {
          height = 1; 
          lhs = Interval x; 
          rhs = new_interval; 
          last_index;
        }
        | (Append {height; rhs; lhs; _ }  as a)-> 
          let lhs_height = rope_height lhs in 
          let rhs_height = rope_height rhs in 
          if lhs_height = rhs_height
          then (* balanced! *)
            Append {height = height + 1; lhs = a; rhs = new_interval;last_index}
          else begin 
            begin 
              if lhs_height <= rhs_height
              then Printf.eprintf "lhs_height(%i) <  rhs_height(%i)\n%!"
              lhs_height rhs_height;
            end;
            assert(lhs_height > rhs_height); 
            Append {height; lhs; rhs = add rhs; last_index} 
          end  
      in
      let gc = match gc with
        | None -> new_interval 
        | Some gc -> add gc 
      in  
      {state with global_cache = Some gc}


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
      match rev_log_entries with
      | Compacted _ -> t 
      | Expanded  {entries } ->
        let rec aux = function
          | [] -> (
            Printf.eprintf "[Raft_logic] Internal error invalid log index\n%!";
            failwith "[Raft_logic] Internal error invalid log index"
          )
            (* 
             * The caller should have called [contains_next_of] 
             * to ensure that this cache contains data next to [since].
             *)

          | {index; term; _}::tl when index = since ->
            {t with 
             prev_index = index; 
             prev_term = term; 
             rev_log_entries = make_expanded tl; 
            } 

          | _::tl ->
            aux tl 
        in 
        aux entries
  
  let rec find i = function
    | Interval ({prev_index; _ } as interval) -> (
      begin 
        if i < prev_index 
        then Printf.eprintf "Error i: %i, prev_index: %i\n%!" i prev_index; 
      end;

      assert(i >= prev_index); 
      interval
    )
    | Append {rhs; lhs; _} -> 
      let lhs_last = last_cached_index lhs in 
      if i >= lhs_last 
      then find i rhs 
      else find i lhs  

  let update_local_cache since log local_cache t = 
    match log with
    | [] -> {
      prev_index = 0; 
      prev_term = 0;
      rev_log_entries = make_expanded [];
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
          match t with
          | None   -> make since log 
          | Some t ->
            if since >= (last_cached_index t)
            then  
              make since log 
            else 
              sub since @@ find since t 

  let fold f e0 global_cache = 
    match global_cache with
    | None -> e0
    | Some rope ->
      let rec aux acc = function
        | Interval log_interval -> 
          f acc log_interval

        | Append {lhs;rhs; _} ->
          aux (aux acc lhs) rhs 
      in
      aux e0 rope

end (* Rev_log_cache *) 

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

  let add_logs datas state = 

    let rec aux term last_log_index log log_size = function
      | [] -> (log, log_size)  
      | (data, id)::tl -> 
        let last_log_index = last_log_index + 1 in 
        let log = {
          index = last_log_index;
          term;
          data;
          id;
        } :: log in 
        aux term last_log_index log (log_size + 1) tl 
    in 

    let log, log_size = 
      let term = state.current_term in 
      let last_log_index = last_log_index state in 
      let log = state.log in 
      let log_size = state.log_size in 

      aux term last_log_index log log_size datas 
    in 
    Rev_log_cache.update_global_cache {state with log; log_size; }

  let merge_logs ~prev_log_index ~prev_log_term ~rev_log_entries state = 

    assert(prev_log_index >= state.commit_index);
    (* This is an invariant of the RAFT protocol. If this was not true
     * then we would remove commited entries from the log. 
     *)
    
    (* This functions merges the request log entries with the
     * current log of the server. The current log is first split by
     * the caller into:
     *
     * - [log] all the logs prior to (and including) [prev_log_index].
     * - [log_size] nb of log entries in [log]
     *
     * This function will then merge the 2 list of logs by adding all the
     * common logs first, then if either one is empty or an entry differs
     * then the entries from the leader will override the one in the server.
     *
     *)
    let merge_log_entries state log_size log =

      let rec aux log_size log = function
        | [] -> (log_size, log)
        | hd::tl -> 
          aux (log_size + 1) (hd::log) tl 
      in

      let (log_size, log) = aux log_size log rev_log_entries in 
      let state = {state with log ; log_size;} in
      let state = Rev_log_cache.update_global_cache state in 
      (state, true)
    in
    
    let rec aux log_size = function
      | [] ->
        if prev_log_index = 0
        then
          (* [case 0] No previous log were ever inserted
           *)
          merge_log_entries state 0 []
        else
          (* [case 1] The prev_log_index is not found in the state log.
           * This server is lagging behind.
           *)
          (state, false)

      | ({index; term; _ }::tl as log) when index = prev_log_index &&
                                             term = prev_log_term ->
        (* [case 2] The prev_log_index matches the leader, all is good,
         * let's append all the new logs.
         *)
        merge_log_entries state log_size log

      | {index; _ }::log when index = prev_log_index ->
        (* [case 3] The prev_log_index is inconstent with the leader.
         * This conflict is resolved by discarding it along with all
         * following log entries.
         * As far as the leader is concerned it's like [case 1] now.
         *)
        let new_state = {state with log} in
        (new_state, false)

      |  hd::tl -> aux (log_size - 1) tl
    in
    match state.log with
    | {index; _}::tl when prev_log_index > index ->
      (*
       * This is the case when a new [Leader] which has more log entries
       * than this server sends a first [Append_entries_request]. It initializes
       * its belief of thie server [last_log_index] to its own [last_log_index].
       *
       * However this server does not have as many log entries.
       *
       * In such a case, we send failure right away.
       *)
      (state, false)

    | _ -> aux state.log_size state.log

  let notifications before after = 
    
    let { commit_index = bcommit_index; role = brole; _ } = before in 
    let { commit_index = acommit_index; role = arole; _ } = after in 
    
    let notifications = [] in  

    let notifications = 
      match brole, arole with
      | Follower _   , Leader _ 
      | Leader _     , Candidate _ ->
        (* Impossible transition which would violate the rules of the 
         * RAFT protocol 
         *)
        assert(false) 

      | Candidate _  , Leader _ -> 
        (* Case of the server becoming a leader *)
        (New_leader {leader_id = after.id;})::notifications 
      
      | Follower {current_leader = Some bleader; _ }, 
        Follower {current_leader = Some aleader; _ } when bleader = aleader ->
        notifications 
        (* No leader change, following the same leader *)

      | _, Follower{current_leader = Some aleader;_} -> 
        (New_leader {leader_id = aleader;})::notifications 
        (* There is a new leader *) 

      | Follower {current_leader = Some _; _}, Candidate _
      | Follower {current_leader = Some _; _}, Follower  {current_leader = None; }
      | Leader  _                            , Follower  {current_leader = None; } ->
        (No_leader::notifications)

      | Leader _                             , Leader _ 
      | Candidate _                          , Candidate _ 
      | Candidate _                          , Follower {current_leader = None} 
      | Follower {current_leader = None; _}  , Follower {current_leader = None; }
      | Follower {current_leader = None; _}  , Candidate _ ->
        notifications
    in

    let notifications = 
      if acommit_index > bcommit_index
      then 
        let rec aux ids = function 
          | {index;id;_ }::tl -> 
              if index > acommit_index 
              then aux ids tl 
              else 
                if index = bcommit_index
                then ids 
                else aux (id :: ids) tl 
          | [] ->  ids 
        in
        (Committed_data {ids = aux [] after.log})::notifications 
      else 
        notifications
    in 
    notifications

  let current_leader {id; role; _} = 
      match role with
      | Follower {current_leader; _ }-> current_leader
      | Candidate _ -> None 
      | Leader _ -> Some id 

  (* The latest non compacted log will be at the front 
   * of the list. 
   *)
  let collect_all_non_compacted_logs state = 
    Rev_log_cache.fold (fun acc -> function
      | {rev_log_entries = Compacted _; _} -> acc 
      | log_interval -> log_interval::acc 
    ) [] state.global_cache 

  let compaction state = 

    match state.role with
    | Candidate _ -> ([], [])
    | Follower  _ -> 
      let non_compacted = collect_all_non_compacted_logs state in 
      begin match non_compacted with
      | _::_::to_be_compacted -> ([], to_be_compacted) 
      (* 
       * We don't want to compact the last 2 logs ... first 
       * the machine memory should have enough capacity (hopefully)
       * second in the event that this Follower become a Leader it's likely
       * that it will have to sync other followers which will have less entries
       * than itself. Therefore having the last 2 cache available would avoid
       * reloading compacted logs. 
       *
       * We also assume that a Follower would never need to un-compact a previously
       * compacted log which is Ok since it is not used by other Followers to replicate
       * data. (Maybe one day we would allow a [Follower] to be used to return previously 
       * commited log, since a follower is usually less busy then the [Leader]. 
       *) 
      | _ -> ([], [])
      end

    | Leader {indices} -> 
      (* For each server next_index we should keep expanded 2 log intervals:
       * a) The one that the next index belongs to 
       * b) The next one after a)
       *
       * This strategy should allow detecting early enough the log interval which needs 
       * un-compacting while still maitainig memory usage low enough. 
       * In the worst case the number of logs kept uncompacted would be:
       *         (nb of servers - 1) * 2 * Rev_log_cache.size  
       *
       * All other logs should be compacted
       *
       * > Note that we should make this `2` value part of the configuration. 
       *
       *)

      let next_indices = 
        let cmp (x:int) (y:int) = compare x y in 
        indices
        |> List.map (fun {next_index; _ } -> next_index)
        |> List.sort_uniq cmp
      in

      let ((c, e,_ ), _) = Rev_log_cache.fold (fun ((c, e, append_next), next_indices) log_interval -> 

        match next_indices with
        | [] -> 
          if append_next
          then ((c, log_interval::e, false), next_indices)
          else ((log_interval::c, e, false), next_indices)

        | next_index::tl ->
          if Rev_log_cache.contains_next_of next_index log_interval
          then ((c, log_interval::e, true), tl)
          else 
            begin 
              assert(next_index > log_interval.last_index);
              if append_next
              then ((c, log_interval::e, false), next_indices)
              else ((log_interval::c, e, false), next_indices)
            end 
      ) (([], [], false), next_indices) state.global_cache in 

      let is_compacted = function | {rev_log_entries = Compacted _ ; _ } -> true | _ -> false in 
      let is_expanded  = function | {rev_log_entries = Expanded  _ ; _ } -> true | _ -> false in 

      let c = List.filter is_expanded  c in 
      let e = List.filter is_compacted e in 
      (e, c)

end 


module Follower = struct 

  let create ?current_leader 
             ?current_term:(current_term = 0) 
             ?voted_for 
             ?log:(log = []) 
             ~configuration ~now ~id () = 
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
      global_cache = None; 
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
          let local_cache = Rev_log_cache.make ~since:last_log_index state.log in
            (* The cache is expected to be empty... which is fine since it will
             * get filled at when a new log entry will be
             * added. 
             *)

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
       *)

    (* 
     * Next step is to make sur that both the [receiver_last_log_index]
     * and [receiver_last_log_term] are matching an log entry in the [Leader] 
     * log.   
     *
     * If the match is found it means that the log entry sent 
     * by the receiver is the good common log entry to synchronize the [Leader]
     * and its [Follower]. 
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
              (* Receiver last log entry is a match with a [Leader] log 
               * entry.
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
