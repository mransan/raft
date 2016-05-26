open Raft_pb

module Rev_log_cache = Raft_revlogcache

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
  {state with log; log_size;}

let merge_logs ~prev_log_index ~prev_log_term ~rev_log_entries state = 

  (* --------------------------------------------------------
   * ? assert(prev_log_index >= (state.commit_index - 1));  ?
   * --------------------------------------------------------
   *
   * We cannot make the assertion above since it is possible 
   * that an [Append_entries] request would be sent twice (either 
   * mistake from [Leader] or the network will duplicate). 
   *
   * In such a case the first is likely to increase the commit index
   * of the [Follower] to a value greated than the [prev_log_index]. So 
   * on the second request it will fail
   * 
   * Ti    commit_index = x , prev_log_index x + 10 
   * Ti+1  request with leader_commit = x + 20 , and 100 logs added  
   * Ti+2  commit_index = x + 20, prev_log_index x + 110 
   * Ti+3  duplicate of Ti+1 request 
   * >>    We can see now that commit_index > prev_log_index 
   *
   * What we actually need to verify is that all the logs being 
   * sent are the same as the previous ones! This code was there 
   * in a previous implementation. 
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
  | Candidate _ -> 
    {to_be_expanded = []; to_be_compacted = []}

  | Follower  _ -> 
    let non_compacted = collect_all_non_compacted_logs state in 
    begin match non_compacted with
    | _::_::to_be_compacted -> 
        {to_be_expanded = []; to_be_compacted}
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
    | _ -> 
        {to_be_expanded = []; to_be_compacted = []}
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
    {to_be_compacted = c; to_be_expanded = e}
