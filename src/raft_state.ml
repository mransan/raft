open Raft_pb

module Log = Raft_log


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
      let rec aux rev_log_entries = function 
        | ({index;_ } as log_entry) ::tl -> 
            if index > acommit_index 
            then aux rev_log_entries tl 
            else 
              if index = bcommit_index
              then rev_log_entries
              else aux (log_entry :: rev_log_entries) tl 
        | [] ->  
          assert(bcommit_index = 0); 
          (* If commit_index is different than 0 then this means 
           * that we could not identify all the [log_entry] which 
           * have been commited between [before] and [after]. 
           *
           * One of the reason could be that the [log_entry]s are not
           * in the [log] but rather in the [global_cache]. 
           * This should be prevented by the fact that [Rev_log_cache.update_global_cache]
           * only move the [log_entry] to the cache wihch are prior to the 
           * previous commit index (ie the one of [before]. 
           * 
           * The other is a plain bug, all entries between 2 commit_index should be 
           * in the log.
           *) 
          rev_log_entries 
      in
      (Committed_data {rev_log_entries = aux [] after.log.recent_entries})::notifications 
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
  Log.Past_interval.fold (fun acc -> function
    | {rev_log_entries = Compacted _; _} -> acc 
    | log_interval -> log_interval::acc 
  ) [] state.log

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

    let ((c, e,_ ), _) = Log.Past_interval.fold (fun ((c, e, append_next), next_indices) log_interval -> 

      match next_indices with
      | [] -> 
        if append_next
        then ((c, log_interval::e, false), next_indices)
        else ((log_interval::c, e, false), next_indices)

      | next_index::tl ->
        if Log.Past_interval.contains_next_of next_index log_interval
        then ((c, log_interval::e, true), tl)
        else 
          begin 
            assert(next_index > log_interval.last_index);
            if append_next
            then ((c, log_interval::e, false), next_indices)
            else ((log_interval::c, e, false), next_indices)
          end 
    ) (([], [], false), next_indices) state.log in 

    let is_compacted : log_interval -> bool = function | {rev_log_entries = Compacted _ ; _ } -> true | _ -> false in 
    let is_expanded  : log_interval -> bool = function | {rev_log_entries = Expanded  _ ; _ } -> true | _ -> false in 

    let c = List.filter is_expanded  c in 
    let e = List.filter is_compacted e in 
    {to_be_compacted = c; to_be_expanded = e}
