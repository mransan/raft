open Raft_pb

let last_cached_index_rope = function 
  | Interval {last_index; _} -> last_index 
  | Append   {last_index; _} -> last_index

module Past_interval = struct 

  type t = Raft_pb.log_interval 
  
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
          Printf.eprintf "[Raft_logic] Internal2 error invalid log index\n%!";
          failwith "[Raft_logic] Internal2 error invalid log index"
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

  let fold f e0 {past_entries; _ } = 
    match past_entries with
    | None -> e0
    | Some rope ->
      let rec aux acc = function
        | Interval log_interval -> 
          f acc log_interval
  
        | Append {lhs;rhs; _} ->
          aux (aux acc lhs) rhs 
      in
      aux e0 rope
  
  let replace ({prev_index; _ } as replacement) ({past_entries; _} as log) =   
    match past_entries with 
    | None -> assert(false)
    | Some rope ->  
      let rec aux = function
        | Interval interval -> 
          assert(interval.prev_index = prev_index);  
          Interval replacement 
  
        | Append ({rhs; lhs; _} as append)  -> 
          let lhs_last = last_cached_index_rope lhs in 
          if prev_index >= lhs_last 
          then Append { append with rhs = aux rhs }
          else Append { append with lhs = aux lhs }
      in
      {log with past_entries = Some (aux rope)}
  
  let find ~index {past_entries; _ } = 
    match past_entries with 
    | None -> raise Not_found
    | Some rope ->
        
      let rec aux = function
        |Interval interval -> 
          if index <= interval.prev_index || 
             index > interval.last_index  
          then raise Not_found
          else interval 
        | Append {rhs; lhs; _} -> 
          let lhs_last = last_cached_index_rope lhs in 
          if index > lhs_last 
          then aux rhs 
          else aux lhs  
      in
      aux rope 
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
  (*
   * Returns true if the local cache contains at least one 
   * next logs after [i]
   *)
  let contains_next_of i {last_index; prev_index;_ } = 
    prev_index <= i && i < last_index
end (* Past_interval *)

type t = log

type interval = Raft_pb.log_interval

type past_entries = Raft_pb.log_interval_rope option

let last_log_index_and_term {log = {recent_entries; _ }; _ } = 
  match recent_entries with
  | {index;term}::_ -> (index, term) 
  | [] -> (0, 0)

let last_log_index state = 
  fst @@ last_log_index_and_term state

let empty = {
  recent_entries = [];
  past_entries = None;
  log_size = 0; 
}

(* 
 * Return the latest log entry index stored in the given 
 * local cache. 
 *
 *)
let last_cached_index = function 
  | None -> 0 
  | Some (Interval {last_index; _}) -> last_index 
  | Some (Append   {last_index; _}) -> last_index

let add new_interval gc = 

  let last_index   = last_cached_index_rope new_interval in 
  let rope_height = function
    | Interval _ -> 0 
    | Append  {height; _} -> height 
  in 

  let rec aux = function 
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
        Append {height; lhs; rhs = aux rhs; last_index} 
      end  
  in
  match gc with
    | None    -> Some (new_interval) 
    | Some gc -> Some (aux gc) 

let service ~prev_commit_index state = 

  let gc    = state.log.past_entries in 
  let since = last_cached_index gc in 

  (* We can only cache the logs which are commited
   *)

  if prev_commit_index - since < state.configuration.log_interval_size
  then state
  else 

    (* 
     * The cache threshold is reached, let's 
     * append to the cache a new log interval with 
     * all the logs since the last cache update.
     *)

    let new_interval = Interval (Past_interval.make ~until:prev_commit_index ~since state.log.recent_entries) in 
    let past_entries = add new_interval gc in 

    let rec aux = function
      | [] -> [] 
      | ({index; _ } as l) ::tl when index = prev_commit_index -> [l] 
      | log_entry::tl -> log_entry :: (aux tl) 
    in
    let log = {state.log with 
      recent_entries = aux state.log.recent_entries; 
      past_entries = past_entries;
    } in 
    let state = {state with log } in  
    state 

let int_compare (x:int) (y:int) = Pervasives.compare x y 

let from_list log_intervals = 

  let log_intervals = List.sort (fun x y -> 
    int_compare x.prev_index y.prev_index
  ) log_intervals in 
  
  List.fold_left (fun gc log_interval -> 
    add (Interval log_interval) gc
  ) None log_intervals 

let add_logs datas state = 

  let rec aux term last_log_index recent_entries log_size = function
    | [] -> (recent_entries, log_size)  
    | (data, id)::tl -> 
      let last_log_index = last_log_index + 1 in 
      let recent_entries = {
        index = last_log_index;
        term;
        data;
        id;
      } :: recent_entries in 
      aux term last_log_index recent_entries (log_size + 1) tl 
  in 

  let log = 
    let term = state.current_term in 
    let last_log_index = last_log_index state in 
    let log = state.log in 
    let recent_entries = log.recent_entries in 
    let log_size = log.log_size in 

    let recent_entries, log_size= 
      aux term last_log_index recent_entries log_size datas 
    in 
    {log with recent_entries; log_size }
  in 
  {state with log}

let find_term_of_index i log_entries = 
  let rec aux = function
    | [] -> raise Not_found 
    | {index; term;_} :: _ when index = i -> term 
    | _::tl -> aux tl  
  in
  aux log_entries

let merge_logs ~prev_log_index ~prev_log_term ~rev_log_entries state = 

  let commit_index = state.commit_index in

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
   * However the RAFT protocol guarantees that no commited entries will 
   * later be invalidated/removed. (See Safety guarantee). 
   * Therefore we can remove those duplicated entries from [rev_log_entries] 
   * which are prior (and included) the latest commited entry. 
   *)
  
  let prev_log_index, prev_log_term, rev_log_entries = 
    if prev_log_index >= commit_index
    then prev_log_index, prev_log_term, rev_log_entries
    else 
      (* prev_log_index is less than commit_index, we must therefore
       * trim the log entries which are previous to the commit_index. 
       *)
      let rec aux = function 
        | [] -> 
          (* This case is possible if 2 [Append_entries] request arrives out of 
           * order. 
           * In this case the second request will contained [log_entry]s which have all
           * been replicated. In those cases it's likely that the [log_entry] corresponding
           * to this server commit_index might not be in the request [rev_log_entries]. 
           *
           * In such a case, none of the logs should be added, in fact they all have been 
           * previously added before. 
           *)
          (commit_index, find_term_of_index commit_index state.log.recent_entries , [])
          
        | {index;term; _ }::tl when index < commit_index -> aux tl 
        | {index;term; _ }::tl -> 
          assert(index = commit_index); 
          (index, term, tl) 
      in
      aux rev_log_entries
  in
   
  assert(prev_log_index >= (state.commit_index - 1));

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
  let merge_log_entries state log_size recent_entries =

    let rec aux log_size recent_entries = function
      | [] -> (log_size, recent_entries)
      | hd::tl -> 
        aux (log_size + 1) (hd::recent_entries) tl 
    in

    let (log_size, recent_entries) = aux log_size recent_entries rev_log_entries in 
    let log = {state.log with recent_entries; log_size} in 
    let state = {state with log } in 
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

    | ({index; term; _ }::tl as recent_entries) when index = prev_log_index &&
                                                     term = prev_log_term ->
      (* [case 2] The prev_log_index matches the leader, all is good,
       * let's append all the new logs.
       *)
      (* TODO should [log_size] be [log_size -1] here. 
       *)
      merge_log_entries state log_size recent_entries

    | {index; _ }::recent_entries when index = prev_log_index ->
      (* [case 3] The prev_log_index is inconstent with the leader.
       * This conflict is resolved by discarding it along with all
       * following log entries.
       * As far as the leader is concerned it's like [case 1] now.
       *)
      (* TODO should [log_size] be [log_size -1] here. 
       * and should log_size be added to the update of the state
       *)
      let log       = {state.log with recent_entries} in 
      let new_state = {state with log} in
      (new_state, false)

    |  hd::tl -> aux (log_size - 1) tl
  in

  begin match state.log.recent_entries with
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
  | _ -> aux state.log.log_size state.log.recent_entries
  end

let rev_log_entries_since since log = 
  let {recent_entries ; past_entries;} = log in 

  match recent_entries with
  | []  -> [], 0  
  | {index; _}::_ -> 

    (* Now the data can either be in the global
     * caches or not. 
     *
     * If the [since] index is greater than the 
     * last cached entry then it's not in the global
     * cache. 
     *)
    let interval = 
      if since >= (last_cached_index past_entries)
      then  
        Past_interval.make since recent_entries
      else 
        Past_interval.sub since @@ Past_interval.find (since + 1) log 
    in
    match interval.rev_log_entries with
    | Expanded {entries} -> entries, interval.prev_term
    | Compacted _        -> [], interval.prev_term


module Builder = struct 

  type t1 = Past_interval.t list 

  type t2 = Raft_pb.log  

  let make_t1 () = []

  let add_interval intervals interval = interval::intervals
  
  let t2_of_t1 past_entries = {
    past_entries = from_list past_entries; 
    recent_entries =[]; 
    log_size = 0; 
  } 

  let add_log_entry log log_entry = 
    let from = last_cached_index log.past_entries in 
    if log_entry.index >= from 
    then {log with 
      recent_entries = log_entry :: log.recent_entries; 
      log_size = log.log_size + 1; 
    } 
    else log 

  let log_of_t2 x = x 

end (* Builder *) 
