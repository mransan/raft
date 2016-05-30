open Raft_pb

type local_cache = Raft_pb.log_interval

type global_cache = Raft_pb.log_interval_rope option

let empty = None 

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

(* 
 * Return the latest log entry index stored in the given 
 * local cache. 
 *
 *)
let last_cached_index = function 
  | None -> 0 
  | Some (Interval {last_index; _}) -> last_index 
  | Some (Append   {last_index; _}) -> last_index

let last_cached_index_rope = function 
  | Interval {last_index; _} -> last_index 
  | Append   {last_index; _} -> last_index


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

let update_global_cache ~prev_commit_index state = 

  let gc    = state.global_cache in 
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

    let new_interval = Interval (make ~until:prev_commit_index ~since state.log) in 
    let global_cache = add new_interval gc in 

    let rec aux = function
      | [] -> [] 
      | ({index; _ } as l) ::tl when index = prev_commit_index -> [l] 
      | log_entry::tl -> log_entry :: (aux tl) 
    in
    let state = {state with 
     global_cache;
     log = aux state.log}
    in
    state 

let int_compare (x:int) (y:int) = Pervasives.compare x y 

let from_list log_intervals = 

  let log_intervals = List.sort (fun x y -> 
    int_compare x.prev_index y.prev_index
  ) log_intervals in 
  
  List.fold_left (fun gc log_interval -> 
    add (Interval log_interval) gc
  ) None log_intervals 

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

let find ~index = function
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

let is_expanded = function
  | ({rev_log_entries = Expanded _ ; _ } : log_interval) -> true 
  | _ -> false 

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
      if is_expanded local_cache && 
         contains_next_of since local_cache 
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
          sub since @@ find (since + 1) t 

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

let replace ({prev_index; _ } as replacement) = function
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
    Some (aux rope)
