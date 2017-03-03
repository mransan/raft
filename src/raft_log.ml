type log_entry = {
  index : int;
  term : int;
  data : bytes;
  id : string;
}

let pp_log_entry fmt {index; term; id; _} =
  Format.fprintf fmt "{index: %i; term: %i, id: %s}"
   index term id 

module IntMap = Map.Make(struct 
  type t = int 
  let compare (x:int) (y:int) = Pervasives.compare x y
end) 

type max_log_size = {
  upper_bound : int;
  lower_bound : int; 
}

type t = {
  recent_entries : log_entry IntMap.t;
  max_log_size : max_log_size;
} 

type log_diff = {
  added_logs : log_entry list; 
  deleted_logs : log_entry list;
}

let empty_diff = { added_logs = []; deleted_logs = [] }

let empty max_log_size = {
  recent_entries = IntMap.empty;
  max_log_size;
}

let last_log_index_and_term {recent_entries; _ } =
  match IntMap.max_binding recent_entries  with
  | (_ , {index;term; _}) -> (index, term)
  | exception Not_found -> (0, 0)

let last_log_index {recent_entries; _}  =
  match IntMap.max_binding recent_entries  with
  | (_ , {index; _}) -> index
  | exception Not_found -> 0

exception Done of log_entry list 

let log_entries_since ~since ~max log =
  let {recent_entries ; _} = log in
  if recent_entries = IntMap.empty
  then ([], 0)
     (* TODO questionable, shoudl all cases go to the sub function *) 
  else 
    let _, prev, sub = IntMap.split since recent_entries in 
    let prev_term = 
      match prev with 
      | None -> assert (since = 0); 0  
      | Some {term; _} -> term 
    in

    let max = since + max in 

    let log_entries = 
      try IntMap.fold (fun index log_entry log_entries -> 
        if index < max
        then log_entry :: log_entries
        else raise (Done log_entries)
      ) sub [] 
      with | Done log_entries -> log_entries
    in 

    (List.rev log_entries, prev_term)
(*    let sub, _, _ = IntMap.split (since + max + 1) sub in 
    (List.map snd (IntMap.bindings sub), prev_term)  *)

(* Enforce that the size of the recent_entries stays within the 
 * max log size configuration *) 
let truncate add_size ({recent_entries; max_log_size; _} as t) = 
  let {upper_bound; lower_bound} = max_log_size in 
  match IntMap.min_binding recent_entries with
  | exception Not_found -> t
    (* when empty, no need for size limitation *)
  | (lower_index, _) -> 
    let (upper_index, _) = IntMap.max_binding recent_entries in 
    let size = upper_index - lower_index + 1 in 
    if size + add_size > upper_bound 
    then 
      let over = size - lower_bound in 
      let lower_index = lower_index + over + add_size - 1 in 
      let _, _, recent_entries = IntMap.split lower_index recent_entries in 
      {t with recent_entries}
    else 
      t
   
let add_log_datas current_term datas log =
  let log = truncate (List.length datas) log in 

  let rec aux term last_log_index recent_entries added_logs = function
    | [] -> (recent_entries, (List.rev added_logs))
    | (data, id)::tl ->
      let last_log_index = last_log_index + 1 in

      let new_log_entry = {
        index = last_log_index;
        term; data; id;
      } in
      
      let added_logs = new_log_entry :: added_logs in

      let recent_entries = 
        IntMap.add last_log_index new_log_entry recent_entries 
      in 

      aux term last_log_index recent_entries added_logs tl
  in

  let term = current_term in
  let last_log_index = last_log_index log in
  let recent_entries = log.recent_entries in

  let recent_entries, added_logs =
    aux term last_log_index recent_entries [] datas
  in

  let log_diff = { deleted_logs = []; added_logs; } in 

  ({log with recent_entries}, log_diff)

let add_log_entries ~rev_log_entries log =
  let log = truncate (List.length rev_log_entries) log in 
  let rec aux recent_entries = function
    | [] ->
      {log with recent_entries}

    | hd::tl ->
      let recent_entries = IntMap.add hd.index hd recent_entries in 
      aux recent_entries tl
  in

  let log_diff = {
    added_logs = rev_log_entries; 
    deleted_logs = []; 
  } in 

  (aux log.recent_entries rev_log_entries, log_diff) 

let remove_log_since ~prev_log_index ~prev_log_term log =
  let {recent_entries; max_log_size = _ } = log in 
  if prev_log_index > (last_log_index log)
  then raise Not_found 
  else 

    let before, e, after = IntMap.split prev_log_index recent_entries in 
    let recent_entries, deleted_logs_map = 
      match e with
      | None -> 
        if prev_log_index = 0 
        then (before, after)
        else raise Not_found 
      | Some ({term; index; _} as log_entry) -> 
        if term = prev_log_term
        then (IntMap.add index log_entry before, after) 
        else raise Not_found
    in 

    let deleted_logs = List.map snd @@ IntMap.bindings deleted_logs_map in

    (
      {log with 
        recent_entries;}, 
      {deleted_logs; added_logs = []}
    ) 

let merge_diff lhs rhs = 
  match lhs, rhs with
  | {added_logs = []; deleted_logs = []}, rhs -> rhs 
  | lhs, {added_logs = []; deleted_logs = []} -> lhs 

  | {added_logs; deleted_logs = []}, 
    {added_logs = []; deleted_logs} 
  | {added_logs = []; deleted_logs}, 
    {added_logs; deleted_logs = []} -> {added_logs; deleted_logs}
  | _ -> assert(false) 

module Builder = struct

  type builder = t

  let make max_log_size = empty max_log_size 

  let add_log_entry log log_entry =
    let log = truncate 1 log in 
    (* assert(log_entry.index > (last_log_index log));
     *)
    { log with
      recent_entries = IntMap.add log_entry.index log_entry log.recent_entries; 
    }

  let to_log x = x

end (* Builder *)
