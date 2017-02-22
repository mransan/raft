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

type t = {
  recent_entries : log_entry IntMap.t;
  log_size : int;
} 

let empty = {
  recent_entries = IntMap.empty;
  log_size = 0;
}

let last_log_index_and_term {recent_entries; _ } =
  match IntMap.max_binding recent_entries  with
  | (_ , {index;term; _}) -> (index, term)
  | exception Not_found -> (0, 0)

let last_log_index log =
  fst @@ last_log_index_and_term log

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
      
    let sub, _, _ = IntMap.split (since + max + 1) sub in 
    (List.map snd (IntMap.bindings sub), prev_term)  

let add_log_datas current_term datas log =

  let rec aux term last_log_index recent_entries log_size = function
    | [] -> (recent_entries, log_size)
    | (data, id)::tl ->
      let last_log_index = last_log_index + 1 in

      let new_log_entry = {
        index = last_log_index;
        term; data; id;
      } in

      let recent_entries = 
        IntMap.add last_log_index new_log_entry recent_entries 
      in 

      aux term last_log_index recent_entries (log_size + 1) tl
  in

  let term = current_term in
  let last_log_index = last_log_index log in
  let recent_entries = log.recent_entries in
  let log_size = log.log_size in

  let recent_entries, log_size =
    aux term last_log_index recent_entries log_size datas
  in
  {recent_entries; log_size}

let add_log_entries ~rev_log_entries log =

  let rec aux log_size recent_entries = function
    | [] ->
      {log_size; recent_entries}

    | hd::tl ->
      let recent_entries = IntMap.add hd.index hd recent_entries in 
      aux (log_size + 1) recent_entries tl
  in

  aux log.log_size log.recent_entries rev_log_entries

let remove_log_since ~prev_log_index ~prev_log_term log =

  let {recent_entries; log_size; } = log in 
  if prev_log_index > (last_log_index log)
  then raise Not_found 
  else 

    let before, e, after = IntMap.split prev_log_index recent_entries in 
    let recent_entries, log_size = 
      match e with
      | None -> 
        if prev_log_index = 0 
        then (before, log_size - (IntMap.cardinal after))  
        else raise Not_found 
      | Some ({term; index; _} as log_entry) -> 
        if term = prev_log_term
        then (
          IntMap.add index log_entry before, 
          log_size - (IntMap.cardinal after) 
        ) 
        else raise Not_found
    in 

    {recent_entries; log_size}

module Builder = struct

  type builder = t

  let make () = empty

  let add_log_entry log log_entry =
    assert(log_entry.index > (last_log_index log));
    {
      recent_entries = IntMap.add log_entry.index log_entry log.recent_entries; 
      log_size = log.log_size + 1;
    }

  let to_log x = x

end (* Builder *)
