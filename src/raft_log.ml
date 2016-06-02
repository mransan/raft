open Raft_pb

module Rope = Raft_rope 

module Term_tree = struct 
  
  type t =  {
    previous_terms : int Rope.t;  
    last_log_entry : log_entry; 
  }

  let empty = {
    previous_terms = None;
    last_log_entry = { index = 0; term = 0; id = ""; data = Bytes.create 0}; 
  }

  let new_log t log_entry = 
    let {
      previous_terms; 
      last_log_entry = {term = last_term; index = last_index; _ };
    } = t in 
    let {index;term; _ } = log_entry in  

    match term - last_term with
    | 0 -> 
      {t with last_log_entry = log_entry} 

    | i when i > 0 -> 
      let prev =  Rope.last_entry_index_in_rope previous_terms in 
      let last =  last_index in  
      let data =  last_term in 
      { 
        previous_terms = Rope.add prev last data previous_terms; 
        last_log_entry = log_entry
      }
  
    | _ -> assert(false)

  let new_last t log_entry = 
    let {
      previous_terms; 
      last_log_entry = {term = last_term; index = last_index; _ };
    } = t in 
    let {index;term; _ } = log_entry in  
    if term = last_term
    then {t with last_log_entry = log_entry}
      (* The caller is simply removing logs from the current term 
       * which is trivial. 
       *)
    else 
      let check = fun {Rope.leaf_data = previous_term ; _ } -> 
          previous_term >= term 
      in 
      {
        previous_terms = Rope.remove_backward_while check previous_terms; 
        last_log_entry = log_entry;
      }

  let pp fmt {previous_terms; last_log_entry} = 
     let pp_previous_terms = Rope.pp Format.pp_print_int  in 
     Format.fprintf fmt "{previous_terms: %a; last_log_entry: %a}" 
       pp_previous_terms previous_terms pp_log_entry last_log_entry  

  let term_of_index index {previous_terms; last_log_entry = {term; _ }} = 
    if index = 0 
    then 0 
    else 
      if index > Rope.last_entry_index_in_rope previous_terms 
      then term 
      else Rope.find ~index previous_terms 

end 

type term_tree = Term_tree.t 

let pp_term_tree = Term_tree.pp 


module Past_interval = struct 

  type t = Raft_pb.log_interval 
  
  let make_expanded entries = 
    Expanded {entries}

  let make ?until ~since log_entries = 
    
    let last_index, log_entries = 
      match until with
      | None -> 
        let last_index = 
          match log_entries with
          | {index;_}::_ -> index
          | _ -> 0
        in
        (last_index, log_entries) 
  
      | Some until -> 
        let rec aux = function
          | ({index; _ }::tl) as log_entries when index = until -> log_entries 
          | _::tl -> aux tl 
          | [] -> []
        in
        (until, aux log_entries) 
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
    aux [] log_entries 

  (*
   * Given the interval ]x;y] return ]since;y] 
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

type t = {
  recent_entries : log_entry list;
  log_size : int;
  past_entries : Raft_pb.log_interval Raft_rope.t;
  term_tree : term_tree; 
} 

module Past_entries = struct 

  let fold f e0 {past_entries; _ } = 
    Rope.fold f e0 past_entries
  
  let replace ({prev_index; _ } as replacement) ({past_entries; _} as log) =   
    {log with past_entries = Rope.replace prev_index replacement past_entries}
  
  let find ~index {past_entries; _ } = 
    Rope.find ~index past_entries
  
  let add new_interval past_entries = 
    Rope.add new_interval.prev_index new_interval.last_index new_interval past_entries 
end 

let last_log_index_and_term {recent_entries; _ } = 
  match recent_entries with
  | {index;term}::_ -> (index, term) 
  | [] -> (0, 0)

let last_log_index log = 
  fst @@ last_log_index_and_term log 

let empty = {
  recent_entries = [];
  past_entries = None;
  log_size = 0; 
  term_tree  = Term_tree.empty
}

let last_past_entry_index = Rope.last_entry_index_in_rope 

let service ~prev_commit_index ~configuration log = 

  let past_entries = log.past_entries in 
  let since = last_past_entry_index past_entries in 

  if prev_commit_index - since < configuration.log_interval_size
  then log
  else 
    (* 
     * The threshold for transfering recent entries to the past entries
     * is reached, let's proceed with creating the interval, adding 
     * it to the past entries while removing those entries
     * from the recent one. 
     *)
    let until = prev_commit_index in 
    let new_interval = Past_interval.make ~until ~since log.recent_entries in 
    let past_entries = Past_entries.add new_interval past_entries in 

    let rec aux = function
      | [] -> [] 
      | ({index; _ } as l) ::tl when index = prev_commit_index -> [l] 
      | log_entry::tl -> log_entry :: (aux tl) 
    in

    {log with 
      recent_entries = aux log.recent_entries; 
      past_entries = past_entries;
    }

let int_compare (x:int) (y:int) = Pervasives.compare x y 

let add_logs current_term datas log = 

  let rec aux term last_log_index recent_entries log_size term_tree = function
    | [] -> (recent_entries, log_size, term_tree)  
    | (data, id)::tl -> 
      let last_log_index = last_log_index + 1 in 
      let new_log_entry = {
        index = last_log_index;
        term;
        data;
        id;
      } in 
      let recent_entries = new_log_entry :: recent_entries in 
      let term_tree  = Term_tree.new_log term_tree new_log_entry in 

      aux term last_log_index recent_entries (log_size + 1) term_tree tl 
  in 

  let term = current_term in 
  let last_log_index = last_log_index log in 
  let recent_entries = log.recent_entries in 
  let log_size = log.log_size in 

  let recent_entries, log_size, term_tree = 
    aux term last_log_index recent_entries log_size log.term_tree datas 
  in 
  {log with recent_entries; log_size; term_tree}

let find_term_of_index i log_entries = 
  let rec aux = function
    | [] -> raise Not_found 
    | {index; term;_} :: _ when index = i -> term 
    | _::tl -> aux tl  
  in
  aux log_entries

let merge_logs ~prev_log_index ~prev_log_term ~rev_log_entries ~commit_index log = 

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
          (commit_index, find_term_of_index commit_index log.recent_entries , [])
          
        | {index;term; _ }::tl when index < commit_index -> aux tl 
        | {index;term; _ }::tl -> 
          assert(index = commit_index); 
          (index, term, tl) 
      in
      aux rev_log_entries
  in
   
  assert(prev_log_index >= (commit_index - 1));

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
  let merge_log_entries log log_size recent_entries =

    (* Before merging we need to make sure the term
     * tree is correctly updated to reflce the recent_entries since
     * those might have been affected by 
     * the [aux] function below.
     *)
    let term_tree = match recent_entries with
      | []      -> Term_tree.empty
      | hd :: _ -> Term_tree.new_last log.term_tree hd 
    in

    let rec aux log_size term_tree recent_entries = function
      | [] -> (log_size, term_tree, recent_entries)
      | hd::tl -> 
        let term_tree = Term_tree.new_log term_tree hd in  
        aux (log_size + 1) term_tree (hd::recent_entries) tl 
    in

    let (log_size, term_tree, recent_entries) = aux log_size term_tree recent_entries rev_log_entries in 
    let log = {log with 
      recent_entries; 
      log_size; 
      term_tree; 
    } in 
    (log, true)
  in
  
  let rec aux log_size = function
    | [] ->
      if prev_log_index = 0
      then
        (* [case 0] No previous log were ever inserted
         *)
        merge_log_entries log 0 []
      else
        (* [case 1] The prev_log_index is not found in the state log.
         * This server is lagging behind.
         *)
        (log, false)

    | ({index; term; _ }::tl as recent_entries) when index = prev_log_index &&
                                                     term = prev_log_term ->
      (* [case 2] The prev_log_index matches the leader, all is good,
       * let's append all the new logs.
       *)
      (* TODO should [log_size] be [log_size -1] here. 
       *)
      merge_log_entries log log_size recent_entries

    | {index; _ }::recent_entries when index = prev_log_index ->
      (* [case 3] The prev_log_index is inconstent with the leader.
       * This conflict is resolved by discarding it along with all
       * following log entries.
       * As far as the leader is concerned it's like [case 1] now.
       *)
      (* TODO should [log_size] be [log_size -1] here. 
       * and should log_size be added to the update of the state
       *)
      let log       = {log with recent_entries} in 
      (log, false)

    |  hd::tl -> aux (log_size - 1) tl
  in

  begin match log.recent_entries with
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
    (log, false)
  | _ -> aux log.log_size log.recent_entries
  end

let rev_log_entries_since since log = 
  let {recent_entries ; past_entries;} = log in 

  match recent_entries with
  | []  -> []
  | {index; _}::_ -> 

    (* Now the data can either be in the recent or past
     * entries.
     *
     * If the [since] index is greater than the 
     * last past past entry index then it's part of the recent
     * entries.
     *)
    let interval = 
      if since >= (last_past_entry_index past_entries)
      then  
        Past_interval.make since recent_entries
      else 
        Past_interval.sub since @@ Past_entries.find (since + 1) log 
    in
    match interval.rev_log_entries with
    | Expanded {entries} -> entries
    | Compacted _        -> []

let term_of_index index {term_tree; _} = 
  Term_tree.term_of_index index term_tree 

module Builder = struct 

  type t1 = Past_interval.t list 

  type t2 = t 

  let make_t1 () = []

  let add_interval intervals interval = interval::intervals

  let from_list log_intervals = 
  
    let log_intervals = List.sort (fun x y -> 
      int_compare x.prev_index y.prev_index
    ) log_intervals in 
    
    List.fold_left (fun past_entries log_interval -> 
      Past_entries.add log_interval past_entries
    ) None log_intervals
  
  let t2_of_t1 past_entries = {
    past_entries = from_list past_entries; 
    recent_entries =[]; 
    log_size = 0; 
    term_tree = Term_tree.empty;
  } 

  let add_log_entry log log_entry = 
    let from = last_past_entry_index log.past_entries in 
    if log_entry.index >= from 
    then {log with 
      recent_entries = log_entry :: log.recent_entries; 
      log_size = log.log_size + 1; 
      term_tree = Term_tree.new_log log.term_tree log_entry; 
    } 
    else {
      log with 
      log_size = log.log_size + 1; 
      term_tree = Term_tree.new_log log.term_tree log_entry;
    }  

  let log_of_t2 x = x 

end (* Builder *) 
