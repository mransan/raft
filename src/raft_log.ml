open Raft_pb

type log_interval_compacted = {
  record_id : string;
}

type log_interval_expanded = {
  entries : Raft_pb.log_entry list;
}

type log_interval_rev_log_entries =
  | Compacted of log_interval_compacted
  | Expanded of log_interval_expanded

and log_interval = {
  prev_index : int;
  prev_term : int;
  last_index : int;
  rev_log_entries : log_interval_rev_log_entries;
}

module Rope = Raft_rope

module Term_tree = struct

  type t =  {
    previous_terms : int Rope.t;
    last_log_entry : log_entry;
  }

  let empty = {
    previous_terms = Rope.empty;
    last_log_entry = { index = 0; term = 0; id = ""; data = Bytes.create 0};
  }

  let last_index_of_previous_terms previous_terms =
    match Rope.last_entry_index_in_rope previous_terms with
    | None -> 0
    | Some x -> x

  (*
   * [handle_new_log_entry term_tree log_entry] returns a new term tree taking into account
   * that [log_entry] is now the latest one.
   *
   * The [term_tree] will only contain a new term interval if the [log_entry] is from
   * a different term than the previous [log_entry].
   *)
  let handle_new_log_entry t log_entry =
    let {
      previous_terms;
      last_log_entry = {term = last_term; index = last_index; _ };
    } = t in
    let {index;term; _ } = log_entry in

    assert(index > last_index);
      (* This function should only be called with a latest log entry,
       * if the log entry is previous to the latest one then
       * [handle_log_removal] function should be called.
       *)
    assert(term >= last_term);
      (* Similarly as the invariant check above, the [log_entry] term
       * cannot be less than the previous one.
       * [handle_log_removal] should be called for this workflow.
       *)

    match term - last_term with
    | 0 ->
      {t with last_log_entry = log_entry}

    | i when i > 0 ->
      let prev =  last_index_of_previous_terms previous_terms in
      let last =  last_index in
      let data =  last_term in
      {
        previous_terms = Rope.add ~prev ~last ~data previous_terms;
        last_log_entry = log_entry
      }

    | _ -> assert(false)

  (*
   * [handle_log_removal term_tree log_entry] resets the [term_tree] so that
   * [log_entry] is now the latest [log_entry].
   *
   * During the RAFT protocol it is possible that non-commited logs
   * will get removed. This function will then synchronize the
   * [term_tree] so that the given [log_entry] is assumed to be the
   * latest.
   *)
  let handle_log_removal t log_entry =
    let {
      previous_terms;
      last_log_entry = {term = last_term; index = last_index; _ };
    } = t in
    let {index;term; _ } = log_entry in

    assert(index <= last_index);
      (* [log_entry] is assumed to be prior to the current [last_log_entry].
       * If it was later then [handle_new_log_entry] function should be called
       * instead.
       *)

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

  let term_of_index index {previous_terms; last_log_entry = {term; index = last_index ; _ }} =
    if index = 0
    then 0
    else
      if index > last_index
      then
        (* The requested index is out of bound and not part of the log.
         *)
        raise Not_found
      else
        if index > last_index_of_previous_terms previous_terms
        then term
        else Rope.find ~index previous_terms

  let pp fmt {previous_terms; last_log_entry} =
     let pp_previous_terms = Rope.pp Format.pp_print_int  in
     Format.fprintf fmt "{previous_terms: %a; last_log_entry: %a}"
       pp_previous_terms previous_terms pp_log_entry last_log_entry

end  (* Term_tree *)

type term_tree = Term_tree.t

let pp_term_tree = Term_tree.pp

module Past_interval = struct

  type t = log_interval

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
          | ({index; _ }::_) as log_entries when index = until -> log_entries
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

      | {index; term; _ }::_ when index = since ->
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
  let contains i {last_index; prev_index;_ } =
    prev_index < i && i <= last_index

end (* Past_interval *)

type t = {
  recent_entries : log_entry list;
  log_size : int;
  past_entries : log_interval Raft_rope.t;
  term_tree : term_tree;
}

module Past_entries = struct

  let fold f e0 {past_entries; _ } =
    Rope.fold f e0 past_entries

  let replace ({prev_index; _ } as replacement) ({past_entries; _} as log) =
    {log with past_entries = Rope.replace ~prev:prev_index ~data:replacement past_entries}

  let find ~index {past_entries; _ } =
    Rope.find ~index past_entries

  let add new_interval past_entries =
    Rope.add ~prev:new_interval.prev_index ~last:new_interval.last_index ~data:new_interval past_entries
end

let empty = {
  recent_entries = [];
  past_entries = None;
  log_size = 0;
  term_tree  = Term_tree.empty
}

let last_log_index_and_term {recent_entries; _ } =
  match recent_entries with
  | {index;term; _}::_ -> (index, term)
  | [] -> (0, 0)

let last_log_index log =
  fst @@ last_log_index_and_term log

let last_past_entry_index x =
  match Rope.last_entry_index_in_rope x with
  | None -> 0
  | Some x -> x

let rev_log_entries_since since log =
  let {recent_entries ; past_entries; _} = log in

  match recent_entries with
  | []  -> []
  | _::_ ->

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
        Past_interval.make ~since recent_entries
      else
        Past_interval.sub since @@ Past_entries.find ~index:(since + 1) log
    in
    match interval.rev_log_entries with
    | Expanded {entries} -> entries
    | Compacted _        -> []

let term_of_index index {term_tree; _} =
  Term_tree.term_of_index index term_tree

let add_log_datas current_term datas log =

  let rec aux term last_log_index recent_entries log_size term_tree = function
    | [] -> (recent_entries, log_size, term_tree)
    | (data, id)::tl ->
      let last_log_index = last_log_index + 1 in
      let handle_new_log_entry_entry = {
        index = last_log_index;
        term;
        data;
        id;
      } in
      let recent_entries = handle_new_log_entry_entry :: recent_entries in
      let term_tree  = Term_tree.handle_new_log_entry term_tree handle_new_log_entry_entry in

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

let add_log_entries ~rev_log_entries log =

  let rec aux log_size term_tree recent_entries = function
    | [] ->
      {log with log_size; term_tree; recent_entries}

    | hd::tl ->
      let term_tree = Term_tree.handle_new_log_entry term_tree hd in
      aux (log_size + 1) term_tree (hd::recent_entries) tl
  in

  aux log.log_size log.term_tree log.recent_entries rev_log_entries

let remove_log_since ~prev_log_index ~prev_log_term log =

  let rec aux log_size = function
    | [] ->
      if prev_log_index = 0
      then ([], 0)
      else raise Not_found

    | ({index; term; _ }::_ as recent_entries) when index = prev_log_index &&
                                                     term = prev_log_term ->
      (recent_entries, log_size)

    | {index; _ }::_ when index = prev_log_index ->
      raise Not_found

    |  _::tl -> aux (log_size - 1) tl
  in

  match log.recent_entries with
  | {index; _}::_ when prev_log_index > index ->
    raise Not_found

  | _ ->
    let recent_entries, log_size = aux log.log_size log.recent_entries in
    let term_tree = match recent_entries with
      | [] -> Term_tree.empty
      | hd::_ -> Term_tree.handle_log_removal log.term_tree hd
    in
    {log with recent_entries; log_size; term_tree}

let service ~prev_commit_index ~log_interval_size log =

  let past_entries = log.past_entries in
  let since = last_past_entry_index past_entries in

  if prev_commit_index - since < log_interval_size
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
      | ({index; _ } as l)::_ when index = prev_commit_index -> [l]
      | log_entry::tl -> log_entry :: (aux tl)
    in

    {log with
      recent_entries = aux log.recent_entries;
      past_entries = past_entries;
    }

let int_compare (x:int) (y:int) = Pervasives.compare x y

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
      term_tree = Term_tree.handle_new_log_entry log.term_tree log_entry;
    }
    else {
      log with
      log_size = log.log_size + 1;
      term_tree = Term_tree.handle_new_log_entry log.term_tree log_entry;
    }

  let log_of_t2 x = x

end (* Builder *)
