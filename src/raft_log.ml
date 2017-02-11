open Raft_pb

module Term_tree = struct

  module Rope = Raft_rope

  type t = {
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

  (* [handle_new_log_entry term_tree log_entry] returns a new term tree taking
   * into account that [log_entry] is now the latest one.
   *
   * The [term_tree] will only contain a new term interval if the [log_entry]
   * is from a different term than the previous [log_entry].
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
      let prev = last_index_of_previous_terms previous_terms in
      let last = last_index in
      let data = last_term in
      {
        previous_terms = Rope.add ~prev ~last ~data previous_terms;
        last_log_entry = log_entry
      }

    | _ -> assert(false)

  (* [handle_log_removal term_tree log_entry] resets the [term_tree] so that
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

  let term_of_index index {previous_terms; last_log_entry} =
    let {term; index = last_index;_} = last_log_entry in
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

let sub_log_entries ?until ~since log_entries =

  let log_entries =
    match until with
    | None -> log_entries
    | Some until ->
      let rec aux = function
        | ({index; _ }::_) as log_entries when index = until -> log_entries
        | _::tl -> aux tl
        | [] -> []
      in
      aux log_entries
  in

  let rec aux rev_log_entries = function
    | [] ->
      if since = 0
      then rev_log_entries
      else begin
        Printf.eprintf "[Raft_logic] Internal2 error invalid log index\n%!";
        failwith "[Raft_logic] Internal2 error invalid log index"
      end

    | {index; _}::_ when index = since ->
      rev_log_entries

    | hd::tl -> aux (hd::rev_log_entries) tl
  in
  aux [] log_entries

type t = {
  recent_entries : log_entry list;
  log_size : int;
  term_tree : term_tree;
}

let empty = {
  recent_entries = [];
  log_size = 0;
  term_tree = Term_tree.empty
}

let last_log_index_and_term {recent_entries; _ } =
  match recent_entries with
  | {index;term; _}::_ -> (index, term)
  | [] -> (0, 0)

let last_log_index log =
  fst @@ last_log_index_and_term log

let rev_log_entries_since since log =
  let {recent_entries ; _} = log in
  match recent_entries with
  | []  -> []
  | _::_ -> sub_log_entries ~since recent_entries

let term_of_index index {term_tree; _} =
  Term_tree.term_of_index index term_tree

let add_log_datas current_term datas log =

  let rec aux term last_log_index recent_entries log_size term_tree = function
    | [] -> (recent_entries, log_size, term_tree)
    | (data, id)::tl ->
      let last_log_index = last_log_index + 1 in

      let handle_new_log_entry_entry = {
        index = last_log_index;
        term; data; id;
      } in

      let recent_entries =
        handle_new_log_entry_entry :: recent_entries
      in

      let term_tree =
        Term_tree.handle_new_log_entry term_tree handle_new_log_entry_entry
      in

      aux term last_log_index recent_entries (log_size + 1) term_tree tl
  in

  let term = current_term in
  let last_log_index = last_log_index log in
  let recent_entries = log.recent_entries in
  let log_size = log.log_size in

  let recent_entries, log_size, term_tree =
    aux term last_log_index recent_entries log_size log.term_tree datas
  in
  {recent_entries; log_size; term_tree}

let add_log_entries ~rev_log_entries log =

  let rec aux log_size term_tree recent_entries = function
    | [] ->
      {log_size; term_tree; recent_entries}

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
    {recent_entries; log_size; term_tree}

module Builder = struct

  type builder = t

  let make () = empty

  let add_log_entry log log_entry =
    assert(log_entry.index > (last_log_index log));
    {
      recent_entries = log_entry :: log.recent_entries;
      log_size = log.log_size + 1;
      term_tree = Term_tree.handle_new_log_entry log.term_tree log_entry;
    }

  let to_log x = x

end (* Builder *)
