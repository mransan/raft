(** All Log related logic *)


type term_tree

val pp_term_tree : Format.formatter -> term_tree -> unit

type t = {
  recent_entries : Raft_pb.log_entry list;
  log_size : int;
  term_tree : term_tree;
}

(** {2 Creators} *)

val empty : t
(** [empty] is an empty log *)

(** {2 Accessors} *)

val last_log_index_and_term : t -> (int * int)
(** [last_log_index_and_term state] return the [(index, term)] of the last log
    entry.
  *)

val last_log_index: t ->  int
(** [last_log_index state] return the index of the last log entry.
  *)

val rev_log_entries_since : int -> t -> Raft_pb.log_entry list
(** [rev_log_entries_since index log] returns the log entries in
    reverse order (ie the earliest log is at the front) from (but excluding)
    [since] and until the latest log entry in [log].

    In other word the returned data is :
    \]since ; last_log_index log\]
 *)

val term_of_index : int -> t -> int
(** [term_of_index index log ] returns the term associated with [index],
    if [index] is not part of the log then [Not_found] is raised.
  *)

(** {2 Modifiers} *)

val add_log_datas : int -> (bytes * string) list -> t -> t
(** [add_log_datas current_term datas state] adds [datas] to the log of the 
    [state].

    Note that the logs are in chronological order

    In other word [List.hd datas] is the earliest entry
    and should be appended first to the server logs.

    Any subsequent logical actions like creating Append Entries request is
    left to the caller.
  *)

val add_log_entries : rev_log_entries:Raft_pb.log_entry list -> t -> t
(** [add_log_entries rev_log_entries log] appends [rev_log_entries] to the
    [log]. The assumption is that the entries are in reverse order so
    the last log_entry in [rev_log_entries] will be the first log_entry in [log]
    after this function executes.
  *)

val remove_log_since : prev_log_index:int -> prev_log_term:int -> t -> t
(** [remove_log_since ~prev_log_index ~prev_log_term log] removes all the 
    entries which are after the log_entry defined by [prev_log_index] and 
    [prev_log_term].

    If [log] does not contain any log_entry defined by [prev_log_index] and 
    [prev_log_term] then [Not_found] is raised.
  *)

module Builder : sig

  type builder

  val make : unit -> builder

  val add_log_entry : builder -> Raft_pb.log_entry -> builder

  val to_log : builder -> t

end (* Builder *)
