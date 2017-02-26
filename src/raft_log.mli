(** All Log related logic *)

type log_entry = {
  index : int;
  term : int;
  data : bytes;
  id : string;
}

val pp_log_entry : Format.formatter -> log_entry -> unit 

(** In memory log size limitation parameters 
    
    When adding new log entries would make the size of the log go over 
    [upper_bound], then log is truncated down to [lower_bound]. *)
type max_log_size = {
  upper_bound : int;
  lower_bound : int; 
}

module IntMap : Map.S with type key = int

type t = {
  recent_entries : log_entry IntMap.t;
  log_size : int;
  max_log_size : max_log_size;
}

type log_diff = {
  added_logs : log_entry list; 
  deleted_logs : log_entry list;
}

(** {2 Creators} *)

val empty : max_log_size -> t
(** [empty] is an empty log *)

val empty_diff : log_diff 

(** {2 Accessors} *)

val last_log_index_and_term : t -> (int * int)
(** [last_log_index_and_term state] return the [(index, term)] of the last log
    entry.  *)

val last_log_index: t ->  int
(** [last_log_index state] return the index of the last log entry.  *)

val log_entries_since : since:int -> max:int -> t -> (log_entry list * int)
(** [rev_log_entries_since index log] returns the log entries in
    reverse order (ie the earliest log is at the front) from (but excluding)
    [since] and until the latest log entry in [log].

    In other word the returned data is :
      \]since ; since + max \] *)

(** {2 Modifiers} *)

val add_log_datas : int -> (bytes * string) list -> t -> (t * log_diff) 
(** [add_log_datas current_term datas state] adds [datas] to the log of the
    [state].

    Note that the logs are in chronological order

    In other word [List.hd datas] is the earliest entry
    and should be appended first to the server logs.

    Any subsequent logical actions like creating Append Entries request is
    left to the caller.  *)

val add_log_entries : rev_log_entries:log_entry list -> t -> (t * log_diff)
(** [add_log_entries rev_log_entries log] appends [rev_log_entries] to the
    [log]. The assumption is that the entries are in reverse order so
    the last log_entry in [rev_log_entries] will be the first log_entry 
    in [log] after this function executes.  *)

val remove_log_since : 
  prev_log_index:int -> prev_log_term:int -> t -> (t * log_diff)
(** [remove_log_since ~prev_log_index ~prev_log_term log] removes all the
    entries which are after the log_entry defined by [prev_log_index] and
    [prev_log_term].

    If [log] does not contain any log_entry defined by [prev_log_index] and
    [prev_log_term] then [Not_found] is raised.
  *)

val merge_diff : log_diff -> log_diff -> log_diff 

module Builder : sig

  type builder

  val make : max_log_size -> builder

  val add_log_entry : builder -> log_entry -> builder

  val to_log : builder -> t

end (* Builder *)
