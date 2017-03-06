(** All Log related logic *)

(** {2 Types} *)

(** unit of log *)
type log_entry = {
  index : int;
  term : int;
  data : bytes;
  id : string;
}

(** Log size limitation parameters.
    
    The log data structure can grow to an inifinite size as new logs 
    keeps being appended. However for practical reason it is important to limit
    the size of the log in memory. [upper_bound] defines the maximum size, 
    while [lower_bound] is the size the log is truncated to when
    reaching its [upper_bound] value. *)
type max_log_size = {
  upper_bound : int;
  lower_bound : int; 
}

module IntMap : Map.S with type key = int

(** log data structure *)
type t = {
  recent_entries : log_entry IntMap.t;
  max_log_size : max_log_size;
}

(** {2 Creators} *)

val empty : max_log_size -> t
(** [empty] is an empty log *)

(** {2 Accessors} *)

val last_log_index_and_term : t -> (int * int)
(** [last_log_index_and_term state] returns the [(index, term)] of the last log
    entry. If the log is empty [(0, 0)] is returned.*)

val last_log_index: t ->  int
(** [last_log_index state] returns the index of the last log entry.*)

(** Various representation for the size of a subset of the log *)
type size = 
  | Number of int  
  | Bytes of int * int (* (value, overhead per entry) *) 

val log_entries_since : since:int -> max:size -> t -> (log_entry list * int)
(** [log_entries_since ~since:index ~max log] returns 
    [(log_entries, prev_log_term)] in chronological order. If not empty 
    [List.hd log_entrie] is the log entry with [index = since + 1]. 
    
    [max] size is enforced by the function.
    *)

(** {2 Modifiers} *)

(** A data structure detailing the modification done to the log by the 
    modifiers functions *)
type log_diff = {
  added_logs : log_entry list; 
  deleted_logs : log_entry list;
}

val empty_diff : log_diff 
(** [empty_diff] represent the no difference value *)


val merge_diff : log_diff -> log_diff -> log_diff 
(** [merge_diff lhs rhs] merges together 2 different diff. Right now it only
    works if lhs or rhs are not both containing either added logs or removed
    logs *)

val add_log_datas : int -> (bytes * string) list -> t -> (t * log_diff) 
(** [add_log_datas current_term datas state] adds [datas] to the log of the
    [state].

    Note that the logs are in chronological order
    In other word [List.hd datas] is the earliest entry
    and should be appended first to the server logs.*)

val add_log_entries : log_entries:log_entry list -> t -> (t * log_diff)
(** [add_log_entries log_entries log] appends [log_entries] to the
    [log]. 
    This function assumes [log_entries] are in chronological order. *)

val remove_log_since : 
  prev_log_index:int -> 
  prev_log_term:int -> 
  t -> 
  (t * log_diff)
(** [remove_log_since ~prev_log_index ~prev_log_term log] removes all the
    entries which are after the log_entry defined by [prev_log_index] and
    [prev_log_term].

    If [log] does not contain any log_entry defined by [prev_log_index] and
    [prev_log_term] then [Not_found] is raised.  *)

(** {2 Utilities} *)

val pp_log_entry : Format.formatter -> log_entry -> unit 
(** [pp_log_entry fmt log_entry] format [log_entry] *)

(** Helper module to build the log type from saved log entries. 
    
    This module is designed to be used by a RAFT server at start time. *)
module Builder : sig

  type builder

  val make : max_log_size -> builder

  val add_log_entry : builder -> log_entry -> builder

  val to_log : builder -> t

end (* Builder *)
