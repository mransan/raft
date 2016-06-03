(** Caching logic for the log entries *)


module Past_interval : sig 

  type t = Raft_pb.log_interval  
  
  val contains_next_of : int -> t -> bool
  (** [contains_next_of since interval] return true if [interval] contains 
      the [log_entry] which index is next of [since]. 
    *)
end 


type term_tree 

val pp_term_tree : Format.formatter -> term_tree -> unit 

type t = {
  recent_entries : Raft_pb.log_entry list;
  log_size : int;
  past_entries : Raft_pb.log_interval Raft_rope.t;
  term_tree : term_tree;
} 

module Past_entries : sig

  val fold : ('a -> Past_interval.t -> 'a) -> 'a -> t -> 'a 
  (** [fold f e0 past_entries] iterates over all the local cache starting in
      ascending order. The first local cache would therefore be the one containing
      the earliest entries. 
    *)

  val replace : Past_interval.t ->  t -> t
  (** [replace interval past_entries] replaces the matching [interval] 
      in [past_entries]. 
  
      This is particularly (and uniquely useful when compacting a [interval]
      in the global cache.
  
      raises [Failure] if [interval] cannot be matched.
    *)

  val find : index:int -> t -> Past_interval.t
  (** [find ~index past_entries] finds the interval \]prev_index;last_index\] which contains
      [index]. 
  
      raises [Not_found] in case no [log_interval] contains [index].
    *) 
end 

val empty : t 
(** [empty] is an empty log *)

val last_log_index_and_term : t -> (int * int) 
(** [last_log_index_and_term state] return the [(index, term)] of the last log
    entry. 
  *)

val last_log_index: t ->  int 
(** [last_log_index state] return the index of the last log entry. 
  *)

val add_logs : int -> (bytes * string) list -> t -> t 
(** [add_logs current_term datas state] adds [datas] to the log of the [state]. 
    
    Note that the logs are in chronological order 
    
      In other word [List.hd datas] is the earliest entry 
      and should be appended first to the server logs.
     
    Any subsequent logical actions like creating Append Entries request is 
    left to the caller. 
  *) 

val service : prev_commit_index:int -> configuration:Raft_pb.configuration -> t -> t
(** [service ~prev_commit_index state]
    If a large enough number of [log_entry]s have been added to the [state]
    log between :
    {ul
    {- The last log entry stored in the global cache }
    {- The [prev_commit_index]}   
    } 
    then those [log_entry]s are added to the cache. Additionally except for 
    the [prev_commit_index] those [log_entries] are also removed from the 
    [state] log. 
  *)

val rev_log_entries_since : int -> t -> Raft_pb.log_entry list

val term_of_index : int -> t -> int 

val append_log_entries : Raft_pb.log_entry list -> t -> t 

val remove_log_since : int -> int -> t -> t 

(** Log Builder utilities to re-create a log value from disk records. 
    
    This expects the application to store on disk both the Past Intervals
    as well as each log entries. 

    The building process is split into 2 phases: 
    {ul
    {- Adding the past intervals}
    {- Adding all the log entries} 
    }

  *)
module Builder : sig 

  type t1 
  (* Phase 1 builder *)

  type t2 
  (* Phase 2 builder *)

  val make_t1 : unit -> t1 

  val add_interval : t1 -> Past_interval.t -> t1 
  
  val t2_of_t1 : t1 -> t2 

  val add_log_entry : t2 -> Raft_pb.log_entry -> t2 

  val log_of_t2 : t2 -> t 

end (* Builder *) 
