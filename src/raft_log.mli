(** Caching logic for the log entries *)

type t = Raft_pb.log 

val empty : t 
(** [empty] is an empty log *)

val last_log_index_and_term : Raft_pb.state -> (int * int) 
(** [last_log_index_and_term state] return the [(index, term)] of the last log
    entry. 
  *)

val last_log_index: Raft_pb.state ->  int 
(** [last_log_index state] return the index of the last log entry. 
  *)

val add_logs : (bytes * string) list -> Raft_pb.state -> Raft_pb.state 
(** [add_logs datas state] adds [datas] to the log of the [state]. 
    
    Note that the logs are in chronological order 
    
      In other word [List.hd datas] is the earliest entry 
      and should be appended first to the server logs.
     
    Any subsequent logical actions like creating Append Entries request is 
    left to the caller. 
  *) 

val merge_logs : 
  prev_log_index:int -> 
  prev_log_term:int -> 
  rev_log_entries: Raft_pb.log_entry list-> 
  Raft_pb.state ->
  (Raft_pb.state * bool)
(** [merge_logs ~prev_log_index ~prev_log_term ~rev_log_entries state] merges 
    the [rev_log_entries] into the [state] log starting at [prev_log_index]. 
   
    We assume that both the previous log entry information (index and term) along
    with the [rev_log_entries] are the thruth about the log state. (This function
    is typically used when receiving an [Append_entries] request from a [Leader]) 
   
    As a concequence if the previous log entry could not be fond in 
    the [state.log] then the state log is truncated with all entries after 
    that one removed. [false] is returned in such a case. 
   
    If [state.log] contains additional log entries past the previous log entries
    those will be removed and replaced with the [rev_log_entries]. Since the merge
    is still successfull [true] is returned.
   
    The most likely scenario is that the [state.log] last entry is matching the 
    previous log entry and [rev_log_entries] is simply appended.
  *)

val service : prev_commit_index:int -> Raft_pb.state -> Raft_pb.state 
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

module Past_interval : sig 

  type t = Raft_pb.log_interval  

  val fold : ('a -> t -> 'a) -> 'a -> Raft_pb.log -> 'a 
  (** [fold f e0 past_entries] iterates over all the local cache starting in
      ascending order. The first local cache would therefore be the one containing
      the earliest entries. 
    *)

  val replace : t ->  Raft_pb.log -> Raft_pb.log 
  (** [replace interval past_entries] replaces the matching [interval] 
      in [past_entries]. 
  
      This is particularly (and uniquely useful when compacting a [interval]
      in the global cache.
  
      raises [Failure] if [interval] cannot be matched.
    *)

  val find : index:int -> Raft_pb.log -> t
  (** [find ~index past_entries] finds the interval \]prev_index;last_index\] which contains
      [index]. 
  
      raises [Not_found] in case no [log_interval] contains [index].
    *) 
end 


val rev_log_entries_since : int -> Raft_pb.log -> (Raft_pb.log_entry list * int)  

type past_entries = Raft_pb.log_interval_rope option

type interval = Raft_pb.log_interval

val from_list : interval list -> past_entries

(** {2 Local Cache} *)

val make : ?until:int -> since:int -> Raft_pb.log_entry list -> interval
(** [make ~until ~since log] creates a local cache with the [log] entries 
    in the range : \]since; until\]
   
    If [until] is not provided it defaults to the last log entries in [log].
  *)

val contains_next_of : int -> interval  -> bool 
(** [contains_next_of prev_index interval] returns [true] is [interval] 
    contains the index following [prev_index]. 
  *) 

(*
val update_interval : 
  int -> 
  interval  -> 
  t -> 
  interval
  *)
(** [update_interval since log interval past_entries] 
   
    Computes a local cache of log entries in reverse order 
    since the given index.
   
    The computation of this new log cache might use a subset 
    of the [interval] a subset of the [past_entries] or the 
    [log] data directly.
   
 *)

