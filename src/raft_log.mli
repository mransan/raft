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

val rev_log_entries_since : int -> Raft_pb.log -> (Raft_pb.log_entry list * int)  

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
  
  val contains_next_of : int -> t -> bool
  (** [contains_next_of since interval] return true if [interval] contains 
      the [log_entry] which index is next of [since]. 
    *)
end 

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

  val log_of_t2 : t2 -> Raft_pb.log 

end (* Builder *) 
