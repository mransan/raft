(** Caching logic for the log entries *)

(** {2 Types} *)

type global_cache = Raft_pb.log_interval_rope option

type local_cache = Raft_pb.log_interval

(** {2 Global cache} *)

val empty : global_cache 
(** [empty] global cache to be used when initializing a state. 
  *)

val update_global_cache : Raft_pb.state -> Raft_pb.state 
(** [update_global_cache state]
    If a large enough number of log entry has been added to the state 
    log since, the a local cache of those added logs is computed and 
    added to the state log. 
   
  *)

val fold : ('a -> local_cache -> 'a) -> 'a -> global_cache -> 'a 
(** [fold f e0 global_cache] iterates over all the local cache starting in
    ascending order. The first local cache would therefore be the one containing
    the earliest entries. 
  *)

(** {2 Local Cache} *)

val make : ?until:int -> since:int -> Raft_pb.log_entry list -> local_cache
(** [make ~until ~since log] creates a local cache with the [log] entries 
    in the range : `]since; until]`
   
    If [until] is not provided it defaults to the last log entries in [log].
  *)

val contains_next_of : int -> local_cache  -> bool 
(** [contains_next_of prev_index local_cache] returns [true] is [local_cache] 
    contains the index following [prev_index]. 
  *) 

val update_local_cache : 
  int -> 
  Raft_pb.log_entry list ->
  local_cache  -> 
  global_cache -> 
  local_cache
(** [update_local_cache since log local_cache global_cache] 
   
    Computes a local cache of log entries in reverse order 
    since the given index.
   
    The computation of this new log cache might use a subset 
    of the [local_cache] a subset of the [global_cache] or the 
    [log] data directly.
   
 *)
