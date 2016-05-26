(** RAFT Server state Utilities, agnostic of the role the server has *)

(** {2 Log functionality} *)

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

(** {2 Role functionality} *)

val is_follower : Raft_pb.state -> bool
(** [is_follower state] returns [true] if [state] role is a follower, [false]
    otherwise. 
  *)

val is_candidate : Raft_pb.state -> bool
(** [is_candidate state] returns [true] if [state] role is a candidate, [false]
    otherwise. 
  *)

val is_leader : Raft_pb.state -> bool
(** [is_leader state] returns [true] if [state] role is a leader, [false]
    otherwise. 
  *)
  
val current_leader: Raft_pb.state -> int option
(** [current_leader state] return the current leader for the current term. 
    
    If no leader is known then [None] is returned. 
  *)

(** {2 Maintenance} *)

val notifications : Raft_pb.state -> Raft_pb.state -> Raft_pb.notification list 
(** [notifications before after] computes the notification between 2 states 
  *)

val compaction : Raft_pb.state -> Raft_pb.compaction_report 
