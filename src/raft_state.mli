(** RAFT Server state Utilities, agnostic of the role the server has *)

(** {2 Log functionality} *)

type t = {
  id : int;
  current_term : int;
  log : Raft_log.t;
  commit_index : int;
  role : Raft_pb.role;
  configuration : Raft_pb.configuration;
}

(** {2 Role functionality} *)

val is_follower : t -> bool
(** [is_follower state] returns [true] if [state] role is a follower, [false]
    otherwise. 
  *)

val is_candidate : t -> bool
(** [is_candidate state] returns [true] if [state] role is a candidate, [false]
    otherwise. 
  *)

val is_leader : t -> bool
(** [is_leader state] returns [true] if [state] role is a leader, [false]
    otherwise. 
  *)
  
val current_leader: t -> int option
(** [current_leader state] return the current leader for the current term. 
    
    If no leader is known then [None] is returned. 
  *)

(** {2 Maintenance} *)

val notifications : t -> t -> Raft_pb.notification list 
(** [notifications before after] computes the notification between 2 states 
  *)

val compaction : t -> Raft_pb.compaction_report 
