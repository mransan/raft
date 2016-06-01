(** RAFT Server state Utilities, agnostic of the role the server has *)

(** {2 Log functionality} *)


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
