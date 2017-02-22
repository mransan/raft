(** Helper function for manipualting Protobuf Generated types.
 *)

module Configuration : sig

  val is_majority : Raft_types.configuration -> int -> bool
  (** [is_majority configuration nb] returns true if [nb] is a majority
   *)

end (* Configuration *)

module Timeout_event : sig

  val next : Raft_types.state -> float -> Raft_types.timeout_event

end (* Timeout_event *)

val leader_change : 
  Raft_types.state -> 
  Raft_types.state -> 
  Raft_types.leader_change option
(** [notifications before after] computes the notification between 2 states
 *)

val committed_logs :
  Raft_types.state -> 
  Raft_types.state -> 
  Raft_log.log_entry list
