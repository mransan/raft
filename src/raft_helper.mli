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
