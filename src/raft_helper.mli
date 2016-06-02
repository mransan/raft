(** Helper function for manipualting Protobuf Generated types. 
 *)


module Configuration : sig

  val is_majority : Raft_pb.configuration -> int -> bool 
  (** [is_majority configuration nb] returns true if [nb] is a majority
   *)

end (* Configuration *)

module Timeout_event : sig

  val next : Raft_state.t -> float -> Raft_pb.timeout_event 

end (* Timeout_event *)
