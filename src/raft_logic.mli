(** Raft protocol functional implementation *)

(** {2 Request_vote} *)

module Request_vote : sig 

  val make : Raft_pb.state -> Raft_pb.request_vote_request
  (** [make state] create the a request vote
   *)

  val handle_request : 
    Raft_pb.state -> 
    Raft_pb.request_vote_request ->
    (Raft_pb.state * Raft_pb.request_vote_response) 
  (** [handle_request state request] returns the updated 
      state and the response to be sent back. 
    *)

  val handle_response : 
    Raft_pb.state -> 
    Raft_pb.request_vote_response -> 
    (Raft_pb.state * Raft_pb.follow_up_action)
  (** [handle_response state response] returns the updated
      state along with the expected follow up action to 
      be performed.
    *)

end (* Request_vote *)

(** {2 Append_entries} *)

module Append_entries : sig 

  val make : 
    Raft_pb.state -> 
    int -> 
    Raft_pb.append_entries_request option
  (** [make state server_id] create an `AppendEntry` request for 
      [server_id].
   *)

  val handle_request : 
    Raft_pb.state -> 
    Raft_pb.append_entries_request ->
    (Raft_pb.state * Raft_pb.append_entries_response) 
  (** [handle_request state request] returns the updated 
      state and the response to be sent back. 
    *)

  val handle_response : 
    Raft_pb.state -> 
    Raft_pb.append_entries_response -> 
    (Raft_pb.state * Raft_pb.follow_up_action)
  (** [handle_response state response] returns the updated
      state along with the expected follow up action to 
      be performed.
    *)

end (* Append_entries *)
