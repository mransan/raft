(** Raft Protocol Functional implementation *)

(** The Raft protocol defines 2 type of communication between servers: 
  {ul 
  {li Request Vote}
  {li Append Entry}
  } 

  This module implements the RAFT protocol logic in a functional way and 
  agnostic of any transport mechanism (TCP/UDP/HTTP). It focuses on the following:
  {ul
  {li Create the request value based on the server state}
  {li Create the new server state and response when receiving a request}
  {li Create the new server state and follow up action when receiving a response}
  }

 *)

type time = float 
  
type message_to_send = Raft_pb.message * int 

(** {2 Request_vote} *)

module Request_vote : sig 

  val make : Raft_pb.state -> Raft_pb.request_vote_request
  (** [make state] create the a request vote
   *)

  val handle_request : 
    Raft_pb.state -> 
    Raft_pb.request_vote_request ->
    time ->  
    (Raft_pb.state * Raft_pb.request_vote_response) 
  (** [handle_request state request] returns the updated 
      state and the response to be sent back. 
    *)

  val handle_response : 
    Raft_pb.state -> 
    Raft_pb.request_vote_response -> 
    time -> 
    (Raft_pb.state * (message_to_send list))
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
    time -> 
    (Raft_pb.state * Raft_pb.append_entries_response) 
  (** [handle_request state request] returns the updated 
      state and the response to be sent back. 
    *)

  val handle_response : 
    Raft_pb.state -> 
    Raft_pb.append_entries_response -> 
    time ->
    (Raft_pb.state * (message_to_send list))
  (** [handle_response state response] returns the updated
      state along with the expected follow up action to 
      be performed.
    *)

end (* Append_entries *)

module Message : sig 

  val handle_message : 
    Raft_pb.state -> 
    Raft_pb.message -> 
    time ->
    Raft_pb.state * (message_to_send list)  
    (** [handle_message state message now] process the message by dispatching it
        to the appropriate module. It also handles keeping track to which server
        the response must be sent. 
     *) 

  val handle_new_election_timeout :
    Raft_pb.state -> 
    time ->
    Raft_pb.state * (message_to_send list) 
  (** [handle_new_election_timeout state now] implements the state change to 
      a Candidate along with the list of request vote message to send. 
    *)

  val handle_heartbeat_timeout : 
    Raft_pb.state -> 
    time -> 
    Raft_pb.state * (message_to_send list) 
  (** [handle_heartbeat_timeout state now] computes the necessary messages
      to send in the case of a heartbeat. 
   *)

  type new_log_response = 
    | Appended of Raft_pb.state * message_to_send list
      (** The new log can correctly be handled by this server (ie it is 
        * a valid [Leader] and new [Append_entries] request message can be 
        * sent to follower servers. 
        *)
    | Forward_to_leader of int 
      (** If the current server is not a [Leader], the new log entry should 
        * not be handled by this server but rather forwarded to the current [Leader]
        * which id is returned. 
        *)

    | Delay 
      (** The current state of the system (as this server is aware) does not 
        * seem to be in a configuration that can handle the the log. 
        * For instance during an election it is possible that this server
        * is a [Candidate] waiting for more votes. Another scenario would be 
        * that this server is a [Follower] which has not yet received confirmation
        * from a valid [Leader]
        *)
  
  val handle_add_log_entry : Raft_pb.state -> bytes -> time -> new_log_response 
  (** [handle_add_log_entry state data now] processes [data] and return the follow
   *   up response. See [new_log_response] for more information. 
   *)

end 
