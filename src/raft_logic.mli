(** Raft Protocol Functional implementation *)

(** The Raft protocol defines 2 type of communication between servers:
    {ul
    {li Request Vote}
    {li Append Entry}
    }
  
    This module implements the RAFT protocol logic in a functional way and
    agnostic of any transport mechanism (TCP/UDP/HTTP).
 *)

(** {2 Types} *)

type time = float
(** Monotonic time *)

type server_id = int
(** server id *)

type message_to_send = Raft_pb.message * server_id
(** message to send to the given server *)


(** {2 Protocol Implementation} *)

val make_initial_state :
  configuration:Raft_pb.configuration ->
  now:time ->
  id:int ->
  unit ->
  Raft_state.t
(** [make_initial_state ~configuration ~now ~id ()] creates an initial server
    state.
  
    The server is initially a [Follower] and its [election_deadline] will be
    set according to the [configuration].
 *)

val handle_message :
  Raft_state.t ->
  Raft_pb.message ->
  time ->
  Raft_state.t * (message_to_send list) * Raft_pb.notification list 
(** [handle_message state message now] handle an incoming Raft message. 
  *)

val handle_new_election_timeout :
  Raft_state.t ->
  time ->
  Raft_state.t * (message_to_send list) * Raft_pb.notification list 
(** [handle_new_election_timeout state now] implements the state change to
    a Candidate along with the list of request vote message to send.
  *)

val handle_heartbeat_timeout :
  Raft_state.t ->
  time ->
  Raft_state.t * (message_to_send list)
(** [handle_heartbeat_timeout state now] computes the necessary messages
     to send in the case of a heartbeat.
  *)

type new_log_response =
  | Appended of Raft_state.t * message_to_send list
    (** The new log can correctly be handled by this server (ie it is
        a valid [Leader] and new [Append_entries] request message can be
        sent to follower servers.
      *)
  | Forward_to_leader of int
    (** If the current server is not a [Leader], the new log entry should
        not be handled by this server but rather forwarded to the current [Leader]
        which id is returned.
      *)

  | Delay
    (** The current state of the system (as this server is aware) does not
        seem to be in a configuration that can handle the the log.
        For instance during an election it is possible that this server
        is a [Candidate] waiting for more votes. Another scenario would be
        that this server is a [Follower] which has not yet received confirmation
        from a valid [Leader]
      *)

val handle_add_log_entries: Raft_state.t -> (bytes * string) list -> time -> new_log_response
(** [handle_add_log_entry state data now] processes [data] and return the follow
    up response. See [new_log_response] for more information.
  *)

val next_timeout_event : Raft_state.t -> time -> Raft_pb.timeout_event
(** [next_timeout_event state now] returns the timeout information
    that the serve should implement.
   
    The server application is responsible for managing the main event
    loop such as listening for messaging and waking up for timeout events.
  *)
