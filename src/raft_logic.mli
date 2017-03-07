(** Protocol Implementation *)

(** This module implements the RAFT protocol logic in a functional way and
    agnostic of any transport mechanism (TCP/UDP/HTTP).  *)

(** {2 Types} *)

(** RAFT Message to send to the given server *)
type message_to_send = Raft_types.message * Raft_types.server_id

(** Data returned by each of the protocol event *)
type result = {
  state : Raft_types.state;  
    (** The new state *)
  messages_to_send : message_to_send list; 
    (** Raft messages to be send to the given servers.*)
  leader_change : Raft_types.leader_change option;
    (** Notification of a change in leaderhsip.*)
  committed_logs : Raft_log.log_entry list;
    (** Log entries which are now commited.*)
  added_logs : Raft_log.log_entry list; 
    (** Log entries added to the log. Note that this could overlap 
        with [committed_logs].*)
  deleted_logs : Raft_log.log_entry list;
    (** Log entries deleted from the log.*)
}

(** {2 Protocol Event Implementation} *)

val init :
  configuration:Raft_types.configuration ->
  now:Raft_types.time ->
  server_id:int ->
  unit ->
  Raft_types.state
(** [init ~configuration ~now ~id ()] creates an initial server
    state.

    The server is initially a [Follower] and its [election_deadline] will be
    set according to the [configuration].  *)

val handle_message :
  Raft_types.state ->
  Raft_types.message ->
  Raft_types.time ->
  result 
(** [handle_message state message now] handle a new RAFT message received.*)

val handle_new_election_timeout :
  Raft_types.state ->
  Raft_types.time ->
  result 
(** [handle_new_election_timeout state now] handles a new election timeout.*)

val handle_heartbeat_timeout :
  Raft_types.state ->
  Raft_types.time ->
  result 
(** [handle_heartbeat_timeout state now] handles an heartbeat event.*)

type new_log_response =
  | Appended of result 
    (** The new log can correctly be handled by this server (ie it is
        a valid [Leader] and new [Append_entries] request message can be
        sent to follower servers.*)
  | Forward_to_leader of int
    (** If the current server is not a [Leader], the new log entry should
        not be handled by this server but rather forwarded to the current 
        [Leader] which id is returned.*)
  | Delay
    (** The current state of the system (as this server is aware) does not
        seem to be in a configuration that can handle the the log.
        For instance during an election it is possible that this server
        is a [Candidate] waiting for more votes. Another scenario would be
        that this server is a [Follower] which has not yet received confirmation
        from a valid [Leader].*)

val handle_add_log_entries:
  Raft_types.state ->
  (bytes * string) list ->
  Raft_types.time ->
  new_log_response
(** [handle_add_log_entries state data now] processes [data] and return the follow
    up response. See [new_log_response] for more information.*)

(** {2 Utilities} *)

val next_timeout_event :
  Raft_types.state ->
  Raft_types.time ->
  Raft_types.timeout_event
(** [next_timeout_event state now] returns the timeout information
    that the serve should implement.

    The server application is responsible for managing the main event
    loop such as listening for messaging and waking up for timeout events.*)

val committed_entries_since : 
  since:int -> 
  Raft_types.state -> 
  Raft_log.log_entry list 
(** [committed_entries_since ~since state] returns all the committed entries
    since the log index [since].*)
