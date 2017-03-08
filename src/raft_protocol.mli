(** Protocol Implementation *)

(** This module implements the RAFT protocol logic in a functional way and is 
    agnostic of:
    {ul
    {- {b Transport protocol}: This module simply defines which message
       should be sent upon any RAFT protocol event. It is the caller 
       responsability to handle all the communication between RAFT servers. 
       Note that the package {b raft-pb} provides message serialization, 
       based on Protobuf technology.}
    {- {b Persistent storage}: The RAFT protocol requires data to be 
       recorded permanently. This implementation simply notifies of state 
       change; the caller is responsible to store this information. 
       Note that the package {b raft-rocks} provides a persistent storage
       solution using RocksDB.}
    }*)

(** {2 Protocol Event Implementation} *)

val init :
  ?log:Raft_log.t ->
  ?commit_index:int -> 
  ?current_term:int -> 
  configuration:Raft_types.configuration ->
  now:Raft_types.time ->
  server_id:Raft_types.server_id ->
  unit ->
  Raft_types.state
(** [init ~configuration ~now ~server_id ()] creates an initial server
    state.

    The server is initially a [Follower] and its [election_deadline] will be
    set according to the [configuration] and the [now] value. *)

val handle_message :
  Raft_types.state ->
  Raft_types.message ->
  Raft_types.time ->
  Raft_types.result 
(** [handle_message state message now] handles a new RAFT message received.*)

val handle_new_election_timeout :
  Raft_types.state ->
  Raft_types.time ->
  Raft_types.result 
(** [handle_new_election_timeout state now] handles a new election timeout.*)

val handle_heartbeat_timeout :
  Raft_types.state ->
  Raft_types.time ->
  Raft_types.result 
(** [handle_heartbeat_timeout state now] handles an heartbeat event.*)

type new_log_response =
  | Appended of Raft_types.result 
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
(** [handle_add_log_entries state data now] processes new log entries [data]
    which is a list [(data, id)]. See new_log_response for more 
    information.*)

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
