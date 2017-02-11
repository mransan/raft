(** RAFT Server state Utilities, agnostic of the role the server has *)

(** {2 Type} *)

type configuration = {
  nb_of_server : int;
    (** Number of servers for the RAFT clusters, servers are then
        identified with 0 indexing *)
  election_timeout : float;
    (** Average duration time for which a server will wait before starting
        a new election when the leader is not sending messages. *)
  election_timeout_range : float;
    (** Duration range for the election timeout. The effective election
        timeout is randomly chosen between timeout +|- range/2.
        This value must be strickly less than [2 *. election_timeout] *)
  hearbeat_timeout : float;
    (** Duration between heartbeat sent by the leader. [hearbeat_timeout]
        must be much less than [election_timeout] *)
  max_nb_logs_per_message : int;
    (** Limit the number of log entries per append entries message  *)
}

type timeout_type =
  | New_leader_election
  | Heartbeat

type timeout_event = {
  timeout : float;
  timeout_type : timeout_type;
}

type notification =
  | Committed_data of Raft_pb.log_entry list
  | New_leader of int
  | No_leader

(** Each follower information *)
type follower_info = {
  server_id : int;
    (** Id of the follower *)
  next_index : int;
    (** Which [Raft_pb.log_entry] should be sent next. *)
  match_index : int;
    (** The last replicated [Raft_pb.log_entry] for this follower *)
  heartbeat_deadline : float;
    (** The time at which a heartbeat should be sent next *)
  outstanding_request : bool;
    (** Whether or not the follower has an outstanding request (ie if
        no response was received since the last time an append entries
        request was sent. *)
  unsent_entries : Raft_pb.log_entry list;
    (** The log entries which were previously not sent due to throttling. This
        acts as a cache to avoid recomputing those.*)
}

(** Leader state *)
type leader_state = follower_info list

(** Candidate state *)
type candidate_state = {
  vote_count : int;
    (** The number of positive vote received so far *)
  election_deadline : float;
    (** The time at whic the election for which the server is a candidate
        is ending *)
}

(** Follower state *)
type follower_state = {
  voted_for : int option;
    (** If [None] then this follower has not voted yet, other it is the id
     *  of the candidate for which it previously voted *)
  current_leader : int option;
    (** Current leader *)
  election_deadline : float;
    (** The time at which the next election should take place and this
     *  follower become a candidate if not leadership was established by
     *  another server. *)
}

(** Role of a server in the RAFT protocol *)
type role =
  | Leader of leader_state
  | Candidate of candidate_state
  | Follower of follower_state

(** Types of a server *)
type state = {
  id : int;
  current_term : int;
  log : Raft_log.t;
  commit_index : int;
  role : role;
  configuration : configuration;
}

(** {2 Role functionality} *)

val is_follower : state -> bool
(** [is_follower state] returns [true] if [state] role is a follower, [false]
    otherwise.
  *)

val is_candidate : state -> bool
(** [is_candidate state] returns [true] if [state] role is a candidate, [false]
    otherwise.
  *)

val is_leader : state -> bool
(** [is_leader state] returns [true] if [state] role is a leader, [false]
    otherwise.
  *)

val current_leader: state -> int option
(** [current_leader state] return the current leader for the current term.

    If no leader is known then [None] is returned.
  *)

(** {2 Maintenance} *)

val notifications : state -> state -> notification list
(** [notifications before after] computes the notification between 2 states
  *)
