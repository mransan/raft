(** RAFT Server state Utilities, agnostic of the role the server has *)

(** {2 Common types } *)

type time = float
(** Monotonic time *)

type server_id = int
(** Server id *)

(** Configuration *)
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

(** {2 Protocol Messages} *) 

type request_vote_request = {
  candidate_term : int;
  candidate_id : server_id;
  candidate_last_log_index : int;
  candidate_last_log_term : int;
}

type request_vote_response = {
  voter_id : server_id;
  voter_term : int;
  vote_granted : bool;
}

type append_entries_request = {
  leader_term : int;
  leader_id : server_id;
  prev_log_index : int;
  prev_log_term : int;
  rev_log_entries : Raft_log.log_entry list;
  leader_commit : int;
}

type append_entries_response_result =
  | Success of int (* receiver_last_log_index *) 
  | Log_failure of int (* receiver_last_log_index *)
  | Term_failure

and append_entries_response = {
  receiver_id : int;
  receiver_term : int;
  result : append_entries_response_result;
}

type message =
  | Request_vote_request of request_vote_request
  | Request_vote_response of request_vote_response
  | Append_entries_request of append_entries_request
  | Append_entries_response of append_entries_response

(** {2 Protocol State} *)

(** Each follower information *)
type follower_info = {
  follower_id : int;
    (** Id of the follower *)
  next_index : int;
    (** Which [Raft_types.log_entry] should be sent next. *)
  match_index : int;
    (** The last replicated [Raft_types.log_entry] for this follower *)
  heartbeat_deadline : float;
    (** The time at which a heartbeat should be sent next *)
  outstanding_request : bool;
    (** Whether or not the follower has an outstanding request (ie if
        no response was received since the last time an append entries
        request was sent. *)
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
  server_id : int;
  current_term : int;
  log : Raft_log.t;
  commit_index : int;
  role : role;
  configuration : configuration;
}

(** {2 API types} *)

type timeout_type =
  | New_leader_election
  | Heartbeat

type timeout_event = {
  timeout : float;
  timeout_type : timeout_type;
}

type leader_change = 
  | New_leader of int
  | No_leader

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

