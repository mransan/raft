(** Protocol Types: State, Message and Events *) 

(** {2 Common types } *)

type time = float
(** Monotonic time (Absolute value) *)

type duration = float 
(** Duration, must be in the same unit as time*)

type server_id = int
(** Server id *)

(** Configuration *)
type configuration = {
  nb_of_server : int;
    (** Number of servers for the RAFT clusters, servers are then
        identified with 0 indexing *)
  election_timeout : duration;
    (** Average duration time for which a server will wait before starting
        a new election when the leader is not sending messages. *)
  election_timeout_range : duration;
    (** Duration range for the election timeout. The effective election
        timeout is randomly chosen between timeout +|- range/2.
        This value must be strickly less than [2 *. election_timeout] *)
  hearbeat_timeout : duration;
    (** Duration between heartbeat sent by the leader. [hearbeat_timeout]
        must be much less than [election_timeout] *)
  max_nb_logs_per_message : Raft_log.size;
    (** Limit the number of log entries per append entries message  *)
  max_log_size : Raft_log.max_log_size; 
    (** define boundaries for the "in-memory log" size limitation *)
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
  log_entries : Raft_log.log_entry list;
  leader_commit : int;
}

type append_entries_response_result =
  | Success of int (** receiver last log index *) 
  | Log_failure of int (** receiver last log index *)
  | Term_failure

type append_entries_response = {
  receiver_id : server_id;
  receiver_term : int;
  result : append_entries_response_result;
}

type message =
  | Request_vote_request of request_vote_request
  | Request_vote_response of request_vote_response
  | Append_entries_request of append_entries_request
  | Append_entries_response of append_entries_response

(** RAFT Message to send to the given server *)
type message_to_send = message * server_id

(** {2 Protocol State} *)

(** Follower information that a leader keeps track of*)
type follower_info = {
  follower_id : server_id;
    (** Id of the follower *)
  next_index : int;
    (** Which [Raft_types.log_entry] should be sent next. *)
  match_index : int;
    (** The last replicated [Raft_types.log_entry] for this follower *)
  heartbeat_deadline : time;
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
  election_deadline : time;
    (** The time at whic the election for which the server is a candidate
        is ending *)
}

(** Follower state *)
type follower_state = {
  voted_for : server_id option;
    (** If [None] then this follower has not voted yet, other it is the id
     *  of the candidate for which it previously voted *)
  current_leader : server_id option;
    (** Current leader *)
  election_deadline : time;
    (** The time at which the next election should take place and this
     *  follower become a candidate if not leadership was established by
     *  another server. *)
}

(** Role of a server in the RAFT protocol *)
type role =
  | Leader of leader_state
  | Candidate of candidate_state
  | Follower of follower_state

(** Raft server state *)
type state = {
  server_id : server_id;
    (** Unique Identifier must be between [0] and [nb_of_server - 1]. *)
  current_term : int;
    (** RAFT protocol divide time into terms during which a single 
        leader can be established. *)
  log : Raft_log.t;
    (** Set of log entries *)
  commit_index : int;
    (** The index of the last log entry to be committed. A committed log 
        is guaranteed by the RAFT protocol to never be removed. *)
  role : role;
    (** Role of the server. *)
  configuration : configuration;
    (** Various parameter to configure the RAFT cluster *) 
}

(** {2 API types} *)

(** The RAFT protocol defines 2 type of Timeout event which should be 
    triggered if no other protocol event has happened. *)
type timeout_type =
  | New_leader_election
    (** Timeout until when the server should start a new election (ie 
        increase term by 1 and become a candidate. *)
  | Heartbeat
    (** Timeout until when the server which is in a Leader state should 
        send a heartbeat message to a follower. Heartbeats are used to maintain
        leadership and indicate the Leader is still operating as normal *)

(** Timeout event which defines when (is absolute time) the next timeout 
    event should be triggered as well as its type *)
type timeout_event = {
  timeout : time;
  timeout_type : timeout_type;
}

(** Notification of a change of leadership in the RAFT protocol *)
type leader_change = 
  | New_leader of server_id
    (** A new leader has been elected *) 
  | No_leader
    (** The previous leader is no longer considered a leader. The previous 
        leader could have been any server including this server. *)

(** Data returned by each of the protocol event *)
type result = {
  state : state;  
    (** The new state *)
  messages_to_send : message_to_send list; 
    (** Raft messages to be send to the given servers.*)
  leader_change : leader_change option;
    (** Notification of a change in leaderhsip.*)
  committed_logs : Raft_log.log_entry list;
    (** Log entries which are now commited.*)
  added_logs : Raft_log.log_entry list; 
    (** Log entries added to the log. Note that this could overlap 
        with [committed_logs].*)
  deleted_logs : Raft_log.log_entry list;
    (** Log entries deleted from the log.*)
}

(** {2 Role functionality} *)

val is_follower : state -> bool
(** [is_follower state] returns [true] if [state] role is a follower, [false]
    otherwise.  *)

val is_candidate : state -> bool
(** [is_candidate state] returns [true] if [state] role is a candidate, [false]
    otherwise.  *)

val is_leader : state -> bool
(** [is_leader state] returns [true] if [state] role is a leader, [false]
    otherwise.  *)

val current_leader: state -> server_id option
(** [current_leader state] return the current leader for the current term.
    If no leader is known then [None] is returned.  *)
