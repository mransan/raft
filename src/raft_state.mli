(** RAFT Server state Utilities, agnostic of the role the server has *)

(** {2 Type} *)

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

(** State of a server *)
type t = {
  id : int;
  current_term : int;
  log : Raft_log.t;
  commit_index : int;
  role : role;
  configuration : Raft_pb.configuration;
}

(** {2 Role functionality} *)

val is_follower : t -> bool
(** [is_follower state] returns [true] if [state] role is a follower, [false]
    otherwise. 
  *)

val is_candidate : t -> bool
(** [is_candidate state] returns [true] if [state] role is a candidate, [false]
    otherwise. 
  *)

val is_leader : t -> bool
(** [is_leader state] returns [true] if [state] role is a leader, [false]
    otherwise. 
  *)
  
val current_leader: t -> int option
(** [current_leader state] return the current leader for the current term. 
    
    If no leader is known then [None] is returned. 
  *)

(** {2 Maintenance} *)

val notifications : t -> t -> Raft_pb.notification list 
(** [notifications before after] computes the notification between 2 states 
  *)

val compaction : t -> Raft_pb.compaction_report 
