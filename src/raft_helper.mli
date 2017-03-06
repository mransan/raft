(** Helper functions for manipulating Raft types *)

module Configuration : sig

  val is_majority : Raft_types.configuration -> int -> bool
  (** [is_majority configuration nb] returns true if [nb] is a majority *)

end (* Configuration *)

module Follower : sig

  val make :
    configuration:Raft_types.configuration ->
    now:float ->
    server_id:int ->
    unit ->
    Raft_types.state
  (** [create ~configuration ~now ~server_id ()] creates an initial
      follower state.*)

  val become :
    ?current_leader:int ->
    now:float ->
    term:int ->
    Raft_types.state ->
    Raft_types.state
  (** [become ~current_leader state term] return the new follower state.

      {ul
      {li [voted_for] is [None]}
      {li [current_leader] is taken from the function argument}
      {li [current_term] is taken from the function argument}
      } *)

end (* Follower *)

module Candidate : sig

  val become : 
    now:float ->  
    Raft_types.state -> 
    Raft_types.state
  (** [become state now] returns the new state with Candidate role. 
      [current_term] is incremented and [vote_count] initialized to 1. 
      (ie we assume the candidate votes for itself.

      The [election_timeout] is reset to a random number between the boundaries
      of the configuration. *)

  val increment_vote_count :
    Raft_types.candidate_state ->
    Raft_types.candidate_state
  (** [increment_vote_count state] increments the candidate vote count
      by 1.

      This function is called upon receiving a successful response to a vote 
      request to one of the servers. *)

end (* Candidate *)

module Leader : sig

  val become : 
    now:float -> 
    Raft_types.state -> 
    Raft_types.state
  (** [become state] returns the new state with a Leader role.

        While only candidate with a majority are allowed by the protocol to
        become a leader, this function does not perform any checks but simply
        initialize the role of Leader.

        The calling application is responsible to ensure that it is correct
        to become a leader.  *)


  val update_follower_last_log_index :
    follower_id:int ->
    index:int ->
    Raft_types.leader_state ->
    (Raft_types.leader_state * int)
  (** [update_receiver_last_log_index leader_state receiver_id last_log_index] 
      updates the leader state with the [last_log_index] information received 
      from a server. (Both [next_index] and [match_index] are updated.

      The function returns [(state, nb_of_replication)]. The 
      [nb_of_replication] is useful for the application to determine how many 
      servers have replicated the log and therefore determine if it can 
      be considered commited.  *)

  val record_response_received :
    follower_id:int ->
    Raft_types.leader_state ->
    Raft_types.leader_state
  (** [record_response_received ~server_id leader_state] keeps track of the
      fact that there are no more outstanding request for [server_id].
    *)

  val decrement_next_index :
    follower_last_log_index:int ->
    follower_id:int ->
    Raft_types.state ->
    Raft_types.leader_state ->
    Raft_types.leader_state

  val min_heartbeat_timout : 
    now:float -> 
    Raft_types.leader_state -> 
    float
  (** [min_heartbeat_timout ~now ~leader_state] returns when the next timeout
      event should occured based on the last request sent to the 
      followers *)

end (* Leader  *)

module Timeout_event : sig

  val next : now:float -> Raft_types.state -> Raft_types.timeout_event
  (** [next ~now state] returns the next timeout event which should happened
      unless another RAFT event happened first. *)

end (* Timeout_event *)

module Diff : sig 
  val leader_change : 
    Raft_types.state -> 
    Raft_types.state -> 
    Raft_types.leader_change option
  (** [notifications before after] computes the notification between 2 states
   *)

  val committed_logs :
    Raft_types.state -> 
    Raft_types.state -> 
    Raft_log.log_entry list
  (** [committed_logs before after] returns the newly committed log entries 
      between [before] and [after] state *)

end (* Diff *)
