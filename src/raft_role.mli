(** Role specific logic *)

module Follower : sig 

  val create : 
    configuration:Raft_state.configuration -> 
    now:float -> 
    id:int -> 
    unit -> 
    Raft_state.t 
  (** [create ~current_leader ~current_term ~voted_for ~log ~configuration ~id ()] creates an initial 
      follower state. 
    *)

  val become : ?current_leader:int -> term:int -> now:float -> Raft_state.t -> Raft_state.t
  (** [become ~current_leader state term] return the new follower state. 
      
      {ul
      {li [voted_for] is [None]}
      {li [current_leader] is taken from the function argument}
      {li [current_term] is taken from the function argument}
      }
    *)
end (* Follower *)

module Candidate : sig

  val become : now:float ->  Raft_state.t -> Raft_state.t
  (** [become state now] returns the new state with Candidate role. [current_term] is 
      incremented and [vote_count] initialized to 1. (ie we assume the candidate
      votes for itself.

      The [election_timeout] is reset to a random number between the boundaries
      of the configuration. 
    *)

  val increment_vote_count : 
    Raft_state.candidate_state -> 
    Raft_state.candidate_state
  (** [increment_vote_count state] increments the candidate vote count 
      by 1.

      This function is called upon receiving a successful response to a vote request
      to one of the servers.
    *)

end (* Candidate *) 

module Leader : sig

  val become : Raft_state.t -> float -> Raft_state.t 
  (** [become state] returns the new state with a Leader role. 
       
        While only candidate with a majority are allowed by the protocol to 
        become a leader, this function does not perform any checks but simply 
        initialize the role of Leader. 
        
        The calling application is responsible to ensure that it is correct
        to become a leader. 
    *)
  
  
  val update_receiver_last_log_index : 
    receiver_id:int -> 
    log_index:int -> 
    Raft_state.leader_state -> 
    (Raft_state.leader_state * int) 
  (** [update_receiver_last_log_index leader_state receiver_id last_log_index] updates the leader
       state with the [last_log_index] information received from a server. (Both [next_index]
       and [match_index] are updated. 
     
       The function returns [(state, nb_of_replication)]. The [nb_of_replication] is 
       useful for the application to determine how many servers have replicated the log and therefore
       determine if it can be considered commited. 
    *)

  val record_response_received : 
    receiver_id:int -> 
    Raft_state.leader_state -> 
    Raft_state.leader_state 
  (** [record_response_received ~server_id leader_state] keeps track of the 
      fact that there are no more outstanding request for [server_id].
    *) 

  val decrement_next_index : 
    log_failure:Raft_pb.append_entries_response_log_failure_data -> 
    receiver_id:int -> 
    Raft_state.t -> 
    Raft_state.leader_state -> 
    Raft_state.leader_state
  
  val min_heartbeat_timout : now:float -> Raft_state.leader_state -> float

end (* Leader  *) 
