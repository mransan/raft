open Raft_pb

module State  = Raft_state
module Leader = Raft_role.Leader

module Configuration = struct

  let is_majority {nb_of_server; _} nb = 
    nb > (nb_of_server / 2) 

end 

module Timeout_event = struct

  let existing_election_wait election_deadline now = 
    {
      timeout      = election_deadline -. now; 
      timeout_type = New_leader_election; 
    }
  
  let make_heartbeat_wait timeout = 
    {
      timeout = timeout; 
      timeout_type = Heartbeat;
    }

  let next state now = 
    match state.State.role with
    | State.Follower {State.election_deadline; _} ->
      existing_election_wait election_deadline  now  
    | State.Leader leader_state -> 
      make_heartbeat_wait (Leader.min_heartbeat_timout ~now leader_state)
    | State.Candidate {State.election_deadline; _ } -> 
      existing_election_wait election_deadline  now  

end 
