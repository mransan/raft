module Types = Raft_types
module Leader = Raft_role.Leader

module Configuration = struct

  let is_majority {Types.nb_of_server; _} nb =
    nb > (nb_of_server / 2)

end

module Timeout_event = struct

  let existing_election_wait election_deadline now =
    Types.({
      timeout = election_deadline -. now;
      timeout_type = New_leader_election;
    })

  let make_heartbeat_wait timeout =
    Types.({
      timeout = timeout;
      timeout_type = Heartbeat;
    })

  let next state now =
    match state.Types.role with
    | Types.Follower {Types.election_deadline; _} ->
      existing_election_wait election_deadline  now
    | Types.Leader leader_state ->
      make_heartbeat_wait (Leader.min_heartbeat_timout ~now leader_state)
    | Types.Candidate {Types.election_deadline; _ } ->
      existing_election_wait election_deadline  now

end
