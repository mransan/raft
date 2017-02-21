module Types = Raft_types
module Leader = Raft_role.Leader
module Log = Raft_log

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

let leader_change before after =
  let open Types in 

  let { role = brole; _ } = before in
  let { role = arole; _ } = after in

  match brole, arole with
  | Follower _   , Leader _
  | Leader _     , Candidate _ ->
    (* Impossible transition which would violate the rules of the
     * RAFT protocol
     *)
    assert(false)

  | Candidate _  , Leader _ ->
    (* Case of the server becoming a leader *)
    Some (New_leader after.server_id)

  | Follower {current_leader = Some bleader; _ },
    Follower {current_leader = Some aleader; _ } when bleader = aleader ->
    None
    (* No leader change, following the same leader *)

  | _, Follower{current_leader = Some aleader;_} ->
    Some (New_leader aleader)
    (* There is a new leader *)

  | Follower {current_leader = Some _; _}, Candidate _
  | Follower {current_leader = Some _; _}, Follower  {current_leader = None; _}
  | Leader  _                            , Follower  {current_leader = None; _} ->
    Some No_leader

  | Leader _                             , Leader _
  | Candidate _                          , Candidate _
  | Candidate _                          , Follower {current_leader = None;_}
  | Follower {current_leader = None; _}  , Follower {current_leader = None;_}
  | Follower {current_leader = None; _}  , Candidate _ ->
    None

let committed_logs before after = 
  let open Types in 

  let { commit_index = bcommit_index;  _ } = before in
  let { commit_index = acommit_index;  _ } = after in

  if acommit_index > bcommit_index
  then
    let recent_entries = after.log.Log.recent_entries in 
    let _, prev_commit, sub = Log.IntMap.split bcommit_index recent_entries in 
    begin match prev_commit with 
    | None -> assert(bcommit_index = 0)
    | Some _ -> ()
    end;
    let sub, last_commit, _ = Log.IntMap.split acommit_index sub in 
    let sub = match last_commit with 
      | None -> assert(false) 
      | Some ({Log.index; _ } as log_entry) -> 
        Log.IntMap.add index log_entry sub 
    in 
    let committed_entries = List.map snd (Log.IntMap.bindings sub) in  
    committed_entries
  else
    []

let added_logs before after = 
  let open Types in 

  let {log = logb; _} = before in 
  let {log = loga; _} = after in 

  let lastb = Raft_log.last_log_index logb in 
  let lasta = Raft_log.last_log_index loga in 

  if lasta <= lastb
  then []
  else 
    let {Raft_log.recent_entries; _} = loga in 
    let _, _, added_entries = Log.IntMap.split lastb recent_entries in 
    List.map snd (Log.IntMap.bindings added_entries)
