open Raft_pb

module Log = Raft_log

type time = float

type server_id = int

type configuration = {
  nb_of_server : int;
  election_timeout : float;
  election_timeout_range : float;
  hearbeat_timeout : float;
  max_nb_logs_per_message : int;
}

type timeout_type =
  | New_leader_election
  | Heartbeat

type timeout_event = {
  timeout : float;
  timeout_type : timeout_type;
}

type notification =
  | Committed_data of log_entry list
  | New_leader of int
  | No_leader

type follower_info = {
  server_id : int;
  next_index : int;
  match_index : int;
  heartbeat_deadline : float;
  outstanding_request : bool;
}

type leader_state = follower_info list

type candidate_state = {
  vote_count : int;
  election_deadline : float;
}

type follower_state = {
  voted_for : int option;
  current_leader : int option;
  election_deadline : float;
}

type role =
  | Leader of leader_state
  | Candidate of candidate_state
  | Follower of follower_state

type state = {
  id : int;
  current_term : int;
  log : Raft_log.t;
  commit_index : int;
  role : role;
  configuration : configuration;
}

let is_follower {role; _} =
  match role with
  | Follower _ -> true
  | _ -> false

let is_candidate {role; _ } =
  match role with
  | Candidate _ -> true
  | _ -> false

let is_leader {role; _ } =
  match role with
  | Leader _ -> true
  | _ -> false

let notifications before after =

  let { commit_index = bcommit_index; role = brole; _ } = before in
  let { commit_index = acommit_index; role = arole; _ } = after in

  let notifications = [] in

  let notifications =
    match brole, arole with
    | Follower _   , Leader _
    | Leader _     , Candidate _ ->
      (* Impossible transition which would violate the rules of the
       * RAFT protocol
       *)
      assert(false)

    | Candidate _  , Leader _ ->
      (* Case of the server becoming a leader *)
      (New_leader after.id)::notifications

    | Follower {current_leader = Some bleader; _ },
      Follower {current_leader = Some aleader; _ } when bleader = aleader ->
      notifications
      (* No leader change, following the same leader *)

    | _, Follower{current_leader = Some aleader;_} ->
      (New_leader aleader)::notifications
      (* There is a new leader *)

    | Follower {current_leader = Some _; _}, Candidate _
    | Follower {current_leader = Some _; _}, Follower  {current_leader = None; _}
    | Leader  _                            , Follower  {current_leader = None; _} ->
      (No_leader::notifications)

    | Leader _                             , Leader _
    | Candidate _                          , Candidate _
    | Candidate _                          , Follower {current_leader = None;_}
    | Follower {current_leader = None; _}  , Follower {current_leader = None;_}
    | Follower {current_leader = None; _}  , Candidate _ ->
      notifications
  in

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
      | Some ({index; _ } as log_entry) -> 
        Log.IntMap.add index log_entry sub 
    in 
    let committed_entries = List.map snd (Log.IntMap.bindings sub) in  
    (Committed_data committed_entries)::notifications
  else
    notifications

let current_leader {id; role; _} =
    match role with
    | Follower {current_leader; _ }-> current_leader
    | Candidate _ -> None
    | Leader _ -> Some id
