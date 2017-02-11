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
  unsent_entries : log_entry list;
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
    let rec aux rev_log_entries = function
      | ({index;_ } as log_entry)::tl ->
          if index > acommit_index
          then aux rev_log_entries tl
          else
            if index = bcommit_index
            then rev_log_entries
            else aux (log_entry :: rev_log_entries) tl
      | [] ->
        assert(bcommit_index = 0);
        (* If commit_index is different than 0 then this means
         * that we could not identify all the [log_entry] which
         * have been commited between [before] and [after].
         *
         * One of the reason could be that the [log_entry]s are not
         * in the [log] but rather in the [global_cache].
         * This should be prevented by the fact that [Rev_log_cache.update_global_cache]
         * only move the [log_entry] to the cache wihch are prior to the
         * previous commit index (ie the one of [before].
         *
         * The other is a plain bug, all entries between 2 commit_index should be
         * in the log.
         *)
        rev_log_entries
    in
    (Committed_data (aux [] after.log.Log.recent_entries))::notifications
  else
    notifications

let current_leader {id; role; _} =
    match role with
    | Follower {current_leader; _ }-> current_leader
    | Candidate _ -> None
    | Leader _ -> Some id

