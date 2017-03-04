module Log = Raft_log

type time = float

type server_id = int

type configuration = {
  nb_of_server : int;
  election_timeout : float;
  election_timeout_range : float;
  hearbeat_timeout : float;
  max_nb_logs_per_message : Raft_log.size;
  max_log_size : Log.max_log_size; 
}

type request_vote_request = {
  candidate_term : int;
  candidate_id : int;
  candidate_last_log_index : int;
  candidate_last_log_term : int;
}

type request_vote_response = {
  voter_id : int;
  voter_term : int;
  vote_granted : bool;
}

type append_entries_request = {
  leader_term : int;
  leader_id : int;
  prev_log_index : int;
  prev_log_term : int;
  rev_log_entries : Log.log_entry list;
  leader_commit : int;
}

type append_entries_response_result =
  | Success of int
  | Log_failure of int
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

type follower_info = {
  follower_id : int;
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
  server_id : int;
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

let current_leader {server_id; role; _} =
  match role with
  | Follower {current_leader; _ }-> current_leader
  | Candidate _ -> None
  | Leader _ -> Some server_id
