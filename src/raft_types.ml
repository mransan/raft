module Log = Raft_log

type time = float

type duration = float

type server_id = int

type configuration = {
  nb_of_server : int;
  election_timeout : duration;
  election_timeout_range : duration;
  hearbeat_timeout : duration;
  max_nb_logs_per_message : Raft_log.size;
  max_log_size : Log.max_log_size; 
}

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
  log_entries : Log.log_entry list;
  leader_commit : int;
}

type append_entries_response_result =
  | Success of int
  | Log_failure of int
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

type message_to_send = message * server_id

type timeout_type =
  | New_leader_election
  | Heartbeat

type timeout_event = {
  timeout : time;
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
  election_deadline : time;
}

type follower_state = {
  voted_for : server_id option;
  current_leader : server_id option;
  election_deadline : time;
}

type role =
  | Leader of leader_state
  | Candidate of candidate_state
  | Follower of follower_state

type state = {
  server_id : server_id;
  current_term : int;
  log : Raft_log.t;
  commit_index : int;
  role : role;
  configuration : configuration;
}

type result = {
  state : state;  
  messages_to_send : message_to_send list; 
  leader_change : leader_change option;
  committed_logs : Raft_log.log_entry list;
  added_logs : Raft_log.log_entry list; 
  deleted_logs : Raft_log.log_entry list;
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
