(** raft.proto Generated Types and Encoding *)

(** {2 Types} *)

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

type log_entry = {
  index : int;
  term : int;
  data : bytes;
}

type append_entries_request = {
  leader_term : int;
  leader_id : int;
  prev_log_index : int;
  prev_log_term : int;
  rev_log_entries : log_entry list;
  leader_commit : int;
}

type append_entries_response_success_data = {
  receiver_last_log_index : int;
}

type append_entries_response_log_failure_data = {
  receiver_last_log_index : int;
  receiver_last_log_term : int;
}

type append_entries_response_result =
  | Success of append_entries_response_success_data
  | Log_failure of append_entries_response_log_failure_data
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

type log_interval = {
  prev_index : int;
  prev_term : int;
  rev_log_entries : log_entry list;
  last_index : int;
}

type server_index = {
  server_id : int;
  next_index : int;
  match_index : int;
  heartbeat_deadline : float;
  outstanding_request : bool;
  local_cache : log_interval;
}

type leader_state = {
  indices : server_index list;
}

type candidate_state = {
  vote_count : int;
  election_deadline : float;
}

type follower_state = {
  voted_for : int option;
  current_leader : int option;
  election_deadline : float;
}

type configuration = {
  nb_of_server : int;
  election_timeout : float;
  election_timeout_range : float;
  hearbeat_timeout : float;
  max_nb_logs_per_message : int;
}

type log_interval_rope =
  | Interval of log_interval
  | Append of log_interval_rope_append

and log_interval_rope_append = {
  height : int;
  lhs : log_interval_rope;
  rhs : log_interval_rope;
  last_index : int;
}

type state_role =
  | Leader of leader_state
  | Candidate of candidate_state
  | Follower of follower_state

and state = {
  id : int;
  current_term : int;
  log : log_entry list;
  log_size : int;
  commit_index : int;
  role : state_role;
  configuration : configuration;
  global_cache : log_interval_rope option;
}

type timeout_event_time_out_type =
  | New_leader_election 
  | Heartbeat 

type timeout_event = {
  timeout : float;
  timeout_type : timeout_event_time_out_type;
}


(** {2 Default values} *)

val default_request_vote_request : 
  ?candidate_term:int ->
  ?candidate_id:int ->
  ?candidate_last_log_index:int ->
  ?candidate_last_log_term:int ->
  unit ->
  request_vote_request
(** [default_request_vote_request ()] is the default value for type [request_vote_request] *)

val default_request_vote_response : 
  ?voter_id:int ->
  ?voter_term:int ->
  ?vote_granted:bool ->
  unit ->
  request_vote_response
(** [default_request_vote_response ()] is the default value for type [request_vote_response] *)

val default_log_entry : 
  ?index:int ->
  ?term:int ->
  ?data:bytes ->
  unit ->
  log_entry
(** [default_log_entry ()] is the default value for type [log_entry] *)

val default_append_entries_request : 
  ?leader_term:int ->
  ?leader_id:int ->
  ?prev_log_index:int ->
  ?prev_log_term:int ->
  ?rev_log_entries:log_entry list ->
  ?leader_commit:int ->
  unit ->
  append_entries_request
(** [default_append_entries_request ()] is the default value for type [append_entries_request] *)

val default_append_entries_response_success_data : 
  ?receiver_last_log_index:int ->
  unit ->
  append_entries_response_success_data
(** [default_append_entries_response_success_data ()] is the default value for type [append_entries_response_success_data] *)

val default_append_entries_response_log_failure_data : 
  ?receiver_last_log_index:int ->
  ?receiver_last_log_term:int ->
  unit ->
  append_entries_response_log_failure_data
(** [default_append_entries_response_log_failure_data ()] is the default value for type [append_entries_response_log_failure_data] *)

val default_append_entries_response_result : unit -> append_entries_response_result
(** [default_append_entries_response_result ()] is the default value for type [append_entries_response_result] *)

val default_append_entries_response : 
  ?receiver_id:int ->
  ?receiver_term:int ->
  ?result:append_entries_response_result ->
  unit ->
  append_entries_response
(** [default_append_entries_response ()] is the default value for type [append_entries_response] *)

val default_message : unit -> message
(** [default_message ()] is the default value for type [message] *)

val default_log_interval : 
  ?prev_index:int ->
  ?prev_term:int ->
  ?rev_log_entries:log_entry list ->
  ?last_index:int ->
  unit ->
  log_interval
(** [default_log_interval ()] is the default value for type [log_interval] *)

val default_server_index : 
  ?server_id:int ->
  ?next_index:int ->
  ?match_index:int ->
  ?heartbeat_deadline:float ->
  ?outstanding_request:bool ->
  ?local_cache:log_interval ->
  unit ->
  server_index
(** [default_server_index ()] is the default value for type [server_index] *)

val default_leader_state : 
  ?indices:server_index list ->
  unit ->
  leader_state
(** [default_leader_state ()] is the default value for type [leader_state] *)

val default_candidate_state : 
  ?vote_count:int ->
  ?election_deadline:float ->
  unit ->
  candidate_state
(** [default_candidate_state ()] is the default value for type [candidate_state] *)

val default_follower_state : 
  ?voted_for:int option ->
  ?current_leader:int option ->
  ?election_deadline:float ->
  unit ->
  follower_state
(** [default_follower_state ()] is the default value for type [follower_state] *)

val default_configuration : 
  ?nb_of_server:int ->
  ?election_timeout:float ->
  ?election_timeout_range:float ->
  ?hearbeat_timeout:float ->
  ?max_nb_logs_per_message:int ->
  unit ->
  configuration
(** [default_configuration ()] is the default value for type [configuration] *)

val default_log_interval_rope : unit -> log_interval_rope
(** [default_log_interval_rope ()] is the default value for type [log_interval_rope] *)

val default_log_interval_rope_append : 
  ?height:int ->
  ?lhs:log_interval_rope ->
  ?rhs:log_interval_rope ->
  ?last_index:int ->
  unit ->
  log_interval_rope_append
(** [default_log_interval_rope_append ()] is the default value for type [log_interval_rope_append] *)

val default_state_role : unit -> state_role
(** [default_state_role ()] is the default value for type [state_role] *)

val default_state : 
  ?id:int ->
  ?current_term:int ->
  ?log:log_entry list ->
  ?log_size:int ->
  ?commit_index:int ->
  ?role:state_role ->
  ?configuration:configuration ->
  ?global_cache:log_interval_rope option ->
  unit ->
  state
(** [default_state ()] is the default value for type [state] *)

val default_timeout_event_time_out_type : unit -> timeout_event_time_out_type
(** [default_timeout_event_time_out_type ()] is the default value for type [timeout_event_time_out_type] *)

val default_timeout_event : 
  ?timeout:float ->
  ?timeout_type:timeout_event_time_out_type ->
  unit ->
  timeout_event
(** [default_timeout_event ()] is the default value for type [timeout_event] *)


(** {2 Protobuf Decoding} *)

val decode_request_vote_request : Pbrt.Decoder.t -> request_vote_request
(** [decode_request_vote_request decoder] decodes a [request_vote_request] value from [decoder] *)

val decode_request_vote_response : Pbrt.Decoder.t -> request_vote_response
(** [decode_request_vote_response decoder] decodes a [request_vote_response] value from [decoder] *)

val decode_log_entry : Pbrt.Decoder.t -> log_entry
(** [decode_log_entry decoder] decodes a [log_entry] value from [decoder] *)

val decode_append_entries_request : Pbrt.Decoder.t -> append_entries_request
(** [decode_append_entries_request decoder] decodes a [append_entries_request] value from [decoder] *)

val decode_append_entries_response_success_data : Pbrt.Decoder.t -> append_entries_response_success_data
(** [decode_append_entries_response_success_data decoder] decodes a [append_entries_response_success_data] value from [decoder] *)

val decode_append_entries_response_log_failure_data : Pbrt.Decoder.t -> append_entries_response_log_failure_data
(** [decode_append_entries_response_log_failure_data decoder] decodes a [append_entries_response_log_failure_data] value from [decoder] *)

val decode_append_entries_response_result : Pbrt.Decoder.t -> append_entries_response_result
(** [decode_append_entries_response_result decoder] decodes a [append_entries_response_result] value from [decoder] *)

val decode_append_entries_response : Pbrt.Decoder.t -> append_entries_response
(** [decode_append_entries_response decoder] decodes a [append_entries_response] value from [decoder] *)

val decode_message : Pbrt.Decoder.t -> message
(** [decode_message decoder] decodes a [message] value from [decoder] *)

val decode_log_interval : Pbrt.Decoder.t -> log_interval
(** [decode_log_interval decoder] decodes a [log_interval] value from [decoder] *)

val decode_server_index : Pbrt.Decoder.t -> server_index
(** [decode_server_index decoder] decodes a [server_index] value from [decoder] *)

val decode_leader_state : Pbrt.Decoder.t -> leader_state
(** [decode_leader_state decoder] decodes a [leader_state] value from [decoder] *)

val decode_candidate_state : Pbrt.Decoder.t -> candidate_state
(** [decode_candidate_state decoder] decodes a [candidate_state] value from [decoder] *)

val decode_follower_state : Pbrt.Decoder.t -> follower_state
(** [decode_follower_state decoder] decodes a [follower_state] value from [decoder] *)

val decode_configuration : Pbrt.Decoder.t -> configuration
(** [decode_configuration decoder] decodes a [configuration] value from [decoder] *)

val decode_log_interval_rope : Pbrt.Decoder.t -> log_interval_rope
(** [decode_log_interval_rope decoder] decodes a [log_interval_rope] value from [decoder] *)

val decode_log_interval_rope_append : Pbrt.Decoder.t -> log_interval_rope_append
(** [decode_log_interval_rope_append decoder] decodes a [log_interval_rope_append] value from [decoder] *)

val decode_state_role : Pbrt.Decoder.t -> state_role
(** [decode_state_role decoder] decodes a [state_role] value from [decoder] *)

val decode_state : Pbrt.Decoder.t -> state
(** [decode_state decoder] decodes a [state] value from [decoder] *)

val decode_timeout_event_time_out_type : Pbrt.Decoder.t -> timeout_event_time_out_type
(** [decode_timeout_event_time_out_type decoder] decodes a [timeout_event_time_out_type] value from [decoder] *)

val decode_timeout_event : Pbrt.Decoder.t -> timeout_event
(** [decode_timeout_event decoder] decodes a [timeout_event] value from [decoder] *)


(** {2 Protobuf Toding} *)

val encode_request_vote_request : request_vote_request -> Pbrt.Encoder.t -> unit
(** [encode_request_vote_request v encoder] encodes [v] with the given [encoder] *)

val encode_request_vote_response : request_vote_response -> Pbrt.Encoder.t -> unit
(** [encode_request_vote_response v encoder] encodes [v] with the given [encoder] *)

val encode_log_entry : log_entry -> Pbrt.Encoder.t -> unit
(** [encode_log_entry v encoder] encodes [v] with the given [encoder] *)

val encode_append_entries_request : append_entries_request -> Pbrt.Encoder.t -> unit
(** [encode_append_entries_request v encoder] encodes [v] with the given [encoder] *)

val encode_append_entries_response_success_data : append_entries_response_success_data -> Pbrt.Encoder.t -> unit
(** [encode_append_entries_response_success_data v encoder] encodes [v] with the given [encoder] *)

val encode_append_entries_response_log_failure_data : append_entries_response_log_failure_data -> Pbrt.Encoder.t -> unit
(** [encode_append_entries_response_log_failure_data v encoder] encodes [v] with the given [encoder] *)

val encode_append_entries_response_result : append_entries_response_result -> Pbrt.Encoder.t -> unit
(** [encode_append_entries_response_result v encoder] encodes [v] with the given [encoder] *)

val encode_append_entries_response : append_entries_response -> Pbrt.Encoder.t -> unit
(** [encode_append_entries_response v encoder] encodes [v] with the given [encoder] *)

val encode_message : message -> Pbrt.Encoder.t -> unit
(** [encode_message v encoder] encodes [v] with the given [encoder] *)

val encode_log_interval : log_interval -> Pbrt.Encoder.t -> unit
(** [encode_log_interval v encoder] encodes [v] with the given [encoder] *)

val encode_server_index : server_index -> Pbrt.Encoder.t -> unit
(** [encode_server_index v encoder] encodes [v] with the given [encoder] *)

val encode_leader_state : leader_state -> Pbrt.Encoder.t -> unit
(** [encode_leader_state v encoder] encodes [v] with the given [encoder] *)

val encode_candidate_state : candidate_state -> Pbrt.Encoder.t -> unit
(** [encode_candidate_state v encoder] encodes [v] with the given [encoder] *)

val encode_follower_state : follower_state -> Pbrt.Encoder.t -> unit
(** [encode_follower_state v encoder] encodes [v] with the given [encoder] *)

val encode_configuration : configuration -> Pbrt.Encoder.t -> unit
(** [encode_configuration v encoder] encodes [v] with the given [encoder] *)

val encode_log_interval_rope : log_interval_rope -> Pbrt.Encoder.t -> unit
(** [encode_log_interval_rope v encoder] encodes [v] with the given [encoder] *)

val encode_log_interval_rope_append : log_interval_rope_append -> Pbrt.Encoder.t -> unit
(** [encode_log_interval_rope_append v encoder] encodes [v] with the given [encoder] *)

val encode_state_role : state_role -> Pbrt.Encoder.t -> unit
(** [encode_state_role v encoder] encodes [v] with the given [encoder] *)

val encode_state : state -> Pbrt.Encoder.t -> unit
(** [encode_state v encoder] encodes [v] with the given [encoder] *)

val encode_timeout_event_time_out_type : timeout_event_time_out_type -> Pbrt.Encoder.t -> unit
(** [encode_timeout_event_time_out_type v encoder] encodes [v] with the given [encoder] *)

val encode_timeout_event : timeout_event -> Pbrt.Encoder.t -> unit
(** [encode_timeout_event v encoder] encodes [v] with the given [encoder] *)


(** {2 Formatters} *)

val pp_request_vote_request : Format.formatter -> request_vote_request -> unit 
(** [pp_request_vote_request v] formats v] *)

val pp_request_vote_response : Format.formatter -> request_vote_response -> unit 
(** [pp_request_vote_response v] formats v] *)

val pp_log_entry : Format.formatter -> log_entry -> unit 
(** [pp_log_entry v] formats v] *)

val pp_append_entries_request : Format.formatter -> append_entries_request -> unit 
(** [pp_append_entries_request v] formats v] *)

val pp_append_entries_response_success_data : Format.formatter -> append_entries_response_success_data -> unit 
(** [pp_append_entries_response_success_data v] formats v] *)

val pp_append_entries_response_log_failure_data : Format.formatter -> append_entries_response_log_failure_data -> unit 
(** [pp_append_entries_response_log_failure_data v] formats v] *)

val pp_append_entries_response_result : Format.formatter -> append_entries_response_result -> unit 
(** [pp_append_entries_response_result v] formats v] *)

val pp_append_entries_response : Format.formatter -> append_entries_response -> unit 
(** [pp_append_entries_response v] formats v] *)

val pp_message : Format.formatter -> message -> unit 
(** [pp_message v] formats v] *)

val pp_log_interval : Format.formatter -> log_interval -> unit 
(** [pp_log_interval v] formats v] *)

val pp_server_index : Format.formatter -> server_index -> unit 
(** [pp_server_index v] formats v] *)

val pp_leader_state : Format.formatter -> leader_state -> unit 
(** [pp_leader_state v] formats v] *)

val pp_candidate_state : Format.formatter -> candidate_state -> unit 
(** [pp_candidate_state v] formats v] *)

val pp_follower_state : Format.formatter -> follower_state -> unit 
(** [pp_follower_state v] formats v] *)

val pp_configuration : Format.formatter -> configuration -> unit 
(** [pp_configuration v] formats v] *)

val pp_log_interval_rope : Format.formatter -> log_interval_rope -> unit 
(** [pp_log_interval_rope v] formats v] *)

val pp_log_interval_rope_append : Format.formatter -> log_interval_rope_append -> unit 
(** [pp_log_interval_rope_append v] formats v] *)

val pp_state_role : Format.formatter -> state_role -> unit 
(** [pp_state_role v] formats v] *)

val pp_state : Format.formatter -> state -> unit 
(** [pp_state v] formats v] *)

val pp_timeout_event_time_out_type : Format.formatter -> timeout_event_time_out_type -> unit 
(** [pp_timeout_event_time_out_type v] formats v] *)

val pp_timeout_event : Format.formatter -> timeout_event -> unit 
(** [pp_timeout_event v] formats v] *)
