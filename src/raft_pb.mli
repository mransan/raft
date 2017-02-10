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
  id : string;
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

type log_interval_compacted = {
  record_id : string;
}

type log_interval_expanded = {
  entries : log_entry list;
}

type log_interval_rev_log_entries =
  | Compacted of log_interval_compacted
  | Expanded of log_interval_expanded

and log_interval = {
  prev_index : int;
  prev_term : int;
  last_index : int;
  rev_log_entries : log_interval_rev_log_entries;
}

type follower_info = {
  server_id : int;
  next_index : int;
  match_index : int;
  heartbeat_deadline : float;
  outstanding_request : bool;
  unsent_entries : log_entry list;
}

type leader_state = {
  followers : follower_info list;
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

type role =
  | Leader of leader_state
  | Candidate of candidate_state
  | Follower of follower_state

type configuration = {
  nb_of_server : int;
  election_timeout : float;
  election_timeout_range : float;
  hearbeat_timeout : float;
  max_nb_logs_per_message : int;
  log_interval_size : int;
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

type compaction_report = {
  to_be_compacted : log_interval list;
  to_be_expanded : log_interval list;
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
  ?id:string ->
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

val default_follower_info : 
  ?server_id:int ->
  ?next_index:int ->
  ?match_index:int ->
  ?heartbeat_deadline:float ->
  ?outstanding_request:bool ->
  ?unsent_entries:log_entry list ->
  unit ->
  follower_info
(** [default_follower_info ()] is the default value for type [follower_info] *)

val default_leader_state : 
  ?followers:follower_info list ->
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

val default_role : unit -> role
(** [default_role ()] is the default value for type [role] *)


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

val decode_follower_info : Pbrt.Decoder.t -> follower_info
(** [decode_follower_info decoder] decodes a [follower_info] value from [decoder] *)

val decode_leader_state : Pbrt.Decoder.t -> leader_state
(** [decode_leader_state decoder] decodes a [leader_state] value from [decoder] *)

val decode_candidate_state : Pbrt.Decoder.t -> candidate_state
(** [decode_candidate_state decoder] decodes a [candidate_state] value from [decoder] *)

val decode_follower_state : Pbrt.Decoder.t -> follower_state
(** [decode_follower_state decoder] decodes a [follower_state] value from [decoder] *)

val decode_role : Pbrt.Decoder.t -> role
(** [decode_role decoder] decodes a [role] value from [decoder] *)


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

val encode_follower_info : follower_info -> Pbrt.Encoder.t -> unit
(** [encode_follower_info v encoder] encodes [v] with the given [encoder] *)

val encode_leader_state : leader_state -> Pbrt.Encoder.t -> unit
(** [encode_leader_state v encoder] encodes [v] with the given [encoder] *)

val encode_candidate_state : candidate_state -> Pbrt.Encoder.t -> unit
(** [encode_candidate_state v encoder] encodes [v] with the given [encoder] *)

val encode_follower_state : follower_state -> Pbrt.Encoder.t -> unit
(** [encode_follower_state v encoder] encodes [v] with the given [encoder] *)

val encode_role : role -> Pbrt.Encoder.t -> unit
(** [encode_role v encoder] encodes [v] with the given [encoder] *)


(** {2 Formatters} *)

val pp_request_vote_request : Format.formatter -> request_vote_request -> unit 
(** [pp_request_vote_request v] formats v *)

val pp_request_vote_response : Format.formatter -> request_vote_response -> unit 
(** [pp_request_vote_response v] formats v *)

val pp_log_entry : Format.formatter -> log_entry -> unit 
(** [pp_log_entry v] formats v *)

val pp_append_entries_request : Format.formatter -> append_entries_request -> unit 
(** [pp_append_entries_request v] formats v *)

val pp_append_entries_response_success_data : Format.formatter -> append_entries_response_success_data -> unit 
(** [pp_append_entries_response_success_data v] formats v *)

val pp_append_entries_response_log_failure_data : Format.formatter -> append_entries_response_log_failure_data -> unit 
(** [pp_append_entries_response_log_failure_data v] formats v *)

val pp_append_entries_response_result : Format.formatter -> append_entries_response_result -> unit 
(** [pp_append_entries_response_result v] formats v *)

val pp_append_entries_response : Format.formatter -> append_entries_response -> unit 
(** [pp_append_entries_response v] formats v *)

val pp_message : Format.formatter -> message -> unit 
(** [pp_message v] formats v *)

val pp_follower_info : Format.formatter -> follower_info -> unit 
(** [pp_follower_info v] formats v *)

val pp_leader_state : Format.formatter -> leader_state -> unit 
(** [pp_leader_state v] formats v *)

val pp_candidate_state : Format.formatter -> candidate_state -> unit 
(** [pp_candidate_state v] formats v *)

val pp_follower_state : Format.formatter -> follower_state -> unit 
(** [pp_follower_state v] formats v *)

val pp_role : Format.formatter -> role -> unit 
(** [pp_role v] formats v *)
