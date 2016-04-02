(** raft.proto Generated Types and Encoding *)

(** {2 Types} *)

type request_vote_request = {
  candidate_term : int;
  candidate_id : int;
  last_log_index : int;
  last_log_term : int;
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
  log_entries : log_entry list;
  leader_commit : int;
}

type append_entries_response_success_data = {
  receiver_last_log_index : int;
}

type append_entries_response_result =
  | Failure
  | Success of append_entries_response_success_data

and append_entries_response = {
  receiver_id : int;
  receiver_term : int;
  result : append_entries_response_result;
}

type server_index = {
  server_id : int;
  server_log_index : int;
}

type leader_state = {
  next_index : server_index list;
  match_index : server_index list;
}

type candidate_state = {
  vote_count : int;
  election_deadline : float;
}

type follower_state = {
  voted_for : int option;
}

type configuration = {
  nb_of_server : int;
  election_timeout : float;
}

type state_role =
  | Leader of leader_state
  | Candidate of candidate_state
  | Follower of follower_state

and state = {
  id : int;
  current_term : int;
  log : log_entry list;
  commit_index : int;
  last_applied : int;
  role : state_role;
  configuration : configuration;
}

type follow_up_action_retry_append_data = {
  server_id : int;
}

type follow_up_action =
  | Act_as_new_leader
  | Start_new_election
  | Nothing_to_do
  | Retry_append of follow_up_action_retry_append_data


(** {2 Default values} *)

val default_request_vote_request : unit -> request_vote_request
(** [default_request_vote_request ()] is the default value for type [request_vote_request] *)

val default_request_vote_response : unit -> request_vote_response
(** [default_request_vote_response ()] is the default value for type [request_vote_response] *)

val default_log_entry : unit -> log_entry
(** [default_log_entry ()] is the default value for type [log_entry] *)

val default_append_entries_request : unit -> append_entries_request
(** [default_append_entries_request ()] is the default value for type [append_entries_request] *)

val default_append_entries_response_success_data : unit -> append_entries_response_success_data
(** [default_append_entries_response_success_data ()] is the default value for type [append_entries_response_success_data] *)


val default_append_entries_response : unit -> append_entries_response
(** [default_append_entries_response ()] is the default value for type [append_entries_response] *)

val default_server_index : unit -> server_index
(** [default_server_index ()] is the default value for type [server_index] *)

val default_leader_state : unit -> leader_state
(** [default_leader_state ()] is the default value for type [leader_state] *)

val default_candidate_state : unit -> candidate_state
(** [default_candidate_state ()] is the default value for type [candidate_state] *)

val default_follower_state : unit -> follower_state
(** [default_follower_state ()] is the default value for type [follower_state] *)

val default_configuration : unit -> configuration
(** [default_configuration ()] is the default value for type [configuration] *)


val default_state : unit -> state
(** [default_state ()] is the default value for type [state] *)

val default_follow_up_action_retry_append_data : unit -> follow_up_action_retry_append_data
(** [default_follow_up_action_retry_append_data ()] is the default value for type [follow_up_action_retry_append_data] *)

val default_follow_up_action : unit -> follow_up_action
(** [default_follow_up_action ()] is the default value for type [follow_up_action] *)


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


val decode_append_entries_response : Pbrt.Decoder.t -> append_entries_response
(** [decode_append_entries_response decoder] decodes a [append_entries_response] value from [decoder] *)

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


val decode_state : Pbrt.Decoder.t -> state
(** [decode_state decoder] decodes a [state] value from [decoder] *)

val decode_follow_up_action_retry_append_data : Pbrt.Decoder.t -> follow_up_action_retry_append_data
(** [decode_follow_up_action_retry_append_data decoder] decodes a [follow_up_action_retry_append_data] value from [decoder] *)

val decode_follow_up_action : Pbrt.Decoder.t -> follow_up_action
(** [decode_follow_up_action decoder] decodes a [follow_up_action] value from [decoder] *)


(** {2 Protobuf Encoding} *)

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


val encode_append_entries_response : append_entries_response -> Pbrt.Encoder.t -> unit
(** [encode_append_entries_response v encoder] encodes [v] with the given [encoder] *)

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


val encode_state : state -> Pbrt.Encoder.t -> unit
(** [encode_state v encoder] encodes [v] with the given [encoder] *)

val encode_follow_up_action_retry_append_data : follow_up_action_retry_append_data -> Pbrt.Encoder.t -> unit
(** [encode_follow_up_action_retry_append_data v encoder] encodes [v] with the given [encoder] *)

val encode_follow_up_action : follow_up_action -> Pbrt.Encoder.t -> unit
(** [encode_follow_up_action v encoder] encodes [v] with the given [encoder] *)


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

val pp_append_entries_response_result : Format.formatter -> append_entries_response_result -> unit 
(** [pp_append_entries_response_result v] formats v] *)

val pp_append_entries_response : Format.formatter -> append_entries_response -> unit 
(** [pp_append_entries_response v] formats v] *)

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

val pp_state_role : Format.formatter -> state_role -> unit 
(** [pp_state_role v] formats v] *)

val pp_state : Format.formatter -> state -> unit 
(** [pp_state v] formats v] *)

val pp_follow_up_action_retry_append_data : Format.formatter -> follow_up_action_retry_append_data -> unit 
(** [pp_follow_up_action_retry_append_data v] formats v] *)

val pp_follow_up_action : Format.formatter -> follow_up_action -> unit 
(** [pp_follow_up_action v] formats v] *)
