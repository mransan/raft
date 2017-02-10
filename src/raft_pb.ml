[@@@ocaml.warning "-27-30-39"]

type request_vote_request = {
  candidate_term : int;
  candidate_id : int;
  candidate_last_log_index : int;
  candidate_last_log_term : int;
}

and request_vote_request_mutable = {
  mutable candidate_term : int;
  mutable candidate_id : int;
  mutable candidate_last_log_index : int;
  mutable candidate_last_log_term : int;
}

type request_vote_response = {
  voter_id : int;
  voter_term : int;
  vote_granted : bool;
}

and request_vote_response_mutable = {
  mutable voter_id : int;
  mutable voter_term : int;
  mutable vote_granted : bool;
}

type log_entry = {
  index : int;
  term : int;
  data : bytes;
  id : string;
}

and log_entry_mutable = {
  mutable index : int;
  mutable term : int;
  mutable data : bytes;
  mutable id : string;
}

type append_entries_request = {
  leader_term : int;
  leader_id : int;
  prev_log_index : int;
  prev_log_term : int;
  rev_log_entries : log_entry list;
  leader_commit : int;
}

and append_entries_request_mutable = {
  mutable leader_term : int;
  mutable leader_id : int;
  mutable prev_log_index : int;
  mutable prev_log_term : int;
  mutable rev_log_entries : log_entry list;
  mutable leader_commit : int;
}

type append_entries_response_success_data = {
  receiver_last_log_index : int;
}

and append_entries_response_success_data_mutable = {
  mutable receiver_last_log_index : int;
}

type append_entries_response_log_failure_data = {
  receiver_last_log_index : int;
}

and append_entries_response_log_failure_data_mutable = {
  mutable receiver_last_log_index : int;
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

and append_entries_response_mutable = {
  mutable receiver_id : int;
  mutable receiver_term : int;
  mutable result : append_entries_response_result;
}

type message =
  | Request_vote_request of request_vote_request
  | Request_vote_response of request_vote_response
  | Append_entries_request of append_entries_request
  | Append_entries_response of append_entries_response

type log_interval_compacted = {
  record_id : string;
}

and log_interval_compacted_mutable = {
  mutable record_id : string;
}

type log_interval_expanded = {
  entries : log_entry list;
}

and log_interval_expanded_mutable = {
  mutable entries : log_entry list;
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

and log_interval_mutable = {
  mutable prev_index : int;
  mutable prev_term : int;
  mutable last_index : int;
  mutable rev_log_entries : log_interval_rev_log_entries;
}

type follower_info = {
  server_id : int;
  next_index : int;
  match_index : int;
  heartbeat_deadline : float;
  outstanding_request : bool;
  unsent_entries : log_entry list;
}

and follower_info_mutable = {
  mutable server_id : int;
  mutable next_index : int;
  mutable match_index : int;
  mutable heartbeat_deadline : float;
  mutable outstanding_request : bool;
  mutable unsent_entries : log_entry list;
}

type leader_state = {
  followers : follower_info list;
}

and leader_state_mutable = {
  mutable followers : follower_info list;
}

type candidate_state = {
  vote_count : int;
  election_deadline : float;
}

and candidate_state_mutable = {
  mutable vote_count : int;
  mutable election_deadline : float;
}

type follower_state = {
  voted_for : int option;
  current_leader : int option;
  election_deadline : float;
}

and follower_state_mutable = {
  mutable voted_for : int option;
  mutable current_leader : int option;
  mutable election_deadline : float;
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

and configuration_mutable = {
  mutable nb_of_server : int;
  mutable election_timeout : float;
  mutable election_timeout_range : float;
  mutable hearbeat_timeout : float;
  mutable max_nb_logs_per_message : int;
  mutable log_interval_size : int;
}

type timeout_event_time_out_type =
  | New_leader_election 
  | Heartbeat 

type timeout_event = {
  timeout : float;
  timeout_type : timeout_event_time_out_type;
}

and timeout_event_mutable = {
  mutable timeout : float;
  mutable timeout_type : timeout_event_time_out_type;
}

type notification =
  | Committed_data of log_entry list
  | New_leader of int 
  | No_leader
type compaction_report = {
  to_be_compacted : log_interval list;
  to_be_expanded : log_interval list;
}

and compaction_report_mutable = {
  mutable to_be_compacted : log_interval list;
  mutable to_be_expanded : log_interval list;
}

let rec default_request_vote_request 
  ?candidate_term:((candidate_term:int) = 0)
  ?candidate_id:((candidate_id:int) = 0)
  ?candidate_last_log_index:((candidate_last_log_index:int) = 0)
  ?candidate_last_log_term:((candidate_last_log_term:int) = 0)
  () : request_vote_request  = {
  candidate_term;
  candidate_id;
  candidate_last_log_index;
  candidate_last_log_term;
}

and default_request_vote_request_mutable () : request_vote_request_mutable = {
  candidate_term = 0;
  candidate_id = 0;
  candidate_last_log_index = 0;
  candidate_last_log_term = 0;
}

let rec default_request_vote_response 
  ?voter_id:((voter_id:int) = 0)
  ?voter_term:((voter_term:int) = 0)
  ?vote_granted:((vote_granted:bool) = false)
  () : request_vote_response  = {
  voter_id;
  voter_term;
  vote_granted;
}

and default_request_vote_response_mutable () : request_vote_response_mutable = {
  voter_id = 0;
  voter_term = 0;
  vote_granted = false;
}

let rec default_log_entry 
  ?index:((index:int) = 0)
  ?term:((term:int) = 0)
  ?data:((data:bytes) = Bytes.create 0)
  ?id:((id:string) = "")
  () : log_entry  = {
  index;
  term;
  data;
  id;
}

and default_log_entry_mutable () : log_entry_mutable = {
  index = 0;
  term = 0;
  data = Bytes.create 0;
  id = "";
}

let rec default_append_entries_request 
  ?leader_term:((leader_term:int) = 0)
  ?leader_id:((leader_id:int) = 0)
  ?prev_log_index:((prev_log_index:int) = 0)
  ?prev_log_term:((prev_log_term:int) = 0)
  ?rev_log_entries:((rev_log_entries:log_entry list) = [])
  ?leader_commit:((leader_commit:int) = 0)
  () : append_entries_request  = {
  leader_term;
  leader_id;
  prev_log_index;
  prev_log_term;
  rev_log_entries;
  leader_commit;
}

and default_append_entries_request_mutable () : append_entries_request_mutable = {
  leader_term = 0;
  leader_id = 0;
  prev_log_index = 0;
  prev_log_term = 0;
  rev_log_entries = [];
  leader_commit = 0;
}

let rec default_append_entries_response_success_data 
  ?receiver_last_log_index:((receiver_last_log_index:int) = 0)
  () : append_entries_response_success_data  = {
  receiver_last_log_index;
}

and default_append_entries_response_success_data_mutable () : append_entries_response_success_data_mutable = {
  receiver_last_log_index = 0;
}

let rec default_append_entries_response_log_failure_data 
  ?receiver_last_log_index:((receiver_last_log_index:int) = 0)
  () : append_entries_response_log_failure_data  = {
  receiver_last_log_index;
}

and default_append_entries_response_log_failure_data_mutable () : append_entries_response_log_failure_data_mutable = {
  receiver_last_log_index = 0;
}

let rec default_append_entries_response_result () : append_entries_response_result = Success (default_append_entries_response_success_data ())

and default_append_entries_response 
  ?receiver_id:((receiver_id:int) = 0)
  ?receiver_term:((receiver_term:int) = 0)
  ?result:((result:append_entries_response_result) = Success (default_append_entries_response_success_data ()))
  () : append_entries_response  = {
  receiver_id;
  receiver_term;
  result;
}

and default_append_entries_response_mutable () : append_entries_response_mutable = {
  receiver_id = 0;
  receiver_term = 0;
  result = Success (default_append_entries_response_success_data ());
}

let rec default_message () : message = Request_vote_request (default_request_vote_request ())

let rec default_log_interval_compacted 
  ?record_id:((record_id:string) = "")
  () : log_interval_compacted  = {
  record_id;
}

and default_log_interval_compacted_mutable () : log_interval_compacted_mutable = {
  record_id = "";
}

let rec default_log_interval_expanded 
  ?entries:((entries:log_entry list) = [])
  () : log_interval_expanded  = {
  entries;
}

and default_log_interval_expanded_mutable () : log_interval_expanded_mutable = {
  entries = [];
}

let rec default_log_interval_rev_log_entries () : log_interval_rev_log_entries = Compacted (default_log_interval_compacted ())

and default_log_interval 
  ?prev_index:((prev_index:int) = 0)
  ?prev_term:((prev_term:int) = 0)
  ?last_index:((last_index:int) = 0)
  ?rev_log_entries:((rev_log_entries:log_interval_rev_log_entries) = Compacted (default_log_interval_compacted ()))
  () : log_interval  = {
  prev_index;
  prev_term;
  last_index;
  rev_log_entries;
}

and default_log_interval_mutable () : log_interval_mutable = {
  prev_index = 0;
  prev_term = 0;
  last_index = 0;
  rev_log_entries = Compacted (default_log_interval_compacted ());
}

let rec default_follower_info 
  ?server_id:((server_id:int) = 0)
  ?next_index:((next_index:int) = 0)
  ?match_index:((match_index:int) = 0)
  ?heartbeat_deadline:((heartbeat_deadline:float) = 0.)
  ?outstanding_request:((outstanding_request:bool) = false)
  ?unsent_entries:((unsent_entries:log_entry list) = [])
  () : follower_info  = {
  server_id;
  next_index;
  match_index;
  heartbeat_deadline;
  outstanding_request;
  unsent_entries;
}

and default_follower_info_mutable () : follower_info_mutable = {
  server_id = 0;
  next_index = 0;
  match_index = 0;
  heartbeat_deadline = 0.;
  outstanding_request = false;
  unsent_entries = [];
}

let rec default_leader_state 
  ?followers:((followers:follower_info list) = [])
  () : leader_state  = {
  followers;
}

and default_leader_state_mutable () : leader_state_mutable = {
  followers = [];
}

let rec default_candidate_state 
  ?vote_count:((vote_count:int) = 0)
  ?election_deadline:((election_deadline:float) = 0.)
  () : candidate_state  = {
  vote_count;
  election_deadline;
}

and default_candidate_state_mutable () : candidate_state_mutable = {
  vote_count = 0;
  election_deadline = 0.;
}

let rec default_follower_state 
  ?voted_for:((voted_for:int option) = None)
  ?current_leader:((current_leader:int option) = None)
  ?election_deadline:((election_deadline:float) = 0.)
  () : follower_state  = {
  voted_for;
  current_leader;
  election_deadline;
}

and default_follower_state_mutable () : follower_state_mutable = {
  voted_for = None;
  current_leader = None;
  election_deadline = 0.;
}

let rec default_role () : role = Leader (default_leader_state ())

let rec default_configuration 
  ?nb_of_server:((nb_of_server:int) = 0)
  ?election_timeout:((election_timeout:float) = 0.)
  ?election_timeout_range:((election_timeout_range:float) = 0.)
  ?hearbeat_timeout:((hearbeat_timeout:float) = 0.)
  ?max_nb_logs_per_message:((max_nb_logs_per_message:int) = 0)
  ?log_interval_size:((log_interval_size:int) = 0)
  () : configuration  = {
  nb_of_server;
  election_timeout;
  election_timeout_range;
  hearbeat_timeout;
  max_nb_logs_per_message;
  log_interval_size;
}

and default_configuration_mutable () : configuration_mutable = {
  nb_of_server = 0;
  election_timeout = 0.;
  election_timeout_range = 0.;
  hearbeat_timeout = 0.;
  max_nb_logs_per_message = 0;
  log_interval_size = 0;
}

let rec default_timeout_event_time_out_type () = (New_leader_election:timeout_event_time_out_type)

let rec default_timeout_event 
  ?timeout:((timeout:float) = 0.)
  ?timeout_type:((timeout_type:timeout_event_time_out_type) = default_timeout_event_time_out_type ())
  () : timeout_event  = {
  timeout;
  timeout_type;
}

and default_timeout_event_mutable () : timeout_event_mutable = {
  timeout = 0.;
  timeout_type = default_timeout_event_time_out_type ();
}

let rec default_compaction_report 
  ?to_be_compacted:((to_be_compacted:log_interval list) = [])
  ?to_be_expanded:((to_be_expanded:log_interval list) = [])
  () : compaction_report  = {
  to_be_compacted;
  to_be_expanded;
}

and default_compaction_report_mutable () : compaction_report_mutable = {
  to_be_compacted = [];
  to_be_expanded = [];
}

let rec decode_request_vote_request d =
  let v = default_request_vote_request_mutable () in
  let candidate_last_log_term_is_set = ref false in
  let candidate_last_log_index_is_set = ref false in
  let candidate_id_is_set = ref false in
  let candidate_term_is_set = ref false in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Varint) -> (
      v.candidate_term <- Pbrt.Decoder.int_as_varint d; candidate_term_is_set := true;
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(request_vote_request), field(1)", pk))
    )
    | Some (2, Pbrt.Varint) -> (
      v.candidate_id <- Pbrt.Decoder.int_as_varint d; candidate_id_is_set := true;
      loop ()
    )
    | Some (2, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(request_vote_request), field(2)", pk))
    )
    | Some (3, Pbrt.Varint) -> (
      v.candidate_last_log_index <- Pbrt.Decoder.int_as_varint d; candidate_last_log_index_is_set := true;
      loop ()
    )
    | Some (3, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(request_vote_request), field(3)", pk))
    )
    | Some (4, Pbrt.Varint) -> (
      v.candidate_last_log_term <- Pbrt.Decoder.int_as_varint d; candidate_last_log_term_is_set := true;
      loop ()
    )
    | Some (4, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(request_vote_request), field(4)", pk))
    )
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  begin if not !candidate_last_log_term_is_set then raise Protobuf.Decoder.(Failure (Missing_field "candidate_last_log_term")) end;
  begin if not !candidate_last_log_index_is_set then raise Protobuf.Decoder.(Failure (Missing_field "candidate_last_log_index")) end;
  begin if not !candidate_id_is_set then raise Protobuf.Decoder.(Failure (Missing_field "candidate_id")) end;
  begin if not !candidate_term_is_set then raise Protobuf.Decoder.(Failure (Missing_field "candidate_term")) end;
  let v:request_vote_request = Obj.magic v in
  v

let rec decode_request_vote_response d =
  let v = default_request_vote_response_mutable () in
  let vote_granted_is_set = ref false in
  let voter_term_is_set = ref false in
  let voter_id_is_set = ref false in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Varint) -> (
      v.voter_id <- Pbrt.Decoder.int_as_varint d; voter_id_is_set := true;
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(request_vote_response), field(1)", pk))
    )
    | Some (2, Pbrt.Varint) -> (
      v.voter_term <- Pbrt.Decoder.int_as_varint d; voter_term_is_set := true;
      loop ()
    )
    | Some (2, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(request_vote_response), field(2)", pk))
    )
    | Some (3, Pbrt.Varint) -> (
      v.vote_granted <- Pbrt.Decoder.bool d; vote_granted_is_set := true;
      loop ()
    )
    | Some (3, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(request_vote_response), field(3)", pk))
    )
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  begin if not !vote_granted_is_set then raise Protobuf.Decoder.(Failure (Missing_field "vote_granted")) end;
  begin if not !voter_term_is_set then raise Protobuf.Decoder.(Failure (Missing_field "voter_term")) end;
  begin if not !voter_id_is_set then raise Protobuf.Decoder.(Failure (Missing_field "voter_id")) end;
  let v:request_vote_response = Obj.magic v in
  v

let rec decode_log_entry d =
  let v = default_log_entry_mutable () in
  let id_is_set = ref false in
  let data_is_set = ref false in
  let term_is_set = ref false in
  let index_is_set = ref false in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Varint) -> (
      v.index <- Pbrt.Decoder.int_as_varint d; index_is_set := true;
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(log_entry), field(1)", pk))
    )
    | Some (2, Pbrt.Varint) -> (
      v.term <- Pbrt.Decoder.int_as_varint d; term_is_set := true;
      loop ()
    )
    | Some (2, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(log_entry), field(2)", pk))
    )
    | Some (3, Pbrt.Bytes) -> (
      v.data <- Pbrt.Decoder.bytes d; data_is_set := true;
      loop ()
    )
    | Some (3, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(log_entry), field(3)", pk))
    )
    | Some (4, Pbrt.Bytes) -> (
      v.id <- Pbrt.Decoder.string d; id_is_set := true;
      loop ()
    )
    | Some (4, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(log_entry), field(4)", pk))
    )
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  begin if not !id_is_set then raise Protobuf.Decoder.(Failure (Missing_field "id")) end;
  begin if not !data_is_set then raise Protobuf.Decoder.(Failure (Missing_field "data")) end;
  begin if not !term_is_set then raise Protobuf.Decoder.(Failure (Missing_field "term")) end;
  begin if not !index_is_set then raise Protobuf.Decoder.(Failure (Missing_field "index")) end;
  let v:log_entry = Obj.magic v in
  v

let rec decode_append_entries_request d =
  let v = default_append_entries_request_mutable () in
  let leader_commit_is_set = ref false in
  let prev_log_term_is_set = ref false in
  let prev_log_index_is_set = ref false in
  let leader_id_is_set = ref false in
  let leader_term_is_set = ref false in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
      v.rev_log_entries <- List.rev v.rev_log_entries;
    )
    | Some (1, Pbrt.Varint) -> (
      v.leader_term <- Pbrt.Decoder.int_as_varint d; leader_term_is_set := true;
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(append_entries_request), field(1)", pk))
    )
    | Some (2, Pbrt.Varint) -> (
      v.leader_id <- Pbrt.Decoder.int_as_varint d; leader_id_is_set := true;
      loop ()
    )
    | Some (2, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(append_entries_request), field(2)", pk))
    )
    | Some (3, Pbrt.Varint) -> (
      v.prev_log_index <- Pbrt.Decoder.int_as_varint d; prev_log_index_is_set := true;
      loop ()
    )
    | Some (3, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(append_entries_request), field(3)", pk))
    )
    | Some (4, Pbrt.Varint) -> (
      v.prev_log_term <- Pbrt.Decoder.int_as_varint d; prev_log_term_is_set := true;
      loop ()
    )
    | Some (4, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(append_entries_request), field(4)", pk))
    )
    | Some (5, Pbrt.Bytes) -> (
      v.rev_log_entries <- (decode_log_entry (Pbrt.Decoder.nested d)) :: v.rev_log_entries;
      loop ()
    )
    | Some (5, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(append_entries_request), field(5)", pk))
    )
    | Some (6, Pbrt.Varint) -> (
      v.leader_commit <- Pbrt.Decoder.int_as_varint d; leader_commit_is_set := true;
      loop ()
    )
    | Some (6, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(append_entries_request), field(6)", pk))
    )
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  begin if not !leader_commit_is_set then raise Protobuf.Decoder.(Failure (Missing_field "leader_commit")) end;
  begin if not !prev_log_term_is_set then raise Protobuf.Decoder.(Failure (Missing_field "prev_log_term")) end;
  begin if not !prev_log_index_is_set then raise Protobuf.Decoder.(Failure (Missing_field "prev_log_index")) end;
  begin if not !leader_id_is_set then raise Protobuf.Decoder.(Failure (Missing_field "leader_id")) end;
  begin if not !leader_term_is_set then raise Protobuf.Decoder.(Failure (Missing_field "leader_term")) end;
  let v:append_entries_request = Obj.magic v in
  v

let rec decode_append_entries_response_success_data d =
  let v = default_append_entries_response_success_data_mutable () in
  let receiver_last_log_index_is_set = ref false in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Varint) -> (
      v.receiver_last_log_index <- Pbrt.Decoder.int_as_varint d; receiver_last_log_index_is_set := true;
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(append_entries_response_success_data), field(1)", pk))
    )
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  begin if not !receiver_last_log_index_is_set then raise Protobuf.Decoder.(Failure (Missing_field "receiver_last_log_index")) end;
  let v:append_entries_response_success_data = Obj.magic v in
  v

let rec decode_append_entries_response_log_failure_data d =
  let v = default_append_entries_response_log_failure_data_mutable () in
  let receiver_last_log_index_is_set = ref false in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Varint) -> (
      v.receiver_last_log_index <- Pbrt.Decoder.int_as_varint d; receiver_last_log_index_is_set := true;
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(append_entries_response_log_failure_data), field(1)", pk))
    )
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  begin if not !receiver_last_log_index_is_set then raise Protobuf.Decoder.(Failure (Missing_field "receiver_last_log_index")) end;
  let v:append_entries_response_log_failure_data = Obj.magic v in
  v

let rec decode_append_entries_response_result d = 
  let rec loop () = 
    let ret:append_entries_response_result = match Pbrt.Decoder.key d with
      | None -> failwith "None of the known key is found"
      | Some (4, _) -> Success (decode_append_entries_response_success_data (Pbrt.Decoder.nested d))
      | Some (5, _) -> Log_failure (decode_append_entries_response_log_failure_data (Pbrt.Decoder.nested d))
      | Some (6, _) -> (Pbrt.Decoder.empty_nested d ; Term_failure)
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

and decode_append_entries_response d =
  let v = default_append_entries_response_mutable () in
  let receiver_term_is_set = ref false in
  let receiver_id_is_set = ref false in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Varint) -> (
      v.receiver_id <- Pbrt.Decoder.int_as_varint d; receiver_id_is_set := true;
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(append_entries_response), field(1)", pk))
    )
    | Some (2, Pbrt.Varint) -> (
      v.receiver_term <- Pbrt.Decoder.int_as_varint d; receiver_term_is_set := true;
      loop ()
    )
    | Some (2, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(append_entries_response), field(2)", pk))
    )
    | Some (4, Pbrt.Bytes) -> (
      v.result <- Success (decode_append_entries_response_success_data (Pbrt.Decoder.nested d));
      loop ()
    )
    | Some (4, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(append_entries_response), field(4)", pk))
    )
    | Some (5, Pbrt.Bytes) -> (
      v.result <- Log_failure (decode_append_entries_response_log_failure_data (Pbrt.Decoder.nested d));
      loop ()
    )
    | Some (5, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(append_entries_response), field(5)", pk))
    )
    | Some (6, Pbrt.Bytes) -> (
      Pbrt.Decoder.empty_nested d;
      v.result <- Term_failure;
      loop ()
    )
    | Some (6, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(append_entries_response), field(6)", pk))
    )
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  begin if not !receiver_term_is_set then raise Protobuf.Decoder.(Failure (Missing_field "receiver_term")) end;
  begin if not !receiver_id_is_set then raise Protobuf.Decoder.(Failure (Missing_field "receiver_id")) end;
  let v:append_entries_response = Obj.magic v in
  v

let rec decode_message d = 
  let rec loop () = 
    let ret:message = match Pbrt.Decoder.key d with
      | None -> failwith "None of the known key is found"
      | Some (1, _) -> Request_vote_request (decode_request_vote_request (Pbrt.Decoder.nested d))
      | Some (2, _) -> Request_vote_response (decode_request_vote_response (Pbrt.Decoder.nested d))
      | Some (3, _) -> Append_entries_request (decode_append_entries_request (Pbrt.Decoder.nested d))
      | Some (4, _) -> Append_entries_response (decode_append_entries_response (Pbrt.Decoder.nested d))
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

let rec decode_log_interval_compacted d =
  let v = default_log_interval_compacted_mutable () in
  let record_id_is_set = ref false in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Bytes) -> (
      v.record_id <- Pbrt.Decoder.string d; record_id_is_set := true;
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(log_interval_compacted), field(1)", pk))
    )
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  begin if not !record_id_is_set then raise Protobuf.Decoder.(Failure (Missing_field "record_id")) end;
  let v:log_interval_compacted = Obj.magic v in
  v

let rec decode_log_interval_expanded d =
  let v = default_log_interval_expanded_mutable () in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
      v.entries <- List.rev v.entries;
    )
    | Some (1, Pbrt.Bytes) -> (
      v.entries <- (decode_log_entry (Pbrt.Decoder.nested d)) :: v.entries;
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(log_interval_expanded), field(1)", pk))
    )
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  let v:log_interval_expanded = Obj.magic v in
  v

let rec decode_log_interval_rev_log_entries d = 
  let rec loop () = 
    let ret:log_interval_rev_log_entries = match Pbrt.Decoder.key d with
      | None -> failwith "None of the known key is found"
      | Some (4, _) -> Compacted (decode_log_interval_compacted (Pbrt.Decoder.nested d))
      | Some (5, _) -> Expanded (decode_log_interval_expanded (Pbrt.Decoder.nested d))
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

and decode_log_interval d =
  let v = default_log_interval_mutable () in
  let last_index_is_set = ref false in
  let prev_term_is_set = ref false in
  let prev_index_is_set = ref false in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Varint) -> (
      v.prev_index <- Pbrt.Decoder.int_as_varint d; prev_index_is_set := true;
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(log_interval), field(1)", pk))
    )
    | Some (2, Pbrt.Varint) -> (
      v.prev_term <- Pbrt.Decoder.int_as_varint d; prev_term_is_set := true;
      loop ()
    )
    | Some (2, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(log_interval), field(2)", pk))
    )
    | Some (3, Pbrt.Varint) -> (
      v.last_index <- Pbrt.Decoder.int_as_varint d; last_index_is_set := true;
      loop ()
    )
    | Some (3, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(log_interval), field(3)", pk))
    )
    | Some (4, Pbrt.Bytes) -> (
      v.rev_log_entries <- Compacted (decode_log_interval_compacted (Pbrt.Decoder.nested d));
      loop ()
    )
    | Some (4, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(log_interval), field(4)", pk))
    )
    | Some (5, Pbrt.Bytes) -> (
      v.rev_log_entries <- Expanded (decode_log_interval_expanded (Pbrt.Decoder.nested d));
      loop ()
    )
    | Some (5, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(log_interval), field(5)", pk))
    )
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  begin if not !last_index_is_set then raise Protobuf.Decoder.(Failure (Missing_field "last_index")) end;
  begin if not !prev_term_is_set then raise Protobuf.Decoder.(Failure (Missing_field "prev_term")) end;
  begin if not !prev_index_is_set then raise Protobuf.Decoder.(Failure (Missing_field "prev_index")) end;
  let v:log_interval = Obj.magic v in
  v

let rec decode_follower_info d =
  let v = default_follower_info_mutable () in
  let outstanding_request_is_set = ref false in
  let heartbeat_deadline_is_set = ref false in
  let match_index_is_set = ref false in
  let next_index_is_set = ref false in
  let server_id_is_set = ref false in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
      v.unsent_entries <- List.rev v.unsent_entries;
    )
    | Some (1, Pbrt.Varint) -> (
      v.server_id <- Pbrt.Decoder.int_as_varint d; server_id_is_set := true;
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(follower_info), field(1)", pk))
    )
    | Some (2, Pbrt.Varint) -> (
      v.next_index <- Pbrt.Decoder.int_as_varint d; next_index_is_set := true;
      loop ()
    )
    | Some (2, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(follower_info), field(2)", pk))
    )
    | Some (3, Pbrt.Varint) -> (
      v.match_index <- Pbrt.Decoder.int_as_varint d; match_index_is_set := true;
      loop ()
    )
    | Some (3, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(follower_info), field(3)", pk))
    )
    | Some (5, Pbrt.Bits32) -> (
      v.heartbeat_deadline <- Pbrt.Decoder.float_as_bits32 d; heartbeat_deadline_is_set := true;
      loop ()
    )
    | Some (5, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(follower_info), field(5)", pk))
    )
    | Some (6, Pbrt.Varint) -> (
      v.outstanding_request <- Pbrt.Decoder.bool d; outstanding_request_is_set := true;
      loop ()
    )
    | Some (6, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(follower_info), field(6)", pk))
    )
    | Some (4, Pbrt.Bytes) -> (
      v.unsent_entries <- (decode_log_entry (Pbrt.Decoder.nested d)) :: v.unsent_entries;
      loop ()
    )
    | Some (4, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(follower_info), field(4)", pk))
    )
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  begin if not !outstanding_request_is_set then raise Protobuf.Decoder.(Failure (Missing_field "outstanding_request")) end;
  begin if not !heartbeat_deadline_is_set then raise Protobuf.Decoder.(Failure (Missing_field "heartbeat_deadline")) end;
  begin if not !match_index_is_set then raise Protobuf.Decoder.(Failure (Missing_field "match_index")) end;
  begin if not !next_index_is_set then raise Protobuf.Decoder.(Failure (Missing_field "next_index")) end;
  begin if not !server_id_is_set then raise Protobuf.Decoder.(Failure (Missing_field "server_id")) end;
  let v:follower_info = Obj.magic v in
  v

let rec decode_leader_state d =
  let v = default_leader_state_mutable () in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
      v.followers <- List.rev v.followers;
    )
    | Some (1, Pbrt.Bytes) -> (
      v.followers <- (decode_follower_info (Pbrt.Decoder.nested d)) :: v.followers;
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(leader_state), field(1)", pk))
    )
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  let v:leader_state = Obj.magic v in
  v

let rec decode_candidate_state d =
  let v = default_candidate_state_mutable () in
  let election_deadline_is_set = ref false in
  let vote_count_is_set = ref false in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Varint) -> (
      v.vote_count <- Pbrt.Decoder.int_as_varint d; vote_count_is_set := true;
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(candidate_state), field(1)", pk))
    )
    | Some (2, Pbrt.Bits64) -> (
      v.election_deadline <- Pbrt.Decoder.float_as_bits64 d; election_deadline_is_set := true;
      loop ()
    )
    | Some (2, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(candidate_state), field(2)", pk))
    )
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  begin if not !election_deadline_is_set then raise Protobuf.Decoder.(Failure (Missing_field "election_deadline")) end;
  begin if not !vote_count_is_set then raise Protobuf.Decoder.(Failure (Missing_field "vote_count")) end;
  let v:candidate_state = Obj.magic v in
  v

let rec decode_follower_state d =
  let v = default_follower_state_mutable () in
  let election_deadline_is_set = ref false in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Varint) -> (
      v.voted_for <- Some (Pbrt.Decoder.int_as_varint d);
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(follower_state), field(1)", pk))
    )
    | Some (2, Pbrt.Varint) -> (
      v.current_leader <- Some (Pbrt.Decoder.int_as_varint d);
      loop ()
    )
    | Some (2, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(follower_state), field(2)", pk))
    )
    | Some (3, Pbrt.Bits64) -> (
      v.election_deadline <- Pbrt.Decoder.float_as_bits64 d; election_deadline_is_set := true;
      loop ()
    )
    | Some (3, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(follower_state), field(3)", pk))
    )
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  begin if not !election_deadline_is_set then raise Protobuf.Decoder.(Failure (Missing_field "election_deadline")) end;
  let v:follower_state = Obj.magic v in
  v

let rec decode_role d = 
  let rec loop () = 
    let ret:role = match Pbrt.Decoder.key d with
      | None -> failwith "None of the known key is found"
      | Some (6, _) -> Leader (decode_leader_state (Pbrt.Decoder.nested d))
      | Some (7, _) -> Candidate (decode_candidate_state (Pbrt.Decoder.nested d))
      | Some (8, _) -> Follower (decode_follower_state (Pbrt.Decoder.nested d))
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

let rec decode_configuration d =
  let v = default_configuration_mutable () in
  let log_interval_size_is_set = ref false in
  let max_nb_logs_per_message_is_set = ref false in
  let hearbeat_timeout_is_set = ref false in
  let election_timeout_range_is_set = ref false in
  let election_timeout_is_set = ref false in
  let nb_of_server_is_set = ref false in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Varint) -> (
      v.nb_of_server <- Pbrt.Decoder.int_as_varint d; nb_of_server_is_set := true;
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(configuration), field(1)", pk))
    )
    | Some (2, Pbrt.Bits64) -> (
      v.election_timeout <- Pbrt.Decoder.float_as_bits64 d; election_timeout_is_set := true;
      loop ()
    )
    | Some (2, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(configuration), field(2)", pk))
    )
    | Some (3, Pbrt.Bits64) -> (
      v.election_timeout_range <- Pbrt.Decoder.float_as_bits64 d; election_timeout_range_is_set := true;
      loop ()
    )
    | Some (3, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(configuration), field(3)", pk))
    )
    | Some (4, Pbrt.Bits64) -> (
      v.hearbeat_timeout <- Pbrt.Decoder.float_as_bits64 d; hearbeat_timeout_is_set := true;
      loop ()
    )
    | Some (4, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(configuration), field(4)", pk))
    )
    | Some (5, Pbrt.Varint) -> (
      v.max_nb_logs_per_message <- Pbrt.Decoder.int_as_varint d; max_nb_logs_per_message_is_set := true;
      loop ()
    )
    | Some (5, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(configuration), field(5)", pk))
    )
    | Some (6, Pbrt.Varint) -> (
      v.log_interval_size <- Pbrt.Decoder.int_as_varint d; log_interval_size_is_set := true;
      loop ()
    )
    | Some (6, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(configuration), field(6)", pk))
    )
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  begin if not !log_interval_size_is_set then raise Protobuf.Decoder.(Failure (Missing_field "log_interval_size")) end;
  begin if not !max_nb_logs_per_message_is_set then raise Protobuf.Decoder.(Failure (Missing_field "max_nb_logs_per_message")) end;
  begin if not !hearbeat_timeout_is_set then raise Protobuf.Decoder.(Failure (Missing_field "hearbeat_timeout")) end;
  begin if not !election_timeout_range_is_set then raise Protobuf.Decoder.(Failure (Missing_field "election_timeout_range")) end;
  begin if not !election_timeout_is_set then raise Protobuf.Decoder.(Failure (Missing_field "election_timeout")) end;
  begin if not !nb_of_server_is_set then raise Protobuf.Decoder.(Failure (Missing_field "nb_of_server")) end;
  let v:configuration = Obj.magic v in
  v

let rec decode_timeout_event_time_out_type d = 
  match Pbrt.Decoder.int_as_varint d with
  | 1 -> (New_leader_election:timeout_event_time_out_type)
  | 2 -> (Heartbeat:timeout_event_time_out_type)
  | _ -> failwith "Unknown value for enum timeout_event_time_out_type"

let rec decode_timeout_event d =
  let v = default_timeout_event_mutable () in
  let timeout_type_is_set = ref false in
  let timeout_is_set = ref false in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Bits64) -> (
      v.timeout <- Pbrt.Decoder.float_as_bits64 d; timeout_is_set := true;
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(timeout_event), field(1)", pk))
    )
    | Some (2, Pbrt.Varint) -> (
      v.timeout_type <- decode_timeout_event_time_out_type d; timeout_type_is_set := true;
      loop ()
    )
    | Some (2, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(timeout_event), field(2)", pk))
    )
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  begin if not !timeout_type_is_set then raise Protobuf.Decoder.(Failure (Missing_field "timeout_type")) end;
  begin if not !timeout_is_set then raise Protobuf.Decoder.(Failure (Missing_field "timeout")) end;
  let v:timeout_event = Obj.magic v in
  v

let rec decode_compaction_report d =
  let v = default_compaction_report_mutable () in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
      v.to_be_expanded <- List.rev v.to_be_expanded;
      v.to_be_compacted <- List.rev v.to_be_compacted;
    )
    | Some (1, Pbrt.Bytes) -> (
      v.to_be_compacted <- (decode_log_interval (Pbrt.Decoder.nested d)) :: v.to_be_compacted;
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(compaction_report), field(1)", pk))
    )
    | Some (2, Pbrt.Bytes) -> (
      v.to_be_expanded <- (decode_log_interval (Pbrt.Decoder.nested d)) :: v.to_be_expanded;
      loop ()
    )
    | Some (2, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(compaction_report), field(2)", pk))
    )
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  let v:compaction_report = Obj.magic v in
  v

let rec encode_request_vote_request (v:request_vote_request) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.candidate_term encoder;
  Pbrt.Encoder.key (2, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.candidate_id encoder;
  Pbrt.Encoder.key (3, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.candidate_last_log_index encoder;
  Pbrt.Encoder.key (4, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.candidate_last_log_term encoder;
  ()

let rec encode_request_vote_response (v:request_vote_response) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.voter_id encoder;
  Pbrt.Encoder.key (2, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.voter_term encoder;
  Pbrt.Encoder.key (3, Pbrt.Varint) encoder; 
  Pbrt.Encoder.bool v.vote_granted encoder;
  ()

let rec encode_log_entry (v:log_entry) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.index encoder;
  Pbrt.Encoder.key (2, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.term encoder;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.bytes v.data encoder;
  Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.id encoder;
  ()

let rec encode_append_entries_request (v:append_entries_request) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.leader_term encoder;
  Pbrt.Encoder.key (2, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.leader_id encoder;
  Pbrt.Encoder.key (3, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.prev_log_index encoder;
  Pbrt.Encoder.key (4, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.prev_log_term encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (5, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_log_entry x) encoder;
  ) v.rev_log_entries;
  Pbrt.Encoder.key (6, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.leader_commit encoder;
  ()

let rec encode_append_entries_response_success_data (v:append_entries_response_success_data) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.receiver_last_log_index encoder;
  ()

let rec encode_append_entries_response_log_failure_data (v:append_entries_response_log_failure_data) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.receiver_last_log_index encoder;
  ()

let rec encode_append_entries_response_result (v:append_entries_response_result) encoder = 
  match v with
  | Success x -> (
    Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_append_entries_response_success_data x) encoder;
  )
  | Log_failure x -> (
    Pbrt.Encoder.key (5, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_append_entries_response_log_failure_data x) encoder;
  )
  | Term_failure -> (
    Pbrt.Encoder.key (6, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder
  )

and encode_append_entries_response (v:append_entries_response) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.receiver_id encoder;
  Pbrt.Encoder.key (2, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.receiver_term encoder;
  (
    match v.result with
    | Success x -> (
      Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
      Pbrt.Encoder.nested (encode_append_entries_response_success_data x) encoder;
    )
    | Log_failure x -> (
      Pbrt.Encoder.key (5, Pbrt.Bytes) encoder; 
      Pbrt.Encoder.nested (encode_append_entries_response_log_failure_data x) encoder;
    )
    | Term_failure -> (
      Pbrt.Encoder.key (6, Pbrt.Bytes) encoder; 
      Pbrt.Encoder.empty_nested encoder
    )
  );
  ()

let rec encode_message (v:message) encoder = 
  match v with
  | Request_vote_request x -> (
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_request_vote_request x) encoder;
  )
  | Request_vote_response x -> (
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_request_vote_response x) encoder;
  )
  | Append_entries_request x -> (
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_append_entries_request x) encoder;
  )
  | Append_entries_response x -> (
    Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_append_entries_response x) encoder;
  )

let rec encode_log_interval_compacted (v:log_interval_compacted) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.record_id encoder;
  ()

let rec encode_log_interval_expanded (v:log_interval_expanded) encoder = 
  List.iter (fun x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_log_entry x) encoder;
  ) v.entries;
  ()

let rec encode_log_interval_rev_log_entries (v:log_interval_rev_log_entries) encoder = 
  match v with
  | Compacted x -> (
    Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_log_interval_compacted x) encoder;
  )
  | Expanded x -> (
    Pbrt.Encoder.key (5, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_log_interval_expanded x) encoder;
  )

and encode_log_interval (v:log_interval) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.prev_index encoder;
  Pbrt.Encoder.key (2, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.prev_term encoder;
  Pbrt.Encoder.key (3, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.last_index encoder;
  (
    match v.rev_log_entries with
    | Compacted x -> (
      Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
      Pbrt.Encoder.nested (encode_log_interval_compacted x) encoder;
    )
    | Expanded x -> (
      Pbrt.Encoder.key (5, Pbrt.Bytes) encoder; 
      Pbrt.Encoder.nested (encode_log_interval_expanded x) encoder;
    )
  );
  ()

let rec encode_follower_info (v:follower_info) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.server_id encoder;
  Pbrt.Encoder.key (2, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.next_index encoder;
  Pbrt.Encoder.key (3, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.match_index encoder;
  Pbrt.Encoder.key (5, Pbrt.Bits32) encoder; 
  Pbrt.Encoder.float_as_bits32 v.heartbeat_deadline encoder;
  Pbrt.Encoder.key (6, Pbrt.Varint) encoder; 
  Pbrt.Encoder.bool v.outstanding_request encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_log_entry x) encoder;
  ) v.unsent_entries;
  ()

let rec encode_leader_state (v:leader_state) encoder = 
  List.iter (fun x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_follower_info x) encoder;
  ) v.followers;
  ()

let rec encode_candidate_state (v:candidate_state) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.vote_count encoder;
  Pbrt.Encoder.key (2, Pbrt.Bits64) encoder; 
  Pbrt.Encoder.float_as_bits64 v.election_deadline encoder;
  ()

let rec encode_follower_state (v:follower_state) encoder = 
  (
    match v.voted_for with 
    | Some x -> (
      Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
      Pbrt.Encoder.int_as_varint x encoder;
    )
    | None -> ();
  );
  (
    match v.current_leader with 
    | Some x -> (
      Pbrt.Encoder.key (2, Pbrt.Varint) encoder; 
      Pbrt.Encoder.int_as_varint x encoder;
    )
    | None -> ();
  );
  Pbrt.Encoder.key (3, Pbrt.Bits64) encoder; 
  Pbrt.Encoder.float_as_bits64 v.election_deadline encoder;
  ()

let rec encode_role (v:role) encoder = 
  match v with
  | Leader x -> (
    Pbrt.Encoder.key (6, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_leader_state x) encoder;
  )
  | Candidate x -> (
    Pbrt.Encoder.key (7, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_candidate_state x) encoder;
  )
  | Follower x -> (
    Pbrt.Encoder.key (8, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_follower_state x) encoder;
  )

let rec encode_configuration (v:configuration) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.nb_of_server encoder;
  Pbrt.Encoder.key (2, Pbrt.Bits64) encoder; 
  Pbrt.Encoder.float_as_bits64 v.election_timeout encoder;
  Pbrt.Encoder.key (3, Pbrt.Bits64) encoder; 
  Pbrt.Encoder.float_as_bits64 v.election_timeout_range encoder;
  Pbrt.Encoder.key (4, Pbrt.Bits64) encoder; 
  Pbrt.Encoder.float_as_bits64 v.hearbeat_timeout encoder;
  Pbrt.Encoder.key (5, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.max_nb_logs_per_message encoder;
  Pbrt.Encoder.key (6, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.log_interval_size encoder;
  ()

let rec encode_timeout_event_time_out_type (v:timeout_event_time_out_type) encoder =
  match v with
  | New_leader_election -> Pbrt.Encoder.int_as_varint 1 encoder
  | Heartbeat -> Pbrt.Encoder.int_as_varint 2 encoder

let rec encode_timeout_event (v:timeout_event) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bits64) encoder; 
  Pbrt.Encoder.float_as_bits64 v.timeout encoder;
  Pbrt.Encoder.key (2, Pbrt.Varint) encoder; 
  encode_timeout_event_time_out_type v.timeout_type encoder;
  ()

let rec encode_compaction_report (v:compaction_report) encoder = 
  List.iter (fun x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_log_interval x) encoder;
  ) v.to_be_compacted;
  List.iter (fun x -> 
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_log_interval x) encoder;
  ) v.to_be_expanded;
  ()

let rec pp_request_vote_request fmt (v:request_vote_request) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "candidate_term" Pbrt.Pp.pp_int fmt v.candidate_term;
    Pbrt.Pp.pp_record_field "candidate_id" Pbrt.Pp.pp_int fmt v.candidate_id;
    Pbrt.Pp.pp_record_field "candidate_last_log_index" Pbrt.Pp.pp_int fmt v.candidate_last_log_index;
    Pbrt.Pp.pp_record_field "candidate_last_log_term" Pbrt.Pp.pp_int fmt v.candidate_last_log_term;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_request_vote_response fmt (v:request_vote_response) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "voter_id" Pbrt.Pp.pp_int fmt v.voter_id;
    Pbrt.Pp.pp_record_field "voter_term" Pbrt.Pp.pp_int fmt v.voter_term;
    Pbrt.Pp.pp_record_field "vote_granted" Pbrt.Pp.pp_bool fmt v.vote_granted;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_log_entry fmt (v:log_entry) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "index" Pbrt.Pp.pp_int fmt v.index;
    Pbrt.Pp.pp_record_field "term" Pbrt.Pp.pp_int fmt v.term;
    Pbrt.Pp.pp_record_field "data" Pbrt.Pp.pp_bytes fmt v.data;
    Pbrt.Pp.pp_record_field "id" Pbrt.Pp.pp_string fmt v.id;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_append_entries_request fmt (v:append_entries_request) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "leader_term" Pbrt.Pp.pp_int fmt v.leader_term;
    Pbrt.Pp.pp_record_field "leader_id" Pbrt.Pp.pp_int fmt v.leader_id;
    Pbrt.Pp.pp_record_field "prev_log_index" Pbrt.Pp.pp_int fmt v.prev_log_index;
    Pbrt.Pp.pp_record_field "prev_log_term" Pbrt.Pp.pp_int fmt v.prev_log_term;
    Pbrt.Pp.pp_record_field "rev_log_entries" (Pbrt.Pp.pp_list pp_log_entry) fmt v.rev_log_entries;
    Pbrt.Pp.pp_record_field "leader_commit" Pbrt.Pp.pp_int fmt v.leader_commit;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_append_entries_response_success_data fmt (v:append_entries_response_success_data) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "receiver_last_log_index" Pbrt.Pp.pp_int fmt v.receiver_last_log_index;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_append_entries_response_log_failure_data fmt (v:append_entries_response_log_failure_data) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "receiver_last_log_index" Pbrt.Pp.pp_int fmt v.receiver_last_log_index;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_append_entries_response_result fmt (v:append_entries_response_result) =
  match v with
  | Success x -> Format.fprintf fmt "@[Success(%a)@]" pp_append_entries_response_success_data x
  | Log_failure x -> Format.fprintf fmt "@[Log_failure(%a)@]" pp_append_entries_response_log_failure_data x
  | Term_failure  -> Format.fprintf fmt "Term_failure"

and pp_append_entries_response fmt (v:append_entries_response) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "receiver_id" Pbrt.Pp.pp_int fmt v.receiver_id;
    Pbrt.Pp.pp_record_field "receiver_term" Pbrt.Pp.pp_int fmt v.receiver_term;
    Pbrt.Pp.pp_record_field "result" pp_append_entries_response_result fmt v.result;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_message fmt (v:message) =
  match v with
  | Request_vote_request x -> Format.fprintf fmt "@[Request_vote_request(%a)@]" pp_request_vote_request x
  | Request_vote_response x -> Format.fprintf fmt "@[Request_vote_response(%a)@]" pp_request_vote_response x
  | Append_entries_request x -> Format.fprintf fmt "@[Append_entries_request(%a)@]" pp_append_entries_request x
  | Append_entries_response x -> Format.fprintf fmt "@[Append_entries_response(%a)@]" pp_append_entries_response x

let rec pp_log_interval_compacted fmt (v:log_interval_compacted) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "record_id" Pbrt.Pp.pp_string fmt v.record_id;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_log_interval_expanded fmt (v:log_interval_expanded) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "entries" (Pbrt.Pp.pp_list pp_log_entry) fmt v.entries;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_log_interval_rev_log_entries fmt (v:log_interval_rev_log_entries) =
  match v with
  | Compacted x -> Format.fprintf fmt "@[Compacted(%a)@]" pp_log_interval_compacted x
  | Expanded x -> Format.fprintf fmt "@[Expanded(%a)@]" pp_log_interval_expanded x

and pp_log_interval fmt (v:log_interval) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "prev_index" Pbrt.Pp.pp_int fmt v.prev_index;
    Pbrt.Pp.pp_record_field "prev_term" Pbrt.Pp.pp_int fmt v.prev_term;
    Pbrt.Pp.pp_record_field "last_index" Pbrt.Pp.pp_int fmt v.last_index;
    Pbrt.Pp.pp_record_field "rev_log_entries" pp_log_interval_rev_log_entries fmt v.rev_log_entries;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_follower_info fmt (v:follower_info) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "server_id" Pbrt.Pp.pp_int fmt v.server_id;
    Pbrt.Pp.pp_record_field "next_index" Pbrt.Pp.pp_int fmt v.next_index;
    Pbrt.Pp.pp_record_field "match_index" Pbrt.Pp.pp_int fmt v.match_index;
    Pbrt.Pp.pp_record_field "heartbeat_deadline" Pbrt.Pp.pp_float fmt v.heartbeat_deadline;
    Pbrt.Pp.pp_record_field "outstanding_request" Pbrt.Pp.pp_bool fmt v.outstanding_request;
    Pbrt.Pp.pp_record_field "unsent_entries" (Pbrt.Pp.pp_list pp_log_entry) fmt v.unsent_entries;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_leader_state fmt (v:leader_state) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "followers" (Pbrt.Pp.pp_list pp_follower_info) fmt v.followers;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_candidate_state fmt (v:candidate_state) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "vote_count" Pbrt.Pp.pp_int fmt v.vote_count;
    Pbrt.Pp.pp_record_field "election_deadline" Pbrt.Pp.pp_float fmt v.election_deadline;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_follower_state fmt (v:follower_state) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "voted_for" (Pbrt.Pp.pp_option Pbrt.Pp.pp_int) fmt v.voted_for;
    Pbrt.Pp.pp_record_field "current_leader" (Pbrt.Pp.pp_option Pbrt.Pp.pp_int) fmt v.current_leader;
    Pbrt.Pp.pp_record_field "election_deadline" Pbrt.Pp.pp_float fmt v.election_deadline;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_role fmt (v:role) =
  match v with
  | Leader x -> Format.fprintf fmt "@[Leader(%a)@]" pp_leader_state x
  | Candidate x -> Format.fprintf fmt "@[Candidate(%a)@]" pp_candidate_state x
  | Follower x -> Format.fprintf fmt "@[Follower(%a)@]" pp_follower_state x

let rec pp_configuration fmt (v:configuration) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "nb_of_server" Pbrt.Pp.pp_int fmt v.nb_of_server;
    Pbrt.Pp.pp_record_field "election_timeout" Pbrt.Pp.pp_float fmt v.election_timeout;
    Pbrt.Pp.pp_record_field "election_timeout_range" Pbrt.Pp.pp_float fmt v.election_timeout_range;
    Pbrt.Pp.pp_record_field "hearbeat_timeout" Pbrt.Pp.pp_float fmt v.hearbeat_timeout;
    Pbrt.Pp.pp_record_field "max_nb_logs_per_message" Pbrt.Pp.pp_int fmt v.max_nb_logs_per_message;
    Pbrt.Pp.pp_record_field "log_interval_size" Pbrt.Pp.pp_int fmt v.log_interval_size;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_timeout_event_time_out_type fmt (v:timeout_event_time_out_type) =
  match v with
  | New_leader_election -> Format.fprintf fmt "New_leader_election"
  | Heartbeat -> Format.fprintf fmt "Heartbeat"

let rec pp_timeout_event fmt (v:timeout_event) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "timeout" Pbrt.Pp.pp_float fmt v.timeout;
    Pbrt.Pp.pp_record_field "timeout_type" pp_timeout_event_time_out_type fmt v.timeout_type;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_compaction_report fmt (v:compaction_report) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "to_be_compacted" (Pbrt.Pp.pp_list pp_log_interval) fmt v.to_be_compacted;
    Pbrt.Pp.pp_record_field "to_be_expanded" (Pbrt.Pp.pp_list pp_log_interval) fmt v.to_be_expanded;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()
