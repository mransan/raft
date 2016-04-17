[@@@ocaml.warning "-30"]

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
}

and log_entry_mutable = {
  mutable index : int;
  mutable term : int;
  mutable data : bytes;
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

type append_entries_response_result =
  | Failure
  | Success of append_entries_response_success_data

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

type server_index = {
  server_id : int;
  server_log_index : int;
}

and server_index_mutable = {
  mutable server_id : int;
  mutable server_log_index : int;
}

type receiver_heartbeat = {
  server_id : int;
  heartbeat_deadline : float;
}

and receiver_heartbeat_mutable = {
  mutable server_id : int;
  mutable heartbeat_deadline : float;
}

type leader_state = {
  next_index : server_index list;
  match_index : server_index list;
  receiver_heartbeats : receiver_heartbeat list;
}

and leader_state_mutable = {
  mutable next_index : server_index list;
  mutable match_index : server_index list;
  mutable receiver_heartbeats : receiver_heartbeat list;
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

type configuration = {
  nb_of_server : int;
  election_timeout : float;
  election_timeout_range : float;
  hearbeat_timeout : float;
  max_nb_logs_per_message : int;
}

and configuration_mutable = {
  mutable nb_of_server : int;
  mutable election_timeout : float;
  mutable election_timeout_range : float;
  mutable hearbeat_timeout : float;
  mutable max_nb_logs_per_message : int;
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
  last_applied : int;
  role : state_role;
  configuration : configuration;
}

and state_mutable = {
  mutable id : int;
  mutable current_term : int;
  mutable log : log_entry list;
  mutable log_size : int;
  mutable commit_index : int;
  mutable last_applied : int;
  mutable role : state_role;
  mutable configuration : configuration;
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
  ?data:((data:bytes) = Bytes.create 64)
  () : log_entry  = {
  index;
  term;
  data;
}

and default_log_entry_mutable () : log_entry_mutable = {
  index = 0;
  term = 0;
  data = Bytes.create 64;
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


let rec default_append_entries_response 
  ?receiver_id:((receiver_id:int) = 0)
  ?receiver_term:((receiver_term:int) = 0)
  ?result:((result:append_entries_response_result) = Failure)
  () : append_entries_response  = {
  receiver_id;
  receiver_term;
  result;
}

and default_append_entries_response_mutable () : append_entries_response_mutable = {
  receiver_id = 0;
  receiver_term = 0;
  result = Failure;
}

let rec default_message () : message = Request_vote_request (default_request_vote_request ())

let rec default_server_index 
  ?server_id:((server_id:int) = 0)
  ?server_log_index:((server_log_index:int) = 0)
  () : server_index  = {
  server_id;
  server_log_index;
}

and default_server_index_mutable () : server_index_mutable = {
  server_id = 0;
  server_log_index = 0;
}

let rec default_receiver_heartbeat 
  ?server_id:((server_id:int) = 0)
  ?heartbeat_deadline:((heartbeat_deadline:float) = 0.)
  () : receiver_heartbeat  = {
  server_id;
  heartbeat_deadline;
}

and default_receiver_heartbeat_mutable () : receiver_heartbeat_mutable = {
  server_id = 0;
  heartbeat_deadline = 0.;
}

let rec default_leader_state 
  ?next_index:((next_index:server_index list) = [])
  ?match_index:((match_index:server_index list) = [])
  ?receiver_heartbeats:((receiver_heartbeats:receiver_heartbeat list) = [])
  () : leader_state  = {
  next_index;
  match_index;
  receiver_heartbeats;
}

and default_leader_state_mutable () : leader_state_mutable = {
  next_index = [];
  match_index = [];
  receiver_heartbeats = [];
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

let rec default_configuration 
  ?nb_of_server:((nb_of_server:int) = 0)
  ?election_timeout:((election_timeout:float) = 0.)
  ?election_timeout_range:((election_timeout_range:float) = 0.)
  ?hearbeat_timeout:((hearbeat_timeout:float) = 0.)
  ?max_nb_logs_per_message:((max_nb_logs_per_message:int) = 0)
  () : configuration  = {
  nb_of_server;
  election_timeout;
  election_timeout_range;
  hearbeat_timeout;
  max_nb_logs_per_message;
}

and default_configuration_mutable () : configuration_mutable = {
  nb_of_server = 0;
  election_timeout = 0.;
  election_timeout_range = 0.;
  hearbeat_timeout = 0.;
  max_nb_logs_per_message = 0;
}


let rec default_state 
  ?id:((id:int) = 0)
  ?current_term:((current_term:int) = 0)
  ?log:((log:log_entry list) = [])
  ?log_size:((log_size:int) = 0)
  ?commit_index:((commit_index:int) = 0)
  ?last_applied:((last_applied:int) = 0)
  ?role:((role:state_role) = Leader (default_leader_state ()))
  ?configuration:((configuration:configuration) = default_configuration ())
  () : state  = {
  id;
  current_term;
  log;
  log_size;
  commit_index;
  last_applied;
  role;
  configuration;
}

and default_state_mutable () : state_mutable = {
  id = 0;
  current_term = 0;
  log = [];
  log_size = 0;
  commit_index = 0;
  last_applied = 0;
  role = Leader (default_leader_state ());
  configuration = default_configuration ();
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

let rec decode_request_vote_request d =
  let v = default_request_vote_request_mutable () in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Varint) -> v.candidate_term <- (Pbrt.Decoder.int_as_varint d); loop ()
    | Some (2, Pbrt.Varint) -> v.candidate_id <- (Pbrt.Decoder.int_as_varint d); loop ()
    | Some (3, Pbrt.Varint) -> v.candidate_last_log_index <- (Pbrt.Decoder.int_as_varint d); loop ()
    | Some (4, Pbrt.Varint) -> v.candidate_last_log_term <- (Pbrt.Decoder.int_as_varint d); loop ()
    | Some (n, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  let v:request_vote_request = Obj.magic v in
  v

let rec decode_request_vote_response d =
  let v = default_request_vote_response_mutable () in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Varint) -> v.voter_id <- (Pbrt.Decoder.int_as_varint d); loop ()
    | Some (2, Pbrt.Varint) -> v.voter_term <- (Pbrt.Decoder.int_as_varint d); loop ()
    | Some (3, Pbrt.Varint) -> v.vote_granted <- (Pbrt.Decoder.bool d); loop ()
    | Some (n, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  let v:request_vote_response = Obj.magic v in
  v

let rec decode_log_entry d =
  let v = default_log_entry_mutable () in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Varint) -> v.index <- (Pbrt.Decoder.int_as_varint d); loop ()
    | Some (2, Pbrt.Varint) -> v.term <- (Pbrt.Decoder.int_as_varint d); loop ()
    | Some (3, Pbrt.Bytes) -> v.data <- (Pbrt.Decoder.bytes d); loop ()
    | Some (n, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  let v:log_entry = Obj.magic v in
  v

let rec decode_append_entries_request d =
  let v = default_append_entries_request_mutable () in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
      v.rev_log_entries <- List.rev v.rev_log_entries;
    )
    | Some (1, Pbrt.Varint) -> v.leader_term <- (Pbrt.Decoder.int_as_varint d); loop ()
    | Some (2, Pbrt.Varint) -> v.leader_id <- (Pbrt.Decoder.int_as_varint d); loop ()
    | Some (3, Pbrt.Varint) -> v.prev_log_index <- (Pbrt.Decoder.int_as_varint d); loop ()
    | Some (4, Pbrt.Varint) -> v.prev_log_term <- (Pbrt.Decoder.int_as_varint d); loop ()
    | Some (5, Pbrt.Bytes) -> v.rev_log_entries <- (decode_log_entry (Pbrt.Decoder.nested d)) :: v.rev_log_entries; loop ()
    | Some (6, Pbrt.Varint) -> v.leader_commit <- (Pbrt.Decoder.int_as_varint d); loop ()
    | Some (n, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  let v:append_entries_request = Obj.magic v in
  v

let rec decode_append_entries_response_success_data d =
  let v = default_append_entries_response_success_data_mutable () in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Varint) -> v.receiver_last_log_index <- (Pbrt.Decoder.int_as_varint d); loop ()
    | Some (n, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  let v:append_entries_response_success_data = Obj.magic v in
  v


let rec decode_append_entries_response d =
  let v = default_append_entries_response_mutable () in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Varint) -> v.receiver_id <- (Pbrt.Decoder.int_as_varint d); loop ()
    | Some (2, Pbrt.Varint) -> v.receiver_term <- (Pbrt.Decoder.int_as_varint d); loop ()
    | Some (3, Pbrt.Bytes) -> v.result <- Failure; Pbrt.Decoder.empty_nested d ; loop ()
    | Some (4, Pbrt.Bytes) -> v.result <- Success (decode_append_entries_response_success_data (Pbrt.Decoder.nested d)) ; loop ()
    | Some (n, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
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

let rec decode_server_index d =
  let v = default_server_index_mutable () in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Varint) -> v.server_id <- (Pbrt.Decoder.int_as_varint d); loop ()
    | Some (2, Pbrt.Varint) -> v.server_log_index <- (Pbrt.Decoder.int_as_varint d); loop ()
    | Some (n, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  let v:server_index = Obj.magic v in
  v

let rec decode_receiver_heartbeat d =
  let v = default_receiver_heartbeat_mutable () in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Varint) -> v.server_id <- (Pbrt.Decoder.int_as_varint d); loop ()
    | Some (2, Pbrt.Bits32) -> v.heartbeat_deadline <- (Pbrt.Decoder.float_as_bits32 d); loop ()
    | Some (n, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  let v:receiver_heartbeat = Obj.magic v in
  v

let rec decode_leader_state d =
  let v = default_leader_state_mutable () in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
      v.receiver_heartbeats <- List.rev v.receiver_heartbeats;
      v.match_index <- List.rev v.match_index;
      v.next_index <- List.rev v.next_index;
    )
    | Some (1, Pbrt.Bytes) -> v.next_index <- (decode_server_index (Pbrt.Decoder.nested d)) :: v.next_index; loop ()
    | Some (2, Pbrt.Bytes) -> v.match_index <- (decode_server_index (Pbrt.Decoder.nested d)) :: v.match_index; loop ()
    | Some (3, Pbrt.Bytes) -> v.receiver_heartbeats <- (decode_receiver_heartbeat (Pbrt.Decoder.nested d)) :: v.receiver_heartbeats; loop ()
    | Some (n, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  let v:leader_state = Obj.magic v in
  v

let rec decode_candidate_state d =
  let v = default_candidate_state_mutable () in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Varint) -> v.vote_count <- (Pbrt.Decoder.int_as_varint d); loop ()
    | Some (2, Pbrt.Bits64) -> v.election_deadline <- (Pbrt.Decoder.float_as_bits64 d); loop ()
    | Some (n, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  let v:candidate_state = Obj.magic v in
  v

let rec decode_follower_state d =
  let v = default_follower_state_mutable () in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Varint) -> v.voted_for <- Some (Pbrt.Decoder.int_as_varint d); loop ()
    | Some (2, Pbrt.Varint) -> v.current_leader <- Some (Pbrt.Decoder.int_as_varint d); loop ()
    | Some (3, Pbrt.Bits64) -> v.election_deadline <- (Pbrt.Decoder.float_as_bits64 d); loop ()
    | Some (n, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  let v:follower_state = Obj.magic v in
  v

let rec decode_configuration d =
  let v = default_configuration_mutable () in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Varint) -> v.nb_of_server <- (Pbrt.Decoder.int_as_varint d); loop ()
    | Some (2, Pbrt.Bits64) -> v.election_timeout <- (Pbrt.Decoder.float_as_bits64 d); loop ()
    | Some (3, Pbrt.Bits64) -> v.election_timeout_range <- (Pbrt.Decoder.float_as_bits64 d); loop ()
    | Some (4, Pbrt.Bits64) -> v.hearbeat_timeout <- (Pbrt.Decoder.float_as_bits64 d); loop ()
    | Some (5, Pbrt.Varint) -> v.max_nb_logs_per_message <- (Pbrt.Decoder.int_as_varint d); loop ()
    | Some (n, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  let v:configuration = Obj.magic v in
  v


let rec decode_state d =
  let v = default_state_mutable () in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
      v.log <- List.rev v.log;
    )
    | Some (1, Pbrt.Varint) -> v.id <- (Pbrt.Decoder.int_as_varint d); loop ()
    | Some (2, Pbrt.Varint) -> v.current_term <- (Pbrt.Decoder.int_as_varint d); loop ()
    | Some (3, Pbrt.Bytes) -> v.log <- (decode_log_entry (Pbrt.Decoder.nested d)) :: v.log; loop ()
    | Some (10, Pbrt.Varint) -> v.log_size <- (Pbrt.Decoder.int_as_varint d); loop ()
    | Some (4, Pbrt.Varint) -> v.commit_index <- (Pbrt.Decoder.int_as_varint d); loop ()
    | Some (5, Pbrt.Varint) -> v.last_applied <- (Pbrt.Decoder.int_as_varint d); loop ()
    | Some (6, Pbrt.Bytes) -> v.role <- Leader (decode_leader_state (Pbrt.Decoder.nested d)) ; loop ()
    | Some (7, Pbrt.Bytes) -> v.role <- Candidate (decode_candidate_state (Pbrt.Decoder.nested d)) ; loop ()
    | Some (8, Pbrt.Bytes) -> v.role <- Follower (decode_follower_state (Pbrt.Decoder.nested d)) ; loop ()
    | Some (9, Pbrt.Bytes) -> v.configuration <- (decode_configuration (Pbrt.Decoder.nested d)); loop ()
    | Some (n, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  let v:state = Obj.magic v in
  v

let rec decode_timeout_event_time_out_type d = 
  match Pbrt.Decoder.int_as_varint d with
  | 1 -> (New_leader_election:timeout_event_time_out_type)
  | 2 -> (Heartbeat:timeout_event_time_out_type)
  | _ -> failwith "Unknown value for enum timeout_event_time_out_type"

let rec decode_timeout_event d =
  let v = default_timeout_event_mutable () in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Bits64) -> v.timeout <- (Pbrt.Decoder.float_as_bits64 d); loop ()
    | Some (2, Pbrt.Varint) -> v.timeout_type <- (decode_timeout_event_time_out_type d); loop ()
    | Some (n, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  let v:timeout_event = Obj.magic v in
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


let rec encode_append_entries_response (v:append_entries_response) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.receiver_id encoder;
  Pbrt.Encoder.key (2, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.receiver_term encoder;
  (match v.result with
  | Failure -> (
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder;
  )
  | Success x -> (
    Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_append_entries_response_success_data x) encoder;
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

let rec encode_server_index (v:server_index) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.server_id encoder;
  Pbrt.Encoder.key (2, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.server_log_index encoder;
  ()

let rec encode_receiver_heartbeat (v:receiver_heartbeat) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.server_id encoder;
  Pbrt.Encoder.key (2, Pbrt.Bits32) encoder; 
  Pbrt.Encoder.float_as_bits32 v.heartbeat_deadline encoder;
  ()

let rec encode_leader_state (v:leader_state) encoder = 
  List.iter (fun x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_server_index x) encoder;
  ) v.next_index;
  List.iter (fun x -> 
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_server_index x) encoder;
  ) v.match_index;
  List.iter (fun x -> 
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_receiver_heartbeat x) encoder;
  ) v.receiver_heartbeats;
  ()

let rec encode_candidate_state (v:candidate_state) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.vote_count encoder;
  Pbrt.Encoder.key (2, Pbrt.Bits64) encoder; 
  Pbrt.Encoder.float_as_bits64 v.election_deadline encoder;
  ()

let rec encode_follower_state (v:follower_state) encoder = 
  (match v.voted_for with 
  | Some x -> (
    Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
    Pbrt.Encoder.int_as_varint x encoder;
  )
  | None -> ());
  (match v.current_leader with 
  | Some x -> (
    Pbrt.Encoder.key (2, Pbrt.Varint) encoder; 
    Pbrt.Encoder.int_as_varint x encoder;
  )
  | None -> ());
  Pbrt.Encoder.key (3, Pbrt.Bits64) encoder; 
  Pbrt.Encoder.float_as_bits64 v.election_deadline encoder;
  ()

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
  ()


let rec encode_state (v:state) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.id encoder;
  Pbrt.Encoder.key (2, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.current_term encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_log_entry x) encoder;
  ) v.log;
  Pbrt.Encoder.key (10, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.log_size encoder;
  Pbrt.Encoder.key (4, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.commit_index encoder;
  Pbrt.Encoder.key (5, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.last_applied encoder;
  (match v.role with
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
  );
  Pbrt.Encoder.key (9, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_configuration v.configuration) encoder;
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

let rec pp_append_entries_response_result fmt (v:append_entries_response_result) =
  match v with
  | Failure  -> Format.fprintf fmt "Failure"
  | Success x -> Format.fprintf fmt "@[Success(%a)@]" pp_append_entries_response_success_data x

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

let rec pp_server_index fmt (v:server_index) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "server_id" Pbrt.Pp.pp_int fmt v.server_id;
    Pbrt.Pp.pp_record_field "server_log_index" Pbrt.Pp.pp_int fmt v.server_log_index;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_receiver_heartbeat fmt (v:receiver_heartbeat) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "server_id" Pbrt.Pp.pp_int fmt v.server_id;
    Pbrt.Pp.pp_record_field "heartbeat_deadline" Pbrt.Pp.pp_float fmt v.heartbeat_deadline;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_leader_state fmt (v:leader_state) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "next_index" (Pbrt.Pp.pp_list pp_server_index) fmt v.next_index;
    Pbrt.Pp.pp_record_field "match_index" (Pbrt.Pp.pp_list pp_server_index) fmt v.match_index;
    Pbrt.Pp.pp_record_field "receiver_heartbeats" (Pbrt.Pp.pp_list pp_receiver_heartbeat) fmt v.receiver_heartbeats;
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

let rec pp_configuration fmt (v:configuration) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "nb_of_server" Pbrt.Pp.pp_int fmt v.nb_of_server;
    Pbrt.Pp.pp_record_field "election_timeout" Pbrt.Pp.pp_float fmt v.election_timeout;
    Pbrt.Pp.pp_record_field "election_timeout_range" Pbrt.Pp.pp_float fmt v.election_timeout_range;
    Pbrt.Pp.pp_record_field "hearbeat_timeout" Pbrt.Pp.pp_float fmt v.hearbeat_timeout;
    Pbrt.Pp.pp_record_field "max_nb_logs_per_message" Pbrt.Pp.pp_int fmt v.max_nb_logs_per_message;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_state_role fmt (v:state_role) =
  match v with
  | Leader x -> Format.fprintf fmt "@[Leader(%a)@]" pp_leader_state x
  | Candidate x -> Format.fprintf fmt "@[Candidate(%a)@]" pp_candidate_state x
  | Follower x -> Format.fprintf fmt "@[Follower(%a)@]" pp_follower_state x

and pp_state fmt (v:state) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "id" Pbrt.Pp.pp_int fmt v.id;
    Pbrt.Pp.pp_record_field "current_term" Pbrt.Pp.pp_int fmt v.current_term;
    Pbrt.Pp.pp_record_field "log" (Pbrt.Pp.pp_list pp_log_entry) fmt v.log;
    Pbrt.Pp.pp_record_field "log_size" Pbrt.Pp.pp_int fmt v.log_size;
    Pbrt.Pp.pp_record_field "commit_index" Pbrt.Pp.pp_int fmt v.commit_index;
    Pbrt.Pp.pp_record_field "last_applied" Pbrt.Pp.pp_int fmt v.last_applied;
    Pbrt.Pp.pp_record_field "role" pp_state_role fmt v.role;
    Pbrt.Pp.pp_record_field "configuration" pp_configuration fmt v.configuration;
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
