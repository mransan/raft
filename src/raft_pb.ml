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
  log_entries : log_entry list;
  leader_commit : int;
}

and append_entries_request_mutable = {
  mutable leader_term : int;
  mutable leader_id : int;
  mutable prev_log_index : int;
  mutable prev_log_term : int;
  mutable log_entries : log_entry list;
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

type server_index = {
  server_id : int;
  server_log_index : int;
}

and server_index_mutable = {
  mutable server_id : int;
  mutable server_log_index : int;
}

type leader_state = {
  next_index : server_index list;
  match_index : server_index list;
}

and leader_state_mutable = {
  mutable next_index : server_index list;
  mutable match_index : server_index list;
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
}

and follower_state_mutable = {
  mutable voted_for : int option;
}

type configuration = {
  nb_of_server : int;
  election_timeout : float;
}

and configuration_mutable = {
  mutable nb_of_server : int;
  mutable election_timeout : float;
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

and state_mutable = {
  mutable id : int;
  mutable current_term : int;
  mutable log : log_entry list;
  mutable commit_index : int;
  mutable last_applied : int;
  mutable role : state_role;
  mutable configuration : configuration;
}

type follow_up_action_retry_append_data = {
  server_id : int;
}

and follow_up_action_retry_append_data_mutable = {
  mutable server_id : int;
}

type follow_up_action =
  | Act_as_new_leader
  | Start_new_election
  | Nothing_to_do
  | Retry_append of follow_up_action_retry_append_data

let rec default_request_vote_request () : request_vote_request = {
  candidate_term = 0;
  candidate_id = 0;
  candidate_last_log_index = 0;
  candidate_last_log_term = 0;
}

and default_request_vote_request_mutable () : request_vote_request_mutable = {
  candidate_term = 0;
  candidate_id = 0;
  candidate_last_log_index = 0;
  candidate_last_log_term = 0;
}

let rec default_request_vote_response () : request_vote_response = {
  voter_id = 0;
  voter_term = 0;
  vote_granted = false;
}

and default_request_vote_response_mutable () : request_vote_response_mutable = {
  voter_id = 0;
  voter_term = 0;
  vote_granted = false;
}

let rec default_log_entry () : log_entry = {
  index = 0;
  term = 0;
  data = Bytes.create 64;
}

and default_log_entry_mutable () : log_entry_mutable = {
  index = 0;
  term = 0;
  data = Bytes.create 64;
}

let rec default_append_entries_request () : append_entries_request = {
  leader_term = 0;
  leader_id = 0;
  prev_log_index = 0;
  prev_log_term = 0;
  log_entries = [];
  leader_commit = 0;
}

and default_append_entries_request_mutable () : append_entries_request_mutable = {
  leader_term = 0;
  leader_id = 0;
  prev_log_index = 0;
  prev_log_term = 0;
  log_entries = [];
  leader_commit = 0;
}

let rec default_append_entries_response_success_data () : append_entries_response_success_data = {
  receiver_last_log_index = 0;
}

and default_append_entries_response_success_data_mutable () : append_entries_response_success_data_mutable = {
  receiver_last_log_index = 0;
}


let rec default_append_entries_response () : append_entries_response = {
  receiver_id = 0;
  receiver_term = 0;
  result = Failure;
}

and default_append_entries_response_mutable () : append_entries_response_mutable = {
  receiver_id = 0;
  receiver_term = 0;
  result = Failure;
}

let rec default_server_index () : server_index = {
  server_id = 0;
  server_log_index = 0;
}

and default_server_index_mutable () : server_index_mutable = {
  server_id = 0;
  server_log_index = 0;
}

let rec default_leader_state () : leader_state = {
  next_index = [];
  match_index = [];
}

and default_leader_state_mutable () : leader_state_mutable = {
  next_index = [];
  match_index = [];
}

let rec default_candidate_state () : candidate_state = {
  vote_count = 0;
  election_deadline = 0.;
}

and default_candidate_state_mutable () : candidate_state_mutable = {
  vote_count = 0;
  election_deadline = 0.;
}

let rec default_follower_state () : follower_state = {
  voted_for = None;
}

and default_follower_state_mutable () : follower_state_mutable = {
  voted_for = None;
}

let rec default_configuration () : configuration = {
  nb_of_server = 0;
  election_timeout = 0.;
}

and default_configuration_mutable () : configuration_mutable = {
  nb_of_server = 0;
  election_timeout = 0.;
}


let rec default_state () : state = {
  id = 0;
  current_term = 0;
  log = [];
  commit_index = 0;
  last_applied = 0;
  role = Leader (default_leader_state ());
  configuration = default_configuration ();
}

and default_state_mutable () : state_mutable = {
  id = 0;
  current_term = 0;
  log = [];
  commit_index = 0;
  last_applied = 0;
  role = Leader (default_leader_state ());
  configuration = default_configuration ();
}

let rec default_follow_up_action_retry_append_data () : follow_up_action_retry_append_data = {
  server_id = 0;
}

and default_follow_up_action_retry_append_data_mutable () : follow_up_action_retry_append_data_mutable = {
  server_id = 0;
}

let rec default_follow_up_action () : follow_up_action = Act_as_new_leader

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
      v.log_entries <- List.rev v.log_entries;
    )
    | Some (1, Pbrt.Varint) -> v.leader_term <- (Pbrt.Decoder.int_as_varint d); loop ()
    | Some (2, Pbrt.Varint) -> v.leader_id <- (Pbrt.Decoder.int_as_varint d); loop ()
    | Some (3, Pbrt.Varint) -> v.prev_log_index <- (Pbrt.Decoder.int_as_varint d); loop ()
    | Some (4, Pbrt.Varint) -> v.prev_log_term <- (Pbrt.Decoder.int_as_varint d); loop ()
    | Some (5, Pbrt.Bytes) -> v.log_entries <- (decode_log_entry (Pbrt.Decoder.nested d)) :: v.log_entries; loop ()
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

let rec decode_leader_state d =
  let v = default_leader_state_mutable () in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
      v.match_index <- List.rev v.match_index;
      v.next_index <- List.rev v.next_index;
    )
    | Some (1, Pbrt.Bytes) -> v.next_index <- (decode_server_index (Pbrt.Decoder.nested d)) :: v.next_index; loop ()
    | Some (2, Pbrt.Bytes) -> v.match_index <- (decode_server_index (Pbrt.Decoder.nested d)) :: v.match_index; loop ()
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

let rec decode_follow_up_action_retry_append_data d =
  let v = default_follow_up_action_retry_append_data_mutable () in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Varint) -> v.server_id <- (Pbrt.Decoder.int_as_varint d); loop ()
    | Some (n, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  let v:follow_up_action_retry_append_data = Obj.magic v in
  v

let rec decode_follow_up_action d = 
  let rec loop () = 
    let ret:follow_up_action = match Pbrt.Decoder.key d with
      | None -> failwith "None of the known key is found"
      | Some (1, _) -> (Pbrt.Decoder.empty_nested d ; Act_as_new_leader)
      | Some (2, _) -> (Pbrt.Decoder.empty_nested d ; Start_new_election)
      | Some (3, _) -> (Pbrt.Decoder.empty_nested d ; Nothing_to_do)
      | Some (4, _) -> Retry_append (decode_follow_up_action_retry_append_data (Pbrt.Decoder.nested d))
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

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
  ) v.log_entries;
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

let rec encode_server_index (v:server_index) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.server_id encoder;
  Pbrt.Encoder.key (2, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.server_log_index encoder;
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
  ()

let rec encode_configuration (v:configuration) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.nb_of_server encoder;
  Pbrt.Encoder.key (2, Pbrt.Bits64) encoder; 
  Pbrt.Encoder.float_as_bits64 v.election_timeout encoder;
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

let rec encode_follow_up_action_retry_append_data (v:follow_up_action_retry_append_data) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.server_id encoder;
  ()

let rec encode_follow_up_action (v:follow_up_action) encoder = 
  match v with
  | Act_as_new_leader -> (
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder;
  )
  | Start_new_election -> (
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder;
  )
  | Nothing_to_do -> (
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder;
  )
  | Retry_append x -> (
    Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_follow_up_action_retry_append_data x) encoder;
  )

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
    Pbrt.Pp.pp_record_field "log_entries" (Pbrt.Pp.pp_list pp_log_entry) fmt v.log_entries;
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

let rec pp_server_index fmt (v:server_index) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "server_id" Pbrt.Pp.pp_int fmt v.server_id;
    Pbrt.Pp.pp_record_field "server_log_index" Pbrt.Pp.pp_int fmt v.server_log_index;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_leader_state fmt (v:leader_state) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "next_index" (Pbrt.Pp.pp_list pp_server_index) fmt v.next_index;
    Pbrt.Pp.pp_record_field "match_index" (Pbrt.Pp.pp_list pp_server_index) fmt v.match_index;
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
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_configuration fmt (v:configuration) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "nb_of_server" Pbrt.Pp.pp_int fmt v.nb_of_server;
    Pbrt.Pp.pp_record_field "election_timeout" Pbrt.Pp.pp_float fmt v.election_timeout;
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
    Pbrt.Pp.pp_record_field "commit_index" Pbrt.Pp.pp_int fmt v.commit_index;
    Pbrt.Pp.pp_record_field "last_applied" Pbrt.Pp.pp_int fmt v.last_applied;
    Pbrt.Pp.pp_record_field "role" pp_state_role fmt v.role;
    Pbrt.Pp.pp_record_field "configuration" pp_configuration fmt v.configuration;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_follow_up_action_retry_append_data fmt (v:follow_up_action_retry_append_data) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "server_id" Pbrt.Pp.pp_int fmt v.server_id;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_follow_up_action fmt (v:follow_up_action) =
  match v with
  | Act_as_new_leader  -> Format.fprintf fmt "Act_as_new_leader"
  | Start_new_election  -> Format.fprintf fmt "Start_new_election"
  | Nothing_to_do  -> Format.fprintf fmt "Nothing_to_do"
  | Retry_append x -> Format.fprintf fmt "@[Retry_append(%a)@]" pp_follow_up_action_retry_append_data x
