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
