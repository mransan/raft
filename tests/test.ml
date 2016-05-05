open Raft_pb

module State     = Raft_helper.State 
module Candidate = Raft_helper.Candidate
module Follower  = Raft_helper.Follower
module Leader    = Raft_helper.Leader 
module Timeout_event = Raft_helper.Timeout_event 

module Logic     = Raft_logic  

let default_configuration = {
  nb_of_server           = 3; 
  election_timeout       = 0.1;
  election_timeout_range = 0.0; 
    (* To get deterministic number for testing.
     *) 
  hearbeat_timeout       = 0.02;
  max_nb_logs_per_message = 10;
}

let initial_state  
  ?role
  ?log:(log = [])
  ?commit_index:(commit_index = 0)
  ?configuration:(configuration = default_configuration)
  ?current_term:(current_term = 0) 
  ~now 
  id = 

  let {election_timeout;_} = configuration in 
  let role = match role with
    | None -> Follower (
      default_follower_state ~election_deadline:(now +.  election_timeout) () 
    )
    | Some x -> x 
  in 
  {
    id;
    current_term;
    log;
    log_size = List.length log;
    commit_index;
    role;
    configuration; 
  }

let assert_current_leader state expected_leader = 
  match state.role with
  | Follower {current_leader = Some id; _ } -> assert(id = expected_leader)
  | _ -> assert(false) 

let assert_no_current_leader state = 
  match state.role with
  | Follower {current_leader = None; _ } -> ()
  | _ -> assert(false) 

let assert_nb_votes state expected_vote_count= 
  match state.role with
  | Candidate {vote_count; _ } -> assert(vote_count = expected_vote_count)
  | _ -> assert(false) 

let assert_event e1 e2 = 
  assert(e1.timeout_type = e2.timeout_type);
  let diff = abs_float (e1.timeout -. e2.timeout) in 
  assert(diff < 0.00001)
  
let ipc_time = 0.001 (* 1 ms *) 

let now = 0.
  
let vote_communication ~from ~to_ ~now () = 
  let request_vote  = Logic.Request_vote.make from in 
  let now = now +. ipc_time in 
  let to_, response = Logic.Request_vote.handle_request to_ request_vote now in 
  let now = now +. ipc_time in 
  let state, response = Logic.Request_vote.handle_response from response now in 
  ((state, response, Timeout_event.next state now), to_, now)

let foo = Bytes.of_string "Foo"
let bar = Bytes.of_string "Bar"
let bim = Bytes.of_string "Bim" 
  
let append_entry_communication ~from ~to_ ~now () = 
  let from, request = Logic.Append_entries.make from 1 in
  match request with
  | Some request -> 
    (* Format.printf "[append entry] request: %a\n" pp_append_entries_request request; 
     *)
    let now = now +. 0.001 in 
    let to_, response = Logic.Append_entries.handle_request to_ request now in  
    (* Format.printf "[append entry] response: %a\n" pp_append_entries_response response; 
     *)
    let now = now +. 0.001 in 
    let state, responses = Logic.Append_entries.handle_response from response now in 
    ((state, responses, Timeout_event.next state now) , to_, response, now) 
  | None -> failwith "request expected"


let () =
  (*
   * This test verifies that when an append entries from a new 
   * leader is sent to a server which has less log than the 
   * new leader, the append entry is rejected so that the leader
   * decrements its `last_log_index` belief of that follower. 
   *
   *)

  let leader_log_version_0 = 
    {index = 3; term = 2; data = bim} ::
    {index = 2; term = 1; data = bar} :: 
    {index = 1; term = 1; data = foo} :: [] 
  in

  let server0 = 
    let current_term = 2 in 
    let log = leader_log_version_0 in 
    Leader.become (initial_state ~commit_index:3 ~log ~current_term ~now 0) now  
  in  

  let server1 = 
    let current_term = 0 in 
    initial_state ~current_term ~now 1 
  in 

  assert(0 = List.length server1.log);
  
  assert((Some 4) = Leader.next_index_for_receiver server0 1);
    (* Initially the leader belief of the next index for
     * a follower is set to its last log index  + 1. 
     *
     * This belief is indeed incorrect in our test case... 
     * (see above the log of the server1 is empty), but 
     * the next `append entry` communication will inform 
     * the leader about it.
     *)

  let (server0, msgs_to_send, next_event), server1, _, now = 
    append_entry_communication ~from:server0 ~to_:server1 ~now () in

  assert(now = 0.002); 
  assert(0 = server0.id);
  assert(2 = server0.current_term); 
  assert(leader_log_version_0 = server0.log);
  assert(3 = server0.commit_index);

  assert(1 = server1.id); 
  assert_current_leader server1 0;
  begin match server1.role with
    | Follower {voted_for = None; } -> ()
    | _ -> assert(false)
  end;
  assert(2 = server1.current_term); 
    (* server1 term was lower than the leader and should
     * therefore have updated its current term to match.
     *)

  assert(0 = List.length server1.log);
    (* The append entry should have been rejected by the 
     * follower and consequently none of the log
     * entries should have been appended by the 
     * follower
     *)

  assert(0 = server1.commit_index); 
    (* No new entries and therefore even if the 
       leader commit index is larger than the follower 
       commit index, the new commit index is following 
       the following formula:
       
       min (leader_commit, last_log_index)
         where leader_commit = 3
               last_log_index = 0
         and therefore commit_index = 0
     *)

  assert((Some 1) = Leader.next_index_for_receiver server0 1);
    (* 
     * When rejecting the [Append_entries] request from server0 [Leader], 
     * server1 sent its [receiver_last_log_index] and term in the response.
     *
     * This information is used by server0 [Leader] to update its [next_index]
     * information for that server1. 
     *)

  begin match msgs_to_send with
  | (Append_entries_request r, receiver_id)::[] -> (
    (* Another concequence of the server1 denying the 
     * append entry request, is that the leader should 
     * send another append query. 
     *)
    assert(1 = receiver_id);
    assert(0 = r.leader_id);
    assert(2 = r.leader_term);
    assert(0 = r.prev_log_index);
    assert(0 = r.prev_log_term);
    assert(3 = r.leader_commit);
    assert(3 = List.length r.rev_log_entries);
  )
  | _ -> assert(false)
  end;

  let () = 
    let expected_event = {
      timeout = default_configuration.hearbeat_timeout -. now;
      timeout_type = Heartbeat;
    } in 
    assert_event expected_event next_event
  in 

  let (server0, msgs_to_send, next_event), server1, _, now = 
    append_entry_communication ~from:server0 ~to_:server1 ~now () in
  
  assert(now = 0.004); 
  assert(msgs_to_send = []); 
  assert((Some 4) = Leader.next_index_for_receiver server0 1);
  assert((Some 3) = Leader.match_index_for_receiver server0 1);
    (* As a concequence of the server1 successfully inserting
       the 3 log entries of the leader, the leader is updating
       its next log index to [3 + 1] for that follower. 
     *)

  let () = 
    (* The leader has solely interacted with server 1 (server1) 
     * and therefore the leader is expected to send a heart beat in
     * hearbeat_timeout - now
     *)
    let expected_event = {
      timeout_type = Heartbeat; 
      timeout = default_configuration.hearbeat_timeout -. now; 
    } in 
    assert_event expected_event next_event
  in
  ()

let () = 
  (* Example *)
  
  (* Create a 3 server configuration. 
   *) 
  let configuration = Raft_pb.( {
    nb_of_server           = 3;
    election_timeout       = 0.1;
    election_timeout_range = 0.01;
    hearbeat_timeout       = 0.02;
    max_nb_logs_per_message = 10;
  }) in 

  (* Create a leader state by simulating a (rigged) election
   *)
  let leader_0 = 
    Raft_helper.Follower.create ~configuration ~now ~id:0 () 
    |> Raft_helper.Candidate.become ~now:0.0 
    |> (fun s -> Raft_helper.Leader.become s now) 
    |> Raft_helper.Leader.add_log (Bytes.of_string "Foo") 
  in 
  
  (* Create a follower
   *)
  let follower_1 = 
    Raft_helper.Follower.create ~configuration ~now ~id:1 () 
  in 

  (* First create an 'Append Entries' request from the 
     leader to server 1.
   *) 
  match Raft_logic.Append_entries.make leader_0 1 with
  | leader_0, Some request -> (

    (* 'Update' server 1 (ie follower) by applying the request. This returns
       the response to send to the leader. 
     *)
    let follower_1, response = Raft_logic.Append_entries.handle_request follower_1 request now in 

    (* 'Update' server 0 (ie leader) by applying the response. This returns
       the new state as well as a follow up action to take. 
     *)
    let _ , _ = Raft_logic.Append_entries.handle_response leader_0 response now in 

    (* Check that the follower has successfully replicated the leader single
       log
     *)
    match follower_1.log with
    | {data; _ } :: [] -> 
      if "Foo" = Bytes.to_string data
      then print_endline "Log successfully replicated in follower"
      else print_endline "Log replication was corrupted"
    | _ -> print_endline "Log replication failure"
  )
  | _, None -> () 

let () = 

  (*
   * In this test we will make sure that the RAFT
   * protocol implementation is resilient to message
   * delivery error.
   * 
   *) 
  
  (* 
   * The first test will focus on sending twice 
   * the Append Entry request. 
   *)

  let server0 = 
    Leader.become (initial_state ~current_term:1 ~now 0) now 
    |> Leader.add_log foo
  in 
  let server1 = initial_state ~current_term:1 ~now 1 in

  assert(0 = List.length server1.log);
  assert(1 = List.length server0.log);
  assert(0 = server0.commit_index);
  assert(Some (1) = Leader.next_index_for_receiver server0 1);
  assert(Some (0) = Leader.match_index_for_receiver server0 1);

  let server0 =  
    match Logic.Append_entries.make server0 1 with
    | _, None -> assert(false)
    | server0, Some request -> (
      let now = now +. 0.001 in
      
      (*
       * The request is sent twice to the server1. 
       *)
      let server1, _        = Logic.Append_entries.handle_request server1 request now in  
      let server1, response = Logic.Append_entries.handle_request server1 request now in  

      assert(1 = List.length server1.log);
      assert_current_leader server1 0; 
      begin match response.result with
      | Term_failure | Log_failure _ -> assert(false)
      | Success {receiver_last_log_index} -> assert(1 = receiver_last_log_index) 
      end; 
      server0
    )
  in

  (* 
   * In the test below the leader sends 2 requests which arrives
   * in wrong order and receiver sends 2 responses arrive in correct
   * order. 
   *
   * a) leader adds log foo
   * b) leader sends an append query (0) with log foo
   * c) leader adds log bar 
   * d) leader sends an append query (1) with log foo bar
   * e) server receives query (1) 
   * f) server sends  response (1)
   * g) server receives query (0) 
   * h) server sends  response (0) 
   * i) -> last log index must still be 2  
   *)

  
  (*Format.printf "server0: %a\n%!" pp_state server0;*)

  let _ = 
    match Logic.Append_entries.make server0 1 with
    | _, None -> assert(false)
    | server0, Some request0 -> (
      let server0 = Leader.add_log bar server0 in  
      (*Format.printf "server0: %a\n%!" pp_state server0;*)
      let server0 = 
        match Logic.Append_entries.make server0 1 with
        | _, None -> assert(false)
        | server0, Some request1 -> (

          (* Handle the request in the oposite
           * order.
           *)
          (*
          let server1, response0 = Logic.Append_entries.handle_request server1 request1 now in  
          assert(2 = List.length server1.log);
          let server1, response1 = Logic.Append_entries.handle_request server1 request0 now in  

          assert(2 = List.length server1.log);

          let server0, _ = Logic.Append_entries.handle_response server0 response0 now in 
          let server0, _ = Logic.Append_entries.handle_response server0 response1 now in 

          assert(Some (3) = Leader.next_index_for_receiver server0 1);
          assert(Some (2) = Leader.match_index_for_receiver server0 1);
          assert(2 = server0.commit_index);
          *)
          server0
        )
      in 
      server0
    );
  in

  ()

let msg_for_server msgs id = 
  match List.find (fun (_, server_id) -> id = server_id) msgs with
  | (msg, _) -> msg 
  | exception Not_found -> assert(false)

let ()  = 

  (* 
   * Verify the correct transition from follower to candidate
   * as well as the valid vote from a follower and 
   * subsequent conversion to leader by the candidate. 
   * 
   *)

  let server0 = initial_state ~now 0 in 
  let server1 = initial_state ~now 1 in 

  (*
   * Let's simulate an election timeout for server0, 
   *)
  let server0, msgs = Raft_logic.Message.handle_new_election_timeout server0 now in 
  
  assert(State.is_candidate server0); 
    (* When an election timeout happen the server should start a new election
     * and become a candidate.
     *)

  assert(1 = server0.current_term); 
    (* Since a new election was started, the [current_term] must be 
     * incremented.
     *)

  assert(2 = List.length msgs);  
  List.iter (fun (msg, _) -> 
    match msg with 
    | Request_vote_request r -> ( 
        assert(r.candidate_term = 1); 
        assert(r.candidate_id = 0);
        assert(r.candidate_last_log_index = 0);
        assert(r.candidate_last_log_term = 0); 
    ) 
    | _ -> assert(false)
  ) msgs;
    (* Upon becoming a candidate the sever0 is expected to send a `RequestVote` 
     * message to all the other servers. Since we are in a cluster 
     * of 3, 2 messages should be sent from  by the new [Candidate].
     *)

  (*
   * Let's now assume the [msg_to_server1] was correctly transmitted between 
   * server, so let's handle it.
   *)

  let now = now +. 0.001 in 

  let server1, msgs = 
    let msg = msg_for_server msgs 1 in  
    Raft_logic.Message.handle_message server1 msg now 
  in

  assert(1 = server1.current_term); 
    (* Server0 (Candidate) sent a higher term to server1 which is expected to 
     * update its own [current_term] and be a follower (which it was already). 
     *)

  begin match server1.role with
  | Follower {voted_for; current_leader; election_deadline; } -> begin 
    (* Server1 should still be a follower. 
     *)
    assert(voted_for = (Some 0)); 
    (* Because server1 did not previously vote for a candidate it should 
     * grant its vote to server0
     *) 
    assert(current_leader = None); 
    (* Granting vote should not affect server1 belief of which server is the leader. 
     *)
    assert(election_deadline = now +. default_configuration.election_timeout); 
    (* Election deadline should be updated.
     *)
  end 
  | _ -> assert(false)
  end;

  begin match msgs with
  | (msg, 0)::[]  -> 
      (* A single response to server 0 is expected from server1.
       *)
    begin match msg with
    | Request_vote_response r -> 
      assert(r.voter_id = 1); 
      assert(r.voter_term = 1); 
      assert(r.vote_granted = true); 
    | _ -> assert(false)
    end
  | _ -> assert(false) 
  end;

  (* 
   * Let's now make this response communicated to the server0 (Candidate)
   *)
  let now = now +. 0.001 in 
  let server0, msgs = 
    let msg = msg_for_server msgs 0 in 
    Raft_logic.Message.handle_message server0 msg now 
  in

  assert(State.is_leader server0); 
    (*
     * Because a single vote is enough to reach a majority, server0
     * becomes a Leader.
     *)

  assert(1 = server0.current_term); 
    (* 
     * Becoming a Leader should not affect the term. (Only 
     * a new election). 
     *)

  begin match server0.role with
  | Leader {indices; _ } -> (
    assert(2 = List.length indices); 
      (* 
       * The leader maintain various state for each of the 
       * other servers. 
       *)

      List.iter (fun {next_index; match_index; heartbeat_deadline; outstanding_request; _ } -> 
        assert(1 = next_index); 
        assert(0 = match_index); 
        assert(now +. default_configuration.hearbeat_timeout  = heartbeat_deadline); 
        assert(false = outstanding_request); 
      ) indices; 
  )
  | _ -> assert(false)
  end;

  assert(2 = List.length msgs); 
    (*
     * Upon becoming a Leader a server must immediately
     * send an [AppendEntry] request to all the other servers
     * to establish its leadership. 
     *)

  List.iter (fun (msg, _ ) -> 
    match msg with
    | Append_entries_request r -> 
      let {
        leader_term;  
        leader_id;
        prev_log_index;
        prev_log_term;
        rev_log_entries;
        leader_commit;
      } = r in 
      assert(1 = leader_term);
      assert(0 = leader_id);
      assert(0 = prev_log_index);
      assert(0 = prev_log_term);
      assert(0 = List.length rev_log_entries); 
        (*
         * We have not yet added any log to the [Leader] so
         * no new entries are sent to the other servers. 
         *)
      assert(0 = leader_commit);
    | _ -> assert(false);
  ) msgs;

  (*
   * Let's send the msg to [server1]. 
   *)
  let now = now +. 0.001 in 
  let server1, msgs = 
    let msg = msg_for_server msgs 1 in  
    Raft_logic.Message.handle_message server1 msg now 
  in

  begin match server1.role with
  | Follower {
    voted_for = Some 0; 
    current_leader = Some 0;  
    election_deadline; 
  } ->
    (*
     * We see that the follower correctly records that it has now
     * a current leader which is server0. 
     *
     * [voted_for] is still assigned to server0 since the current term
     * has not changed.  
     *)
    
    assert(election_deadline = now +. default_configuration.election_timeout)
    (*
     * Because it just receive a message from the [Leader], the
     * [election_deadline] is extended for another [election_timeout] amount
     * of time. 
     *)
  | _ -> assert(false) 
  end;

  assert(1 = List.length msgs);
    (*
     * The server1 is sending an append entry response 
     *)
    
  begin match msgs with
  | (Append_entries_response {receiver_id; receiver_term; result}, 0) :: [] -> ( 
    assert(1 = receiver_id);
    assert(1 = receiver_term); 
    assert(Success {receiver_last_log_index = 0} = result);
  )
  | _ -> assert(false)
  end;

  (*
   * Let's now start server2 and assume that it has not received
   * the previous messages from server0. 
   * 
   * It will start an election but we should see that because there
   * is already a valid leader for the term its election will fail.
   *)

  let server2 = initial_state ~now 2 in 
  let server2, request_vote_msgs = 
    Raft_logic.Message.handle_new_election_timeout server2 now 
  in

  assert(State.is_candidate server2); 
  assert(1 = server2.current_term); 
  assert(2 = List.length request_vote_msgs);

  let now = now +. 0.001 in
  let server1, msgs = 
    let msg = msg_for_server request_vote_msgs 1 in 
    Raft_logic.Message.handle_message server1 msg now 
  in 

  begin match msgs with
  | (Request_vote_response r, 2)::[] -> ( 
    let {
      voter_id;
      voter_term;
      vote_granted;
    } = r in
    assert(false = vote_granted);
      (* 
       * Server1 has already voted for server0 in this election
       * term, so it should deny its vote to server2. 
       *)
    
    assert(1 = voter_id);
    assert(1 = voter_term);
  )
  | _ -> assert(false)
  end;

  (*
   * Next we are communicating the response from server1 
   * to server2 and make sure that it does not become a [Leader]
   * but rather continue to be a Candidate. 
   *)

  let now = now +. 0.001 in 

  let server2, msgs = 
    let msg = msg_for_server msgs 2 in 
    Raft_logic.Message.handle_message server2 msg now 
  in 

  assert(State.is_candidate server2); 
    (* 
     * Despite the vote not being granted by server1, server2
     * should still be the same Candidate. 
     *) 

  assert(1 = server2.current_term);
    (* 
     * No new election should have been started. 
     *)

  assert([] = msgs);
    (* 
     * Server2 is still a candidate but for the time being 
     * has no message to send to server1. 
     *)

  (*
   * Let's now verify that server0 being the leader 
   *)
  let server0, msgs = 
    let msg = msg_for_server request_vote_msgs 0 in 
    Raft_logic.Message.handle_message server0 msg now 
  in
  
  assert(State.is_leader server0); 
    (* 
     * Server0 is still a [Leader] and should not be affected
     * by a Candidate for the same term. 
     *
     * (This would be different if the Candidate was for a later
     * term)
     *)
  
  begin match msgs with
  | (Request_vote_response r, 2) :: [] -> (

    let {
      voter_id;
      voter_term;
      vote_granted;
    } = r in 
    assert(0 = voter_id); 
    assert(1 = voter_term); 
    assert(false = vote_granted);
      (* 
       * Server0 being the [Leader] for term 1, it should 
       * not grant its vote. 
       *)
  ) 
  | _ -> assert(false)
  end;

  let now = now +. 0.002 in 

  (*
   * Let's now communicate this latter response from server0
   * to server2
   *)
  let server2, msgs = 
    let msg = msg_for_server msgs 2 in 
    Raft_logic.Message.handle_message server2 msg now 
  in

  assert(State.is_candidate server2); 
    (*
     * Yes despite all other server denying their vote, server2
     * is still a [Candidate]. It should not take any further 
     * action until its election timeout has elapsed. 
     *)
  assert([] = msgs);


  (*
   * Since the heartbeat timeout is usually much shorter 
   * than a new election timeout, it's likely that server0
   * (ie the current [Leader]) will send heartbeats to the other 
   * 2 servers. 
   *) 

  (*
   * First let's make sure that even because the heartbeat deadline
   * for any server has not been reached, a heartbeat timeout should not 
   * trigger new messages. 
   *) 

  let server0, msgs = Raft_logic.Message.handle_heartbeat_timeout server0 now in 

  assert([] = msgs); 
    (*
     * Heartbeat messages are only sent to servers which have not recveived a
     * message for at least the [hearbeat_timeout] amount of time. 
     * It's not the case since [hearbeat_timeout] is 0.02. 
     *)

  let now = now +. default_configuration.hearbeat_timeout in  

  let server0, hb_msgs = Raft_logic.Message.handle_heartbeat_timeout server0 now in 

  assert(2 = List.length hb_msgs);
    (*
     * Because we added [hearbeat_timeout] to the previous time, we know for 
     * sure that heartbeats messages are past due for all of the followers. 
     *)
  List.iter (fun (msg, _) ->
    match msg with
    | Append_entries_request r -> (
      assert(r.leader_term = 1);
      assert(r.leader_id = 0);
      assert(r.prev_log_index = 0);
      assert(r.prev_log_term = 0);
      assert(r.rev_log_entries = []);
      assert(r.leader_commit = 0);
    )
    | _ -> assert(false);
  ) hb_msgs;

  (*
   * Let's first make the heartbeat message be handled by server 2
   * which is still a candidate. 
   *
   * However upon receiving the [Append_entries] request from a [Leader]
   * with a current term at least equal or superior to itself, it should 
   * convert to a [Follower].
   *)
  let now = now +. 0.001 in 
  let server2, msgs = 
    let msg = msg_for_server hb_msgs 2 in  
    Raft_logic.Message.handle_message server2 msg now 
  in
  
  assert(State.is_follower server2); 
  assert(1 = server2.current_term);
    (* 
     * Receiving an [Append_entries] request with a term at least equal
     * or supperior to one [current_term] means that the sender is a valid
     * [Leader] for that term and therefore the recipient must become a 
     * [Follower].
     *)

  begin match server2.role with
  | Follower fs -> (
    assert(fs.voted_for = Some 2);
      (* 
       * For that term, since this server was also a candidate it 
       * did already vote for itself. 
       *)
    assert(fs.current_leader = Some 0);
      (* 
       * server2 is now aware that server0 is the [Leader] for 
       * term 1
       *)
    assert(fs.election_deadline = now +.  default_configuration.election_timeout); 
  )
  | _ -> assert(false);
  end;

  assert(1 = List.length msgs);
    (* 
     * Response for the [Append_entries]
     *)
  begin match msgs with 
  | ((Append_entries_response r), server_id) :: []  -> (
    assert(server_id = 0); 
    assert(r.receiver_id = 2);
    assert(r.receiver_term = 1);
    assert(r.result = Success { receiver_last_log_index = 0 });
  ) 
  | _ -> assert(false)
  end;

  let server0, msgs = 
    let msg = msg_for_server msgs 0 in 
    Raft_logic.Message.handle_message server0 msg now 
  in  

  (* 
   * Server1 was already a follower and aware of server0 leadership
   * the new heartbeat would not change that. 
   *)

  let server1, msgs = 
    let msg =  msg_for_server hb_msgs 1 in 
    Raft_logic.Message.handle_message server1 msg now
  in 

  assert(State.is_follower server1); 
   (* No change in the role, server0 is a valid [Leader], 
    * server1 stays a [Follower]. 
    *)

  assert(1 = List.length msgs);
   (* Single [Append_entries_response] expected. 
    *)

  begin match msgs with
  | (Append_entries_response r, 0)::[] -> (
    assert(r.receiver_id = 1);
    assert(r.receiver_term = 1);
    assert(r.result = Success { receiver_last_log_index = 0 });
  )
  | _ -> assert(false)
  end;
  
  let now = now +. 0.001 in 

  (* 
   * Let's handle the response from server1 in server0
   *
   * Note that it's important for the rest of the test that
   * we explicitely handle the response from server1 in server0. 
   * Each [Leader] is keeping track of whether or not there is an 
   * [outstanding_request] for each server. This later affect
   * the behavior of the [Leader] upon add a log entry. 
   *
   *)
  let server0, msgs = 
    let msg = msg_for_server msgs 0 in 
    Raft_logic.Message.handle_message server0 msg now 
  in  

  assert(State.is_leader server0); 
  
  let now = now +. 0.001 in 

  (*
   * Let's now add a log entry to the [Leader] and trigger
   * the corresponding [Append_entry] request to the other servers. 
   *)

  let new_log_response = 
    let data = Bytes.of_string "Message1" in 
    Raft_logic.Message.handle_add_log_entries server0 [data] now 
  in

  let server0, data1_msgs = 
    let open Raft_logic.Message in 
    match new_log_response with
    | Appended (state, msgs) -> (state, msgs) 
      (* 
       * server0 is the [Leader] and is therefore expected to 
       * handle the new log entry. 
       *)

    | Delay | Forward_to_leader _ -> assert(false)
  in 

  assert(1 = List.length server0.log);
    (* 
     * The new log entry should have been created
     *)
  
    begin match server0.log with
  | {index = 1; term = 1; _ } :: []  -> ()
    (* 
     * Log should start as 1 and be set to the current 
     * term (ie 1.)
     *)

  | _ -> assert(false)
  end;

  assert(2 = List.length data1_msgs);
    (* 
     * Both follower have no outstanding request and have also less 
     * log entries than the [Leader], therefore they 
     * should get a new [Append_entries_request] message with the new 
     * log
     *)

  List.iter (fun (msg, _) -> 
    match msg with
    | Append_entries_request r -> (
      assert(r.leader_term = 1);
      assert(r.leader_id = 0);
      assert(r.prev_log_index = 0);
      assert(r.prev_log_term = 0); 
      assert(1 = List.length r.rev_log_entries); 
        (* 
         * Contains the log entry to be synchronized.
         *)
      begin match r.rev_log_entries with
      | {index = 1; term =1; _} :: [] -> ()
      | _ -> assert(false)
      end;
      assert(r.leader_commit = 0);
    )
    | _ -> assert(false)
  ) data1_msgs;  

  let now = now +. 0.001 in 

  (*
   * Let's send the [Append_entries_request] with a single 
   * [log_entry] inside to server1. 
   *)

  let server1, msgs = 
    let msg = msg_for_server data1_msgs 1 in 
    Raft_logic.Message.handle_message server1 msg now 
  in

  assert(State.is_follower server1); 
    (* 
     * No change of role for server1, [Append_entries] only 
     * re-inforce that server0 is the [Leader]. 
     *)

  assert(1 = List.length server1.log); 
    (* 
     * The [log_entry] with index 1 has been replicated 
     * on server1. 
     *)

  assert(0 = server1.commit_index); 
    (* 
     * While server1 has successfully replicated the log entry 
     * with [index = 1], it cannot assume that this latter log 
     * entry has been commited (ie that it has been replicated on 
     * a majority of servers). 
     *
     * Therefore the [commit_index] is still 0. 
     *
     * It will be updated upon receiving the next [Append_entries_request]
     *)

  begin match msgs with
  | (Append_entries_response r, 0) :: [] -> (
    assert(r.receiver_id = 1);
    assert(r.receiver_term = 1);
    assert(r.result = Success { receiver_last_log_index = 1; });  
     (* 
      * server1 has correctly replicated the log entry which has index1. 
      *)
  ) 
  | _ -> assert(false)
  end;

  (* 
   * Let's handle this succesfully response in server0 [Leader].
   *)

  let server0, msgs = 
    let msg = msg_for_server msgs 0  in 
    Raft_logic.Message.handle_message server0 msg now 
  in 

  assert(State.is_leader server0); 
  assert(1 = server0.commit_index); 
    (* 
     * server1 has replicated the log successfully so this means
     * that a majority of servers have done the replication. 
     * The [Leader] commit_index can now be updated to that latest 
     * log index (ie 1). 
     *
     *)
  
  (* 
   * On purpose we are not propagating the [Append_entries_request] from
   * server0 to server2 which contains the log entry [1]. 
   *
   * This is to test that:
   * - On the next log entry being added to [server0] no message should 
   *   be sent to server2 which still has an outstanding request 
   * - On the next heartbeat timeout server2 should then receive an 
   *   [Append_entries_request] with both log entry. 
   *
   *)

  let now = now +. 0.001 in 

  let new_log_response = 
    let data = Bytes.of_string "Message2" in 
    Raft_logic.Message.handle_add_log_entries server0 [data] now
  in

  let server0, data2_msg = 
    let open Raft_logic.Message in 
    match new_log_response with
    | Delay | Forward_to_leader _ -> assert(false) 
    | Appended (x, y) -> (x, y)
  in

  assert(State.is_leader server0); 
  
  assert(2 = List.length server0.log); 
    (* 
     * The second log entry should have been appended to the 
     * server log. 
     *) 

  begin match List.hd server0.log with 
  | {index = 2; term = 1; _ }  -> () 
    (* 
     * Make sure the index is incremented by 
     * 1.
     *)
  | _ -> assert(false) 
  end;

  assert(1 = server0.commit_index); 
    (* 
     * The new log entry (with index [2]) should not be commited
     * since no request/response interaction has yet been done. 
     *)

  assert(1 = List.length data2_msg); 
    (* 
     * Since server2 has an outstanding request, it should not 
     * be sent an additional request. 
     *
     *)

  begin match data2_msg with 
  | (Append_entries_request r, 1) :: []  -> (
    assert(r.leader_term = 1);
    assert(r.leader_id = 0);
    assert(r.prev_log_index = 1); 
      (* 
       * server0 [Leader] knowns that the server1 has successfully
       * inserver the log entry with [index = 1] and therefore 
       * will send [prev_log_index] with value 1.
       *)
    assert(r.prev_log_term = 1); 
    assert(1 = List.length r.rev_log_entries);  
    assert(r.leader_commit = 1); 
  ) 
  | _ -> assert(false)
  end;

  let now = now +. 0.001 in 

  let server1, msgs = 
    let msg = msg_for_server data2_msg 1 in 
    Raft_logic.Message.handle_message server1 msg now 
  in 

  assert(State.is_follower server1); 

  assert(2 = List.length server1.log); 
    (* 
     * server1 should have correctly replicated the log 
     * with [index = 2]. 
     *)

  assert(1 = server1.commit_index); 
    (* 
     * The [Append_entries_request] contained the [commit_index] 
     * of the [Leader] and therefore server1 has 
     * updated its own. 
     *)

  assert(1 = List.length msgs); 
    (* 
     * Only a single response to server0 should be 
     * sent back. 
     *)

  begin match msgs with
  | (Append_entries_response r, 0) :: [] -> (
    assert(r.receiver_id  = 1);
    assert(r.receiver_term = 1); 
    assert(r.result = Success {receiver_last_log_index = 2; }); 
  ) 
  | _ -> assert(false) 
  end;

  let now = now +. 0.001 in 

  let server0, msgs = 
    let msg = msg_for_server msgs 0  in 
    Raft_logic.Message.handle_message server0 msg now 
  in

  assert(State.is_leader server0); 

  assert(2 = server0.commit_index); 
    (* 
     * A successfull replication is enough for a majority. 
     *)

  
  (* 
   * Let's now look at what happens after a heartbeat timeout. 
   *
   * For server1, the [Leader] should only sent an empty [Append_entries_request]
   * since it knows that server1 has successfully replicated all of its logs. 
   *
   * For server2 however, none of the log have been replicated. In fact as far 
   * as server0 [Leader] is concerned there is still an outstanding request. Realistically
   * this request could either have been lost or server2 or the response could have never been 
   * received by server0. In either case (right now it's the first one) RAFT should work!
   *
   *)

  let now = now +. default_configuration.hearbeat_timeout in 

  let server0, msgs = Raft_logic.Message.handle_heartbeat_timeout server0 now in 

  assert(2 = List.length msgs); 

  begin match msg_for_server msgs 1 with
  | Append_entries_request r -> (
    assert(r.leader_term = 1); 
    assert(r.leader_id = 0); 
    assert(r.prev_log_term = 1); 
    assert(r.rev_log_entries = []);
      (*
       * As expected no new log entry should be sent
       * since they all have been previously and [server1]
       * replied successfully. 
       *)
    
    assert(r.prev_log_index = 2); 
      (*
       * Confirmation of the above statement, server0 [Leader]
       * knows that log with [index = 2] has been replicated. 
       *)
    assert(r.leader_commit = 2);
  ) 
  | _ -> assert(false)
  end;

  begin match msg_for_server msgs 2 with
  | Append_entries_request r -> (
    assert(r.leader_term = 1); 
    assert(r.leader_id = 0); 
    assert(r.prev_log_term = 0); 
    assert(List.length r.rev_log_entries = 1);
      (*
       * server2 has nevery replied to serve0 [Leader]. 
       * TODO Explain Caching
       *)
    
    assert(r.prev_log_index = 0); 
      (*
       * Confirmation of the above statement, server0 [Leader]
       * has never received confirmation that server2 has replicated 
       * the data. 
       *)
    assert(r.leader_commit = 2);
  ) 
  | _ -> assert(false)
  end;

  let now =  now +. 0.001 in 

  let server1, server1_response = 
    let msg = msg_for_server msgs 1 in
    Raft_logic.Message.handle_message server1 msg now 
  in 
  
  assert(State.is_follower server1); 
  assert(2 = server1.commit_index); 
   (* 
    * server1 is updating its commit index based on latest 
    * [leader_commit] value of 2 in the request it received. 
    *)

  assert(1 = List.length server1_response); 
   (* 
    * Only a single response is expected.
    *)
  
  let server2, server2_response = 
    let msg = msg_for_server msgs 2 in
    Raft_logic.Message.handle_message server2 msg now 
  in 
  
  assert(State.is_follower server2); 
  assert(1 = server2.commit_index); 
   (* 
    * server2 is updating its commit index based on latest 
    * [leader_commit] value of 2 in the request it received. 
    *
    * However it has not replicated the second log due to the cache
    * mechanism
    *)

  assert(1 = List.length server2.log); 
   (* 
    * server2 should have caught up with server0 and replicated
    * all the logs in the cache (ie 1)
    *)
  
  assert(1 = List.length server2_response); 
   (* 
    * Only a single response is expected.
    *)

  begin match server2_response with
  | (Append_entries_response r, 0)::[] -> (
    assert(r.receiver_id = 2); 
    assert(r.receiver_term = 1); 
    assert(r.result = Success {receiver_last_log_index = 1; }); 
      (* 
       * server2 has successfully replicated the 1 log entries 
       *) 
  ) 
  | _ -> assert(false)
  end;

  let server0, msg_to_send = 

    let server0, msg_to_send = 
      let msg = msg_for_server server1_response 0  in 
      Raft_logic.Message.handle_message server0 msg now 
    in 
    assert([] = msg_to_send); 
      (* Server1 has replicated the 2 logs it has nothing 
       * left. 
       *)
    
    let msg = msg_for_server server2_response 0  in 
    Raft_logic.Message.handle_message server0 msg now 
  in 

  assert(State.is_leader server0); 
  assert(2 = server0.commit_index); 
  assert(2 = List.length server0.log);

  (* 
   * Because server2 has only one log replicated, the [Leader]
   * must now send the remaining log. 
   *)
  assert(1 = List.length msg_to_send); 

  begin match msg_to_send with
  | (Append_entries_request r, 2)::[] -> (
    assert(r.leader_term = 1);
    assert(r.leader_id = 0);
    assert(r.prev_log_index = 1);
    assert(r.prev_log_term = 1);
    assert(1 = List.length r.rev_log_entries);
    assert(r.leader_commit  = 2);
  ) 
  | _ -> assert(false)
  end;

  let server2, msg_to_send = 
    let msg = msg_for_server msg_to_send 2 in 
    Raft_logic.Message.handle_message server2 msg now 
  in 

  assert(State.is_follower server2); 
  assert(2 = List.length server2.log);
    (* server2 has correctly replicated the [log_entry] with
     * [index = 2]. 
     *)

  assert(2 = server2.commit_index); 
    (* This time the commit_index is 2 since both [log_entry] with
     * [index = 2] has been replicated and the [leader_commit] is equal
     * to 2. 
     *)

  assert(1 = List.length msg_to_send); 
    (* [Append_entries] response for server0. 
     *)
  begin match msg_to_send with
  | (Append_entries_response r, 0)::[] -> (
    assert(r.receiver_id = 2); 
    assert(r.receiver_term = 1);
    assert(r.result = Success {receiver_last_log_index = 2}); 
  ) 
  | _ -> assert(false)
  end;

  let server0, msgs = 
    let msg = msg_for_server msg_to_send 0  in 
    Raft_logic.Message.handle_message server0 msg now
  in 
  
  assert([] = msgs); 
  assert(State.is_leader server0); 
  assert(2 = server0.commit_index); 

  (* 
   * Let's now add a 3rd [log_entry] to the [Leader]. 
   *
   * We'll replicate this 3rd entry on [server1] only and 
   * then simulate a [Leader] crash. 
   *
   * The consequence of a [Leader] crash will be that one of the 
   * follower will start a new election. 
   * However only [server1] should become a [Leader] since it has replicated
   * more [log_entry]s than [server2]. 
   *
   * We will simulate and test the above assumption.
   *)

  (* 
   * Let's add 3rd [log_entry]. 
   *)

  let now = now +. 0.002 in 

  let new_log_response = 
    let data = Bytes.of_string "Message3" in 
    Raft_logic.Message.handle_add_log_entries server0 [data] now 
  in 

  let server0, msgs = 
    match new_log_response with
    | Raft_logic.Message.Appended (server, msgs) -> (server, msgs)
    | _ -> assert(false)
  in 
  assert(State.is_leader server0); 
  assert(3 = List.length server0.log); 
    (* 
     * Correctly added log since server0 is a [Leader]. 
     *)

  assert(2 = List.length msgs); 
    (* 
     * 2 [Append_entries] requests, one for each of the other
     * 2 servers. 
     *)

  (*
   * Let's now replicate the log on server1 ONLY. 
   *)

  let server1, _ = 
    let msg = msg_for_server msgs 1 in 
    Raft_logic.Message.handle_message server1 msg now 
  in 

  assert(State.is_follower server1); 
  assert(3 = List.length server1.log); 
    (* 
     * The 3rd [log_entry] is correctly replicated. 
     *) 

  assert(2 = server1.commit_index);
    (* 
     * No change since the [commit_index] was still 
     * 2 in the [Append_entries] request. 
     *)

  (*
   * Now let's assume server0 [Leader] has crashed. 
   *
   * We'll make [server2] starts a new election!
   *)

  let now = now +. default_configuration.election_timeout  in

  let server2, msgs = 
    Raft_logic.Message.handle_new_election_timeout server2 now 
  in 

  assert(State.is_candidate server2); 
    (* 
     * Server2 started a new election.
     *)

  assert(2 = server2.current_term); 
    (* 
     * Each new election increments the term
     *)
  
  assert(2 = List.length msgs); 
    (* 
     * 2 [Request_vote] request for each of the other 2 
     * servers. 
     *)

  List.iter (fun (r, _) -> 
    match r with
    | Request_vote_request r -> (
      assert(r.candidate_term  = 2);
      assert(r.candidate_id = 2);
      assert(r.candidate_last_log_index = 2); 
      assert(r.candidate_last_log_term = 1);
    ) 
    | _ -> assert(false)
  ) msgs; 

  let now = now +. 0.001 in 

  let server1, msgs = 
    let msg = msg_for_server msgs 1 in 
    Raft_logic.Message.handle_message server1 msg now 
  in 

  assert(State.is_follower server1); 
  assert(2 = server1.current_term); 
    (* 
     * The sender (ie server2) term was greater than the current
     * term of server1, it should then 
     * increments its current term. 
     *) 

  assert(1 = List.length msgs); 
    (* 
     * [Request_vote] response to server2.
     *)

  begin match msgs with
  | (Request_vote_response r, 2)::[] -> (
    assert(r.voter_id = 1);
    assert(r.voter_term = 2); 
    assert(r.vote_granted = false); 
     (* 
      * [server1] has more [log_entry] than [server2] and therefore 
      * rejects [server2] candidacy. 
      *
      * This is to ensure the safety property of the RAFT protocol
      * so that no commited entries are later invalidated.
      *)
  ) 
  | _  -> assert(false)
  end;

  
  (* 
   * Let's now have server1 starts a new election. 
   *) 

  let now = now +. default_configuration.election_timeout in 

  let server1, msgs = 
    Raft_logic.Message.handle_new_election_timeout server1 now 
  in 

  assert(State.is_candidate server1); 
  assert(3 = server1.current_term);
  assert(2 =  List.length msgs);

  List.iter (fun (r, _) -> 
    begin match r with
    | Request_vote_request r -> (
      assert(r.candidate_term = 3);
      assert(r.candidate_id = 1);
      assert(r.candidate_last_log_index =3);
      assert(r.candidate_last_log_term = 1);
    ) 
    | _ -> assert(false)
    end
  ) msgs; 

  let server2, msgs = 
    let msg = msg_for_server msgs 2 in 
    Raft_logic.Message.handle_message server2 msg now 
  in 

  assert(State.is_follower server2); 
    (* 
     * server1 [current_term] is greater than the one
     * in server2 (3 versus 2). 
     *
     * Therefore server2 becomes a [Follower] and update 
     * its current_term. 
     *)

  assert(3 = server2.current_term);

  assert(1 = List.length msgs); 

  begin match msgs with
  | (Request_vote_response r, 1)::[] -> (
    assert(r.voter_id = 2);
    assert(r.voter_term = 3);
    assert(r.vote_granted = true);
      (* 
       * Vote is indeed granted since server1 has a greater 
       * [last_log_index].
       *)
  ) 
  | _ -> assert(false)
  end;

  (*
   *  TODO:
   *  - Handle the vote response by server1 and check that it becomes
   *  the new [Leader]. 
   *  - Make sure that the [Append_entries] request sent upon
   *  becoming a [Leader] by server1 to server2 contains no [log_entry]
   *  since [server1] will by default assume that the other servers have replicated
   *  the same [log_entry] as it did. 
   *  - Verify that then server2 denies this latest [Append_entries]. 
   *  - Verify that then server1 sents the new (and corrected) [Append_entries] 
   *    with the [log_entry] which was not replicated on server2 before. (ie the 3rd one). 
   *)
  ()
