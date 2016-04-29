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

let () = 
  (* 
   * Verify the correct transition from follower to candidate
   * as well as the valid vote from a follower and 
   * subsequent conversion to leader by the candidate. 
   * 
   *)

  (* 
   * Convert follower to candidate 
   *)

  let server0 = Candidate.become ~now (initial_state ~now 0)  in 
  
  begin match server0.role with
    | Candidate {vote_count; election_deadline} -> (
      assert(1 = vote_count); 
      assert(0.1 = election_deadline);
       (* When converting to candidate, the server automatically 
        * vote for itself.
        *)
    )
    | _ -> assert(false)
  end; 
  assert (State.is_candidate server0);
  assert (1 = server0.current_term);
    (* Make sure that any new election increments the 
     * current term by 1.
     *)
  assert (0 = server0.id);
  assert ([] = server0.log);
  assert (0 = server0.log_size);

  let server1  = initial_state ~now 1 in 

  assert (State.is_follower server1);
  begin match server1.role with
    | Follower {voted_for = None} -> ()
    | _ -> assert(false)
  end;
  assert (1 = server1.id);
  assert (0 = server1.current_term);
  assert ([] = server1.log);
  assert (0 = server1.log_size);
  
  (* 
   * Vote for the Candidate
   * Convert from Candidate to Leader after getting majority
   *)
  
  let (server0, msgs_to_send, next_event), server1, now 
    = vote_communication ~from:server0 ~to_:server1 ~now () in 

  assert(now = 0.002);
  
  assert (State.is_follower server1);
  begin match server1.role with
    | Follower {voted_for = Some 0} -> ()
    | _ -> assert(false)
  end;
  assert (1 = server1.id);
  assert (1 = server1.current_term);
    (* Make sure that the follower has correctly updated
     * its current term to the one of the candidate since
     * the latter one is larger.
     *)
  assert ([] = server1.log);
  
  begin match server0.role with
  | Leader {next_index; match_index} -> (
    assert(2 = List.length next_index); 
    List.iter (fun {server_log_index; _ } -> 
      assert (1 = server_log_index); 
        (* The initial value for all server next log index 
         * should be equal to the last log index + 1. 
         *)
    ) next_index;
    assert(2 = List.length match_index); 
    List.iter (fun {server_log_index; _ } -> 
      assert (0 = server_log_index); 
    ) match_index;
  )
  | _ -> assert(false)
  end; 
  assert (1 = server0.current_term);
    (* Becoming a leader should not alter 
     * the term. 
     *)
  assert (0 = server0.id);
  let () =
    let expected_event = {
      timeout = default_configuration.hearbeat_timeout; 
      timeout_type  = Heartbeat; 
    } in 
    assert_event expected_event next_event
  in
  assert(2 = List.length msgs_to_send);

  (* 
   * Let's assume here that the new leader i s
   * correctly sending Append Query request
   * to all the server and that this operation
   * takes 1ms
   *)
  
  let now = now +. 0.001 in 
  assert(now = 0.003); 

  (*
   * We now check that the next next action
   * is to wait on Rpc with the heartbeat timeout 
   * set to the value in the configuration - 1ms elapsed
   * since becoming a leader.
   *)
  
  let next_event = Timeout_event.next server0  now in 
  assert_event {
    timeout = default_configuration.hearbeat_timeout -. 0.001;
    timeout_type = Heartbeat;
  } next_event;

  (* 
  Format.printf "server0': %a\n" pp_state server0; 
  Format.printf "server1' : %a\n" pp_state server1; 
  Format.printf "followup action: %a\n" pp_next_event next_event; 
  *)
  ()

let () = 

  (* 
   * This test verifies that when a follower has previously voted 
   * for a candidate it correctly denies its vote to a subsequent
   * candidate within that same term. 
   *)

  let server0  = initial_state ~now 0 in 
  let candidate1 = Candidate.become ~now (initial_state ~now 1) in
  let candidate2 = Candidate.become ~now (initial_state ~now 2) in

  assert(1 = candidate1.id);
  assert(2 = candidate2.id); 
  assert(candidate1.current_term = candidate2.current_term);

  (*
   * Let's perform the vote request from canidate 1 to 
   * server0 first. Since server0 has not 
   * voted before and canidate1 is a valid candidate, 
   * server0 must grant its vote. 
   *)

  let (candidate1, msgs_to_send, next_event1), server0, now  
    = vote_communication ~from:candidate1 ~to_:server0 ~now () in 

  assert(now = 0.002); 

  (* 
   * Let's make sure that the server0 has correctly 
   * voted for server 1. 
   *)

  begin match server0.role with
    | Follower {voted_for = Some 1} -> () 
    | _ -> assert(false)
  end;
  
  (* 
   * Because we're in a 3 server cluster configuration, 
   * that one vote is enough for candidate1 to become
   * a leader.
   *)
  
  assert(State.is_leader candidate1);
  let () = 
    let expected_event = {
      timeout = default_configuration.hearbeat_timeout; 
      timeout_type = Heartbeat; 
    } in 
    assert_event expected_event next_event1
  in
  assert(2 = List.length msgs_to_send);

  (* 
   * Let's perform the second vote request which is 
   * now coming from candidate2 to the same server0. 
   *
   * Because server0 has already granted its vote, it should
   * not grant it again to candidate2. 
   *)
  
  let (candidate2, msgs_to_send, next_event2), server0, now 
    = vote_communication ~from:candidate2 ~to_:server0 ~now () in 

  assert(now = 0.004);
  assert(not (State.is_leader candidate2));
    (* Candidate2 was second and therefore did not
       get the vote. 
     *)

  (* 
   * Because 4 ms had alreadly elapsed between the time
   * candidate2 initiated the election and the response received 
   * from server0, the timeout is therefore 
   * (election_timeout - 0.004) 
   *)
  let () = 
    let expected_event = {
      timeout = default_configuration.election_timeout -. 0.004; 
      timeout_type = New_leader_election;
    } in 
    assert(expected_event = next_event2)
  in
  assert([] = msgs_to_send); 
    (* candidate2 is not a leader and therefore no message
       should be sent.
     *)
  ()

let foo = Bytes.of_string "Foo"
let bar = Bytes.of_string "Bar"
let bim = Bytes.of_string "Bim" 
  
let append_entry_communication ~from ~to_ ~now () = 
  let request = Logic.Append_entries.make from 1 in
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
   * In this test we look at the normal mode of replication of logs
   * between a freshly elected leader and one of the follower
   *
   * We ensure that: 
   * 1) Logs are correctly inserted for the first time 
   * 2) Commit index is correctly updated on the follower when 
   *    receiving an append entry
   * 3) Empty append entry request (ie Heart Beat) are 
   *    handled well
   * 4) Logs are correctly inserted when logs were 
   *    previously inserted.
   *)

  let configuration = {default_configuration with 
    max_nb_logs_per_message = 1
  } in 
  let server0   = 
    Leader.become (initial_state ~configuration ~current_term:1 ~now 0) now 
    |> Leader.add_log foo
    |> Leader.add_log bar
  in 
  
  let log_version_0 = 
    {index = 2; term = 1; data = bar} :: 
    {index = 1; term = 1; data = foo} :: []
  in

  (* Check initial leader state. 
   *)
  assert (0 = server0.id);
  assert (1 = server0.current_term);
  begin match server0.log with
    | {index = 2; term = 1; data = bar} :: 
      {index = 1; term = 1; data = foo} :: [] ->
      ()
    | _ -> assert(false)
  end; 
  assert(2 = server0.log_size);
  begin match server0.role with
    | Leader {next_index; match_index} -> (
      assert(2 = List.length next_index);
      assert(2 = List.length match_index);
    )
    | _ -> assert(false)
  end;

  (* Check initial follower state.
   *)
  let server1 = initial_state ~now 1 in 
  assert(1 = server1.id);
  assert(0 = server1.current_term);
  assert(0 = List.length server1.log);
  assert(0 = server1.log_size);
  assert_no_current_leader server1;

  let (server0, msgs_to_send, next_event), server1, _, now = 
    append_entry_communication ~from:server0 ~to_:server1 ~now () in 
  
  (* Check leader state after request/response communication
   *)
  let check_server0 ~commit_index ~next_index server0 = 
    assert (0 = server0.id);
    assert (1 = server0.current_term);
    assert (log_version_0 = server0.log); 
    assert (2 = server0.log_size); 
    assert (State.is_leader server0); 
    assert ((Some next_index) = Leader.next_index_for_receiver server0 1); 
    assert ((Some 1) = Leader.next_index_for_receiver server0 2); 
    assert (commit_index = server0.commit_index);
  in

  check_server0 ~commit_index:1 ~next_index:2 server0;
    (* Because the max number of message 1, then only the log 
     * index1 was sent in the request, hence next index 2 and 
     * the only commited index is 1. (It is commited because
     * a single server replication is a majority in our 3 server 
     * configuration);
     *)
  
  assert (now = 0.002);
  
  assert(1 = List.length msgs_to_send); 
  (* There is a msg to send since the server0 log contains log entries
   * not replicated on the server1. (Due to max nb of messages). 
   *) 

  (* 
   * The leader has not sent any requests since time 0 when 
   * it became leader to the server 2. Therefore 
   * the min timeout is (hearbeat_timeout -. 0.002). 
   *)
  let () = 
    let expected_event = {
      timeout_type = Heartbeat;
      timeout = default_configuration.hearbeat_timeout -. 0.002; 
    } in 
    assert_event expected_event next_event; 
  in
  
  (* Check the follower state after request/request communication
   *)
  let check_server1 ~log_length ~commit_index server1 = 
    assert(1 = server1.id);
    assert(1 = server1.current_term);
    assert_current_leader server1 0;
    assert(log_length  = List.length server1.log);
    assert(log_length  = server1.log_size);
      (* The 2 leader logs were inserted
       *)
    begin match server1.role with
    | Follower {voted_for = None} -> ()
    | _ -> assert(false)
    end; 
    assert(commit_index = server1.commit_index);
  in

  check_server1 ~log_length:1 ~commit_index:0 server1;
    (* None of the new entry are yet commited since 
       for that to happen the leader would have needed 
       to receive a mojority of successful append logs
       and send a new append log with its updated 
       commit index. 
       
       In this particular case in the next
       append entries (see below) the follower would have its 
       commit_index updated to 2.
     *)
  
  let (server0, msgs_to_send, next_event), server1, _, now = 
    append_entry_communication ~from:server0 ~to_:server1 ~now () in 
  assert(now = 0.004); 
  assert([] = msgs_to_send);
  check_server0 ~commit_index:2 ~next_index:3 server0;
    (* The second Append entries request contained the second log
     * entry from server 0. After the sucessful replication, the commit
     * index is now 2 and the next index 3. 
     * 
     * All the logs were sent and therefore [msgs_to_send] is empty.
     *)
  check_server1 ~commit_index:1 ~log_length:2 server1;
    (* The server1 has replicated the second log and is now
     * aware that the first log was commited. It will only 
     * be aware of the second log commited in the next request.
     *)

  (* server0 has still not send any request to server 2 
   * and therefore the timeout will be hearbeat_timeout - now. 
   *) 
  let () = 
    let expected_event = {
      timeout = default_configuration.hearbeat_timeout -. now;
      timeout_type = Heartbeat;
    } in 
    assert_event expected_event next_event
  in 

  let server0 = 
    server0
    |> Leader.add_log bim
  in

  let (server0, msgs_to_send, next_event), server1, _, now = 
    append_entry_communication ~from:server0 ~to_:server1 ~now () in 

  let check_server0 server0 = 
    let log_version_1 = {index=3;term=1;data=bim;}::log_version_0 in
    assert (0 = server0.id);
    assert (1 = server0.current_term);
    assert (log_version_1 = server0.log); 
    assert (server0.log_size  = List.length server0.log);
    assert (State.is_leader server0); 
    assert ((Some 4) = Leader.next_index_for_receiver server0 1); 
    assert ((Some 1) = Leader.next_index_for_receiver server0 2); 
    assert (3 = server0.commit_index);
  in
  check_server0 server0;

  assert([] = msgs_to_send); 
    (* Leader new log entry was successfully replicated as part of 
       the Append Entry communication above, therefore no subsequent
       messages need to be sent.
     *)

  let check_server1 ~commit_index server1 = 
    assert(1 = server1.id);
    assert(1 = server1.current_term);
    assert_current_leader server1 0;
    assert(3 = List.length server1.log);
    assert(3 = server1.log_size);
      (* The third log [bim] should have been inserted as part 
         of the append entry request.
       *)
    begin match server1.role with
    | Follower {voted_for = None} -> ()
    | _ -> assert(false)
    end; 
    assert(commit_index = server1.commit_index);
  in

  check_server1 ~commit_index:2 server1;
  
  let (server0, msgs_to_send, next_event), server1, _, now = 
    append_entry_communication ~from:server0 ~to_:server1 ~now () in 
    (* Since no new log were inserted, this should have send 
       an empty append entry request but with a leader commit index = 3 which
       should be reflected in the commit index of the follower.
     *)
  check_server0 server0;
  check_server1 ~commit_index:3 server1;

  assert([] = msgs_to_send); 
    (* Since the leader (server0) logs are fully replicated on 
       server1, no msg needs to sent. 
     *)

  (* 
  Format.printf "server0: %a\n" pp_state server0; 
  Format.printf "server1' : %a\n" pp_state server1; 
  Format.printf "followup action: %a\n" pp_next_event next_event; 
  *)
  ()

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
  assert((Some 3) = Leader.next_index_for_receiver server0 1);
    (* 
     * Because the server1 denied the append entry, 
     * the leader is supposed to decrement its belief of 
     * what the next log index is for that follower. 
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
    assert(2 = r.prev_log_index);
    assert(1 = r.prev_log_term);
    assert(3 = r.leader_commit);
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
  assert((Some 2) = Leader.next_index_for_receiver server0 1);
  
  begin match msgs_to_send with
  | (Append_entries_request r, receiver_id)::[] -> (
    (* The server1 is lagging by 3 log and will therefore 3 
     * append entry iteration for a successful handling.
     *) 
    assert(1 = receiver_id);
    assert(0 = r.leader_id);
    assert(2 = r.leader_term);
    assert(1 = r.prev_log_index);
    assert(1 = r.prev_log_term);
    assert(3 = r.leader_commit);
  )
  | _ -> assert(false)
  end;

  let (server0, msgs_to_send, next_event), server1, _, now = 
    append_entry_communication ~from:server0 ~to_:server1 ~now () in
  assert(now = 0.006); 
  assert((Some 1) = Leader.next_index_for_receiver server0 1);
  begin match msgs_to_send with
  | (Append_entries_request r, receiver_id)::[] -> (
     (* Now the leader has the correct believe of what should 
      * be the next log index for the server1, next roundtrip
      * should be successful.
      *)
    assert(1 = receiver_id);
    assert(0 = r.leader_id);
    assert(2 = r.leader_term);
    assert(0 = r.prev_log_index);
    assert(0 = r.prev_log_term);
    assert(3 = r.leader_commit);
  )
  | _ -> assert(false)
  end;

  let (server0, msgs_to_send, next_event), server1, _, now = 
    append_entry_communication ~from:server0 ~to_:server1 ~now () in
  assert(now = 0.008); 
  assert(3 = List.length server1.log); 
  assert(3 = server1.commit_index); 
    (* The server1 has successfully appended the 3 log it 
       was missing

       Because the leader had a commit index of 3, the follower 
       having inserted up to index 3 will also have its 
       commit index set to 3.
     *)

  assert([] = msgs_to_send);

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

  (*
   * In this test we will verify the safety feature 
   * of the Raft protocol, by simulating a scenario
   * where one server does not get the log from the leader 
   * and later on is raising an election. 
   *
   * Bceause the leader logs were replicated on the other 
   * follower (enough for majority), they ended up being 
   * commited and the safety guaratee must ensure that those 
   * logs are never invalided/removed.
   *
   * We will verify that the new candidate cannot be elected
   * due to the fact that it is not up to date, hence denying 
   * it a leader position role during which it would have invalidated
   * previous commited logs. 
   *
   *)

  let leader_log_version_0 = 
    {index = 3; term = 1; data = bim} ::
    {index = 2; term = 1; data = bar} :: 
    {index = 1; term = 1; data = foo} :: [] 
  in

  let server0 = 
    let current_term = 1 in 
    let log = leader_log_version_0 in 
    Leader.become (initial_state ~commit_index:3 ~log ~current_term ~now 0) now 
  in  

  let server1 = 
    (* Follower1 is the server which has both 
     * voted for server 0 in the election term 1 and 
     * has also replicated the leader 3 logs. 
     *
     * See previous unit tests for the interaction
     * details.
     *)
    let {election_timeout; _ } = default_configuration in 
    let current_term = 1 in 
    let role = Follower (
      default_follower_state 
        ~voted_for:(Some 0) 
        ~current_leader:(Some 0)
        ~election_deadline:(now +. election_timeout) () 
    )in 
    initial_state ~commit_index:3 ~role ~log:leader_log_version_0 ~current_term ~now 1 
  in 

  let server2 = 
    (* server2 is the server which was 'disconnected' 
     * during term1 of server0. 
     *
     * Therefore it never received any append entries
     * query from server0. 
     *
     * Because server2 is now aware of any leaders it will
     * continuously call for new election and increment 
     * its current term each time. 
     * 
     * Let's take an arbitrary number of 10 terms, indicating
     * that this server has attempted than many time to 
     * become leader.
     *)
    let current_term = 10 in 
    Candidate.become ~now (initial_state ~current_term ~now 2)
  in  

  (* verify initial properties
   *)
  assert(State.is_leader server0);
  assert(State.is_follower server1);
  assert(State.is_candidate server2);

  assert(server0.current_term = 1);
  assert(server1.current_term = 1);
  assert(server2.current_term = 11);

  assert(3 = List.length server0.log);
  assert(3 = List.length server1.log);
  assert(0 = List.length server2.log);
  
  assert(3 = server0.commit_index);
  assert(3 = server1.commit_index);
  assert(0 = server2.commit_index);

  assert_current_leader server1 0;

  (*
   * First vote communication for term 11 between server2 (candidate)
   * and server1 (follower of the server0 leader)
   *)

  let (server2, msgs_to_send, next_event), server1, now =
    vote_communication ~from:server2 ~to_:server1 ~now () in
  assert(now = 0.002);

  assert(State.is_candidate server2); 
  (* Despite not being granted the vote, server2 is 
     still a candidate
   *)
  assert_nb_votes server2 1; 
  (* server2 only vote at is the one from itself.
   *)

  assert([] = msgs_to_send);
  (* No follow up msg to send. server2 is still a 
     candidate waiting for new responses or an election timeout
   *)

  (* The vote was not granted due to the fact 
     that server1 log was more up to date. 
   *)
  assert(State.is_follower server1); 
  assert(11 = server1.current_term); 
    (* Follower 1 updated its term to match 
     * the one from Follower2 since its is greater. 
     *)
  
  let () = 
    let expected_event = {
      timeout = default_configuration.election_timeout -. now; 
      timeout_type = New_leader_election; 
    } in 
    assert_event expected_event next_event
  in 

  (*
   * First vote communication for term 11 between Follower2 (candidate)
   * and server0
   *)

  let (server2, msgs_to_send, next_event), server0, now =
    vote_communication ~from:server2 ~to_:server0 ~now () in
  assert(now = 0.004);
  
  assert(State.is_candidate server2); 
    (* The vote was not granted due to the fact 
       that server0 log was more up to date. 
     *)
  assert_nb_votes server2 1; 
    (* Because the vote was not granted, sever2 vote count 
     * is still limited to the vote it granted itself.
     *)
  assert([] = msgs_to_send);
    (* No message to send, the next action to for server2
     * is to wait until the next election timeout has 
     * elapsed and start another one.
     *)

  assert(State.is_follower server0); 
  assert(11 = server0.current_term); 
    (* Because server1 current term is greated, server0 
     * had to step down from its leader role to become 
     * a follower and update its current term to 
     * match server2 (candidate).
     *)

  let () = 
    let expected_event = {
      timeout_type = New_leader_election; 
      timeout = default_configuration.election_timeout -. now; 
    } in 
    assert_event expected_event next_event
  in 
  
  (*
   * At this time there are no leader in term 11. 
   * Follower2 is a candidate who cannot be elected and 
   * both Leader0 and Follower1 are now followers. 
   *
   * This situation will continue for as many new 
   * term initiated by server2. Because of random
   * election time out eventually either server0 
   * or server1 will initiate a new election term. 
   *
   * We'll assume server1 initiate first.
   *)

  let server1 = Candidate.become ~now server1 in
  assert(12 = server1.current_term); 

  let (server1, msgs_to_send, next_event), server0, now = 
    vote_communication ~from:server1 ~to_:server0 ~now () in 

  assert(State.is_leader server1); 
    (* server0 current log is at the same stage as 
     * server1... hence it grants its vote to server1. In 
     * a configuration of 3 server this is sufficient for a
     * majority and server1 become leader. 
     *) 
  let () = 
    let expected_event = {
      timeout = default_configuration.hearbeat_timeout;
      timeout_type = Heartbeat; 
    } in 
    assert_event expected_event next_event;
  in 
  assert(2 = List.length msgs_to_send);

  begin match server0.role with
    | Follower {voted_for = Some 1} -> () 
    | _ -> assert(false)
  end;
  assert(12 = server0.current_term);

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
  | Some request -> (

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
  | None -> () 

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

  match Logic.Append_entries.make server0 1 with
  | None -> assert(false)
  | Some request -> (
    let now = now +. 0.001 in
    
    (*
     * The request is sent twice to the server1. 
     *)
    let server1, _        = Logic.Append_entries.handle_request server1 request now in  
    let server1, response = Logic.Append_entries.handle_request server1 request now in  

    assert(1 = List.length server1.log);
    assert_current_leader server1 0; 
    begin match response.result with
    | Failure -> assert(false)
    | Success {receiver_last_log_index} -> assert(1 = receiver_last_log_index) 
    end
  ); 

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

  match Logic.Append_entries.make server0 1 with
  | None -> assert(false)
  | Some request0 -> (
    let server0 = Leader.add_log bar server0 in  
    match Logic.Append_entries.make server0 1 with
    | None -> assert(false)
    | Some request1 -> (
      (* Handle the request in the oposite
       * order.
       *)
      let server1, response0 = Logic.Append_entries.handle_request server1 request1 now in  
      let server1, response1 = Logic.Append_entries.handle_request server1 request0 now in  

      assert(2 = List.length server1.log);

      let server0, _ = Logic.Append_entries.handle_response server0 response0 now in 
      let server0, _ = Logic.Append_entries.handle_response server0 response1 now in 

      assert(Some (3) = Leader.next_index_for_receiver server0 1);
      assert(Some (2) = Leader.match_index_for_receiver server0 1);
      assert(2 = server0.commit_index);
    )  
  );

  ()


let ()  = 
  (* 
   * Verify the correct transition from follower to candidate
   * as well as the valid vote from a follower and 
   * subsequent conversion to leader by the candidate. 
   * 
   *)

  let server0 = initial_state ~now 0 in 
  let server1 = initial_state ~now 1 in 

  let server0, msgs = Raft_logic.Message.handle_new_election_timeout server0 now in 
  
  assert(State.is_candidate server0); 
    (* Because there is a new election timeout we would expect the server0 to 
     * become a candidate. 
     *)
  assert(1 = server0.current_term); 
    (* Since a new election was started, the [current_term] must be 
     * incremented.
     *)
  assert(2 = List.length msgs);  
    (* Upon becoming a candidate the sever0 is expected to send a `RequestVote` 
     * message to all the other servers. Since we are in a cluster of 3, 2 messages
     * are then expected.
     *)

  let msg_to_server1 = 
    match List.find (fun (_, id) -> id = 1) msgs with
    | (msg, _ ) -> msg 
    | exception Not_found -> assert(false) 
  in 

  (*
   * Let's now assume the [msg_to_server1] was correctly transmitted between 
   * server, so let's handle it.
   *)

  let now = now +. 0.001 in 

  let server1, msgs = Raft_logic.Message.handle_message server1 msg_to_server1 now in 

  assert(1 = server1.current_term); 
    (* Server0 (Candidate) send a higher term to server1 which is expected to 
     * increase its own [current_term]. 
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

  let msg = 
    match msgs with
    | (msg, 0)::[]  -> (
        (* A single response to server 0 is expected from server1.
         *)
      begin match msg with
      | Request_vote_response {voter_id = 1; voter_term = 1; vote_granted = true} -> 
        msg 
        (* 
         * Make sure that the response is a granted vote. 
         *)

      | _ -> assert(false)
      end
    ) 
    | _ -> assert(false) 
  in

  (* 
   * Let's now make this response communicated to the server0 (Candidate)
   *)

  let now = now +. 0.001 in 
  let server0, msgs = Raft_logic.Message.handle_message server0 msg now in 

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
  | Leader {next_index; match_index; receiver_heartbeats; } -> (
    assert(2 = List.length next_index); 
    assert(2 = List.length match_index); 
    assert(2 = List.length receiver_heartbeats); 
      (* 
       * The leader maintain various state for each of the 
       * other servers. 
       *)
      List.iter (fun {server_log_index;_ } -> 
        assert(1 = server_log_index); 
      ) next_index; 
      List.iter (fun {server_log_index;_ } -> 
        assert(0 = server_log_index); 
      ) match_index; 
      List.iter (fun {heartbeat_deadline;_ } -> 
        assert(now +. default_configuration.hearbeat_timeout  = heartbeat_deadline); 
      ) receiver_heartbeats; 
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


  let msg = 
    match List.find (fun (_, id) -> id = 1) msgs with
    | (msg, _) -> msg 
    | exception Not_found -> assert(false)
  in 

  let now = now +. 0.001 in 
  let server1, msgs = Raft_logic.Message.handle_message server1 msg now in 

  begin match server1.role with
  | Follower {
    voted_for = None ; 
    current_leader = Some 0;  
    election_deadline; 
  } ->
    (*
     * We see that the follower correctly records that it has now
     * a current leader which is server0. 
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

  ()

