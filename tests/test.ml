open Raft_pb

module State     = Raft_helper.State 
module Candidate = Raft_helper.Candidate
module Follower  = Raft_helper.Follower
module Leader    = Raft_helper.Leader 

module Logic     = Raft_logic  

let default_configuration ()  = {
  nb_of_server = 3; 
  election_timeout = 0.1;
}

let initial_state  
  ?role:(role = Follower {voted_for = None; current_leader = None;}) 
  ?log:(log = [])
  ?commit_index:(commit_index = 0)
  ?configuration:(configuration = default_configuration ())
  ?current_term:(current_term = 0) id = {
  id;
  current_term;
  log;
  commit_index;
  last_applied = 0;
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

  
let now = 0.
  
let vote_communication ~from ~to_ ~now () = 
  let request_vote  = Logic.Request_vote.make from in 
  let to_, response, _ = Logic.Request_vote.handle_request to_ request_vote now in 
  Logic.Request_vote.handle_response from response now, to_

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

  let candidate0 = Candidate.become ~now (initial_state 0)  in 
  
  begin match candidate0.role with
    | Candidate {vote_count; election_deadline} -> (
      assert(1 = vote_count); 
      assert(0.1 = election_deadline);
       (* When converting to candidate, the server automatically 
        * vote for itself.
        *)
    )
    | _ -> assert(false)
  end; 
  assert (State.is_candidate candidate0);
  assert (1 = candidate0.current_term);
    (* Make sure that any new election increments the 
     * current term by 1.
     *)
  assert (0 = candidate0.id);

  let follower1  = initial_state 1 in 

  assert (State.is_follower follower1);
  begin match follower1.role with
    | Follower {voted_for = None} -> ()
    | _ -> assert(false)
  end;
  assert (1 = follower1.id);
  assert (0 = follower1.current_term);
  assert ([] = follower1.log);
  
  (* 
   * Vote for the Candidate
   * Convert from Candidate to Follower after getting majority
   *)
  
  let (candidate0, follow_up_action), follower1 
    = vote_communication ~from:candidate0 ~to_:follower1 ~now () in 
  
  assert (State.is_follower follower1);
  begin match follower1.role with
    | Follower {voted_for = Some 0} -> ()
    | _ -> assert(false)
  end;
  assert (1 = follower1.id);
  assert (1 = follower1.current_term);
    (* Make sure that the follower has correctly updated
     * its current term to the one of the candidate since
     * the latter one is larger.
     *)
  assert ([] = follower1.log);
  
  begin match candidate0.role with
  | Leader {next_index; match_index} -> (
    assert(3 = List.length next_index); 
    List.iter (fun {server_log_index; _ } -> 
      assert (1 = server_log_index); 
        (* The initial value for all server next log index 
         * should be equal to the last log index + 1. 
         *)
    ) next_index;
    assert(3 = List.length match_index); 
    List.iter (fun {server_log_index; _ } -> 
      assert (0 = server_log_index); 
    ) match_index;
  )
  | _ -> assert(false)
  end; 
  assert (1 = candidate0.current_term);
    (* Becoming a leader should not alter 
     * the term. 
     *)
  assert (0 = candidate0.id);

  assert (Act_as_new_leader = follow_up_action);

  Format.printf "candidate0': %a\n" pp_state candidate0; 
  Format.printf "follower1' : %a\n" pp_state follower1; 
  Format.printf "followup action: %a\n" pp_follow_up_action follow_up_action; 
  ()

let () = 

  (* 
   * This test verifies that when a follower has previously voted 
   * for a candidate it correctly denies its vote to a subsequent
   * candidate within that same term. 
   *)

  let follower0  = initial_state 0 in 
  let candidate1 = Candidate.become ~now (initial_state 1) in
  let candidate2 = Candidate.become ~now (initial_state 2) in

  assert(1 = candidate1.id);
  assert(2 = candidate2.id); 
  assert(candidate1.current_term = candidate2.current_term);

  let (candidate1, follow_up_action1), follower0 
    = vote_communication ~from:candidate1 ~to_:follower0 ~now () in 

  begin match follower0.role with
    | Follower {voted_for = Some 1} -> () 
    | _ -> assert(false)
  end;
  
  let (candidate2, follow_up_action2), follower0 
    = vote_communication ~from:candidate2 ~to_:follower0 ~now () in 

  assert(State.is_leader candidate1);
    (* Candidate 1 was the first and therefore got the 
     * follower0 vote which in our configuration of 3 
     * is a majority to become leader.
     *)
  assert(Act_as_new_leader = follow_up_action1);

  assert(not (State.is_leader candidate2));
    (* Candidate2 was second and therefore did not
       get the vote. 
     *)
  assert(Wait_for_rpc {election_deadline = 0.1; } = follow_up_action2);
    (* TODO ... here really it means wait until 
     * next message or election timeout. 
     * 
     * Need to confirm if we need a dedicated follow up 
     * action.
     *) 
  ()

let foo = Bytes.of_string "Foo"
let bar = Bytes.of_string "Bar"
let bim = Bytes.of_string "Bim" 
  
let append_entry_communication ~from ~to_ ~now () = 
  let request = Logic.Append_entries.make from 1 in
  match request with
  | Some request -> 
    let to_, response, _ = Logic.Append_entries.handle_request to_ request now in  
    Logic.Append_entries.handle_response from response now, to_, response  
  | None -> failwith "request expected"

let () = 

  (*
   * In this test we look at the normal mode of replication of logs
   * between an freshly elected leader and one of the follower
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

  let leader0   = 
    Leader.become (initial_state ~current_term:1 0) 
    |> Leader.add_log foo
    |> Leader.add_log bar
  in 
  
  let log_version_0 = 
    {index = 2; term = 1; data = bar} :: 
    {index = 1; term = 1; data = foo} :: []
  in

  (* Check initial leader state. 
   *)
  assert (0 = leader0.id);
  assert (1 = leader0.current_term);
  begin match leader0.log with
    | {index = 2; term = 1; data = bar} :: 
      {index = 1; term = 1; data = foo} :: [] ->
      ()
    | _ -> assert(false)
  end; 
  begin match leader0.role with
    | Leader {next_index; match_index} -> (
      assert(3 = List.length next_index);
      assert(3 = List.length match_index);
    )
    | _ -> assert(false)
  end;

  (* Check initial follower state.
   *)
  let follower1 = initial_state 1 in 
  assert(1 = follower1.id);
  assert(0 = follower1.current_term);
  assert(0 = List.length follower1.log);
  assert_no_current_leader follower1;

  
  let (leader0, follow_up_action), follower1, _ = 
    append_entry_communication ~from:leader0 ~to_:follower1 ~now () in 

  (* Check leader state after request/response communication
   *)
  let check_leader0 leader0 = 
    assert (0 = leader0.id);
    assert (1 = leader0.current_term);
    assert (log_version_0 = leader0.log); 
    assert (State.is_leader leader0); 
    assert ((Some 3) = Leader.next_index_for_receiver leader0 1); 
    assert ((Some 1) = Leader.next_index_for_receiver leader0 2); 
    assert (2 = leader0.commit_index);
  in

  check_leader0 leader0;

  (* Check the follower state after request/request communication
   *)
  let check_follower1 ~commit_index follower1 = 
    assert(1 = follower1.id);
    assert(1 = follower1.current_term);
    assert_current_leader follower1 0;
    assert(2 = List.length follower1.log);
      (* The 2 leader logs were inserted
       *)
    begin match follower1.role with
    | Follower {voted_for = None} -> ()
    | _ -> assert(false)
    end; 
    assert(commit_index = follower1.commit_index);
  in

  check_follower1 ~commit_index:0 follower1;
    (* None of the new entry are yet commited since 
       for that to happen the leader would have needed 
       to receive a mojority of successful append logs
       and send a new append log with its updated 
       commit index. 
       
       In this particular case in the next
       append entries (see below) the follower would have its 
       commit_index updated to 2.
     *)
  
  let (leader0, follow_up_action), follower1, _ = 
    append_entry_communication ~from:leader0 ~to_:follower1 ~now () in 
  check_leader0 leader0;
    (* The leader should have been unchanged since no new log entries
       were added and they had all been send to the receiver 1 
       in the first request. 
     *)
  check_follower1 ~commit_index:2 follower1;
    (* The follower1 has the exact same state as before the 
       second request except for the commit index. (see above). 
       
       Since the request contained no new log entries, all
       other state variables have not changed.
     *)

  let leader0 = 
    leader0
    |> Leader.add_log bim
  in

  let (leader0, follow_up_action), follower1, _ = 
    append_entry_communication ~from:leader0 ~to_:follower1 ~now () in 

  let check_leader0 leader0 = 
    let log_version_1 = {index=3;term=1;data=bim;}::log_version_0 in
    assert (0 = leader0.id);
    assert (1 = leader0.current_term);
    assert (log_version_1 = leader0.log); 
    assert (State.is_leader leader0); 
    assert ((Some 4) = Leader.next_index_for_receiver leader0 1); 
    assert ((Some 1) = Leader.next_index_for_receiver leader0 2); 
    assert (3 = leader0.commit_index);
  in
  check_leader0 leader0;

  let check_follower1 ~commit_index follower1 = 
    assert(1 = follower1.id);
    assert(1 = follower1.current_term);
    assert_current_leader follower1 0;
    assert(3 = List.length follower1.log);
      (* The third log [bim] should have been inserted as part 
         of the append entry request.
       *)
    begin match follower1.role with
    | Follower {voted_for = None} -> ()
    | _ -> assert(false)
    end; 
    assert(commit_index = follower1.commit_index);
  in

  check_follower1 ~commit_index:2 follower1;
  
  let (leader0, follow_up_action), follower1, _ = 
    append_entry_communication ~from:leader0 ~to_:follower1 ~now () in 
    (* Since no new log were inserted, this should have send 
       an empty append entry request but with a leader commit index = 3 which
       should be reflected in the commit index of the follower.
     *)
  check_leader0 leader0;
  check_follower1 ~commit_index:3 follower1;

  Format.printf "leader0: %a\n" pp_state leader0; 
  Format.printf "follower1' : %a\n" pp_state follower1; 
  Format.printf "followup action: %a\n" pp_follow_up_action follow_up_action; 
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

  let leader0 = 
    let current_term = 2 in 
    let log = leader_log_version_0 in 
    Leader.become (initial_state ~commit_index:3 ~log ~current_term 0) 
  in  

  let follower1 = 
    let current_term = 0 in 
    initial_state ~current_term 1 
  in 
  
  assert((Some 4) = Leader.next_index_for_receiver leader0 1);
    (* Initially the leader belief of the next index for
     * a follower is set to its last log index  + 1. 
     *
     * This belief is indeed incorrect in our test case... 
     * (see above the lof of the follower1 is empty), but 
     * the next `append entry` communication will inform 
     * the leader about it.
     *)

  let (leader0, follow_up_action), follower1, _ = 
    append_entry_communication ~from:leader0 ~to_:follower1 ~now () in

  assert(0 = leader0.id);
  assert(2 = leader0.current_term); 
  assert(leader_log_version_0 = leader0.log);
  assert(3 = leader0.commit_index);

  assert(1 = follower1.id); 
  assert_current_leader follower1 0;
  begin match follower1.role with
    | Follower {voted_for = None; } -> ()
    | _ -> assert(false)
  end;
  assert(2 = follower1.current_term); 
    (* follower1 term was lower than the leader and should
     * therefore have updated its current term to match.
     *)
  assert(0 = List.length follower1.log);
    (* The append entry should have been rejected by the 
     * follower and consequently none of the log
     * entries should have been appended by the 
     * follower
     *)
  assert(0 = follower1.commit_index); 
    (* No new entries and therefore even if the 
       leader commit index is larger than the follower 
       commit index, the new commit index is following 
       the following formula:
       
       min (leader_commit, last_log_index)
         where leader_commit = 3
               last_log_index = 0
         and therefore commit_index = 0
     *)
  assert((Some 3) = Leader.next_index_for_receiver leader0 1);
    (* 
     * Because the follower1 denied the append entry, 
     * the leader is supposed to decrement its belief of 
     * what the next log index is for that follower. 
     *)
  assert(Retry_append {server_id = 1} = follow_up_action); 
    (* Another concequence of the follower1 denying the 
     * append entry request, is that the leader should 
     * send another append query. 
     *)
  
  let (leader0, follow_up_action), follower1, _ = 
    append_entry_communication ~from:leader0 ~to_:follower1 ~now () in
  assert((Some 2) = Leader.next_index_for_receiver leader0 1);
  assert(Retry_append {server_id = 1} = follow_up_action); 
    (* The follower1 is lagging by 3 log and will therefore 3 
     * append entry iteration for a successful handling.
     *) 
  
  let (leader0, follow_up_action), follower1, _ = 
    append_entry_communication ~from:leader0 ~to_:follower1 ~now () in
  assert((Some 1) = Leader.next_index_for_receiver leader0 1);
  assert(Retry_append {server_id = 1} = follow_up_action); 
     (* Now the leader has the correct believe of what should 
      * be the next log index for the follower1, next roundtrip
      * should be successful.
      *)

  let (leader0, follow_up_action), follower1, _ = 
    append_entry_communication ~from:leader0 ~to_:follower1 ~now () in
  assert(3 = List.length follower1.log); 
  assert(3 = follower1.commit_index); 
    (* The follower1 has successfully appended the 3 log it 
       was missing

       Because the leader had a commit index of 3, the follower 
       having inserted up to index 3 will also have its 
       commit index set to 3.
     *)
  assert((Some 4) = Leader.next_index_for_receiver leader0 1);
  assert(Nothing_to_do = follow_up_action); 
    (* As a concequence of the follower1 successfully inserting
       the 3 log entries of the leader, the leader is updating
       its next log index to [3 + 1] for that follower. 
     *)
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

  let leader0 = 
    let current_term = 1 in 
    let log = leader_log_version_0 in 
    Leader.become (initial_state ~commit_index:3 ~log ~current_term 0) 
  in  

  let follower1 = 
    (* Follower1 is the server which has both 
     * voted for server 0 in the election term 1 and 
     * has also replicated the leader 3 logs. 
     *
     * See previous unit tests for the interaction
     * details.
     *)
    let current_term = 1 in 
    let role = Follower {
      voted_for = Some 0; 
      current_leader = Some 0;
    } in 
    initial_state ~commit_index:3 ~role ~log:leader_log_version_0 ~current_term 1 
  in 

  let follower2 = 
    (* Follower2 is the server which was 'disconnected' 
     * during term1 of leader0. 
     *
     * Therefore it never received any append entries
     * query from leader0. 
     *
     * Because follower2 is now aware of any leaders it will
     * continuously call for new election and increment 
     * its current term each time. 
     * 
     * Let's take an arbitrary number of 10 terms, indicating
     * that this server has attempted than many time to 
     * become leader.
     *)
    let current_term = 10 in 
    Candidate.become ~now (initial_state ~current_term 2)
  in  

  (* verify initial properties
   *)
  assert(State.is_leader leader0);
  assert(State.is_follower follower1);
  assert(State.is_candidate follower2);

  assert(leader0.current_term = 1);
  assert(follower1.current_term = 1);
  assert(follower2.current_term = 11);

  assert(3 = List.length leader0.log);
  assert(3 = List.length follower1.log);
  assert(0 = List.length follower2.log);
  
  assert(3 = leader0.commit_index);
  assert(3 = follower1.commit_index);
  assert(0 = follower2.commit_index);

  assert_current_leader follower1 0;

  (*
   * First vote communication for term 11 between Follower2 (candidate)
   * and Follower1
   *)

  let (follower2, follow_up_action), follower1 =
    vote_communication ~from:follower2 ~to_:follower1 ~now () in

  assert(State.is_candidate follower2); 
    (* The vote was not granted due to the fact 
       that follower1 log was more up to date. 
     *)
  assert(State.is_follower follower1); 
  assert(11 = follower1.current_term); 
    (* Follower 1 updated its term to match 
     * the one from Follower2 since its is greater. 
     *)
  assert(Wait_for_rpc {election_deadline = 0.1} = follow_up_action);

  (*
   * First vote communication for term 11 between Follower2 (candidate)
   * and leader0
   *)

  let (follower2, follow_up_action), leader0 =
    vote_communication ~from:follower2 ~to_:leader0 ~now () in
  
  assert(State.is_candidate follower2); 
    (* The vote was not granted due to the fact 
       that leader0 log was more up to date. 
     *)
  assert(State.is_follower leader0); 
  assert(11 = leader0.current_term); 
    (* Because follower1 current term is greated, leader0 
     * had to step down from its leader role to become 
     * a follower and update its current term to 
     * match followe2 (candidate).
     *)
  
  (*
   * At this time there are no leader in term 11. 
   * Follower2 is a candidate who cannot be elected and 
   * both Leader0 and Follower1 are now followers. 
   *
   * This situation will continue for as many new 
   * term initiated by Follower2. Because of random
   * election time out eventually either leader0 
   * or follower1 will initiate a new election term. 
   *
   * We'll assume follower1 initiate first.
   *)

  let follower1 = Candidate.become ~now follower1 in
  assert(12 = follower1.current_term); 

  let (follower1, follow_up_action), leader0 = 
    vote_communication ~from:follower1 ~to_:leader0 ~now () in 

  assert(State.is_leader follower1); 
    (* Leader0 current log is at the same stage as 
     * follower1... hence it grants its vote to follower1. In 
     * a configuration of 3 server this is sufficient for a
     * majority and follower1 become leader. 
     *) 
  begin match leader0.role with
    | Follower {voted_for = Some 1} -> () 
    | _ -> assert(false)
  end;
  assert(12 = leader0.current_term);

  ()

let () = 
  (* Example *)
  
  (* Create a 3 server configuration. 
   *) 
  let configuration = Raft_pb.( {
    nb_of_server     = 3;
    election_timeout = 0.1;
  }) in 

  (* Create a leader state by simulating a (rigged) election
   *)
  let leader_0 = 
    Raft_helper.Follower.create ~configuration ~id:0 () 
    |> Raft_helper.Candidate.become ~now:0.0 
    |> Raft_helper.Leader.become 
    |> Raft_helper.Leader.add_log (Bytes.of_string "Foo") 
  in 
  
  (* Create a follower
   *)
  let follower_1 = 
    Raft_helper.Follower.create ~configuration ~id:1 () 
  in 

  (* First create an 'Append Entries' request from the 
     leader to server 1.
   *) 
  match Raft_logic.Append_entries.make leader_0 1 with
  | Some request -> (

    (* 'Update' server 1 (ie follower) by applying the request. This returns
       the response to send to the leader. 
     *)
    let follower_1, response, _ = Raft_logic.Append_entries.handle_request follower_1 request now in 

    (* 'Update' server 0 (ie leader) by applying the response. This returns
       the new state as well as a follow up action to take. 
     *)
    let leader0 , _ = Raft_logic.Append_entries.handle_response leader_0 response now in 

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
