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
  ?log:(log = [])
  ?commit_index:(commit_index = 0)
  ?configuration:(configuration = default_configuration ())
  ?current_term:(current_term = 0) id = {
  id;
  current_term;
  log;
  commit_index;
  last_applied = 0;
  role = Follower {voted_for = None} ;
  configuration; 
}
  
let now = 0.
  
let vote_communication ~from ~to_ () = 
  let request_vote  = Logic.Request_vote.make from in 
  let to_, response = Logic.Request_vote.handle_request to_ request_vote in 
  Logic.Request_vote.handle_response from response, to_

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

  let candidate0 = Candidate.make (initial_state 0) now in 
  
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
    = vote_communication ~from:candidate0 ~to_:follower1 () in 
  
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
  let candidate1 = Candidate.make (initial_state 1) now in
  let candidate2 = Candidate.make (initial_state 2) now in

  assert(1 = candidate1.id);
  assert(2 = candidate2.id); 
  assert(candidate1.current_term = candidate2.current_term);

  let (candidate1, follow_up_action1), follower0 
    = vote_communication ~from:candidate1 ~to_:follower0 () in 

  begin match follower0.role with
    | Follower {voted_for = Some 1} -> () 
    | _ -> assert(false)
  end;
  
  let (candidate2, follow_up_action2), follower0 
    = vote_communication ~from:candidate2 ~to_:follower0 () in 

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
  assert(Nothing_to_do = follow_up_action2);
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
  
let perform_append_entry leader0 follower1 = 
  let request = Logic.Append_entries.make leader0 1 in
  match request with
  | Some request -> 
    let follower1, response = Logic.Append_entries.handle_request follower1 request in  
    Logic.Append_entries.handle_response leader0 response, follower1, response  
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
    Leader.make (initial_state ~current_term:1 0) 
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

  
  let (leader0, follow_up_action), follower1, _ = perform_append_entry leader0 follower1 in 

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
  
  let (leader0, follow_up_action), follower1, _ = perform_append_entry leader0 follower1 in 
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

  let (leader0, follow_up_action), follower1, _ = perform_append_entry leader0 follower1 in 

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
  
  let (leader0, follow_up_action), follower1, _ = perform_append_entry leader0 follower1 in 
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
    Leader.make (initial_state ~commit_index:3 ~log ~current_term 0) 
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

  let (leader0, follow_up_action), follower1, _ = perform_append_entry leader0 follower1 in

  assert(0 = leader0.id);
  assert(2 = leader0.current_term); 
  assert(leader_log_version_0 = leader0.log);
  assert(3 = leader0.commit_index);

  assert(1 = follower1.id); 
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
  
  let (leader0, follow_up_action), follower1, _ = perform_append_entry leader0 follower1 in
  assert((Some 2) = Leader.next_index_for_receiver leader0 1);
  assert(Retry_append {server_id = 1} = follow_up_action); 
    (* The follower1 is lagging by 3 log and will therefore 3 
     * append entry iteration for a successful handling.
     *) 
  
  let (leader0, follow_up_action), follower1, _ = perform_append_entry leader0 follower1 in
  assert((Some 1) = Leader.next_index_for_receiver leader0 1);
  assert(Retry_append {server_id = 1} = follow_up_action); 
     (* Now the leader has the correct believe of what should 
      * be the next log index for the follower1, next roundtrip
      * should be successful.
      *)

  let (leader0, follow_up_action), follower1, _ = perform_append_entry leader0 follower1 in
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
