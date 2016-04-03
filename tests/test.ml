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
  ?configuration:(configuration = default_configuration ())
  ?current_term:(current_term = 0) id = {
  id;
  current_term;
  log = []; 
  commit_index = 0;
  last_applied = 0;
  role = Follower {voted_for = None} ;
  configuration; 
}
  
let now = 0.

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
    )
    | _ -> assert(false)
  end; 
  assert (State.is_candidate candidate0);
  assert (1 = candidate0.current_term);
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
  
  let (candidate0, follow_up_action), follower1 = 
    let request_vote = Logic.Request_vote.make candidate0 in 
    let follower1, response = Logic.Request_vote.handle_request follower1 request_vote in 
    Logic.Request_vote.handle_response candidate0 response, follower1 
  in 
  
  assert (State.is_follower follower1);
  begin match follower1.role with
    | Follower {voted_for = Some 0} -> ()
    | _ -> assert(false)
  end;
  assert (1 = follower1.id);
  assert (1 = follower1.current_term);
  assert ([] = follower1.log);
  
  begin match candidate0.role with
  | Leader {next_index; match_index} ->(
    assert(3 = List.length next_index); 
    List.iter (fun {server_log_index; _ } -> 
      assert (1 = server_log_index); 
    ) next_index;
    assert(3 = List.length match_index); 
    List.iter (fun {server_log_index; _ } -> 
      assert (0 = server_log_index); 
    ) match_index;
    )
  | _ -> assert(false)
  end; 
  assert (1 = candidate0.current_term);
  assert (0 = candidate0.id);

  assert (Act_as_new_leader = follow_up_action);

  Format.printf "candidate0': %a\n" pp_state candidate0; 
  Format.printf "follower1' : %a\n" pp_state follower1; 
  Format.printf "followup action: %a\n" pp_follow_up_action follow_up_action; 
  ()

let () = 

  let foo = Bytes.of_string "Foo" in 
  let bar = Bytes.of_string "Bar" in 
  
  let leader0   = 
    Leader.make (initial_state ~current_term:1 0) 
    |> Leader.add_log foo
    |> Leader.add_log bar
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

  let perform_append_entry leader0 follower1 = 
    let request = Logic.Append_entries.make leader0 1 in
    match request with
    | Some request -> 
      let follower1, response = Logic.Append_entries.handle_request follower1 request in  
      Logic.Append_entries.handle_response leader0 response, follower1, response  
    | None -> failwith "request expected"
  in
  
  let (leader0, follow_up_action), follower1, response = perform_append_entry leader0 follower1 in 
  
  (* Check leader state after request/response communication
   *)
  let check_leader0 leader0 = 
    assert (0 = leader0.id);
    assert (1 = leader0.current_term);
    begin match leader0.log with
      | {index = 2; term = 1; data = bar} :: 
        {index = 1; term = 1; data = foo} :: [] ->
        ()
      | _ -> assert(false)
    end; 
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
  
  let (leader0, follow_up_action), follower1, response = perform_append_entry leader0 follower1 in 
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

  Format.printf "response: %a\n" pp_append_entries_response response; 
  Format.printf "leader0: %a\n" pp_state leader0; 
  Format.printf "follower1' : %a\n" pp_state follower1; 
  Format.printf "followup action: %a\n" pp_follow_up_action follow_up_action; 
  ()
