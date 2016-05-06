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

let now = 0.
  
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

  let now = now +. 0.001 in

  (* 
   * Let's now handle the successful vote response from 
   * server2. 
   *)

  let server1, msgs =
    let msg = msg_for_server msgs 1 in  
    Raft_logic.Message.handle_message server1 msg now 
  in 

  assert(State.is_leader server1); 
    (* 
     * One vote is enough to become a [Leader].
     *)

  assert(3 = server1.current_term); 
    (* 
     * [current_term] should be the same after becoming
     * a [Leader].
     *)

  assert(2 = List.length msgs);
    (*
     * Imediately after becoming a [Leader], the server
     * will send [Append_entries] to establish its
     * leadership.
     *)

  List.iter (fun (r, _) -> 
    match r with 
    | Append_entries_request r -> (
      assert(r.leader_term = 3);
      assert(r.leader_id = 1);
      assert(r.prev_log_index = 3);
      assert(r.prev_log_term = 1);
      assert(r.rev_log_entries = []);
        (*
         * Initially the [Leader] believes that all other servers
         * have replicated the same [log_entry]s as itself.
         *)
      assert(r.leader_commit = 2);
    )
    | _ -> assert(false)
  ) msgs;


  let now = now +.  0.001 in 

  let server2, msgs = 
    let msg = msg_for_server msgs 2 in 
    Raft_logic.Message.handle_message server2 msg now
  in

  assert(State.is_follower server2);
  assert(3 = server2.current_term);

  assert(1 = List.length msgs); 
    (*
     * Single response to server1. 
     *)

  begin match msgs with
  | (Append_entries_response r, 1)::[] -> (
    assert(r.receiver_id = 2);
    assert(r.receiver_term = 3);
    assert(r.result = Log_failure { 
      receiver_last_log_index = 2; 
      receiver_last_log_term =1;
    }); 
      (* 
       * server2 did not replicate the 3rd [log_entry] that server1 
       * did during [term = 1].
       *
       * Therefore the previous [Append_entries] request is rejected.
       *)
  )
  | _ -> assert(false)
  end;

  (*
   * Let's propagate that response back to server1. 
   *)

  let now = now +. 0.001 in

  let server1, msgs = 
    let msg = msg_for_server msgs 1 in 
    Raft_logic.Message.handle_message server1 msg now
  in

  assert(State.is_leader server1);
  assert(3 = server1.current_term);

  assert(1 = List.length msgs);
    (*
     * A new request for server2 has been computed which 
     * should now contain the 3rd [log_entry]
     *)

  begin match msgs with
  | (Append_entries_request r, 2) :: [] -> (
    assert(r.leader_term = 3);
    assert(r.leader_id = 1);
    assert(r.prev_log_index = 2);
    assert(r.prev_log_term = 1);
    assert(1 = List.length r.rev_log_entries); 
    assert(r.leader_commit = 2);
  )
  | _ -> assert(false)
  end;

  let now = now +. 0.001 in 
  
  let server2, msgs = 
    let msg = msg_for_server msgs 2 in 
    Raft_logic.Message.handle_message server2 msg now
  in

  assert(State.is_follower server2);
  assert(3 = server2.current_term);

  assert(3 = List.length server2.log); 
    (* 
     * server2 has correctly replicated the 3rd [log_entry]. 
     *)

  assert(1 = List.length msgs); 
    (*
     * Single response for server1.
     *)
  begin match msgs with
  | (Append_entries_response r, 1) :: [] -> (
    assert(r.receiver_id = 2);
    assert(r.receiver_term = 3);
    assert(r.result = Success { receiver_last_log_index = 3});
      (*
       * Confirmation that the replication of the log has 
       * been successful. 
       *)
  )
  | _ -> assert(false)
  end;


  let now = now +. 0.001  in 

  let server1, msgs = 
    let msg = msg_for_server msgs 1 in 
    Raft_logic.Message.handle_message server1 msg now 
  in

  assert(State.is_leader server1);
  assert(3 = server1.current_term);

  assert(3 = server1.commit_index);
    (*
     * The 3rd [log_entry] has been replicated one one other 
     * server than the [Leader]; this makes a majority and therefore
     * indicates that [commit_index] can be set to 3.
     *)

  assert([] = msgs);

  ()
