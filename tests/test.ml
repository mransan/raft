open Raft_pb

module State     = Raft_state
module Candidate = Raft_role.Candidate
module Follower  = Raft_role.Follower
module Leader    = Raft_role.Leader
module Timeout_event = Raft_helper.Timeout_event
module Rev_log_cache = Raft_revlogcache

module Logic     = Raft_logic

let option_val = function | Some x -> x | None -> failwith "option_val" 

let default_configuration = {
  nb_of_server = 3;
  election_timeout = 0.1;
  election_timeout_range = 0.0;
    (* To get deterministic number for testing.
     *)
  hearbeat_timeout = 0.02;
  max_nb_logs_per_message = 10;
  log_interval_size = 5;
}

let initial_state
  ?configuration:(configuration = default_configuration)
  ~now
  id =

  Raft_logic.make_initial_state ~configuration ~now ~id ()

let now = 0.

let msg_for_server msgs id =
  match List.find (fun (_, server_id) -> id = server_id) msgs with
  | (msg, _) -> msg
  | exception Not_found -> assert(false)

let request_response ~from ~to_ ~now requests =
  let to_, responses, _ =
    let msg = msg_for_server requests (to_.id) in
    Raft_logic.handle_message to_ msg now
  in

  let now = now +. 0.001 in

  let from, msgs, _ =
    let msg = msg_for_server responses (from.id) in
    Raft_logic.handle_message from msg now
  in
  (from, to_, msgs, now)


let ()  =

  (*
   * Let's create the 3 servers of our test cluster.
   * --------------------------------------------------------------------------
   *)

  let server0 = initial_state ~now 0 in
  let server1 = initial_state ~now 1 in
  let server2 = initial_state ~now 2 in

  (*
   * All of those severs should have an election timeout randomly
   * generated between [election_timeout +/- election_timeout_range/2].
   *
   *)
  let next_event = Raft_logic.next_timeout_event server0 now in
  assert(next_event.timeout = default_configuration.election_timeout);
  assert(next_event.timeout_type = New_leader_election);

  assert(0 = server0.current_term);
  assert(0 = server1.current_term);
  assert(0 = server2.current_term);
    (* The current term initiale value is expected
     * to be 0.
     *)

  (*
   * Let's now simulate an election timeout for server0,
   * --------------------------------------------------------------------------
   *)

  let server0, msgs, notifications = Raft_logic.handle_new_election_timeout server0 now in

  assert(State.is_candidate server0);
    (* When an election timeout happens the server starts a new election
     * and become a [Candidate].
     *)

  assert([] = notifications);
    (* There was no previous Leader so no notifications
     *)

  assert(1 = server0.current_term);
    (* Part of the new election process is to increment the [current_term]
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
   * Let's propage the [Request_vote] request message from server0 to
   * server1.
   * --------------------------------------------------------------------------
   *)

  let now = now +. 0.001 in

  let server1, msgs, notifications =
    let msg = msg_for_server msgs 1 in
    Raft_logic.handle_message server1 msg now
  in

  assert(1 = server1.current_term);
    (* Server0 [Candidate] sent a higher term to server1 which is then expected to
     * update its own [current_term] and be a follower (which it was already).
     *)

  assert([] = notifications);

  begin match server1.role with
  | Follower {voted_for; current_leader; election_deadline; } -> begin
    (* Server1 should still be a follower.
     *)
    assert(voted_for = (Some 0));
    (* Because server1 did not previously vote for a candidate it should
     * grant its vote to server0
     *)
    assert(current_leader = None);
    (* Granting a vote does not guarantee that server0 will be a [Leader].
     *
     * Note that only a valid [Append_entries] request can establish the leadership
     * role of the sender. (We will this later).
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
        (* The message confirms the server1 state and decision to grant its vote
         * to server0.
         *)
    | _ -> assert(false)
    end
  | _ -> assert(false)
  end;

  (*
   * Let's now make this response communicated to the server0
   * the current [Candidate]
   * --------------------------------------------------------------------------
   *)

  let now = now +. 0.001 in
  let server0, msgs, notifications =
    let msg = msg_for_server msgs 0 in
    Raft_logic.handle_message server0 msg now
  in

  assert(State.is_leader server0);
  assert((New_leader {leader_id = 0})::[] = notifications);
    (*
     * Because a single vote is enough to reach a majority in a 3-server cluster,
     * server0 becomes a [Leader].
     *)


  assert(1 = server0.current_term);
    (*
     * Becoming a [Leader] should not affect the term. (Only a new election).
     *)

  begin match server0.role with
  | Leader {indices; _ } -> (
    assert(2 = List.length indices);
      (*
       * The leader maintain various state for each of the
       * other servers.
       *)

      List.iter (fun server_index ->

        assert(server_index.next_index = 1);
        assert(server_index.match_index = 0);
        assert(server_index.heartbeat_deadline = now +. default_configuration.hearbeat_timeout);
        assert(server_index.outstanding_request = false);

      ) indices;
  )
  | _ -> assert(false)
  end;

  assert(2 = List.length msgs);
    (*
     * Upon becoming a Leader a server must immediately
     * send an [Append_entries] request to all the other servers
     * to establish its leadership.
     *)

  List.iter (fun (msg, _ ) ->
    match msg with
    | Append_entries_request r ->
      assert(1  = r.leader_term);
      assert(0  = r.leader_id);
      assert(0  = r.prev_log_index);
      assert(0  = r.prev_log_term);
      assert([] = r.rev_log_entries);
        (*
         * We have not yet added any log to the [Leader] so
         * no new entries are sent to the other servers.
         *)
      assert(0 = r.leader_commit);
    | _ -> assert(false);
  ) msgs;

  (*
   * Let's send the [Append_entries] request to [server1].
   *
   * (Note that we delibirately ignore server2 for now).
   *
   * --------------------------------------------------------------------------
   *)

  let now = now +. 0.001 in
  let server1, msgs, notifications =
    let msg = msg_for_server msgs 1 in
    Raft_logic.handle_message server1 msg now
  in

  assert((New_leader {leader_id = 0})::[] = notifications);

  begin match server1.role with
  | Follower f -> (

    assert(f.voted_for = Some 0);
      (* [voted_for] is still assigned to server0 since the current term
       * has not changed.
       *)

    assert(f.current_leader = Some 0);
      (* [Append_entries] request indicates the leadership role of the sender.
       *
       * server1 then updates its state to keep track of the current [Leader] (ie
       * server0 in our case).
       *)

    assert(f.election_deadline = now +. default_configuration.election_timeout);
      (*
       * Because it just receive a message from the [Leader], the
       * [election_deadline] is extended for another [election_timeout] amount
       * of time.
       *)
  )
  | _ -> assert(false)
  end;

  assert(1 = List.length msgs);
    (*
     * The server1 is expected to send a single response back to the
     * sender (ie server0).
     *)

  begin match msgs with
  | (Append_entries_response r , 0) :: [] -> (
    assert(r.receiver_id = 1);
    assert(r.receiver_term = 1);
    assert(r.result = Success {receiver_last_log_index = 0});
  )
  | _ -> assert(false)
  end;

  (*
   * Let's now assume that server2 has not received the previous
   * [Request_vote] request messages from server0.
   *
   * It will logically starts an election and starts sending [Request_vote]
   * request for its own candidacy.
   *
   * --------------------------------------------------------------------------
   *)

  let server2, request_vote_msgs, notifications=
    Raft_logic.handle_new_election_timeout server2 now
  in

  assert([] = notifications);
    (* Server2 never got an [Append_entries] request which would have
     * established server0 leadership. Therefore as far as server2 is
     * concerned there were no leaders.
     *)

  assert(State.is_candidate server2);
  assert(1 = server2.current_term);
  assert(2 = List.length request_vote_msgs);

  List.iter (fun (msg, _) ->
    match msg with
    | Request_vote_request r -> (
      assert(r.candidate_term = 1);
      assert(r.candidate_id = 2);
      assert(r.candidate_last_log_index = 0);
      assert(r.candidate_last_log_term = 0);
    )
    | _ -> assert(false)
  ) request_vote_msgs;

  (*
   * Let's propagate the [Request_vote] request from server2 to server1
   *
   * (Note that server1 has already voted for server0 during that term).
   *
   * --------------------------------------------------------------------------
   *)

  let now = now +. 0.001 in
  let server1, msgs, notifications =
    let msg = msg_for_server request_vote_msgs 1 in
    Raft_logic.handle_message server1 msg now
  in

  assert([] = notifications);

  begin match server1.role with
  | Follower f -> (
    assert(f.voted_for = Some 0);
    assert(f.current_leader = Some 0);
      (* server0 state is unaffected by this new [Candidate] for this
       * term.
       *)
  )
  | _ -> assert(false);
  end;

  begin match msgs with
  | (Request_vote_response r, 2)::[] -> (
    assert(r.voter_id = 1);
    assert(r.voter_term = 1);
    assert(r.vote_granted = false);
      (*
       * server1 has already voted for server0 in this election
       * term, so it should deny its vote to server2.
       *)
  )
  | _ -> assert(false)
  end;

  (*
   * Next we are communicating the response from server1
   * to server2 and make sure that it does not become a [Leader]
   * but rather continue to be a [Candidate].
   *
   * --------------------------------------------------------------------------
   *)

  let now = now +. 0.001 in

  let server2, msgs, notifications =
    let msg = msg_for_server msgs 2 in
    Raft_logic.handle_message server2 msg now
  in

  assert(State.is_candidate server2);
  assert([] = notifications);
    (*
     * Despite the vote not being granted by server1, server2
     * should continue to be a [Candidate] until either
     *
     * - a [Request_vote] response with a granted vote is replied
     *   by another server
     *
     * - new election timeout elapsed in this case it will start
     *   a new election.
     *
     * - a vald [Leader] sends an [Append_entries] request, in which
     *   case it will become a [Follower].
     *)

  assert(1 = server2.current_term);
    (*
     * No new election should have been started.
     *)

  assert([] = msgs);
    (*
     * server2 is still a candidate but for the time being
     * has no message to send to server1.
     *)

  (*
   * Let's now propagate the [Request_vote] request from server2 to
   * server0 which is the current [Leader] for the term 1.
   *
   * --------------------------------------------------------------------------
   *)

  let server0, msgs, notifications =
    let msg = msg_for_server request_vote_msgs 0 in
    Raft_logic.handle_message server0 msg now
  in

  assert(State.is_leader server0);
  assert([] = notifications);
    (*
     * Server0 is still a [Leader] and should not be affected
     * by a Candidate for the same term.
     *
     * (This would be different if the Candidate was for a later term)
     *)

  begin match msgs with
  | (Request_vote_response r, 2) :: [] -> (

    assert(r.voter_id = 0);
    assert(r.voter_term = 1);
    assert(r.vote_granted = false);
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
   *
   * --------------------------------------------------------------------------
   *)

  let server2, msgs, notifications =
    let msg = msg_for_server msgs 2 in
    Raft_logic.handle_message server2 msg now
  in

  assert(State.is_candidate server2);
  assert([] = notifications);
    (*
     * Yes despite all other server denying their vote, server2
     * is still a [Candidate]. It should not take any further
     * action until its election timeout has elapsed.
     *)

  assert([] = msgs);
    (* No new message from server2 for this elections. All [Request_vote]
     * requests have been sent and the unsucessful responses received.
     *)

  (*
   * Since the heartbeat timeout is usually much shorter
   * than a new election timeout, it's likely that server0
   * (ie the current [Leader]) will send heartbeats to the other
   * 2 servers.
   *
   * --------------------------------------------------------------------------
   *)

  (*
   * First let's make sure that even because the heartbeat deadline
   * for any server has not been reached, a heartbeat timeout should not
   * trigger new messages.
   *)

  let server0, msgs = Raft_logic.handle_heartbeat_timeout server0 now in

  assert([] = msgs);
    (*
     * Heartbeat messages are only sent to servers which have not recveived a
     * message for at least the [hearbeat_timeout] amount of time.
     * It's not the case since [hearbeat_timeout] is 0.02.
     *)

  let now = now +. default_configuration.hearbeat_timeout in

  let server0, hb_msgs = Raft_logic.handle_heartbeat_timeout server0 now in

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
   * which is still a [Candidate].
   *
   * Upon receiving the [Append_entries] request from a [Leader]
   * with a current term at least equal or superior to itself, it should
   * convert to a [Follower].
   *)

  let now = now +. 0.001 in
  let server2, msgs, notifications =
    let msg = msg_for_server hb_msgs 2 in
    Raft_logic.handle_message server2 msg now
  in

  assert(State.is_follower server2);
  assert(1 = server2.current_term);
  assert((New_leader {leader_id = 0})::[] = notifications);
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
  (*
   * Let's communicate the heartbeat response from server2 (now Follower) to
   * server0 (Leader).
   *
   * --------------------------------------------------------------------------
   *)

  let server0, msgs, notifications =
    let msg = msg_for_server msgs 0 in
    Raft_logic.handle_message server0 msg now
  in
  assert([] = notifications);
  assert([] = msgs);

  (*
   * Server1 was already a follower and aware of server0 leadership
   * the new heartbeat would not change that.
   *
   * --------------------------------------------------------------------------
   *)

  let server1, msgs, notifications=
    let msg =  msg_for_server hb_msgs 1 in
    Raft_logic.handle_message server1 msg now
  in

  assert(State.is_follower server1);
   (* No change in the role, server0 is a valid [Leader],
    * server1 stays a [Follower].
    *)

  assert([] = notifications);
    (* The heartbeat message from server0 did not bring new
     * information (ie server1 already new server0 was the
     * [Leader].)
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
   * [outstanding_request] for each server.
   *
   * The [Leader] would then avoid sending new [Append_entries] requests
   * which already have an outstanding requests. However if a heartbeat timeout
   * has been reached, a new [Append_entries] request will be sent no matter
   * what.
   *
   *
   * --------------------------------------------------------------------------
   *)

  let server0, msgs, notifications =
    let msg = msg_for_server msgs 0 in
    Raft_logic.handle_message server0 msg now
  in

  assert(State.is_leader server0);
  assert([] = notifications);
  assert([] = msgs);

  let now = now +. 0.001 in

  (*
   * Let's now add a log entry to the [Leader] which is expected to trigger
   * the corresponding [Append_entries] requests to the other servers.
   *
   * --------------------------------------------------------------------------
   *)

  let new_log_response =
    let data = Bytes.of_string "Message1" in
    Raft_logic.handle_add_log_entries server0 [(data, "a")] now
  in

  let server0, data1_msgs =
    let open Raft_logic in
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
     * The new log entry should have been appended to the current empty log.
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
     * Both [Follower]s have no outstanding request and have also less
     * log entries than the [Leader], therefore they
     * should get a new [Append_entries] request message with the new
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
   * [log_entry] to server1.
   *
   * --------------------------------------------------------------------------
   *)

  let server1, msgs, notifications =
    let msg = msg_for_server data1_msgs 1 in
    Raft_logic.handle_message server1 msg now
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
  assert([] = notifications);
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
   *
   * --------------------------------------------------------------------------
   *)

  let server0, msgs, notifications=
    let msg = msg_for_server msgs 0  in
    Raft_logic.handle_message server0 msg now
  in

  assert(State.is_leader server0);
  assert(1 = server0.commit_index);
  assert((Committed_data {ids = ["a"]})::[] = notifications);
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
   *
   * - On the next heartbeat timeout server2 should then receive an
   *   [Append_entries_request] with both log entry.
   *
   *)

  let now = now +. 0.001 in

  let new_log_response =
    let data = Bytes.of_string "Message2" in
    Raft_logic.handle_add_log_entries server0 [(data,"b")] now
  in

  let server0, data2_msg =
    let open Raft_logic in
    match new_log_response with
    | Delay | Forward_to_leader _ -> assert(false)
    | Appended (state, msgs) -> (state, msgs)
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
     * be sent an additional request. Only server1 should receive
     * an [Append_entries] request.
     *
     *)

  begin match data2_msg with
  | (Append_entries_request r, 1) :: []  -> (
    assert(r.leader_term = 1);
    assert(r.leader_id = 0);
    assert(r.prev_log_index = 1);
      (*
       * server0 [Leader] knows that the server1 has successfully
       * replicated the log entry with [index = 1] and therefore
       * will send [prev_log_index] with value 1.
       *)
    assert(r.prev_log_term = 1);
    assert(1 = List.length r.rev_log_entries);
      (* Only the last [log_entry] should be sent to that follower,
       * since the first [log_entry] was already replicated.
       *)
    assert(r.leader_commit = 1);
  )
  | _ -> assert(false)
  end;

  let now = now +. 0.001 in

  (*
   * Let's propagate this [Append_entries] request to server1 for
   * replication.
   *
   * --------------------------------------------------------------------------
   *)

  let server1, msgs, notifications =
    let msg = msg_for_server data2_msg 1 in
    Raft_logic.handle_message server1 msg now
  in

  assert(State.is_follower server1);


  assert(2 = List.length server1.log);
    (*
     * server1 should have correctly replicated the log
     * with [index = 2].
     *)

  assert((Committed_data {ids = ["a"]})::[] = notifications);
  assert(1 = server1.commit_index);
    (*
     * The [Append_entries] request contained the [commit_index]
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
     (* server1 notifies the [Leader] about the last log it has
      * replicated.
      *)
  )
  | _ -> assert(false)
  end;

  (*
   * Let's now propagate that successful [Append_entries] response to
   * server0.
   *
   * --------------------------------------------------------------------------
   *)

  let now = now +. 0.001 in

  let server0, msgs, notifications=
    let msg = msg_for_server msgs 0  in
    Raft_logic.handle_message server0 msg now
  in

  assert(State.is_leader server0);

  assert((Committed_data {ids = ["b"]})::[] = notifications);
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
   * --------------------------------------------------------------------------
   *)

  let now = now +. default_configuration.hearbeat_timeout in

  let server0, msgs = Raft_logic.handle_heartbeat_timeout server0 now in

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


  (*
   * Let's send the heartbeat msg to server1. As previously asserted
   * this heartbeat message should contain no additional log but the commit_index (set to
   * 2) will be new information to server1.
   *
   * --------------------------------------------------------------------------
   *)

  let server1, server1_response, notifications =
    let msg = msg_for_server msgs 1 in
    Raft_logic.handle_message server1 msg now
  in

  assert(State.is_follower server1);

  assert((Committed_data {ids = ["b"]})::[] = notifications);
  assert(2 = server1.commit_index);
   (*
    * server1 is updating its commit index based on latest
    * [leader_commit] value of 2 in the request it received.
    *)

  assert(1 = List.length server1_response);
   (*
    * Only a single response is expected.
    *)


  (*
   * Let's send the heartbeat msg to server2. As previously asserted
   * this heartbeat message contains 1 log entry since the previous
   * [Append_entries] request from the [Leader] server0 was no communicated
   * to it.
   *
   * --------------------------------------------------------------------------
   *)

  let server2, server2_response, notications =
    let msg = msg_for_server msgs 2 in
    Raft_logic.handle_message server2 msg now
  in

  assert(State.is_follower server2);
  assert((Committed_data {ids = ["a"]})::[] = notications);
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

  (*
   * Let's communicate the 2 successful [Append_entries] response back
   * to the [Leader] server0.
   *
   * --------------------------------------------------------------------------
   *)

  let server0, msg_to_send, notications =

    let server0, msg_to_send, notications=
      let msg = msg_for_server server1_response 0  in
      Raft_logic.handle_message server0 msg now
    in
    assert([] = msg_to_send);
    assert([] = notications);
      (* Server1 has replicated the 2 logs it has nothing
       * left.
       *)

    let msg = msg_for_server server2_response 0  in
    Raft_logic.handle_message server0 msg now
  in

  assert(State.is_leader server0);
  assert(2 = server0.commit_index);
  assert(2 = List.length server0.log);
  assert([] = notications);

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

  let server2, msg_to_send, notications =
    let msg = msg_for_server msg_to_send 2 in
    Raft_logic.handle_message server2 msg now
  in

  assert(State.is_follower server2);
  assert(2 = List.length server2.log);
    (* server2 has correctly replicated the [log_entry] with
     * [index = 2].
     *)

  assert(2 = server2.commit_index);
  assert((Committed_data {ids = ["b"]})::[] = notifications);
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

  let server0, msgs, notifications =
    let msg = msg_for_server msg_to_send 0  in
    Raft_logic.handle_message server0 msg now
  in

  assert([] = msgs);
  assert([] = notifications);
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
   *
   * --------------------------------------------------------------------------
   *)

  (*
   * let's add 3rd [log_entry].
   *
   * --------------------------------------------------------------------------
   *)

  let now = now +. 0.002 in

  let new_log_response =
    let data = Bytes.of_string "Message3" in
    Raft_logic.handle_add_log_entries server0 [(data, "c") ] now
  in


  let server0, msgs =
    match new_log_response with
    | Raft_logic.Appended (server, msgs) -> (server, msgs)
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
   * Let's send the [Append_entries] request to server1 Only.
   *
   * --------------------------------------------------------------------------
   *)

  let server1, _, notifications =
    let msg = msg_for_server msgs 1 in
    Raft_logic.handle_message server1 msg now
  in

  assert(State.is_follower server1);
  assert(3 = List.length server1.log);
    (*
     * The 3rd [log_entry] is correctly replicated.
     *)

  assert([] = notifications);
  assert(2  = server1.commit_index);
    (*
     * No change since the [commit_index] was still
     * 2 in the [Append_entries] request.
     *)

  (*
   * Now let's assume server0 [Leader] has crashed.
   *
   * We'll make [server2] starts a new election!
   *
   * --------------------------------------------------------------------------
   *)

  let now = now +. default_configuration.election_timeout  in

  let server2, msgs, notifications =
    Raft_logic.handle_new_election_timeout server2 now
  in

  assert(State.is_candidate server2);
    (*
     * Server2 started a new election.
     *)

  assert(No_leader::[] = notifications);
    (*
     * Becoming a candidate and incrementing the terms
     * means that there are no longer a [Leader].
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


  (*
   * Let's now communicate the [Request_vote] requset to server1
   *
   * --------------------------------------------------------------------------
   *)

  let now = now +. 0.001 in

  let server1, msgs, notification =
    let msg = msg_for_server msgs 1 in
    Raft_logic.handle_message server1 msg now
  in

  assert(State.is_follower server1);
  assert(2 = server1.current_term);
    (*
     * The sender (ie server2) term was greater than the current
     * term of server1, it should then
     * increments its current term.
     *)

  assert(No_leader::[] = notification);
    (*
     * The change of term means that there are no current [Leader]
     * for it yet.
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
   *
   * --------------------------------------------------------------------------
   *)

  let now = now +. default_configuration.election_timeout in

  let server1, msgs, notifications =
    Raft_logic.handle_new_election_timeout server1 now
  in

  assert([] = notifications);

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

  (*
   * Let's communicate the [Request_vote] from server1 to server2.
   * (server0 is still not available)
   *
   * --------------------------------------------------------------------------
   *)

  let server2, msgs, notifications =
    let msg = msg_for_server msgs 2 in
    Raft_logic.handle_message server2 msg now
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

  assert([] = notifications);
    (*
     * Server2 already knew there was no [Leader] since it was a candidate
     * in the previous term and never got elected.
     *)

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
   *
   * --------------------------------------------------------------------------
   *)

  let server1, msgs, notifications =
    let msg = msg_for_server msgs 1 in
    Raft_logic.handle_message server1 msg now
  in

  assert((New_leader {leader_id = 1})::[] = notifications);
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

  (*
   * Let's now propagate the first [Append_entries] from server1  which shall
   * establish its leadership.
   *
   * --------------------------------------------------------------------------
   *)

  let now = now +.  0.001 in

  let server2, msgs, notifications =
    let msg = msg_for_server msgs 2 in
    Raft_logic.handle_message server2 msg now
  in

  assert((New_leader {leader_id = 1})::[] = notifications);
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
   * Let's propagate that unsuccessful response back to server1.
   *
   * --------------------------------------------------------------------------
   *)

  let now = now +. 0.001 in

  let server1, msgs, notifications =
    let msg = msg_for_server msgs 1 in
    Raft_logic.handle_message server1 msg now
  in

  assert(State.is_leader server1);
  assert(3 = server1.current_term);

  assert([] = notifications);

  assert(1 = List.length msgs);
  assert(2 = server1.commit_index);
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
     (* The missing 3rd log entry is now part of the
      * request for server2 to catch up
      *)
    assert(r.leader_commit = 2);
  )
  | _ -> assert(false)
  end;

  (*
   * Let's communicate this new [Append_entries] (with the 3rd log entry) request
   * to server1
   *
   * --------------------------------------------------------------------------
   *)

  let now = now +. 0.001 in

  let server2, msgs, notifications =
    let msg = msg_for_server msgs 2 in
    Raft_logic.handle_message server2 msg now
  in

  assert(State.is_follower server2);
  assert(3 = server2.current_term);

  assert(3 = List.length server2.log);
    (*
     * server2 has correctly replicated the 3rd [log_entry].
     *)

  assert([] = notifications);
  assert(2  = server2.commit_index);
    (*  The 3rd log while succesfully replicated is not  yet
     *  commited.
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

  (*
   * Let's communicate the successful [Append_entries] response back to
   * server1.
   *
   * --------------------------------------------------------------------------
   *)

  let now = now +. 0.001  in

  let server1, msgs, notifications =
    let msg = msg_for_server msgs 1 in
    Raft_logic.handle_message server1 msg now
  in

  assert(State.is_leader server1);
  assert(3 = server1.current_term);

  assert((Committed_data {ids = ["c"]})::[] = notifications);
  assert(3 = server1.commit_index);
    (*
     * The 3rd [log_entry] has been replicated one one other
     * server than the [Leader]; this makes a majority and therefore
     * indicates that [commit_index] can be set to 3.
     *)

  assert([] = msgs);


  (*
   * Let's now add 2 log entry at a time which would
   * make a total of 5 log entries.
   *
   * We have set the log_interval_size to be 5, which mean that
   * a new log interval should be added to the cache after those
   * 2 entries were commited.
   *
   *)

  let new_log_response =
    let datas = [
      (Bytes.of_string "Message4", "d");
      (Bytes.of_string "Message5", "e");
    ] in
    Raft_logic.handle_add_log_entries server1 datas now
  in

  let server1, data45_msgs =
    let open Raft_logic in
    match new_log_response with
    | Appended (state, msgs) -> (state, msgs)
    | Delay | Forward_to_leader _ -> assert(false)
  in

  assert(5 = List.length server1.log);
  assert(5 = State.last_log_index server1);

  assert(3 = server1.commit_index);
    (* The 2 logs have not been commited.
     *)

  assert(None = server1.global_cache);
    (* Only 3 logs have been commited, this is less than
     * the [log_interval_size] so the global_cache
     * is still empty.
     *)

  assert(2 = List.length data45_msgs);

  List.iter (fun (msg, _) ->

    match msg with
    | Append_entries_request r -> (
      let {
        leader_term;
        leader_id;
        prev_log_index;
        prev_log_term;
        rev_log_entries;
        leader_commit;
      } =  r in
      assert(3 = leader_term);
      assert(1 = leader_id);
      assert(prev_log_index = 3);
      assert(prev_log_term  = 1);
      assert(2 = List.length rev_log_entries);
      assert(3 = leader_commit);
    )
    | _ -> assert(false);
  ) data45_msgs;

  (*
   * Server0 is still crashed, server2 is good so let's send the latest [Append_entries]
   * request.
   *
   * --------------------------------------------------------------------------
   *)

  let now = now +. 0.001  in

  let server2, msgs, notifications =
    let msg = msg_for_server data45_msgs 2 in
    Raft_logic.handle_message server2 msg now
  in

  assert(State.is_follower server2);
  assert(3 = server2.current_term);

  assert(5 = List.length server2.log);
    (* The last 2 logs where succesfully replicated
     *)
  begin match server2.log with
  | {data; _ } :: _ ->
    assert((Bytes.of_string "Message5") = data);
    (* Let's make sure the order was properly replicated.
     *)
  | _ -> assert(false);
  end;

  assert(3 = server2.commit_index);
    (* Replicated from server1, previous one was 2 so we
     * can expect a notification.
     *)

  assert(1 = List.length notifications);
  begin match notifications with
  | (Committed_data {ids = ["c"] }) :: [] -> ()
  | _ -> assert(false);
  end;

  begin match msgs with
  | (Append_entries_response r, 1) :: [] -> (
    let {
      receiver_id;
      receiver_term;
      result;
    } = r in
    assert(2 = receiver_id);
    assert(3 = receiver_term);
    assert(Success {receiver_last_log_index = 5} = result);
  )
  | _ -> assert(false);
  end;

  (*
   * Let's now communicate that succesful response to server1
   * which should now have a commit index
   *
   * --------------------------------------------------------------------------
   *)

  let now = now +. 0.001  in

  let server1, msgs, notifications =
    let msg = msg_for_server msgs 1 in
    Raft_logic.handle_message server1 msg now
  in

  assert(5 = server1.commit_index);
  List.iter (fun (msg, id) ->
    Format.printf "%i : %a\n" id pp_message msg
  ) msgs;
  assert([] = msgs);
  assert(1 = List.length notifications);

  begin match notifications with
  | (Committed_data {ids}) :: [] ->
    assert(2 = List.length ids);
  | _ -> assert(false);
  end;

  (*
   * The test of the cache is not something that a client API should
   * rely upon, therefore those test might change in the future.
   *
   * --------------------------------------------------------------------------
   *)
  let new_log_response =
    let datas = [
      (Bytes.of_string "Message6", "f");
    ] in
    Raft_logic.handle_add_log_entries server1 datas now
  in

  let server1, data6_msgs =
    let open Raft_logic in
    match new_log_response with
    | Appended (state, msgs) -> (state, msgs)
    | Delay | Forward_to_leader _ -> assert(false)
  in

  assert(5 = server1.commit_index);
  assert(1 = List.length data6_msgs);
    (* Note that server0 still has an outstanding requests so
     * new ones are sent.
     *)

  begin match server1.global_cache with
  | None -> assert(false)
  | Some (Interval {prev_index; prev_term; rev_log_entries; last_index}) ->
    (* The commit_index is 5 and the previously cached index is 0 (ie no cache),
     * since our configure log_interval_size is 5, a new [LogInterval] should
     * be appended.
     *
     * Since the cache was previously empty then it should just be a single
     * interval with the 5 log entries in it.
     *)
    assert(0 = prev_index);
    assert(0 = prev_term);
    begin match rev_log_entries with
    | Expanded {entries} ->
      assert(5 = List.length entries);
      assert((List.hd entries).index = 1);
        (* Make sure that the entries are stored in the reverse order
         * (ie earlier first).
         *)
    | _ -> assert(false)
    end;
    assert(5 = last_index);
      (* The last index is included in the cache (unlike prev_index).
       *)
  | Some (Append _) -> assert(false)
  end;

  let now = now +. default_configuration.hearbeat_timeout in

  let new_log_response =
    let datas = [
      (Bytes.of_string "Message7", "g");
      (Bytes.of_string "Message8", "h");
      (Bytes.of_string "Message9", "i");
      (Bytes.of_string "MessageA", "j");
      (Bytes.of_string "MessageB", "k");
      (Bytes.of_string "MessageC", "l");
    ] in
    Raft_logic.handle_add_log_entries server1 datas now
  in

  let server1, data7toC_msgs =
    let open Raft_logic in
    match new_log_response with
    | Appended (state, msgs) -> (state, msgs)
    | Delay | Forward_to_leader _ -> assert(false)
  in

  assert(12 = List.length server1.log);
  assert(5  = server1.commit_index);
   (* we did not do the [Append_entries] request for the 6th log
    * entries so no commit change.
    *)

  assert(2 = List.length data7toC_msgs);
   (* Both server0/server2 have oustanding requests as far as server
    * is concerned, however we are past the [hearbeat_timeout] so
    * new messages must be sent.
    *)

  (* TODO add checks for the msgs *)

  (*
   * Let's send the [Append_entries] to server2
   *)

  let server2, msgs, notifications =
    let msg = msg_for_server data7toC_msgs 2 in
    Raft_logic.handle_message server2 msg now
  in

  assert(State.is_follower server2);
  assert(3 = server2.current_term);

  assert(6 = List.length server2.log);
    (* TODO explain cache
     *)

  assert(5 = server2.commit_index);
  assert(1 = List.length notifications);
  begin match notifications with
  | (Committed_data {ids})::[] -> assert(2 = List.length ids)
  | _ -> assert(false)
  end;
    (* commit index updated to match server1.
     * and therefore a new notification with
     * 2 entries is expected
     *)

  assert(1 = List.length msgs);
    (* Single response to server1
     *)

  begin match msgs with
  | (Append_entries_response r, 1)::[] ->

    let {
      receiver_id;
      receiver_term;
      result;
    } = r in
    assert(2 = receiver_id);
    assert(3 = receiver_term);
    assert(Success {receiver_last_log_index = 6} = result);
  | _ -> assert(false)
  end;

  let server1, msgs, notifications =
    let msg = msg_for_server msgs 1 in
    Raft_logic.handle_message server1 msg now
  in

  assert(State.is_leader server1);
  assert(6 = server1.commit_index);

  assert(1 = List.length msgs);
    (* Only one message back to server2, server1 still has an outstanding
     * request to server0 so no point in sending another one until
     * timeout time has elapsed (not the case right now).
     *)
  begin match msgs with
  | (Append_entries_request r, 2)::[] ->
    let {
      leader_term;
      leader_id;
      prev_log_index;
      prev_log_term;
      rev_log_entries;
      leader_commit;
    } = r in
    assert(3 = leader_term);
    assert(1 = leader_id);
    assert(prev_log_index = 6);
    assert(prev_log_term = 3);
    assert(6 = leader_commit);
    assert(6 = List.length rev_log_entries);
  | _ -> assert(false);
  end;

  let server2, msgs, notifications =
    let msg = msg_for_server msgs 2 in
    Raft_logic.handle_message server2 msg now
  in

  assert(12 = List.length server2.log);
    (* All log replicated *)

  assert(6 = server2.commit_index);
  assert(1 = List.length notifications);
    (* Newly commited information means that there is a notification
     * about commited data.
     *)

  assert(1 = List.length msgs);
    (* Single response to server1 *)

  begin match msgs with
  | (Append_entries_response r, 1)::[] -> (
    let {
      receiver_id;
      receiver_term;
      result;
    } = r in
    assert(2 = receiver_id);
    assert(3 = receiver_term);
    assert(Success {receiver_last_log_index = 12} = result);
  )
  | _ -> assert(false);
  end;

  (*
   * Let's communicate the successful response from server2 to
   * server1.
   *
   * --------------------------------------------------------------------------
   *)

  let server1, msgs, notifications =
    let msg = msg_for_server msgs 1 in
    Raft_logic.handle_message server1 msg now
  in

  assert(12 = server1.commit_index);
  assert(1  = List.length notifications);
    (* New commited logs means notification back!
     *)

  (* The cache should contain 2 entries. This means that
   * the top level node of the rope should be an Append with
   * both lhs and rhs to be Interval nodes.
   *
   * The 2 interval nodes should have continuous (but not replicated
   * data).
   *)
  begin match server1.global_cache with
  | None -> assert(false)
  | Some (Append a) ->
    let {
      height;
      lhs;
      rhs;
      last_index;
    } = a in
    assert(1 = height);
    assert(last_index = 12);
    begin match lhs, rhs with
    | Interval l, Interval r ->
      let { prev_index; prev_term; last_index; rev_log_entries; } = l in
      assert(prev_index = 0);
      assert(prev_term  = 0);
      assert(last_index = 5);
      begin match rev_log_entries with
      | Expanded {entries} -> assert(5 = List.length entries);
      | _ -> assert(false);
      end;

      let { prev_index; prev_term; last_index; rev_log_entries; } = r in
      assert(prev_index = 5);
      assert(prev_term  = 3);
      assert(last_index = 12);
      begin match rev_log_entries with
      | Expanded {entries} -> assert(7 = List.length entries);
      | _ -> assert(false);
      end;
    | _ -> assert(false);
    end;
  | _ -> assert(false);
  end;
  
  let now = now  +. 0.001 in

  (*
   * Let's now add 8 logs which would make a total of 20
   *
   * -----------------------------------------------------------------------
   *)
  let new_log_response =
    let datas = [
      (Bytes.of_string "MessageD", "n");
      (Bytes.of_string "MessageE", "o");
      (Bytes.of_string "MessageF", "p");
      (Bytes.of_string "MessageG", "q");
      (Bytes.of_string "MessageH", "r");
      (Bytes.of_string "MessageI", "s");
      (Bytes.of_string "MessageJ", "y");
      (Bytes.of_string "MessageK", "u");
    ] in
    Raft_logic.handle_add_log_entries server1 datas now
  in

  let server1, data20_msgs =
    let open Raft_logic in
    match new_log_response with
    | Appended (state, msgs) -> (state, msgs)
    | Delay | Forward_to_leader _ -> assert(false)
  in

  let server1, server2, msgs, now =
    request_response ~from:server1 ~to_:server2 ~now data20_msgs
  in
  assert(20 = server1.commit_index);
  assert(12 = server2.commit_index);
  assert(20 = List.length server1.log);
  assert(20 = List.length server2.log);


  (* Adding 8 elements have now been commited since the last cached
   * index... this is > 5 and so a new [LogInterval] should be added with
   * those 8 entries.
   *
   * Based on the insert logic the global cache rope data structure should
   * look like this
   *
   *                Append
   *              /      \
   *           Append     Interval
   *         /       \     ]12;20]
   *    Interval   Interval
   *     ]0; 5]     ]5;12]
   *
   *)

  begin match server1.global_cache with
  | None -> assert(false)
  | Some (Append {rhs; lhs; last_index; height})  ->
    assert(2  = height);
    assert(20 = last_index);
    begin match lhs with
    | Append {rhs; lhs ; last_index; height} ->
      assert(12 = last_index);
      assert(1  = height);
    | _ -> assert(false);
    end;
    begin match rhs with
    | Interval {prev_index; prev_term; last_index; rev_log_entries} ->
      assert(12 = prev_index);
      assert(3  = prev_term);
      assert(20 = last_index);
      begin match rev_log_entries with
      | Expanded {entries} -> assert(8 = List.length entries)
      | _ -> assert(false)
      end;
    | _ -> assert(false);
    end;
  | _ -> assert(false);
  end;


  (*
   * Compaction test!
   *
   * The compaction algorithm will look at where the next 
   * indices for all followers are with respect to the intervals 
   * of the global cache. 
   *
   * server2 next index = 21 (it has replicated all logs) 
   * server0 next index = 4  (server0 was down ever since server1
   *   became leader and therefore its next index is still set to the
   *   initial value : log size @ election time + 1
   *
   * The compaction will then recommend that the log interval to which 
   * next indices belong to are kept expanded. Additionally the log intervals 
   * following those should also be kept expanded to allow some headroom. 
   *
   * In our current global cache:
   * - next index 21 : does not belong to any log interval -> no effect
   * - next index 4  : belong to the right most log interval -> both ]0;5] and 
   *   ]5;12] should be kept expanded 
   * -> ]12;20] should be compacted.
   *
   *)
  let {to_be_expanded = e; to_be_compacted = c} = State.compaction server1 in  

  assert(e = []);
    (* No compaction required *)
  assert(1 = List.length c); 

  begin match c with
  | {prev_index = 12; prev_term = 3; last_index = 20; _ }::[] -> ()
  | _ -> assert(false)
  end;

  let () =

    (* This section gradually compact all the logs from left (earlier) to 
     * right (later) and make sure the compaction
     * algorithm works well.
     *)

    let f li= {li with rev_log_entries = Compacted {record_id = "test"}} in 
    let server1 = 
      let global_cache = Rev_log_cache.replace ~prev_index:0 ~f server1.global_cache in 
      {server1 with global_cache} 
    in  
    let {to_be_expanded = e; to_be_compacted = c} = State.compaction server1 in  
    assert(1 = List.length e); 
      (* The log interval that we have intentionally compacted above
       * is correctly selected for expansion.
       *)
    assert(1 = List.length c);
      (* The log interval ]12;20] should still be compacted. 
       *)
    
    let server1 = 
      let global_cache = Rev_log_cache.replace ~prev_index:5 ~f server1.global_cache in 
      {server1 with global_cache} 
    in  
    let {to_be_expanded = e; to_be_compacted = c} = State.compaction server1 in  
    assert(2 = List.length e); 
    assert(1 = List.length c); 
    
    let server1 = 
      let global_cache = Rev_log_cache.replace ~prev_index:12 ~f server1.global_cache in 
      {server1 with global_cache} 
    in  
    
    let {to_be_expanded = e; to_be_compacted = c} = State.compaction server1 in  
    assert(2 = List.length e); 
    assert(0 = List.length c); 
      (* The log interval ]12;20] is no longer required to be compacted
       *)
  in 

  ()
