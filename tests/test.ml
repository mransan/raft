[@@@ocaml.warning "-45"]

open Raft_types
open Raft_log

module Types = Raft_types
module Candidate = Raft_helper.Candidate
module Follower = Raft_helper.Follower
module Leader = Raft_helper.Leader
module Timeout_event = Raft_helper.Timeout_event
module Log = Raft_log

module Logic = Raft_logic

let default_configuration = {
  nb_of_server = 3;
  election_timeout = 0.1;
  election_timeout_range = 0.0;
    (* To get deterministic number for testing.
     *)
  hearbeat_timeout = 0.02;
  max_nb_logs_per_message = Number 10;
  max_log_size = {
    upper_bound = 7; 
    lower_bound = 5; 
  }
}

let recent_log_length {log = {recent_entries; _ }; _ } =
  IntMap.cardinal recent_entries

let recent_log_hd {log = {recent_entries; _ }; _ } =
  snd @@  IntMap.max_binding recent_entries 

let initial_state ~now server_id  =
  let configuration = default_configuration in 
  Raft_logic.init ~configuration ~now ~server_id ()

let now = 0.

let msg_for_server msgs id =
  match List.find (fun (_, server_id) -> id = server_id) msgs with
  | (msg, _) -> msg
  | exception Not_found -> assert(false)

type t = {
  server0 : state; 
  server1 : state; 
  server2 : state; 
}

let init () = {
  server0 = initial_state ~now 0;
  server1 = initial_state ~now 1;
  server2 = initial_state ~now 2;
}

(* This part of the test will simulate the successful 
 * election of server0. Server1 will grant its vote during the election, 
 * while server2 is kept disconnected from server0 and does not receive
 * any message. 
 *
 * At the end of this function, server0 is the leader, server1 is 
 * a follower of server0 and server2 is still a follower not aware 
 * than an election occured. *)
let election_1 {server0; server1; server2} now = 

  (* All of those severs should have an election timeout randomly
   * generated between [election_timeout +/- election_timeout_range/2]. *)
  let next_event = Raft_logic.next_timeout_event server0 now in
  assert(next_event.timeout = default_configuration.election_timeout);
  assert(next_event.timeout_type = New_leader_election);

  assert(0 = server0.current_term);
  assert(0 = server1.current_term);
  assert(0 = server2.current_term);
    (* The current term initiale value is expected
     * to be 0.  *)

  (*
   * Server0 reach election timeout and kicks off the election 
   * -------------------------------------------------------------------
   *)

  let {
    Logic.state = server0;
    messages_to_send = msgs; 
    leader_change; _} = Raft_logic.handle_new_election_timeout server0 now in

  assert(Types.is_candidate server0);
    (* When an election timeout happens the server starts a new election
     * and become a [Candidate].  *)

  assert(None = leader_change);
    (* There was no previous Leader so no leader_change *)

  assert(1 = server0.current_term);
    (* Part of the new election process is to increment the [current_term] *)

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
     * of 3, 2 messages should be sent from  by the new [Candidate].  *)

  (*
   * Send Request_vote_request Server0 -> Server1 
   * -----------------------------------------------------------------------
   *)

  let now = now +. 0.001 in

  let res = 
    let msg = msg_for_server msgs 1 in
    Raft_logic.handle_message server1 msg now
  in

  let {
    Logic.state = server1; 
    messages_to_send = msgs; 
    leader_change;
    committed_logs; 
    added_logs; 
    deleted_logs;
  } = res in

  assert(1 = server1.current_term);
    (* Server0 [Candidate] sent a higher term to server1 which is then 
     * expected to update its own [current_term] and be a follower 
     * (which it was already).  *)

  assert(None = leader_change);
  assert([] = committed_logs);
  assert([] = added_logs);
  assert([] = deleted_logs);

  begin match server1.role with
  | Follower {voted_for; current_leader; election_deadline; } -> begin
    (* Server1 should still be a follower.  *)

    assert(voted_for = (Some 0));
    (* Because server1 did not previously vote for a candidate it should
     * grant its vote to server0 *)
    assert(current_leader = None);
    (* Granting a vote does not guarantee that server0 will be a [Leader].
     *
     * Note that only a valid [Append_entries] request can establish the leadership
     * role of the sender. (We will this later).  *)
    assert(election_deadline = now +. default_configuration.election_timeout);
    (* Election deadline should be updated. *)
  end
  | _ -> assert(false)
  end;

  begin match msgs with
  | (msg, 0)::[]  ->
      (* A single response to server 0 is expected from server1.  *)
    begin match msg with
    | Request_vote_response r ->
      assert(r.voter_id = 1);
      assert(r.voter_term = 1);
      assert(r.vote_granted = true);
        (* The message confirms the server1 state and decision to grant its vote
         * to server0.  *)
    | _ -> assert(false)
    end
  | _ -> assert(false)
  end;

  (*
   * Send Request_vote_response Server1 -> Server0
   * --------------------------------------------------------------------------
   *)

  let now = now +. 0.001 in
  
  let res = 
    let msg = msg_for_server msgs 0 in
    Raft_logic.handle_message server0 msg now
  in

  let {
    Logic.state = server0; 
    messages_to_send = msgs; 
    leader_change;
    committed_logs; 
    added_logs;
    deleted_logs;
  } = res in
  
  assert(Types.is_leader server0);
  assert(Some (New_leader 0) = leader_change);
    (* Because a single vote is enough to reach a majority in a 3-server 
     * cluster, server0 becomes a [Leader].  *)
  assert(committed_logs = []);
  assert(added_logs = []);
  assert([] = deleted_logs);

  assert(1 = server0.current_term);
    (* Becoming a [Leader] should not affect the term. (Only a new election).*)

  begin match server0.role with
  | Leader followers -> (
    assert(2 = List.length followers);
      (* The leader maintain various state for each of the
       * other servers.  *)

      List.iter (fun follower ->

        assert(follower.next_index = 1);
        assert(follower.match_index = 0);
        assert(follower.heartbeat_deadline = 
               now +. default_configuration.hearbeat_timeout);
        assert(follower.outstanding_request = true);

      ) followers;
  )
  | _ -> assert(false)
  end;

  assert(2 = List.length msgs);
    (* Upon becoming a Leader a server must immediately
     * send an [Append_entries] request to all the other servers
     * to establish its leadership. *)

  List.iter (fun (msg, _ ) ->
    match msg with
    | Append_entries_request r ->
      assert(1  = r.leader_term);
      assert(0  = r.leader_id);
      assert(0  = r.prev_log_index);
      assert(0  = r.prev_log_term);
      assert([] = r.log_entries);
        (* We have not yet added any log to the [Leader] so
         * no new entries are sent to the other servers.  *)
      assert(0 = r.leader_commit);
    | _ -> assert(false);
  ) msgs;

  (*
   * Send Append_entries_request Server0 -> Server1
   * ----------------------------------------------------------------------
   *)

  let now = now +. 0.001 in
  let res = 
    let msg = msg_for_server msgs 1 in
    Raft_logic.handle_message server1 msg now
  in

  let {
    Logic.state = server1; 
    messages_to_send = msgs; 
    leader_change;
    committed_logs; 
    added_logs;
    deleted_logs;
  } = res in

  assert(Some (New_leader 0)= leader_change);
  assert([] = committed_logs);
  assert(added_logs = []);
  assert([] = deleted_logs);

  begin match server1.role with
  | Follower f -> (

    assert(f.voted_for = Some 0);
      (* [voted_for] is still assigned to server0 since the current term
       * has not changed. *)

    assert(f.current_leader = Some 0);
      (* [Append_entries] request indicates the leadership role of the sender.
       *
       * server1 then updates its state to keep track of the current [Leader] (ie
       * server0 in our case). *)

    assert(f.election_deadline = now +. default_configuration.election_timeout);
      (* Because it just receive a message from the [Leader], the
       * [election_deadline] is extended for another [election_timeout] amount
       * of time. *)
  )
  | _ -> assert(false)
  end;

  assert(1 = List.length msgs);
    (* The server1 is expected to send a single response back to the
     * sender (ie server0). *)

  begin match List.hd msgs with
  | (Append_entries_response r, 0) -> (
    assert(r.receiver_id = 1);
    assert(r.receiver_term = 1);
    assert(r.result = Success 0); 
  )
  | _ -> assert(false)
  end;

  ({server0; server1; server2}, now)


(* In this part of the test server2 which was previously disconnected 
 * during the election_1 is now starting its own election for the same
 * term 1. Both server1 and server0 denies their vote since they have both
 * already granted their vote in this term. 
 *
 * At the end of the election server2 is still a candidate while server0
 * maintains its leader role and server1 is its follower.  *) 
let failed_election_1 {server0; server1; server2} now = 
  
  (*
   * Since server2 has not received any message from a leader, 
   * it will start a new election.
   * ----------------------------------------------------------------------
   *)

  let {
    Logic.state = server2; 
    messages_to_send = request_vote_msgs; 
    leader_change; 
    committed_logs;
    added_logs; 
    deleted_logs; } = Raft_logic.handle_new_election_timeout server2 now
  in

  assert(None = leader_change);
  assert([] = committed_logs);
  assert(added_logs = []);
  assert(deleted_logs = []);

  assert(Types.is_candidate server2);
    (* Server2 never got an [Append_entries] request which would have
     * established server0 leadership. Therefore as far as server2 is
     * concerned there were no leaders.  *)
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
   * Send Request_vote_request Server2 -> Server1 
   * ---------------------------------------------------------------------
   *)

  let now = now +. 0.001 in
  let res = 
    let msg = msg_for_server request_vote_msgs 1 in
    Raft_logic.handle_message server1 msg now
  in

  let {
    Logic.state = server1; 
    messages_to_send = msgs; 
    leader_change;
    committed_logs; 
    added_logs;
    deleted_logs;
  } = res in

  assert(None = leader_change);
  assert([] = committed_logs);
  assert(added_logs = []);
  assert([] = deleted_logs);

  begin match server1.role with
  | Follower f -> (
    assert(f.voted_for = Some 0);
    assert(f.current_leader = Some 0);
      (* server0 state is unaffected by this new [Candidate] for this
       * term.  *)
  )
  | _ -> assert(false);
  end;

  begin match msgs with
  | (Request_vote_response r, 2)::[] -> (
    assert(r.voter_id = 1);
    assert(r.voter_term = 1);
    assert(r.vote_granted = false);
      (* Server1 has already voted for server0 in this election
       * term, so it should deny its vote to server2.  *)
  )
  | _ -> assert(false)
  end;

  (* Send Request_vote_response Server1 -> Server2 
   *
   * (vote not granted)
   * ---------------------------------------------------------------------
   *)

  let now = now +. 0.001 in

  let res = 
    let msg = msg_for_server msgs 2 in
    Raft_logic.handle_message server2 msg now
  in

  let {
    Logic.state = server2; 
    messages_to_send = msgs; 
    leader_change;
    committed_logs; 
    added_logs;
    deleted_logs;
  } = res in

  assert(Types.is_candidate server2);
  assert(None = leader_change);
    (* Despite the vote not being granted by server1, server2
     * should continue to be a [Candidate] until either
     *
     * - a [Request_vote] response with a granted vote is replied
     *   by another server
     *
     * - new election timeout elapsed in this case it will start
     *   a new election.
     *
     * - a valid [Leader] sends an [Append_entries] request, in which
     *   case it will become a [Follower].
     *)

  assert([] = committed_logs);
  assert(added_logs = []);
  assert([] = deleted_logs);
  assert(1 = server2.current_term);
    (* No new election should have been started.
     *)

  assert([] = msgs);
    (* server2 is still a candidate but for the time being it has no 
     * message to send to any servers.  *)

  (*
   * Send Request_vote Server2 -> Server0
   * -----------------------------------------------------------------------
   *)

  let res = 
    let msg = msg_for_server request_vote_msgs 0 in
    Raft_logic.handle_message server0 msg now
  in

  let {
    Logic.state = server0; 
    messages_to_send = msgs; 
    leader_change;
    committed_logs; 
    added_logs;
    deleted_logs;
  } = res in

  assert(Types.is_leader server0);
  assert(None = leader_change);
    (* Server0 is still a [Leader] and should not be affected
     * by a Candidate for the same term.
     *
     * (This would be different if the Candidate was for a later term) *)
  assert(1 = List.length msgs);
  assert([] = committed_logs);
  assert(added_logs = []);
  assert([] = deleted_logs);

  begin match List.hd msgs with
  | (Request_vote_response r, 2) -> (

    assert(r.voter_id = 0);
    assert(r.voter_term = 1);
    assert(r.vote_granted = false);
      (* Server0 being the [Leader] for term 1, it should
       * not grant its vote.  *)
  )
  | _ -> assert(false)
  end;

  let now = now +. 0.002 in

  (*
   * Send Request_vote_response Server0 -> Server2
   *
   * (vote not granted)
   * -----------------------------------------------------------------------
   *)

  let res = 
    let msg = msg_for_server msgs 2 in
    Raft_logic.handle_message server2 msg now
  in

  let {
    Logic.state = server2; 
    messages_to_send = msgs; 
    leader_change;
    committed_logs; 
    added_logs;
    deleted_logs;
  } = res in

  assert(deleted_logs = []);
  assert(Types.is_candidate server2);
    (* Yes despite all other server denying their vote, server2
     * is still a [Candidate]. It should not take any further
     * action until its election timeout has elapsed.  *)

  assert(None = leader_change);
  assert([] = committed_logs);
  assert(added_logs = []);

  assert([] = msgs);
    (* No new message from server2 for this elections. All [Request_vote]
     * requests have been sent and the unsucessful responses received.  *)
  ({server0; server1; server2}, now)

(* In this part of the test server0 the leader is sending heartbeat
 * messages (ie empty Append_entries_request) to all the followers. 
 *
 * Server2 is no longer disconnected and will receive the msg; the msg 
 * will establish server0 leadership to server2 which will update its role
 * to be a follower of server0. 
 *
 * At the end server0 is the leader and both server1 and server2 are followers
 * of server0.*)
let leader_heartbeat_1 {server0; server1; server2} now = 

  (* Since the heartbeat timeout is usually much shorter
   * than a new election timeout, it's likely that server0
   * (ie the current [Leader]) will send heartbeats to the other
   * 2 servers.
   * -----------------------------------------------------------------------
   *)

  (* First let's make sure that even because the heartbeat deadline
   * for any server has not been reached, a heartbeat timeout should not
   * trigger new messages.
   *)

  let {Logic.state = server0; messages_to_send = msgs; _ } =
    Raft_logic.handle_heartbeat_timeout server0 now 
  in

  assert([] = msgs);
    (* Heartbeat messages are only sent to servers which have not recveived a
     * message for at least the [hearbeat_timeout] amount of time.
     * It's not the case since [hearbeat_timeout] is 0.02.  *)

  let now = now +. default_configuration.hearbeat_timeout in

  let {Logic.state = server0; messages_to_send = hb_msgs; _ } =
      Raft_logic.handle_heartbeat_timeout server0 now 
  in

  assert(2 = List.length hb_msgs);
    (* Because we added [hearbeat_timeout] to the previous time, we know for
     * sure that heartbeats messages are past due for all of the followers. *)

  List.iter (fun (msg, _) ->
    match msg with
    | Append_entries_request r -> (
      assert(r.leader_term = 1);
      assert(r.leader_id = 0);
      assert(r.prev_log_index = 0);
      assert(r.prev_log_term = 0);
      assert(r.log_entries = []);
      assert(r.leader_commit = 0);
    )
    | _ -> assert(false);
  ) hb_msgs;

  (*
   * Send Append_entries_request Server0 -> Server1 
   *
   * (server2 becomes a follower)
   * --------------------------------------------------------------------
   *)

  let now = now +. 0.001 in
  let res = 
    let msg = msg_for_server hb_msgs 2 in
    Raft_logic.handle_message server2 msg now
  in

  let {
    Logic.state = server2; 
    messages_to_send = msgs; 
    leader_change;
    committed_logs; 
    added_logs;
    deleted_logs;
  } = res in

  assert(Types.is_follower server2);
  assert(1 = server2.current_term);
  assert(Some (New_leader 0)= leader_change);
  assert([] = committed_logs);
  assert(added_logs = []);
  assert(deleted_logs = []);
    (* Receiving an [Append_entries] request with a term at least equal
     * or supperior to one [current_term] means that the sender is a valid
     * [Leader] for that term and therefore the recipient must become a
     * [Follower].  *)

  begin match server2.role with
  | Follower fs -> (
    assert(fs.voted_for = Some 2);
      (* For that term, since this server was also a candidate it
       * did already vote for itself.  *)
    assert(fs.current_leader = Some 0);
      (* server2 is now aware that server0 is the [Leader] for
       * term 1 *)
    assert(fs.election_deadline = 
              now +. default_configuration.election_timeout);
  )
  | _ -> assert(false);
  end;

  assert(1 = List.length msgs);
    (* Response for the [Append_entries] *)

  begin match msgs with
  | ((Append_entries_response r), server_id) :: []  -> (
    assert(server_id = 0);
    assert(r.receiver_id = 2);
    assert(r.receiver_term = 1);
    assert(r.result = Success 0);
  )
  | _ -> assert(false)
  end;

  (*
   * Send Append_entries_response Server2 -> Server0
   *-----------------------------------------------------------------------
   *)

  let res = 
    let msg = msg_for_server msgs 0 in
    Raft_logic.handle_message server0 msg now
  in

  let {
    Logic.state = server0; 
    messages_to_send = msgs; 
    leader_change;
    committed_logs; 
    added_logs;
    deleted_logs;
  } = res in
  assert(None = leader_change);
  assert([] = committed_logs);
  assert([] = msgs);
  assert(added_logs = []);
  assert(deleted_logs = []);

  (*
   * Send Append_entries_request Server0 -> Server1
   * -----------------------------------------------------------------------
   *)

  let res = 
    let msg = msg_for_server hb_msgs 1 in
    Raft_logic.handle_message server1 msg now
  in

  let {
    Logic.state = server1; 
    messages_to_send = msgs; 
    leader_change;
    committed_logs; 
    added_logs;
    deleted_logs;
  } = res in

  assert(Types.is_follower server1);
   (* No change in the role, server0 is a valid [Leader],
    * server1 stays a [Follower]. *)

  assert(None = leader_change);
  assert([] = committed_logs);
  assert(added_logs = []);
  assert(deleted_logs = []);
    (* The heartbeat message from server0 did not bring new
     * information (ie server1 already new server0 was the
     * [Leader].) *)

  assert(1 = List.length msgs);
   (* Single [Append_entries_response] expected. *)

  begin match msgs with
  | (Append_entries_response r, 0)::[] -> (
    assert(r.receiver_id = 1);
    assert(r.receiver_term = 1);
    assert(r.result = Success 0); 
  )
  | _ -> assert(false)
  end;

  let now = now +. 0.001 in

  (*
   * Send Append_entries_response Server1 -> Server0 
   *
   * --------------------------------------------------------------------------
   *)

  (* Note that it's important for the rest of the test that
   * we explicitely handle the response from server1 in server0.
   * Each [Leader] is keeping track of whether or not there is an
   * [outstanding_request] for each server.
   *
   * The [Leader] would then avoid sending new [Append_entries] requests
   * which already have an outstanding requests. However if a heartbeat timeout
   * has been reached, a new [Append_entries] request will be sent no matter
   * what.  *)

  let res = 
    let msg = msg_for_server msgs 0 in
    Raft_logic.handle_message server0 msg now
  in

  let {
    Logic.state = server0; 
    messages_to_send = msgs; 
    leader_change;
    committed_logs; 
    added_logs;
    deleted_logs;
  } = res in

  assert(Types.is_leader server0);
  assert(None = leader_change);
  assert([] = committed_logs);
  assert(added_logs = []);
  assert(deleted_logs = []);
  assert([] = msgs);

  ({server0; server1; server2}, now)

(* In this part of the a new log is added to server0 the leader. This log 
 * triggers 2 Append_entries_request to server1 and server2. Only server1
 * successfully receives the msg, replicates the log and send back the 
 * response to server0. server2 will not receive the msg. 
 *
 * At the end server0 is the leader with a single log entry in its log; its
 * commit index is 1 since it was successfully replicated on server1. Server1
 * is still a follower with a single log entry in its log and a commit index 
 * of 0 since it is not yet aware that this first log entry was commited. 
 * Server2 is still a follower with an empty log since it never received 
 * the log entry #1. *)
let add_first_log {server0; server1; server2} now = 
  
  (* Let's now add a log entry to the [Leader] which is expected to trigger
   * the corresponding [Append_entries] requests to the other servers.  *)
  let new_log_result =
    let data = Bytes.of_string "Message01" in
    Raft_logic.handle_add_log_entries server0 [(data, "01")] now
  in

  let server0, data1_msgs =
    let open Raft_logic in
    match new_log_result with
    | Appended result -> 
        let {
          Logic.state; 
          messages_to_send; 
          committed_logs; 
          added_logs;deleted_logs;_} = result in 
        assert([] = committed_logs); 
        assert(deleted_logs = []);
        begin match added_logs with
        | [{id="01"; index = 1; term = 1; _}] -> () 
        | _ -> assert(false)
        end; 
        (state, messages_to_send)
      (* server0 is the [Leader] and is therefore expected to
       * handle the new log entry.  *)

    | Delay | Forward_to_leader _ -> assert(false)
  in

  assert(1 = recent_log_length server0);
    (* The new log entry should have been appended to the current empty log.*)

  begin match recent_log_hd server0 with 
  | {index = 1; term = 1; _ } -> ()
    (* Log should start as 1 and be set to the current
     * term (ie 1.) *)

  | _ -> assert(false)
  end;

  assert(2 = List.length data1_msgs);
    (* Both [Follower]s have no outstanding request and have also less
     * log entries than the [Leader], therefore they
     * should get a new [Append_entries] request message with the new
     * log *)

  List.iter (fun (msg, _) ->
    match msg with
    | Append_entries_request r -> (
      assert(r.leader_term = 1);
      assert(r.leader_id = 0);
      assert(r.prev_log_index = 0);
      assert(r.prev_log_term = 0);
      assert(1 = List.length r.log_entries);
        (*
         * Contains the log entry to be synchronized.
         *)
      begin match r.log_entries with
      | {index = 1; term = 1; _} :: [] -> ()
      | _ -> assert(false)
      end;
      assert(r.leader_commit = 0);
    )
    | _ -> assert(false)
  ) data1_msgs;

  let now = now +. 0.001 in

  (*
   * Send Append_entries_request Server0 -> Server1 
   * ----------------------------------------------------------------------
   *)

  let res = 
    let msg = msg_for_server data1_msgs 1 in
    Raft_logic.handle_message server1 msg now
  in

  let {
    Logic.state = server1; 
    messages_to_send = msgs; 
    leader_change;
    committed_logs; 
    added_logs;
    deleted_logs;
  } = res in

  assert(Types.is_follower server1);
    (* No change of role for server1, [Append_entries] only
     * re-inforce that server0 is the [Leader].  *)

  begin match added_logs with
  | [{id = "01"; index = 1; term = 1; _}] -> () 
  | _ -> assert(false)
  end;
  assert(1 = recent_log_length server1);
    (* The [log_entry] with index 1 has been replicated
     * on server1. *)

  assert(0 = server1.commit_index);
  assert(None = leader_change);
  assert(deleted_logs = []);
  assert([] = committed_logs);
    (* While server1 has successfully replicated the log entry
     * with [index = 1], it cannot assume that this latter log
     * entry has been committed (ie that it has been replicated on
     * a majority of servers).
     *
     * Therefore the [commit_index] is still 0.
     *
     * It will be updated upon receiving the next [Append_entries_request] *)

  begin match msgs with
  | (Append_entries_response r, 0) :: [] -> (
    assert(r.receiver_id = 1);
    assert(r.receiver_term = 1);
    assert(r.result = Success 1);
     (* server1 has correctly replicated the log entry which has index1. *)
  )
  | _ -> assert(false)
  end;

  (*
   * Send Append_entries_response Server1 -> Server0 
   * --------------------------------------------------------------------
   *)

  let res = 
    let msg = msg_for_server msgs 0 in
    Raft_logic.handle_message server0 msg now
  in

  let {
    Logic.state = server0; 
    messages_to_send = _; 
    leader_change;
    committed_logs; 
    added_logs;
    deleted_logs;
  } = res in

  assert(Types.is_leader server0);
  assert(1 = server0.commit_index);
  assert(None = leader_change);
  assert(deleted_logs = []);
  assert(added_logs = []);
    (* The log with index1 was already added so added_log is empty *)
  begin match committed_logs with
  | {id = "01"; _ }::[] -> ()
    (* server1 has replicated the log successfully so this means
     * that a majority of servers have done the replication.
     * The [Leader] commit_index can now be updated to that latest
     * log index (ie 1).  *)
  | _ -> assert(false)
  end;

  ({server0; server1; server2}, now)

(* In this part of the test a second log entry is added to server0 the leader.
 * This log entry is successfully replicated on server1 only. Server2 is 
 * not receiving the Append_entries_request message. Server1 commit index 
 * is now [1] since the leader commit index of 1 was sent in the request. 
 *
 * At the end server0 is the leader with 2 log entries and a commit index 
 * of 2, server1 is a follower with 2 log entries and a commit index of 1, 
 * server2 is a follower with an empty log. *)
let add_second_log {server0; server1; server2} now = 
  let now = now +. 0.001 in

  let new_log_result =
    let data = Bytes.of_string "Message02" in
    Raft_logic.handle_add_log_entries server0 [(data,"02")] now
  in

  let server0, data2_msg =
    let open Raft_logic in
    match new_log_result with
    | Delay | Forward_to_leader _ -> assert(false)
    | Appended {Logic.state; messages_to_send; added_logs; _ } -> begin 
      begin match added_logs with
      | [{id = "02"; index = 2; term = 1; _}] -> ()
      | _ -> assert(false)
      end;
      (state, messages_to_send)
    end 
  in

  assert(Types.is_leader server0);

  assert(2 = recent_log_length server0);
    (* The second log entry should have been appended to the
     * server log. *)

  begin match recent_log_hd server0 with
  | {index = 2; term = 1; _ }  -> ()
    (* Make sure the index is incremented by 1. *)
  | _ -> assert(false)
  end;

  assert(1 = server0.commit_index);
    (* The new log entry (with index [2]) should not be committed
     * since no request/response interaction has yet been done.  *)

  assert(1 = List.length data2_msg);
    (* Since server2 has an outstanding request, it should not
     * be sent an additional request. Only server1 should receive
     * an [Append_entries] request. *)

  begin match List.hd data2_msg with
  | (Append_entries_request r, 1) -> (

    assert(r.leader_term = 1);
    assert(r.leader_id = 0);
    assert(r.prev_log_index = 1);
      (* Server0 [Leader] knows that the server1 has successfully
       * replicated the log entry with [index = 1] and therefore
       * will send [prev_log_index] with value 1.  *)

    assert(r.prev_log_term = 1);
    assert(1 = List.length r.log_entries);
      (* Only the last [log_entry] should be sent to that follower,
       * since the first [log_entry] was already replicated.
       *)
    assert(r.leader_commit = 1);
  )
  | _ -> assert(false)
  end;

  let now = now +. 0.001 in

  (*
   * Send Append_entries_request Server0 -> Server1
   * -------------------------------------------------------------------
   *)

  let res = 
    let msg = msg_for_server data2_msg 1 in
    Raft_logic.handle_message server1 msg now
  in

  let {
    Logic.state = server1; 
    messages_to_send = msgs; 
    leader_change;
    committed_logs; 
    added_logs;
    deleted_logs;
  } = res in

  assert(Types.is_follower server1);

  assert(2 = recent_log_length server1);
    (* server1 should have correctly replicated the log
     * with [index = 2].  *)

  assert(None = leader_change);

  assert(1 = server1.commit_index);
  begin match committed_logs with
  | {id = "01"; _ }::[] -> ()
  | _ -> assert(false)
  end;
    (* The [Append_entries] request contained the [commit_index]
     * of the [Leader] (1 in this case) and therefore server1 has
     * updated its own.  *)
  begin match added_logs with
  | {id = "02"; index = 2; term = 1; _}::[] -> () 
  | _ -> assert(false) 
  end;
    (* The new log number 2 is now added in this follower .. but not commited
     * as verified previously. *)

  assert(deleted_logs = []);
  assert(1 = List.length msgs);
    (* Only a single response to server0 should be
     * sent back. *)

  begin match List.hd msgs with
  | (Append_entries_response r, 0) -> (
    assert(r.receiver_id = 1);
    assert(r.receiver_term = 1);
    assert(r.result = Success 2); 
     (* server1 notifies the [Leader] about the last log it has
      * replicated.
      *)
  )
  | _ -> assert(false)
  end;

  (*
   * Send Append_entries_response Server1 -> Server0 
   * ----------------------------------------------------------------------
   *)

  let now = now +. 0.001 in

  let res = 
    let msg = msg_for_server msgs 0 in
    Raft_logic.handle_message server0 msg now
  in

  let {
    Logic.state = server0; 
    messages_to_send = _; 
    leader_change;
    committed_logs; 
    added_logs;
    deleted_logs;
  } = res in

  assert(Types.is_leader server0);

  assert(None = leader_change);
  
  assert(2 = server0.commit_index);
  begin match committed_logs with
  | {id = "02"; _ }::[] -> ()
  | _ -> assert(false)
  end;
    (* A successfull replication is enough for a majority.  *)
  assert(added_logs = []);
  assert(deleted_logs = []);
  ({server0; server1; server2}, now) 

(* In this part of the test, the leader reaches its heartbeat timeout 
 * and send an Append_entries_request to each of its follower. Since
 * server1 has replicated all the logs, its message contains no log 
 * entries but its commit_index will be set to 2. Server2 msg will 
 * contains the last 2 log entries since it has not replicated them. 
 *
 * This time server2 receives the messages, replicates the 2 log and update
 * its commit index to [2]. 
 *
 * At the end of the test, server0 is the leader with 2 log index and a 
 * commit index of [2]. server1 and server2 are followers with 2 log index 
 * and commit index of [2]. *)
let leader_heartbeat_2 {server0; server1; server2} now = 
  
  let now = now +. default_configuration.hearbeat_timeout in

  let {Logic.state = server0; messages_to_send = msgs; added_logs; _ } =
    Raft_logic.handle_heartbeat_timeout server0 now 
  in

  assert(added_logs = []);
  assert(2 = List.length msgs);

  begin match msg_for_server msgs 1 with
  | Append_entries_request r -> (
    assert(r.leader_term = 1);
    assert(r.leader_id = 0);
    assert(r.prev_log_term = 1);
    assert(r.log_entries = []);
      (* As expected no new log entry should be sent
       * since they all have been previously and [server1]
       * replied successfully.  *)

    assert(r.prev_log_index = 2);
    assert(r.leader_commit = 2);
  )
  | _ -> assert(false)
  end;

  begin match msg_for_server msgs 2 with
  | Append_entries_request r -> (
    assert(r.leader_term = 1);
    assert(r.leader_id = 0);
    assert(r.prev_log_term = 0);

    assert(List.length r.log_entries = 2);
    assert(r.prev_log_index = 0);
      (* this reflect the knowledge of server0 (leader) which has never
       * received an Append_entries_response from server2 indicating 
       * that logs were replicated. *)
    assert(r.leader_commit = 2);
  )
  | _ -> assert(false)
  end;

  let now = now +. 0.001 in

  (*
   * Send Append_entries_request Server0 -> Server1
   * ---------------------------------------------------------------------
   *)
  let res = 
    let msg = msg_for_server msgs 1 in
    Raft_logic.handle_message server1 msg now
  in

  let {
    Logic.state = server1; 
    messages_to_send = server1_response; 
    leader_change;
    committed_logs; 
    added_logs;
    deleted_logs;
  } = res in

  assert(Types.is_follower server1);
  assert(None = leader_change);

  begin match committed_logs with
  | {id = "02"; _ }::[] -> ()
  | _ -> assert(false)
  end;
  assert(2 = server1.commit_index);
    (* server1 is updating its commit index based on latest
     * [leader_commit] value of 2 in the request it received.  *)
  
  assert(added_logs = []);
    (* All logs have already been replicated in server 1. *)

  assert(deleted_logs = []);

  assert(1 = List.length server1_response);
   (* Only a single response is expected.  *)

  (*
   * Send Append_entries_request Server0 -> Server2
   * ---------------------------------------------------------------------
   *)

  let res = 
    let msg = msg_for_server msgs 2 in
    Raft_logic.handle_message server2 msg now
  in

  let {
    Logic.state = server2; 
    messages_to_send = server2_response; 
    leader_change;
    committed_logs; 
    added_logs;
    deleted_logs;
  } = res in

  assert(Types.is_follower server2);
  begin match committed_logs with
  | {id = "01"; _ }::{id = "02"; _}::[] -> ()
  | _ -> assert(false)
  end;
  assert(added_logs = committed_logs); 
    (* The 2 previous logs were never replicated to the server 2 before 
     * this heartbeat message *)
  assert(deleted_logs = []);
  assert(None = leader_change);
  assert(2 = server2.commit_index);
   (* server2 is updating its commit index based on latest
    * [leader_commit] value of 2 in the request it received. *)

  assert(2 = recent_log_length server2);
   (* server2 should have caught up with server0 and replicated
    * all the logs in the cache (ie 1) *)

  assert(1 = List.length server2_response);
   (* Only a single response is expected.  *)

  begin match server2_response with
  | (Append_entries_response r, 0)::[] -> (
    assert(r.receiver_id = 2);
    assert(r.receiver_term = 1);
    assert(r.result = Success 2);
      (* server2 has successfully replicated the 2 log entries *)
  )
  | _ -> assert(false)
  end;

  (*
   * Send Append_entries_response Server2 -> Server0
   * ----------------------------------------------------------------------
   *)

  let res = 
    let res = 
      let msg = msg_for_server server1_response 0 in
      Raft_logic.handle_message server0 msg now
    in

    let {
      Logic.state = server0; 
      messages_to_send; 
      leader_change;
      committed_logs; 
      added_logs;
      deleted_logs;
    } = res in
    
    assert([] = messages_to_send);
    assert(None = leader_change);
    assert([] = committed_logs);
    assert(added_logs = []);
    assert(deleted_logs = []);
      (* Server1 has replicated the 2 logs it has nothing
       * left.  *)

    let msg = msg_for_server server2_response 0  in
    Raft_logic.handle_message server0 msg now
  in

  let {
    Logic.state = server0; 
    messages_to_send; 
    leader_change;
    committed_logs; 
    added_logs;
    deleted_logs;
  } = res in

  assert(Types.is_leader server0);
  assert(2 = server0.commit_index);
  assert(2 = recent_log_length server0);
  assert(None = leader_change);
  assert([] = committed_logs);
  assert(added_logs = []);
  assert(deleted_logs = []);

  (* Both servers have replicated the 2 logs, no outstanding
   * logs to be sent.  *)
  assert(0 = List.length messages_to_send);

  ({server0; server1; server2}, now)

(* In this part of the test, a 3rd log is added to the leader which 
 * successfully replicates it on server1 but the Append_entries_response 
 * is not sent back to server0. Therefore the log is not yet committed. 
 * Server 2 does not replicate the 3rd log however. 
 *
 * At the end server0 is the leader with 3 logs and commit_index set to [2], 
 * server1 has 3 logs with commit_index set to [2] and server2 has 2 logs 
 * and commit_index set to [2]. *)
let add_third_log {server0; server1; server2} now = 
  
  (* Let's now add a 3rd [log_entry] to the [Leader].
   *
   * We'll replicate this 3rd entry on [server1] only and
   * then simulate a [Leader] crash.
   *
   * The consequence of a [Leader] crash will be that one of the
   * follower will start a new election.
   * However only [server1] should become a [Leader] since it has replicated
   * more [log_entry]s than [server2].
   *
   * We will simulate and test the above assumption. *)

  let now = now +. 0.002 in

  let new_log_result =
    let data = Bytes.of_string "Message03" in
    Raft_logic.handle_add_log_entries server0 [(data, "03") ] now
  in

  let server0, msgs =
    match new_log_result with
    | Logic.Appended {Logic.state; messages_to_send; added_logs; _ } -> 
        begin match added_logs with
        | {id = "03"; index = 3; term = 1; _ } :: [] -> () 
        | _ -> assert(false)
        end;
        (state, messages_to_send)
    | _ -> assert(false)
  in
  assert(Types.is_leader server0);
  assert(3 = recent_log_length server0);
    (* Correctly added log since server0 is a [Leader].  *)

  assert(2 = List.length msgs);
    (* 2 [Append_entries] requests, one for each of the other
     * 2 servers. *)

  (*
   * Send Append_entries_request Server0 -> Server1 
   * ---------------------------------------------------------------------
   *)

  let res = 
    let msg = msg_for_server msgs 1 in
    Raft_logic.handle_message server1 msg now
  in

  let {
    Logic.state = server1; 
    messages_to_send = _; 
    leader_change;
    committed_logs; 
    added_logs;
    deleted_logs;
  } = res in

  assert(Types.is_follower server1);
  assert(3 = recent_log_length server1);
    (* * The 3rd [log_entry] is correctly replicated.  *)

  assert(None = leader_change);
  assert([] = committed_logs);
  assert(2  = server1.commit_index);
    (* No change since the [commit_index] was still
     * 2 in the [Append_entries] request.  *)
  begin match added_logs with
  | {id = "03"; index = 3; term =1 ; _ } :: [] -> () 
  | _ -> assert(false)
  end;
  
  assert(deleted_logs = []);

  (* The response is not send back to server0 *)
  ({server0; server1; server2}, now)

(* In this part of the test, server2 starts a new election, however since
 * it has only 2 log entries and server1 has 3 log entries, server1 will 
 * not grant its vote.
 *
 * At the end of the test server2 is a candidate for term [2] with only
 * its own vote granted. Server1 is still a follower with no leader and 
 * no vote granted in term [2]. Server0 is still a leader for term [1]. *) 
let failed_election_2 {server0; server1; server2} now = 
  
  (*
   * Server2 starts a new election
   * --------------------------------------------------------------------------
   *)

  let now = now +. default_configuration.election_timeout  in

  let {
    Logic.state = server2; 
    messages_to_send = msgs; 
    leader_change;
    committed_logs;
    added_logs; 
    deleted_logs} = Raft_logic.handle_new_election_timeout server2 now
  in

  assert(deleted_logs = []);
  assert(Types.is_candidate server2);
    (* Server2 started a new election. *)

  assert(Some No_leader = leader_change);
    (* Becoming a candidate and incrementing the terms
     * means that there are no longer a [Leader].  *)
  assert([] = committed_logs);
  assert(added_logs = []);

  assert(2 = server2.current_term);
    (* Each new election increments the term *)

  assert(2 = List.length msgs);
    (* 2 [Request_vote] request for each of the other 2
     * servers.  *)

  List.iter (fun (r, _) ->
    match r with
    | Request_vote_request r -> (
      assert(r.candidate_term = 2);
      assert(r.candidate_id = 2);
      assert(r.candidate_last_log_index = 2);
      assert(r.candidate_last_log_term = 1);
    )
    | _ -> assert(false)
  ) msgs;


  (*
   * Send Request_vote_request Server2 -> Server1
   * ----------------------------------------------------------------------
   *)

  let now = now +. 0.001 in

  let res = 
    let msg = msg_for_server msgs 1 in
    Raft_logic.handle_message server1 msg now
  in

  let {
    Logic.state = server1; 
    messages_to_send = msgs; 
    leader_change;
    committed_logs; 
    added_logs;
    deleted_logs;
  } = res in

  assert(Types.is_follower server1);
  assert(2 = server1.current_term);
    (* The sender (ie server2) term was greater than the current
     * term of server1, it should then
     * increments its current term. *)

  assert(Some No_leader = leader_change);
    (* The change of term means that there are no current [Leader]
     * for it yet.  *)
  assert([] = committed_logs);
  assert(added_logs = []);
  assert(deleted_logs = []);

  assert(1 = List.length msgs);
    (* [Request_vote] response to server2.  *)

  begin match msgs with
  | (Request_vote_response r, 2)::[] -> (
    assert(r.voter_id = 1);
    assert(r.voter_term = 2);
    assert(r.vote_granted = false);
     (* [server1] has more [log_entry] than [server2] and therefore
      * rejects [server2] candidacy.
      *
      * This is to ensure the safety property of the RAFT protocol
      * so that no committed entries are later invalidated.  *)
  )
  | _  -> assert(false)
  end;
  ({server0; server1; server2}, now) 

(* In this part of the test, server1 takes its turn to start a new election 
 * in term3. This time since it has more logs than server2, server2 grants its 
 * vote and server1 becomes a leader. 
 *
 * Upon become a leader server1 send Append_entries_request to both
 * server0 and server2 assuming their previous log index matches his (ie [3]). 
 * Server0 is disconnected and the msg is not sent to it. Server2 receives
 * the message, however it has not replicated log [3] and therefore sends 
 * back a log failure indicating its previous log index which is [2]. 
 * Server1 will then update its information about server2 and send back 
 * a corrected Append_entries_request, with this time the missing third 
 * entry. Upon receiving the Append_entries_response server1 will mark
 * the 3rd log entry to be commited!
 *
 * At the end server0 is still in a leader role but for term 1 and it has 
 * 3 log entries with commit index of 2. Both server1 and server2 are in 
 * term 3 with server1 the leader and server2 a follower. They both have 3
 * log entries, server1 commit index is 3 while server2 is 2. *) 
let election_2 {server0; server1; server2} now = 
  
  (*
   * Let's now have server1 starts a new election.
   * ----------------------------------------------------------------------
   *)

  let now = now +. default_configuration.election_timeout in

  let {
    Logic.state = server1; 
    messages_to_send = msgs; 
    leader_change; 
    committed_logs; 
    added_logs; 
    deleted_logs} = Raft_logic.handle_new_election_timeout server1 now
  in

  assert(None = leader_change);
  assert([] = committed_logs);
  assert(added_logs = []);
  assert(deleted_logs = []);

  assert(Types.is_candidate server1);
  assert(3 = server1.current_term);
  assert(2 =  List.length msgs);

  List.iter (fun (r, _) ->
    begin match r with
    | Request_vote_request r -> (
      assert(r.candidate_term = 3);
      assert(r.candidate_id = 1);
      assert(r.candidate_last_log_index = 3);
      assert(r.candidate_last_log_term = 1);
    )
    | _ -> assert(false)
    end
  ) msgs;

  (*
   * Send Request_vote_request server1 -> server2 
   * -----------------------------------------------------------------------
   *)

  let res = 
    let msg = msg_for_server msgs 2 in
    Raft_logic.handle_message server2 msg now
  in

  let {
    Logic.state = server2; 
    messages_to_send = msgs; 
    leader_change;
    committed_logs; 
    added_logs;
    deleted_logs;
  } = res in

  assert(Types.is_follower server2);
    (* server1 [current_term] is greater than the one
     * in server2 (3 versus 2).
     *
     * Therefore server2 becomes a [Follower] and update
     * its current_term.  *)

  assert(3 = server2.current_term);

  assert(None = leader_change);
  assert(deleted_logs = []);
  assert([] = committed_logs);
  assert(added_logs = []);
    (* Server2 already knew there was no [Leader] since it was a candidate
     * in the previous term and never got elected.  *)

  assert(1 = List.length msgs);

  begin match msgs with
  | (Request_vote_response r, 1)::[] -> (
    assert(r.voter_id = 2);
    assert(r.voter_term = 3);
    assert(r.vote_granted = true);
      (* Vote is indeed granted since server1 has a greater
       * [last_log_index].  *)
  )
  | _ -> assert(false)
  end;

  let now = now +. 0.001 in

  (*
   * Send Request_vote_response Server2 -> Server1 
   * ---------------------------------------------------------------------
   *)

  let res = 
    let msg = msg_for_server msgs 1 in
    Raft_logic.handle_message server1 msg now
  in

  let {
    Logic.state = server1; 
    messages_to_send = msgs; 
    leader_change;
    committed_logs; 
    added_logs;
    deleted_logs;
  } = res in

  assert(Some (New_leader 1) = leader_change);
  assert([] = committed_logs);
  assert(added_logs = []);
  assert(deleted_logs = []);
  assert(Types.is_leader server1);
    (* One vote is enough to become a [Leader].  *)

  assert(3 = server1.current_term);
    (* [current_term] should be the same after becoming
     * a [Leader].  *)

  assert(2 = List.length msgs);
    (* Imediately after becoming a [Leader], the server
     * will send [Append_entries] to establish its
     * leadership.  *)

  List.iter (fun (r, _) ->
    match r with
    | Append_entries_request r -> (
      assert(r.leader_term = 3);
      assert(r.leader_id = 1);
      assert(r.prev_log_index = 3);
      assert(r.prev_log_term = 1);
      assert(r.log_entries = []);
        (* Initially the [Leader] believes that all other servers
         * have replicated the same [log_entry]s as itself.  *)
      assert(r.leader_commit = 2);
    )
    | _ -> assert(false)
  ) msgs;

  (* Send Append_entries_request Server1 -> Server2
   * -----------------------------------------------------------------------
   *)

  let now = now +.  0.001 in

  let res = 
    let msg = msg_for_server msgs 2 in
    Raft_logic.handle_message server2 msg now
  in

  let {
    Logic.state = server2; 
    messages_to_send = msgs; 
    leader_change;
    committed_logs; 
    added_logs;
    deleted_logs;
  } = res in

  assert(deleted_logs = []);
  assert(Some (New_leader 1) = leader_change);
  assert([] = committed_logs);
  assert(Types.is_follower server2);
  assert(3 = server2.current_term);
  assert(added_logs = []);
    (* See explanation below as to why the 3rd log entry was not replicated *)

  assert(1 = List.length msgs);
    (* Single response to server1.  *)

  begin match msgs with
  | (Append_entries_response r, 1)::[] -> (
    assert(r.receiver_id = 2);
    assert(r.receiver_term = 3);
    assert(r.result = Log_failure 2);      
      (* server2 did not replicate the 3rd [log_entry] that server1
       * did during [term = 1].
       *
       * Therefore the previous [Append_entries] request is rejected. *)
  )
  | _ -> assert(false)
  end;

  (*
   * Send Append_entries_response Server2 -> Server1 
   * ----------------------------------------------------------------------
   *)

  let now = now +. 0.001 in

  let res = 
    let msg = msg_for_server msgs 1 in
    Raft_logic.handle_message server1 msg now
  in

  let {
    Logic.state = server1; 
    messages_to_send = msgs; 
    leader_change;
    committed_logs; 
    added_logs;
    deleted_logs;
  } = res in

  assert(deleted_logs = []);
  assert(Types.is_leader server1);
  assert(3 = server1.current_term);

  assert(None = leader_change);
  assert([] = committed_logs);
  assert(added_logs = []);

  assert(1 = List.length msgs);
  assert(2 = server1.commit_index);
    (* A new request for server2 has been computed which
     * should now contain the 3rd [log_entry] *)

  begin match msgs with
  | (Append_entries_request r, 2) :: [] -> (
    assert(r.leader_term = 3);
    assert(r.leader_id = 1);
    assert(r.prev_log_index = 2);
    assert(r.prev_log_term = 1);
    assert(1 = List.length r.log_entries);
     (* The missing 3rd log entry is now part of the
      * request for server2 to catch up
      *)
    assert(r.leader_commit = 2);
  )
  | _ -> assert(false)
  end;

  (*
   * Send Append_entries_request Server1 -> Server2
   * ----------------------------------------------------------------------
   *)

  let now = now +. 0.001 in

  let res = 
    let msg = msg_for_server msgs 2 in
    Raft_logic.handle_message server2 msg now
  in

  let {
    Logic.state = server2; 
    messages_to_send = msgs; 
    leader_change;
    committed_logs; 
    added_logs;
    deleted_logs;
  } = res in

  assert(Types.is_follower server2);
  assert(3 = server2.current_term);

  assert(3 = recent_log_length server2);
    (* server2 has correctly replicated the 3rd [log_entry].  *)

  assert(deleted_logs = []);
  assert(None = leader_change);
  assert([] = committed_logs);
  assert(2  = server2.commit_index);
    (*  The 3rd log while succesfully replicated is not  yet
     *  committed.
     *)
  begin match added_logs with
  | {id = "03"; index = 3; term = 1; _ }::[]  -> ()
  | _ -> assert(false); 
  end;

  assert(1 = List.length msgs);
    (* Single response for server1.  *)

  begin match msgs with
  | (Append_entries_response r, 1) :: [] -> (
    assert(r.receiver_id = 2);
    assert(r.receiver_term = 3);
    assert(r.result = Success 3);
      (* Confirmation that the replication of the log has
       * been successful.  *)
  )
  | _ -> assert(false)
  end;

  (* Send Append_entries_response Server2 -> Server1 
   * ----------------------------------------------------------------------
   *)

  let now = now +. 0.001  in

  let res = 
    let msg = msg_for_server msgs 1 in
    Raft_logic.handle_message server1 msg now
  in

  let {
    Logic.state = server1; 
    messages_to_send = msgs; 
    leader_change;
    committed_logs; 
    added_logs;
    deleted_logs;
  } = res in

  assert(Types.is_leader server1);
  assert(3 = server1.current_term);

  assert(deleted_logs = []);
  begin match committed_logs with
  | {id = "03"; _ }::[] -> ()
  | _ -> assert(false)
  end;
  assert(None = leader_change);
  assert(3 = server1.commit_index);
    (*
     * The 3rd [log_entry] has been replicated one one other
     * server than the [Leader]; this makes a majority and therefore
     * indicates that [commit_index] can be set to 3.
     *)
  assert(added_logs = []);
  assert([] = msgs);
  ({server0; server1; server2}, now)

(* In this part of the test we simply add 2 logs at the same time to server1
 * the latest leader. Server1 successfully adds, replicate and commits 
 * both logs. 
 * Server2 replicates the logs but its commit index is not yet updated. 
 * Server0 is still disconnected and the leader of term [1]. *)
let add_4_and_5_logs {server0; server1; server2} now = 

  let new_log_result =
    let datas = [
      (Bytes.of_string "Message04", "04");
      (Bytes.of_string "Message05", "05");
    ] in
    Raft_logic.handle_add_log_entries server1 datas now
  in

  let server1, data45_msgs =
    let open Raft_logic in
    match new_log_result with
    | Appended {Logic.state; messages_to_send; added_logs; _ } -> 
      begin 
        assert(2 = List.length added_logs); 
        (state, messages_to_send)
      end
    | Delay | Forward_to_leader _ -> assert(false)
  in

  assert(5 = recent_log_length server1);
  assert(5 = Log.last_log_index server1.log);

  assert(3 = server1.commit_index);
    (* The 2 logs have not been committed.  *)

  assert(1 = List.length data45_msgs);
    (* Only one message needs to be sent since server0 has still an outstanding
     * request. (ie server1 the leader never received any `append entries 
     * response` from that server *)

  List.iter (fun (msg, _) ->

    match msg with
    | Append_entries_request r -> (
      let {
        leader_term;
        leader_id;
        prev_log_index;
        prev_log_term;
        log_entries;
        leader_commit;
      } =  r in
      assert(3 = leader_term);
      assert(1 = leader_id);
      assert(prev_log_index = 3);
      assert(prev_log_term = 1);
      assert(2 = List.length log_entries);
      assert(3 = leader_commit);
    )
    | _ -> assert(false);
  ) data45_msgs;

  (* 
   * Send Append_entries_request Server1 -> Server2
   * ------------------------------------------------------------------
   *)

  let now = now +. 0.001  in

  let res = 
    let msg = msg_for_server data45_msgs 2 in
    Raft_logic.handle_message server2 msg now
  in

  let {
    Logic.state = server2; 
    messages_to_send = msgs; 
    leader_change;
    committed_logs; 
    added_logs;
    deleted_logs;
  } = res in

  assert(deleted_logs = []);
  assert(Types.is_follower server2);
  assert(3 = server2.current_term);

  assert(2 = List.length added_logs);
  assert(5 = recent_log_length server2);
    (* The last 2 logs where succesfully replicated
     *)
  begin match recent_log_hd server2 with
  | {data; _ } ->
    assert((Bytes.of_string "Message05") = data);
    (* Let's make sure the order was properly replicated.
     *)
  end;

  assert(3 = server2.commit_index);
    (* Replicated from server1, previous one was 2 so we
     * can expect a notification. *)

  assert(None = leader_change);
  begin match committed_logs with
  | {id = "03"; _ }::[] -> ()
  | _ -> assert(false)
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
    assert(Success 5 = result);
  )
  | _ -> assert(false);
  end;

  (*
   * Send Append_entries_response Server2 -> Server1 
   * ------------------------------------------------------------------------
   *)

  let now = now +. 0.001  in

  let res = 
    let msg = msg_for_server msgs 1 in
    Raft_logic.handle_message server1 msg now
  in

  let {
    Logic.state = server1; 
    messages_to_send = msgs; 
    leader_change;
    committed_logs; 
    added_logs;
    deleted_logs;
  } = res in

  assert(deleted_logs = []);
  assert(5 = server1.commit_index);
  assert([] = msgs);
  assert(2 = List.length committed_logs);
  assert(added_logs = []);
  assert(None = leader_change);

  ({server0; server1; server2}, now)

(* In this part of the test we add a log to server0 which is disconnected 
 * (ie the network is partitioned) from the other servers. Because server0 
 * is still a leader for term [1] it will add the log but will not receive
 * any response from the other server, therefore this 4th log entry will 
 * not be commited. 
 *
 * At the end, server0 is still a leader for term [1], it has 4 log entries 
 * but a commit_index of [2]. Server1 is the leader of term [3] and server2 is
 * a follower of server1. *)

let add_log_to_outdated_leader {server0; server1; server2} now = 
  let new_log_result =
    let data = Bytes.of_string "NeverCommitted" in
    Raft_logic.handle_add_log_entries server0 [(data, "NC")] now
  in

  let server0, msgs =
    let open Raft_logic in
    match new_log_result with
    | Appended result -> 
        let {
          Logic.state; 
          messages_to_send; 
          committed_logs; 
          added_logs;deleted_logs; _} = result in 
        assert([] = committed_logs); 
        assert(deleted_logs = []);
        assert(1 = List.length added_logs); 
        begin match added_logs with
        | [{id="NC"; index = 4; term = 1; _}] -> () 
        | _ -> assert(false)
        end; 
        (state, messages_to_send)
      (* server0 is the [Leader] and is therefore expected to
       * handle the new log entry.  *)

    | Delay | Forward_to_leader _ -> assert(false)
  in

  assert(2 = List.length msgs);
  assert(4 = recent_log_length server0); 
  assert(2 = server0.commit_index); 
  
  List.iter (fun (msg, _) ->
    match msg with
    | Append_entries_request r -> (
      assert(r.leader_term = 1);
      assert(r.leader_id = 0);
      assert(r.prev_log_index = 2);
      assert(r.prev_log_term = 1);
      assert(2 = List.length r.log_entries);
        (* * Contains the log entry to be synchronized.  *)
      begin match r.log_entries with
      | {index = 3; term = 1; _} :: {index = 4; id = "NC";_} :: [] -> ()
      | _ -> assert(false)
      end;
      assert(r.leader_commit = 2);
    )
    | _ -> assert(false)
  ) msgs;

  ({server0; server1; server2}, now)

(* In this part of the test server1 the leader in term [3] will send 
 * heartbeat messages to both server0 and server2. Server0 is no longer
 * disconnected and receives the message, the synchronization sequence 
 * between the latest leader (server1) and the previous one (server0) is 
 * deleting non commited messages in server0. 
 *
 * At the end server1 is the leader and has 5 log entries and a commit 
 * index of 5. Both server0 and server2 have fully replicated the 
 * 5 log entries and their commit index is 5 as well*)
let leader_heartbeat_3 {server0; server1; server2} now = 
  let now = now +. default_configuration.hearbeat_timeout in
  
  let {Logic.state = server1; messages_to_send = msgs; added_logs; _ } =
    Raft_logic.handle_heartbeat_timeout server1 now 
  in

  assert(added_logs = []);
  assert(2 = List.length msgs);

  begin match msg_for_server msgs 0 with
  | Append_entries_request r -> (
    assert(r.leader_term = 3);
    assert(r.leader_id = 1);
    assert(r.prev_log_term = 1);
    assert(2 = List.length r.log_entries); 
      (* Because no msg was received from server0, server1 knowledge of 
       * server0 still believes its prev_log_index to be 3... the same as 
       * when it was elected a leader. Therefore for the message contains
       * the 2 latest entries. *)
    assert(r.prev_log_index = 3);
    assert(r.leader_commit = 5);
  )
  | _ -> assert(false)
  end;
  
  begin match msg_for_server msgs 2 with
  | Append_entries_request r -> (
    assert(r.leader_term = 3);
    assert(r.leader_id = 1);
    assert(r.prev_log_term = 3);
    assert(0 = List.length r.log_entries); 
    assert(r.prev_log_index = 5);
      (* Server2 has replicated both log_entries 4 & 5 *)
    assert(r.leader_commit = 5);
  )
  | _ -> assert(false)
  end;

  (* 
   * Send Append_entries_request Server1 -> Server0 
   * ---------------------------------------------------------------------
   *)

  let res = 
    let msg = msg_for_server msgs 0 in
    Raft_logic.handle_message server0 msg now
  in

  let {
    Logic.state = server0; 
    messages_to_send = server0_response; 
    leader_change;
    committed_logs; 
    added_logs;
    deleted_logs;
  } = res in

  begin match deleted_logs with
  | {index = 4; term = 1; id = "NC"; _} :: [] -> () 
  | _ -> assert(false)
  end;
    (* The non commited log added by server0 in term [1] is deleted since
     * it was never replicated anywher and server1 is now the leader
     * in a later term *)

  assert(Types.is_follower server0); 
  assert(3 = server0.current_term); 
  assert(5 = recent_log_length server0);
  begin match committed_logs with
  | {index = 3; term = 1; _}::{index = 4; term = 3; _}::
    {index = 5; term = 3; _}::[] -> ()
  | _ -> assert(false)
  end;
  assert(5 = server0.commit_index);
  assert(2 = List.length added_logs); 

  begin match msg_for_server server0_response 1 with 
  | Append_entries_response r ->  
    assert(r.receiver_id = 0);
    assert(r.receiver_term = 3);
    assert(r.result = Success 5); 
  | _ -> assert(false); 
  end; 
  assert(Some (New_leader 1) = leader_change);

  (* 
   * Send Append_entries_request Server1 -> Server2 
   * ----------------------------------------------------------------------
   *)
  
  let res = 
    let msg = msg_for_server msgs 2 in
    Raft_logic.handle_message server2 msg now
  in

  let {
    Logic.state = server2; 
    messages_to_send = server2_response; 
    leader_change;
    committed_logs; 
    added_logs;
    deleted_logs;
  } = res in

  assert(deleted_logs = []);
  assert(Types.is_follower server2); 
  assert(3 = server2.current_term); 
  assert(5 = recent_log_length server2);
  begin match committed_logs with
  | {index = 4; term = 3; _}::{index = 5; term = 3; _}::[] -> ()
  | _ -> assert(false)
  end;
  assert(5 = server2.commit_index);
  assert(0 = List.length added_logs); 

  begin match msg_for_server server2_response 1 with 
  | Append_entries_response r ->  
    assert(r.receiver_id = 2);
    assert(r.receiver_term = 3);
    assert(r.result = Success 5); 
  | _ -> assert(false); 
  end; 
  assert(None= leader_change);

  let now = now +. 0.001 in 
  
  (* 
   * Spend Append_entries_response Server0 -> Server1 
   * -------------------------------------------------------------------
   *) 

  let res = 
    let msg = msg_for_server server0_response 1 in
    Raft_logic.handle_message server1 msg now
  in
  let {
    Logic.state = server1; 
    messages_to_send = msgs; 
    leader_change;
    committed_logs; 
    added_logs;
    deleted_logs;
  } = res in
  assert(deleted_logs = []);
  assert([] = msgs); 
  assert([] = committed_logs); 
  assert([] = added_logs); 
  assert(None = leader_change);

  (* 
   * Spend Append_entries_response Server2 -> Server1 
   * -------------------------------------------------------------------
   *) 
  
  let res = 
    let msg = msg_for_server server2_response 1 in
    Raft_logic.handle_message server1 msg now
  in
  let {
    Logic.state = server1; 
    messages_to_send = msgs; 
    leader_change;
    committed_logs; 
    added_logs;
    deleted_logs;
  } = res in
  assert(deleted_logs = []);
  assert([] = msgs); 
  assert([] = committed_logs); 
  assert([] = added_logs); 
  assert(None = leader_change);

  ({server0; server1; server2}, now)

(* Prior to this test, the log size was 5, but in this test we're adding
 * 3 new entries which means the resulting size would be 8. 8 is greated
 * than the configured max log size upper boud and so the log size 
 * limitation enforcement should kick in. Log entries [1] [2] [3] should 
 * be removed from the Raft_log.recent_entries to make sure 
 * the recent entries size is set to lower bound (ie [5]). 
 *
 * Intentionally at the end of this test we returned the server state
 * unmodified, this test is solely checking the log size enforcement 
 * and has no side effect *)
let enforce_log_size ({server0; server1; _} as servers) now = 
  let new_log_result =
    let datas = [
      (Bytes.of_string "Message06", "06");
      (Bytes.of_string "Message07", "07");
      (Bytes.of_string "Message08", "08");
    ] in
    Raft_logic.handle_add_log_entries server1 datas now
  in
  
  let min_max_index server = 
    (
      fst @@ Log.IntMap.min_binding server.log.Log.recent_entries, 
      fst @@ Log.IntMap.max_binding server.log.Log.recent_entries
    ) 
  in

  let min_index, max_index = min_max_index server1 in  
  assert(1 = min_index); 
  assert(5 = max_index); 

  let server1, msgs =
    let open Raft_logic in
    match new_log_result with
    | Appended {Logic.state; messages_to_send; added_logs; _ } -> 
      begin 
        assert(3 = List.length added_logs); 
        (state, messages_to_send)
      end
    | Delay | Forward_to_leader _ -> assert(false)
  in

  let min_index, max_index = min_max_index server1 in  
  assert(4 = min_index); 
  assert(8 = max_index); 

  (* 
   * Send Append_entries_request Server1 -> Server0 
   * ---------------------------------------------- 
   *)

  let res = 
    let msg = msg_for_server msgs 0 in
    Raft_logic.handle_message server0 msg now
  in

  let {
    Logic.state = server0; _
  } = res in

  let min_index, max_index = min_max_index server0 in 
  assert(4 = min_index); 
  assert(8 = max_index); 

  (servers, now)

let ()  =

  let servers = init () in 
  let servers, now = election_1 servers now in 
  let servers, now = failed_election_1 servers now in 
  let servers, now = leader_heartbeat_1 servers now in 
  let servers, now = add_first_log servers now in 
  let servers, now = add_second_log servers now in 
  let servers, now = leader_heartbeat_2 servers now in
  let servers, now = add_third_log servers now in 
  let servers, now = failed_election_2 servers now in  
  let servers, now = election_2 servers now in 
  let servers, now = add_4_and_5_logs servers now in 
  let servers, now = add_log_to_outdated_leader servers now in 
  let servers, now = leader_heartbeat_3 servers now in 
  let servers, now = enforce_log_size servers now in 
  let _ = servers and _ = now in 

  ()
