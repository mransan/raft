module Types = Raft_types
module Follower = Raft_role.Follower
module Candidate = Raft_role.Candidate
module Leader = Raft_role.Leader
module Configuration = Raft_helper.Configuration
module Log = Raft_log
module Timeout_event = Raft_helper.Timeout_event
module Helper = Raft_helper

type message_to_send = Types.message * Types.server_id

type result = {
  state : Raft_types.state;  
  messages_to_send : message_to_send list; 
  leader_change : Raft_types.leader_change option;
  committed_logs : Raft_log.log_entry list;
  added_logs : Raft_log.log_entry list; 
  deleted_logs : Raft_log.log_entry list;
}

let make_result ?(msgs_to_send = []) ?leader_change ?(deleted_logs = []) 
                ?(committed_logs = []) ?(added_logs = []) state = {
  state;
  messages_to_send = msgs_to_send;
  leader_change; 
  committed_logs;
  added_logs;
  deleted_logs;
}

module Log_entry_util = struct

  let make_append_entries prev_log_index state =

    let to_send, prev_log_term = 
      let since = prev_log_index in 
      let max = state.Types.configuration.Types.max_nb_logs_per_message in 
      Log.log_entries_since ~since ~max state.Types.log
    in 

    let request = Types.({
      leader_term = state.current_term;
      leader_id = state.server_id;
      prev_log_index;
      prev_log_term;
      rev_log_entries = to_send;
      leader_commit = state.commit_index;
    }) in

    request

  let compute_append_entries ?(force = false) state followers now =

    let rec aux followers msgs_to_send = function
      | [] -> (List.rev followers, msgs_to_send)

      | follower::tl ->
        let {
          Types.follower_id;
          heartbeat_deadline;
          outstanding_request;
          next_index;_
        } = follower in

        let shoud_send_request =
          if force || now >= heartbeat_deadline
          then true
            (* The heartbeat deadline is past due, the [Leader] must
             * sent an [Append_entries] request.  *)
          else
            if outstanding_request
            then false
              (* In case of an outstanding request there is no point
               * in sending a new request to that server.
               * Even if the outstanding request was lost and it could be 
               * beneficial to send a new request, this would happen
               * at the next heartbeat. We assume it's more likely that 
               * the server is down and therefore there is no need to keep 
               * on sending the same request over and over. *)
            else
              let prev_index = next_index - 1 in
              let last_log_index = Log.last_log_index state.Types.log in
              if prev_index = last_log_index
              then false
                (* The receipient has the most recent log entry and the 
                 * heartbeat deadline has not expired, no need to send a 
                 * new heartbeat. *)
              else true
                (* The recipient is missing recent log entries *)
        in

        if shoud_send_request
        then
          let request = make_append_entries (next_index - 1) state in
          let follower =
            let outstanding_request = true in
            let heartbeat_deadline =
              now +. state.Types.configuration.Types.hearbeat_timeout
            in
            {follower with
              Types.outstanding_request;
              Types.heartbeat_deadline;
            }
          in
          let followers = follower::followers in
          let msgs_to_send = 
            let msg = (Types.Append_entries_request request, follower_id) in 
            msg::msgs_to_send 
          in
          aux followers msgs_to_send tl
        else
          aux (follower::followers) msgs_to_send tl
    in
    aux [] [] followers

end (* Log_entry_util *)

(** {2 Request Vote} *)

let make_request_vote_request state =
  let index, term = Log.last_log_index_and_term state.Types.log in
  Types.({
    candidate_term = state.current_term;
    candidate_id = state.server_id;
    candidate_last_log_index = index;
    candidate_last_log_term = term;
  })

let handle_request_vote_request state request now =
  let {
    Types.candidate_id; 
    candidate_term; 
    candidate_last_log_index;_} = request in

  let make_response state vote_granted =
    Types.({
      voter_term = state.current_term; 
      voter_id = state.server_id ; 
      vote_granted; 
    })
  in

  if candidate_term < state.Types.current_term
  then
    (* This request is coming from a candidate lagging behind ...
     * no vote for him. *)
    (state, make_response state false)
  else
    let state =
      (* Enforce invariant that if this server is lagging behind
       * it must convert to a follower and update to that term. *)
      if candidate_term > state.Types.current_term
      then Follower.become ~term:candidate_term ~now state
      else state
    in

    let last_log_index = Log.last_log_index state.Types.log in

    if candidate_last_log_index < last_log_index
    then
      (* Enforce the safety constraint by denying vote if this server
       * last log is more recent than the candidate one.*)
      (state, make_response state false)
    else
      let role = state.Types.role in
      match role with
      | Types.Follower {Types.voted_for = None; _} ->
        (* This server has never voted before, candidate is getting the vote
         *
         * In accordance to the `Rules for Servers`, the follower must
         * reset its election deadline when granting its vote. *)

        let {
          Types.configuration = {
            Types.election_timeout; _
          }; _} = state in

        let state = {state with
          Types.role = Types.Follower {
            Types.voted_for = Some candidate_id;
            Types.current_leader = None;
            Types.election_deadline = now +. election_timeout;
          }
        } in
        (state, make_response state true)

      | Types.Follower {Types.voted_for = Some id; _} when id = candidate_id ->
        (* This server has already voted for that candidate... reminding him *)
        (state, make_response state true)

      | _ ->
        (* Server has previously voted for another candidate or
         * itself *)
        (state, make_response state false)

let handle_request_vote_response state response now =
  let {Types.current_term; role; configuration; _ } = state in
  let {Types.voter_term; vote_granted; _ } = response in

  if voter_term > current_term
  then
    (* Enforce invariant that if this server is lagging behind
     * it must convert to a follower and update to the latest term. *)
    let state = Follower.become ~term:voter_term ~now state in
    (state, [])

  else
    match role, vote_granted  with
    | Types.Candidate ({Types.vote_count; _ } as candidate_state) , true ->

      if  Configuration.is_majority configuration (vote_count + 1)
      then
        (* By reaching a majority of votes, the candidate is now 
         * the new leader *)
        let state = Leader.become state now in

        (* As a new leader, the server must send Append entries request
         * to the other servers to both establish its leadership and
         * start synching its log with the others.  *)
        begin match state.Types.role with
        | Types.Leader followers ->

          let followers, msgs_to_send = 
            let force = true in 
            Log_entry_util.compute_append_entries ~force state followers now
          in 

          let state = Types.{state with role = Leader followers} in 
          (state, msgs_to_send) 
        | _ -> assert(false)
        end
      else
        (* Candidate has a new vote but not yet reached the majority *)
        let new_state = Types.{state with
          role = Candidate (Candidate.increment_vote_count candidate_state);
        } in
        (new_state, [] (* no message to send *))

    | Types.Candidate _ , false
      (* The vote was denied, the election keeps on going until
       * its deadline.  *)

    | Types.Follower _ , _
    | Types.Leader   _ , _ -> (state, [])
      (* If the server is either Follower or Leader, it means that
       * it has changed role in between the time it sent the
       * [RequestVote] request and this response.
       * This response can therefore safely be ignored and the server
       * keeps its current state.Types.  *)

(** {2 Append Entries} *)

let update_state leader_commit receiver_last_log_index log state =
  if leader_commit > state.Types.commit_index
  then
    let commit_index = min leader_commit receiver_last_log_index in
    {state with Types.log; commit_index}
  else
    {state with Types.log}

let handle_append_entries_request state request now =
  let {Types.leader_term; leader_id; _} = request in

  let make_response state result =
    Types.({
      receiver_term = state.current_term; 
      receiver_id = state.server_id; 
      result;
    })
  in

  if leader_term < state.Types.current_term
  then
    (* This request is coming from a leader lagging behind... *)
    (state, make_response state Types.Term_failure, Log.empty_diff)

  else
    (* This request is coming from a legetimate leader,
     * let's ensure that this server is a follower.  *)
    let state =
      let current_leader = leader_id and term = leader_term in
      Follower.become ~current_leader ~term ~now state
    in

    (* Next step is to handle the log entries from the leader.  *)
    let {
      Types.prev_log_index;
      prev_log_term;
      rev_log_entries;
      leader_commit; _ } = request in

    let (
      receiver_last_log_index,
      receiver_last_log_term
    ) = Log.last_log_index_and_term state.Types.log in

    let commit_index = state.Types.commit_index in

    if prev_log_index < commit_index
    then
      (* The only reason that can happen is if the messages
       * are delivered out of order. (Which is completely possible).
       * No need to update this follower. *)
      let success = Types.Success  receiver_last_log_index in 
      (state, make_response state success, Log.empty_diff)

    else
      if leader_term = receiver_last_log_term
      then
        match compare prev_log_index receiver_last_log_index with
        | 0 ->
          (* Leader info about the receiver last log index is matching
           * perfectly, we can append the logs.  *)
          let log, log_diff = 
            Log.add_log_entries ~rev_log_entries state.Types.log 
          in
          let receiver_last_log_index = Log.last_log_index log in
          let state =
            update_state leader_commit receiver_last_log_index log state
          in

          let success = Types.Success  receiver_last_log_index in 
          (state, make_response state success, log_diff)

        | x when x > 0 ->
          (* This should really never happen since:
           * - No logs belonging to the Leader term can be removed
           * - The leader is supposed to keep track of the latest log from
           *   the receiver within the same term.
           *)
          let failure = Types.Log_failure receiver_last_log_index in 
          (state, make_response state failure, Log.empty_diff)

        | _ (* x when x < 0 *) ->
          (* This case is possible when messages are received out of order  by
           * the Follower
           *
           * Note that even if the prev_log_index is earlier, it's important
           * that no log entry is removed from the log if they come from the
           * current leader.
           *
           * The current leader might have sent a commit message back to a
           * client believing that the log entry is replicated on this server.
           * If we remove the log entry we violate the assumption.  *)
          let success = Types.Success  receiver_last_log_index in 
          (state, make_response state success, Log.empty_diff)

      else (* leader_term > receiver_last_log_term *)

        if prev_log_index > receiver_last_log_index
        then
          (* This is likely the case after a new election, the Leader has
           * more log entries in its log and assumes that all followers have 
           * the same number of log entries. *)
          let failure = Types.Log_failure receiver_last_log_index in 
          (state, make_response state failure, Log.empty_diff)
        else
          (* Because it is a new Leader, this follower can safely remove all
           * the logs from previous terms which were not committed.  *)

          match Log.remove_log_since ~prev_log_index
                ~prev_log_term state.Types.log with
          | exception Not_found ->
            let failure = Types.Log_failure commit_index in
            (state, make_response state failure, Log.empty_diff)
            (* This is the case where there is a mismatch between the [Leader]
             * and this server and the log entry identified with
             * (prev_log_index, prev_log_term)
             * could not be found.
             *
             * In such a case, the safest log entry to synchronize upon is the
             * commit_index
             * of the follower. *)

          | log, log_diff ->
            let log, log_diff' = Log.add_log_entries ~rev_log_entries log in
            let log_diff = Log.merge_diff log_diff log_diff' in 
            let receiver_last_log_index = Log.last_log_index log in
            let state =
              update_state leader_commit receiver_last_log_index log state
            in
            let success = Types.Success receiver_last_log_index in 
            (state, make_response state success, log_diff)

let handle_append_entries_response state response now =
  let {
    Types.receiver_term; 
    receiver_id = follower_id ; 
    result} = response in

  if receiver_term > state.Types.current_term
  then
    (* Enforce invariant that if the server is lagging behind
     * it must convert to a follower and update to that term.  *)
    (Follower.become ~term:receiver_term ~now state , [])

  else
    match state.Types.role with
    | Types.Follower _
    | Types.Candidate _ -> (state, [])

    | Types.Leader followers ->

      let followers =
        Leader.record_response_received ~follower_id followers
      in

      begin match result with
      | Types.Success follower_last_log_index ->
        (* Log entries were successfully inserted by the receiver...
         *
         * let's update our leader state about that receiver *)

        let configuration = state.Types.configuration in

        let followers , nb_of_replications = 
          Leader.update_follower_last_log_index 
                ~follower_id ~index:follower_last_log_index followers
        in

        let state =
          (* Check if the received log entry from has reached
           * a majority of server.
           * Note that we need to add `+1` simply to count this
           * server (ie leader) which does not update its next/match
           * *)
          if Configuration.is_majority configuration (nb_of_replications + 1) &&
             follower_last_log_index > state.Types.commit_index
          then {state with Types.commit_index = follower_last_log_index;}
          else state
        in

        let followers, msgs_to_send =
          Log_entry_util.compute_append_entries state followers now
        in

        let state = Types.({state with role = Leader followers}) in

        (state, msgs_to_send)

    | Types.Log_failure follower_last_log_index ->
      (* The receiver log is not matching this server current belief.
       * If a leader this server should decrement the next
       * log index and retry the append.  *)
      let leader_state = 
        Leader.decrement_next_index 
              ~follower_last_log_index ~follower_id state followers
      in
      let leader_state, msgs_to_send = 
        Log_entry_util.compute_append_entries state leader_state now 
      in
      let state = Types.({state with role = Leader leader_state}) in
      (state, msgs_to_send)

    | Types.Term_failure  ->
      (state, [])
      (* The receiver could have detected that this server term was not the 
       * latest one and sent the [Term_failure] response.
       *
       * This could typically happen in a network partition:
       *      
       *      Old-leader---------X-------New Leader
       *           \                         /
       *            ---------Follower---------
       *
       * In the diagram above this server is the Old leader.
       *)

    end (* match result *)

let make_initial_state ~configuration ~now ~server_id () =
  Follower.create ~configuration ~now ~server_id ()

let handle_message state message now =
  let state', msgs_to_send, log_diff =
    match message with
    | Types.Request_vote_request ({Types.candidate_id; _ } as r) ->
      let state, response = handle_request_vote_request state r now in
      let msgs = 
        (Types.Request_vote_response response, candidate_id)::[]
      in 
      (state, msgs, Log.empty_diff)

    | Types.Append_entries_request ({Types.leader_id; _ } as r) ->
      let state, response, log_diff = 
        handle_append_entries_request state r now 
      in 
      let msgs = 
        (Types.Append_entries_response response, leader_id) :: []
      in 
      (state, msgs, log_diff)

    | Types.Request_vote_response r ->
      let state, msgs = handle_request_vote_response state r now in
      (state, msgs, Log.empty_diff)

    | Types.Append_entries_response r ->
      let state, msgs = handle_append_entries_response state r now in
      (state, msgs, Log.empty_diff) 

  in
  let leader_change = Helper.leader_change state state' in 
  let committed_logs = Helper.committed_logs state state' in
  let {Log.added_logs; deleted_logs} = log_diff in 
  make_result ~msgs_to_send ?leader_change ~added_logs 
              ~deleted_logs ~committed_logs state'

(* Iterates over all the other server ids. (ie the ones different
 * from the state id).  *)
let fold_over_servers f e0 state =

  let {
    Types.server_id;
    configuration = {Types.nb_of_server; _}; _
  } = state in

  let rec aux acc = function
    | -1 -> acc
    | id   ->
      let next = id - 1 in
      if id  = server_id
      then aux acc next
      else aux (f acc id) next
  in
  aux e0 (nb_of_server - 1)
let handle_new_election_timeout state now =
  let state' = Candidate.become ~now state in
  let msgs_to_send =
    fold_over_servers (fun acc server_id ->
      let request = make_request_vote_request state' in
      (Types.Request_vote_request request, server_id) :: acc
    ) [] state'
  in
  let leader_change = Helper.leader_change state state' in 
  let committed_logs = Helper.committed_logs state state' in
  make_result ~msgs_to_send ?leader_change ~committed_logs state'

let handle_heartbeat_timeout state now =
  match state.Types.role with
  | Types.Leader leader_state ->
    let leader_state, msgs_to_send =
      Log_entry_util.compute_append_entries state leader_state now
    in
    let state = Types.({state with role = Leader leader_state}) in
    make_result ~msgs_to_send state
  | _ ->
    make_result state

type new_log_response =
  | Appended of result 
  | Forward_to_leader of int
  | Delay

let handle_add_log_entries state datas now =
  match state.Types.role with
  | Types.Follower {Types.current_leader = None ; _ }
  | Types.Candidate _ ->
    Delay
    (* Server in the middle of an election with no [Leader]
     * agreed upon yet *)

  | Types.Follower {Types.current_leader = Some leader_id; _ } ->
    Forward_to_leader leader_id
    (* The [Leader] should be the one centralizing all the
     * new log entries.  *)

  | Types.Leader leader_state ->

    let log, log_diff = 
      Log.add_log_datas state.Types.current_term datas state.Types.log
    in
    
    let state' = Types.({state with log }) in

    let leader_state, msgs_to_send =
      Log_entry_util.compute_append_entries state' leader_state now
    in
    
    let state' = Types.({state' with role = Leader leader_state }) in
    let {Log.added_logs; deleted_logs} = log_diff in 
    Appended (make_result ~msgs_to_send ~added_logs ~deleted_logs state')

let next_timeout_event = Timeout_event.next

let committed_entries_since ~since {Types.commit_index; log; _} = 
  fst @@ Log.log_entries_since ~since ~max:commit_index log  
