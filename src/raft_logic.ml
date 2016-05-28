
open Raft_pb

module State            = Raft_state
module Follower         = Raft_role.Follower
module Candidate        = Raft_role.Candidate
module Leader           = Raft_role.Leader
module Configuration    = Raft_helper.Configuration
module Rev_log_cache    = Raft_revlogcache
module Timeout_event    = Raft_helper.Timeout_event

type time = float

type server_id = int

type message_to_send = message * int

module Log_entry_util = struct

  let rec keep_first_n l = function
    | 0 -> []
    | n ->
      begin match l with
      | hd::tl -> hd::(keep_first_n tl (n - 1))
      | []     -> []
      end

  let make_append_entries prev_log_index local_cache state =

    let global_cache = state.global_cache in 

    let local_cache = 
      Rev_log_cache.update_local_cache prev_log_index state.log local_cache global_cache 
    in

    let max = state.configuration.max_nb_logs_per_message in

    let rev_log_entries = 
      match local_cache.rev_log_entries with
      | Compacted _ -> [] 
      | Expanded {entries} -> 
        keep_first_n entries max 
    in

    let request = {
      leader_term = state.current_term;
      leader_id = state.id;
      prev_log_index;
      prev_log_term = local_cache.prev_term;
      rev_log_entries;
      leader_commit = state.commit_index;
    } in
    (local_cache, request)

  let compute_append_entries state {indices} now =

    let indices, msgs_to_send = List.fold_left (fun (indices, msgs_to_send) server_index ->
      let {
        server_id;
        heartbeat_deadline;
        outstanding_request;
        next_index;
        local_cache; _ } = server_index in

      let shoud_send_request =
        if now >= heartbeat_deadline
        then true
          (* The heartbeat deadline is past due, the [Leader] must
           * sent an [Append_entries] request.
           *)
        else
          if outstanding_request
          then false
            (* In case of outstanding request there is no point
             * in sending request to that server.
             * Even if the outstanding request was lost and it would
             * be good to send a new request, this would happen
             * at the next heartbeat.
             *)
          else
            let prev_index = next_index - 1 in
            let last_log_index = State.last_log_index state in
            if prev_index = last_log_index
            then false
              (* The receipient has the most recent data so no need
               * to send a request.
               *)
            else true
              (* The recipient has missing data.
               *)
      in

      if shoud_send_request
      then
        let local_cache, request = make_append_entries (next_index - 1) local_cache state in
        let server_index = 
          let outstanding_request = true in 
          let heartbeat_deadline = now +. state.configuration.hearbeat_timeout in 
          {server_index with
            local_cache; 
            outstanding_request; 
            heartbeat_deadline; 
          } 
        in 
        let indices      = server_index::indices in
        let msgs_to_send = (Append_entries_request request, server_id)::msgs_to_send in
        (indices, msgs_to_send)

      else
        (server_index::indices, msgs_to_send)

    ) ([], []) indices in

    let leader_state = {indices} in

    (leader_state, msgs_to_send)

end (* Log_entry_util *)

(** {2 Request Vote} *)

let make_request_vote_request state =
  let index, term  = State.last_log_index_and_term state in
  {
    candidate_term = state.current_term;
    candidate_id = state.id;
    candidate_last_log_index = index;
    candidate_last_log_term = term;
  }

let handle_request_vote_request state request now =

  let {candidate_id; candidate_term; candidate_last_log_index;_} = request in

  let make_response state vote_granted =
    {voter_term = state.current_term; voter_id = state.id ; vote_granted; }
  in

  if candidate_term < state.current_term
  then
    (*
     * This request is coming from a candidate lagging behind ...
     * no vote for him.
     *
     *)
    (state, make_response state false)
  else
    let state =
      (*
       * Enforce invariant that if this server is lagging behind
       * it must convert to a follower and update to that term.
       *
       *)
      if candidate_term > state.current_term
      then 
        Follower.become ~term:candidate_term ~now state
      else 
        state
    in
    let last_log_index = State.last_log_index state in
    if candidate_last_log_index < last_log_index
    then
      (*
       * Enforce the safety constraint by denying vote if this server
       * last log is more recent than the candidate one.
       *
       *)
      (state, make_response state false)
    else
      let role = state.role in
      match role with
      | Follower {voted_for = None; _} ->
        (*
         * This server has never voted before, candidate is getting the vote
         *
         * In accordance to the `Rules for Servers`, the follower must
         * reset its election deadline when granting its vote.
         *
         *)

        let {configuration = {election_timeout; _ }; _} = state in
        let state ={state with
          role = Follower {
            voted_for         = Some candidate_id;
            current_leader    = None;
            election_deadline = now +. election_timeout;
          }
        } in
        (state, make_response state true)

      | Follower {voted_for = Some id} when id = candidate_id ->
        (*
         * This server has already voted for that candidate ... reminding him
         *
         *)
        (state, make_response state true)

      | _ ->
        (*
         * Server has previously voted for another candidate or
         * itself
         *
         *)
        (state, make_response state false)

let handle_request_vote_response state response now =

  let {current_term; role; configuration; _ } = state in
  let {voter_term; vote_granted} = response in

  if voter_term > current_term
  then
    (*
     * Enforce invariant that if this server is lagging behind
     * it must convert to a follower and update to the latest term.
     *
     *)
    let state = Follower.become ~term:voter_term ~now state in
    (state, [])

  else
    match role, vote_granted  with
    | Candidate ({vote_count; election_deadline} as candidate_state) , true ->
      let has_majority = vote_count >= (configuration.nb_of_server / 2) in
      if  has_majority
      then
        (*
         * By reaching a majority of votes, the
         * candidate is now the new leader
         *
         *)
        let state = Leader.become state now in

        (*
         * As a new leader, the server must send Append entries request
         * to the other servers to both establish its leadership and
         * start synching its log with the others.
         *)
        begin match state.role with
        | Leader {indices} -> (
          let indices, msgs_to_send = List.fold_left (fun (indices, msgs_to_send) ({server_id; next_index; local_cache } as index) ->

              let prev_log_index = next_index - 1 in
              let cache, req  = Log_entry_util.make_append_entries prev_log_index local_cache state in
              let msg_to_send = (Append_entries_request req, server_id) in
              ({index with local_cache}::indices, msg_to_send::msgs_to_send)

            ) ([], []) indices
          in
          ({state with role = Leader {indices}}, msgs_to_send)
        )
        | _ -> assert(false)
        end
      else
        (* Candidate has a new vote but not yet reached the majority
         *)
        let new_state = {state with
          role = Candidate (Candidate.increment_vote_count candidate_state);
        } in
        (new_state, [])

    | Candidate _ , false
      (* The vote was denied, the election keeps on going until
       * its deadline.
       *)

    | Follower _ , _
    | Leader   _ , _ -> (state, [])
      (* If the server is either Follower or Leader, it means that
       * it has changed role in between the time it sent the
       * [RequestVote] request and this response.
       * This response can therefore safely be ignored and the server
       * keeps its current state.
       *)

(** {2 Append Entries} *)

let handle_append_entries_request state request now =

  let {leader_term; leader_commit; leader_id;} = request in

  let make_response state result =
    {receiver_term = state.current_term; receiver_id = state.id; result;}
  in

  if leader_term < state.current_term
  then
    (* This request is coming from a leader lagging behind...
     *
     * TODO [INVARIANTS] We could check here that this request is 
     * not coming from the current leader. (Which would be a violation
     * of the protocol rules). 
     *)
    (state, make_response state Term_failure)

  else
    (* This request is coming from a legetimate leader,
     * let's ensure that this server is a follower.
     *)
    let state  = Follower.become ~current_leader:leader_id ~term:leader_term ~now state in

    (* Next step is to handle the log entries from the leader.
     *
     *)
    let {
      prev_log_index; 
      prev_log_term; 
      rev_log_entries; 
      leader_commit} = request in
      
    let state, success = 
      State.merge_logs ~prev_log_index ~prev_log_term ~rev_log_entries state 
    in 
    let receiver_last_log_index, 
        receiver_last_log_term = State.last_log_index_and_term state in  
    
    let state =
      (* Update this server commit index based on value sent from
       * the leader.
       * Note that it is not guaranteed that the leader has sent
       * all the log until it commit index so the min value
       * is taken.
       *)
      if leader_commit > state.commit_index
      then Rev_log_cache.update_global_cache state.commit_index {state with
        commit_index = min leader_commit receiver_last_log_index
      }
      else state
    in

    let response = 
      if success
      then 
        make_response state (Success {receiver_last_log_index}) 
      else 
        make_response state (Log_failure {
          receiver_commit_index = state.commit_index;
        }) 
    in

    (state, response)

let handle_append_entries_response state ({receiver_term; _ } as response) now =

  let {receiver_term; receiver_id; result} = response in

  if receiver_term > state.current_term
  then
    (* Enforce invariant that if the server is lagging behind
     * it must convert to a follower and update to that term.
     *)
    let state = Follower.become ~term:receiver_term ~now state in
    (state, [])

  else
    match state.role with
    | Follower _
    | Candidate _ -> (state, [])

    | Leader leader_state ->

      let leader_state =
        Leader.record_response_received ~receiver_id leader_state
      in

      begin match result with
      | Success {receiver_last_log_index} ->
        (* Log entries were successfully inserted by the receiver...
         *
         * let's update our leader state about that receiver
         *)

        let configuration = state.configuration in

        let leader_state, nb_of_replications = Leader.update_receiver_last_log_index
          ~receiver_id
          ~log_index:receiver_last_log_index
          leader_state
        in

        let state =
          (*
           * Check if the received log entry from has reached
           * a majority of server.
           * Note that we need to add `+1` simply to count this
           * server (ie leader) which does not update its next/match
           *
           *)
          if Configuration.is_majority configuration (nb_of_replications + 1) &&
             receiver_last_log_index > state.commit_index
          then 
            Rev_log_cache.update_global_cache state.commit_index {state with commit_index = receiver_last_log_index}
          else 
            state
        in

        let leader_state, msgs_to_send = 
          Log_entry_util.compute_append_entries state leader_state now 
        in

        let state = {state with role = Leader leader_state} in

        (state, msgs_to_send)

    | Log_failure log_failure ->
      (*
       * The receiver log is not matching this server current belief.
       * If a leader this server should decrement the next
       * log index and retry the append.
       *
       *)
      let leader_state = Leader.decrement_next_index ~log_failure ~receiver_id state leader_state in
      let leader_state, msgs_to_send = Log_entry_util.compute_append_entries state leader_state now in
      let state  = {state with role = Leader leader_state} in
      (state,msgs_to_send)

    | Term_failure  -> assert(false)
      (*
       * This should have already been detected when comparing the receiver_term with
       * the [state.current_term].
       *)

    end (* match result *)

let make_initial_state ~configuration ~now ~id () =
  Follower.create ~configuration ~now ~id ()

let handle_message state message now =
  let state', message_to_send = 
    match message with
    | Request_vote_request ({candidate_id; _ } as r) ->
      let state, response = handle_request_vote_request state r now in
      (state, [(Request_vote_response response, candidate_id)])

    | Append_entries_request ({leader_id; _ } as r) ->
      let state, response = handle_append_entries_request state r now in
      (state, [(Append_entries_response response, leader_id)])

    | Request_vote_response r ->
      handle_request_vote_response state r now

    | Append_entries_response r ->
      handle_append_entries_response state r now
  in
  (state', message_to_send , State.notifications state state')

(* Iterates over all the other server ids. (ie the ones different
 * from the state id).
 *)
let fold_over_servers f e0 {id; configuration = {nb_of_server;_ }; _ } =
  let rec aux acc = function
    | -1 -> acc
    | server_id  ->
      let next = server_id - 1 in
      if server_id = id
      then aux acc next
      else aux (f acc server_id) next
  in
  aux e0 (nb_of_server - 1)

let handle_new_election_timeout state now =
  let state' = Candidate.become ~now state in
  let msgs  =
    fold_over_servers (fun acc server_id ->
      let request = make_request_vote_request state' in
      (Request_vote_request request, server_id) :: acc
    ) [] state'
  in
  (state', msgs, State.notifications state state')

let handle_heartbeat_timeout ({role; configuration; _ } as state) now =
  match state.role with
  | Leader leader_state ->
    let leader_state, msgs_to_send =
      Log_entry_util.compute_append_entries state leader_state now
    in
    let state = {state with role = Leader leader_state} in
    (state, msgs_to_send)
  | _ ->
    (state, [])

type new_log_response =
  | Appended of state * message_to_send list
  | Forward_to_leader of int
  | Delay

let handle_add_log_entries state datas now =
  match state.role with
  | Follower {current_leader = None ; _ }
  | Candidate _ ->
    Delay
    (* Server in the middle of an election with no [Leader]
     * agreed upon yet
     *)

  | Follower {current_leader = Some leader_id; _ } ->
    Forward_to_leader leader_id
    (* The [Leader] should be the one centralizing all the
     * new log entries.
     *)

  | Leader _ -> 

    let state = State.add_logs datas state in 
    begin match state.role with
    | Follower  _ | Candidate _ -> assert(false)
      (* We don't expect the [Leader.add_log] functions to
       * change the role.
       *)

    | Leader ({indices} as leader_state) ->
      let leader_state, msgs_to_send = Log_entry_util.compute_append_entries state leader_state now in
      let state = {state with role = Leader leader_state } in
      Appended (state, msgs_to_send)

    end (* match state.role *)

let next_timeout_event = Timeout_event.next
