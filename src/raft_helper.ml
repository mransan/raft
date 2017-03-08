module Types = Raft_types
module Log = Raft_log

module Configuration = struct

  let is_majority {Types.nb_of_server; _} nb =
    nb > (nb_of_server / 2)

  let election_timeout configuration = 
    let {
      Types.election_timeout = t;
      election_timeout_range = r; _ } = configuration in
    t +. (Random.float r -. (r /. 2.)) 

end

module Follower = struct

  let make ?log ?commit_index ?current_term ~configuration ~now ~server_id () =

    let current_term = match current_term with
      | None -> 0 
      | Some current_term -> current_term 
    in 

    let log = match log with
      | None -> Log.empty configuration.Types.max_log_size 
      | Some log -> log 
    in 

    let commit_index = match commit_index with
      | None -> 0 
      | Some commit_index -> commit_index
    in 

    let timeout = Configuration.election_timeout configuration in
    {
      Types.current_term;
      server_id;
      log;
      commit_index;
      role = Types.(Follower {
        voted_for = None;
        current_leader = None;
        election_deadline = now +.  timeout
      });
      configuration;
    }

  let become ?current_leader ~now ~term state =
    let election_deadline = 
      now +. Configuration.election_timeout state.Types.configuration 
    in 

    let role = match state.Types.role with
      | Types.Follower follower_state ->
        let voted_for =
          if term = state.Types.current_term
          then follower_state.Types.voted_for
          else None
        in
        Types.Follower {
          Types.current_leader;
          Types.election_deadline;
          Types.voted_for;
        }
      | Types.Candidate _ when state.Types.current_term = term ->
        Types.Follower {
          Types.voted_for = Some state.Types.server_id;
          Types.current_leader;
          Types.election_deadline;
        }
      | _ -> Types.Follower {
        Types.voted_for = None;
        Types.current_leader;
        Types.election_deadline;
      }
    in
    { state with Types.current_term = term; role }

end

module Candidate = struct

  let become ~now state =
    let timeout = Configuration.election_timeout state.Types.configuration in 
    let role = Types.Candidate {
      Types.vote_count = 1;
      Types.election_deadline = now +. timeout;
    } in
    {state with
     Types.role;
     Types.current_term = state.Types.current_term + 1;
    }

  let increment_vote_count ({Types.vote_count; _ } as candidate_state) =
    {candidate_state with Types.vote_count = vote_count + 1}

end

module Leader = struct

  let become ~now state =

    let last_log_index = Log.last_log_index state.Types.log in

    let {
      Types.nb_of_server;
      hearbeat_timeout; _} = state.Types.configuration in

    let rec aux followers = function
      | (-1) -> followers
      |  i   ->
        if i = state.Types.server_id
        then
          aux followers (i -1)
        else
          let next_index = last_log_index + 1 in
          let match_index = 0  in

          let follower:Types.follower_info = {
            Types.follower_id = i;
            next_index;
            match_index;
            outstanding_request = false;
            heartbeat_deadline = now +. hearbeat_timeout;
              (*
               * Here the expectation is that after becoming a leader
               * the client application will send a message to all the 
               * receivers and therefore the heartbeat_deadline is set
               * to [now + timeout] rather than [now]. *)
          } in
          aux (follower::followers) (i - 1)
    in
    let followers = aux [] (nb_of_server - 1) in

    {state with Types.role = Types.(Leader followers)}

  (* Reusable function to update the index of a particular
   * receiver id. *)
  let update_follower ~follower_id ~f leader_state =

    List.map (fun follower ->
      if follower.Types.follower_id = follower_id 
      then (f follower)
      else follower
    ) leader_state

  let update_follower_last_log_index ~follower_id ~index followers =

    let followers = update_follower ~follower_id ~f:(fun follower ->
      let {Types.next_index; match_index; _} = follower  in
      let upd_next_index = index + 1 in
      let upd_match_index = index in
      if upd_match_index > match_index && upd_next_index > next_index
      then
        {follower with Types.next_index = index + 1; match_index = index}
      else
        (* It is possible to receive out of order responses from the other
         * raft servers.
         *
         * In such a case we don't want to decrement the next index 
         * of the server since the server is expected to never remove 
         * previously saved log entries.  *)
        follower
    ) followers 
    in

    (* Calculate the number of server which also have replicated that
       log entry *)
    let nb_of_replications = List.fold_left (fun n {Types.match_index; _ } ->
      if match_index >= index 
      then n + 1
      else n
    ) 0 followers in

    (followers, nb_of_replications)

  let decrement_next_index 
                ~follower_last_log_index ~follower_id state followers =
    let latest_log_index = Log.last_log_index state.Types.log  in

    assert(follower_last_log_index <= latest_log_index);
      (* This is an invariant. The server cannot have replicated more logs
       * than the Leader.
       *
       * However due to message re-ordering it is possible to receive a 
       * [LogFailure] with the receiver_last_log_index equal to 
       * the latest_log_index.
       *
       * Consider the following scenario
       * - [Leader] Append_entry prev_index = x rev_log_entries [x+1]
       * - [Server] receives the request and correctly replicate x + 1
       * - !! RESPONSE IS LOST !!
       * - [Leader] Append_entry prev_index = x rev_log_entries [x+1]
       * - [Server] return a failure since it has replicated x + 1 and cannot
       *   remove that log entry since it is coming from the current term 
       *   leader.  *) 
    update_follower ~follower_id ~f:(fun index ->
      {index with
       Types.next_index = follower_last_log_index + 1;
       Types.match_index = follower_last_log_index}
    )  followers 

  let record_response_received ~follower_id followers =

    update_follower
      ~follower_id
      ~f:(fun index ->
        {index with Types.outstanding_request = false;}
      )
      followers 

  let min_heartbeat_timout ~now followers =

    let min_heartbeat_deadline = List.fold_left (fun min_deadline f ->
      let {Types.heartbeat_deadline; _ } = f in
        if heartbeat_deadline < min_deadline
        then heartbeat_deadline
        else min_deadline
      ) max_float followers
    in
    min_heartbeat_deadline -. now

end (* Leader *)

module Timeout_event = struct

  let existing_election_wait election_deadline now =
    Types.({
      timeout = election_deadline -. now;
      timeout_type = New_leader_election;
    })

  let make_heartbeat_wait timeout =
    Types.({
      timeout = timeout;
      timeout_type = Heartbeat;
    })

  let next ~now state =
    match state.Types.role with
    | Types.Follower {Types.election_deadline; _} ->
      existing_election_wait election_deadline  now
    | Types.Leader leader_state ->
      make_heartbeat_wait (Leader.min_heartbeat_timout ~now leader_state)
    | Types.Candidate {Types.election_deadline; _ } ->
      existing_election_wait election_deadline  now

end (* Timeout_event *)

module Diff = struct 

  let leader_change before after =
    let open Types in 
  
    let { role = brole; _ } = before in
    let { role = arole; _ } = after in
  
    match brole, arole with
    | Follower _   , Leader _
    | Leader _     , Candidate _ ->
      (* Impossible transition which would violate the rules of the
       * RAFT protocol *)
      assert(false)
  
    | Candidate _  , Leader _ ->
      (* Case of the server becoming a leader *)
      Some (New_leader after.server_id)
  
    | Follower {current_leader = Some bleader; _ },
      Follower {current_leader = Some aleader; _ } when bleader = aleader ->
      None
      (* No leader change, following the same leader *)
  
    | _, Follower{current_leader = Some aleader;_} ->
      Some (New_leader aleader)
      (* There is a new leader *)
  
    | Follower {current_leader = Some _; _}, Candidate _
    | Follower {current_leader = Some _; _}, Follower {current_leader = None; _}
    | Leader  _  , Follower  {current_leader = None; _} ->
      Some No_leader
  
    | Leader _                             , Leader _
    | Candidate _                          , Candidate _
    | Candidate _                          , Follower {current_leader = None;_}
    | Follower {current_leader = None; _}  , Follower {current_leader = None;_}
    | Follower {current_leader = None; _}  , Candidate _ ->
      None
  
  let committed_logs before after = 
    let open Types in 
  
    let { commit_index = bcommit_index;  _ } = before in
    let { commit_index = acommit_index;  _ } = after in
  
    if acommit_index > bcommit_index
    then
      let recent_entries = after.log.Log.recent_entries in 
      let _, prev_commit, sub = Log.IntMap.split bcommit_index recent_entries in 
      begin match prev_commit with 
      | None -> assert(bcommit_index = 0)
      | Some _ -> ()
      end;
      let sub, last_commit, _ = Log.IntMap.split acommit_index sub in 
      let sub = match last_commit with 
        | None -> assert(false) 
        | Some ({Log.index; _ } as log_entry) -> 
          Log.IntMap.add index log_entry sub 
      in 
      let committed_entries = List.map snd (Log.IntMap.bindings sub) in  
      committed_entries
    else
      (* Should we assert false if after < before, ie it's a violation 
         of the RAFT protocol *)
      []

end (* Diff *)
