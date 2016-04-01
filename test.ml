open Raft_pb

module Candidate = Raft_helper.Candidate
module Follower  = Raft_helper.Follower
module Leader    = Raft_helper.Leader 

module Request   = Raft_helper.Request 
module Logic     = Raft_logic  

let default_configuration ()  = {
  nb_of_server = 3; 
  election_timeout = 0.1;
}

let initial_state ?configuration:(configuration = default_configuration ()) id = {
  id;
  current_term = 0; 
  log = []; 
  commit_index = 1234;
  last_applied = 1234;
  role = Follower {voted_for = None} ;
  configuration; 
}
  
let now = 0.

let () = 

  let candidate0 = Candidate.make (initial_state 0) now in 
  let follower1  = initial_state 1 in 
  
  let (candidate0', follow_up_action), follower1' = 
    let request_vote = Request.make_request_vote candidate0 in 
    let follower1', response = Logic.Request_vote.handle_request follower1 request_vote in 
    Logic.Request_vote.handle_response candidate0 response, follower1' 
  in 

  Format.printf "candidate0': %a\n" pp_state candidate0'; 
  Format.printf "follower1' : %a\n" pp_state follower1'; 
  Format.printf "followup action: %a\n" pp_follow_up_action follow_up_action; 
  ()

let () = 
  
  let leader0   = 
    Leader.make (initial_state 0) 
    |> Leader.add_log (Bytes.of_string "Foo") 
    |> Leader.add_log (Bytes.of_string "Bar")
  in 
  let follower1 = initial_state 1 in 

  let (leader0, follow_up_action), follower1, response =
    let request = Request.make_append_entries leader0 1 in
    match request with
    | Some request -> 
      let follower1, response = Logic.Append_entries.handle_request follower1 request in  
      Logic.Append_entries.handle_response leader0 response, follower1, response  
    | None -> failwith "request expected"
  in 

  Format.printf "response: %a\n" pp_append_entries_response response; 
  Format.printf "leader0: %a\n" pp_state leader0; 
  Format.printf "follower1' : %a\n" pp_state follower1; 
  Format.printf "followup action: %a\n" pp_follow_up_action follow_up_action; 
  ()
