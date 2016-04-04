## Raft Protocol ##

> This repo implements the [Raft consensus protocol](https://raft.github.io/) 
> in a functional way using the OCaml language. 

The library solely implements the pure logical part of the protocol. It is 
**agnostic** of the transport mechanism, server identification and 
payload (ie log data) format. 

## Project structure ##

#### [Types](src/raft.proto) ####

All the types involved in the RAFT protocoal are defined in the 
[protobuf](https://developers.google.com/protocol-buffers/) format (see [raft.proto](src/raft.proto)). 
[ocaml-protoc](https://github.com/mransan/ocaml-protoc/) compiler is then used to 
generate [the equivalent types](src/raft_pb.mli) in OCaml as well as serialization functions.


#### [Helper Functions](src/raft_helper.mli) ####

The module [Raft_helper](src/raft_helper.mli) provides convenience routines to manipulate the 
generated [types](src/raft_pb.mli). 


#### [Protocol Logic](src/raft_logic.mli) ####

The protocol logic is implemented in the [Raft_logic](src/raft_logic.mli) module. The protcol implementation
is divided into 2 subsections for each of the 2 request/response of the protocol:
* Request Vote : For leader election 
* Append log entry: For state replication 

Each subsection implements the following logic:

**Make Request**

> This function creates a request value given the current state of the server and optionally which
> server the request is sent to. 

**Handle Request**

> This function computes the effect of the received request to the server state. The implementaiton being 
> functional, a new state is returned. 

**Handle Response**

> This function computes the effect of a received response to the server. Along with a new state, a 
> **Follow Up Action** is returned to let the application perform the expected actions. 


#### Example

The example below demonstrates how to use the library to implement an Append Entry request/response
communication between a leader (server 0) and a follower (server 1). In this example the 
leader has a log which needs to be replicated on the follower. 

```OCaml
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
    let follower_1, response = Raft_logic.Append_entries.handle_request follower_1 request in 

    (* 'Update' server 0 (ie leader) by applying the response. This returns
       the new state as well as a follow up action to take. 
     *)
    let leader_0, action = Raft_logic.Append_entries.handle_response leader_0 response in 

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
```
