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
    | {data; _ } :: `` -> 
      if "Foo" = Bytes.to_string data
      then print_endline "Log successfully replicated in follower"
      else print_endline "Log replication was corrupted"
    | _ -> print_endline "Log replication failure"
  )
  | None -> () 
```

## Notes on implementing Log size limit ##

**Problem with current implementation**

> The current implementation keeps on adding `log entry`s to an in-memory 
> log data structure and never deletes them. 
> Eventually the process will run out of memory. 

The idea would be to keep only the most recent `log entry`s in memory; keep in 
mind that all `log entry`s are permanently recorded on disk. 

**Problem with Limitting the log size**

In normal mode of operations, followers are replicating the leader log 
quickly and are never too far behind. Problem arise when one of the server is
taken offline for a long time. In this case the follower has a lot of 
`log entry`s to catch up on. This creates 2 major issues:

* The leader does a lot of of work to make the follower catch up. The leader
  keep sending Append Entries Request one after the othe and this usually 
  slows down the leader process which also need to keep append new 
  `log entry`s. 

* If we limit the log size proposed above, then it is likely that if 
  follower has been taken offline for an extended period of time the logs 
  it needs to replicate next are not be stored in the in-memory log data 
  structure of the leader.  

**Solution overview** 

First, the leader can easily detect when the follower is too far behind 
(see notes on backlog situation detection). The next Append Entries request 
it sends can indicate to the follower that no logs can be replicated 
until the follower has caught up. Additionally through heartbeat Append Entries 
request the leader can also indicates the minimum log index to be replicated 
on the follower for it to resume the RAFT replication.

In order to not disrupt the leader, a seperate process called `backlog` can 
run on each of the RAFT server. Its sole purpose is streaming logs back to the 
follower which is lagging behind. This multi process approach offers several 
advantages: 

* The backlog server protocol could be optimize for sending large quantity
  of logs. For instance compression could be used and TCP is likely more 
  suited as well. 

* The backlog process won't affect the raft server (leader); it will use 
  limited memory, and can run on a separate CPU. Furthermore it will only 
  read past `log entry`s from the permanent storage which are immutable. 
  Database like rocksdb allows a separate process to open the DB in a read
  only mode, and our design could leverage fully this feature. 

* The follower which is behind could use the backlog server of another follower,
  in general the leader process is the one which consumes the most resources, 
  so it could be better to use a follower. 

**Solution Details**

* Leader detecs the backlog situation when the follower sends back an 
  Append Entries Response with `prev log index` which is smaller than the 
  earliest `log entry` in the in-memory data structure.

* Append Entries Request needs to be modified to notify the follower that it is
  in a backlog situation.

* Follower state needs to keep track of the backlog requirement (ie the 
  earlies `log entry` to be replicated. 

* `Raft_logic.result` should inform the app of the backlog requirement. This 
  will then leveraged by the application to contact a backlog server for 
  streaming back the missing logs. 
  
* Add `Raft_logic.handle_add_backlog` for the application to fill back the 
  in memory log data structure of the follower.
