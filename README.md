# Raft Protocol

This repo implements the [Raft consensus protocol](https://raft.github.io/) in a functional way using the 
OCaml language. 

#### Project structure

**Types** 

All the types involved in the RAFT protocoal are defined in the 
[protobuf](https://developers.google.com/protocol-buffers/) format (see [raft.proto](src/raft.proto)). 
[ocaml-protoc](https://github.com/mransan/ocaml-protoc/) compiler is then used to 
generate [the equivalent types](src/raft_pb.mli) in OCaml as well as serialization functions.


**Helper Functions** 

The module [Raft_helper](src/raft_helper.mli) provides convenience routines to manipulate the 
generated [types](src/raft_pb.mli). 


**Protocol Logic** 

The protocol logic is implemented in the [Raft_logic](src/raft_logic.mli) module. The protcol implementation
is divided into 2 subsections for each of the 2 request/response of the protocol:
* Request Vote : For leader election 
* Append log entry: For state replication 

Each subsection implements the following logic:

*Make Request*

This function creates a request value given the current state of the server and optionally which
server the request is sent to. 

*Handle Request*
This function computes the effect of the received request to the server state. The implementaiton being 
functional, a new state is returned. 

*Handle Response*
This function computes the effect of a received response to the server. Along with a new state, a 
**Follow Up Action** is returned to let the application perform the expected actions. 

