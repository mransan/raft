# A Functional Implementation of the RAFT protocol

## Introduction

> In this serie of blog posts we will implement the consensus protocol RAFT in a purely functional style. 

* Consensus algorithms are interesting and recent protocols which are particularly relevant with modern distributed architetures. The RAFT protocol is relatively simple to understand and offers a great introduction to the consensus problem. 

* Functional Programming is great and this implementation documents a concrete use case for a protocol implementation

* OCaml language is elegant, well proven and really fast. No OCaml knowledge is required for this blog posts and if you are interesting in learning this language with a concrete use case rather than iterating through the Language features then stay tuned. We'll only cover a very small fraction of the language and its ecosystem.

Here are the main technology we'll be using:

* Protobuf for the protocol messages and state 
* `ocaml-protoc` to compile Protobuf message files to OCaml 
* OCaml core language
* Lwt OCaml library for concurrent programming (with futures and promises)
* Unix UDP for the transport protocol

## Consensus Protocols


