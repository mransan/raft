# A Functional Implementation of the RAFT protocol

## Introduction

> In this serie of blog posts we will implement the consensus protocol RAFT in 
> a purely functional style. 

**Consensus algorithms** are interesting protocols which are particularly relevant with modern distributed architetures. The RAFT protocol proposes a relatively simple to understand approach and offers a great introduction to the consensus problem. 

The benefit of **Functional Programming** have been discussed (and argued over) numerous time, the focus here is rather to provide an implementation of a protocol with a functional approach.

**OCaml** language is elegant, well proven and really fast. No OCaml knowledge is required for these blog posts and if you are interested in learning this language with a concrete use case rather than iterating through the language features then stay tuned. We'll only cover a very small fraction of the language and its ecosystem but hopefuly it will make you want to lean more.

Here are the main technology we'll be using:

* Protobuf for the protocol messages and state 
* `ocaml-protoc` to compile Protobuf message files to OCaml 
* OCaml core language
* Lwt OCaml library for concurrent programming (with futures and promises)
* Unix UDP for the transport protocol

## Consensus Protocols

Consensus protocols ensure that participating servers will eventually be consistent even if certain failure happened. Such protocol can differ with regards to the state they manage as well as the type of failure they are resilient to. 

**RAFT** protocol ensures the consistent execution of a `state machine` and is resilient to `fail-stop` failures. `fail-stop` failures are essentially server crashes or a server not receiving messages. **RAFT** protocol does not support Byzantine failures which is when a server is acting maliciously.

In the next section we will look into details about what is a state machine with a concrete and simple example implemented in OCaml.

#### State Machine 

A `state machine` is composed of a state and a series of command to be executed on the state. Each command has a payload and executing the command will modify the state. 

For instance if we want to represent a (limited) algebra state machine we could have the following:
```OCaml
(** Named variable  *)
type var = {
  name : string; 
  value : float;
} 

(** State of the state machine *)
type state = var list 

(** Command of the state machine  *)
type cmd = 
  | Store          of var 
  | Add_and_assign of string * string * string 
                  (* (lhs , rhs , new_variable_name) *)
```

Let's stop here for a second and look at our first OCaml code. The code above demonstrates the use of the 3 most widely used OCaml types

* **Records** (`type var = {...}`): Similar to a C `struct` 
* **List** (`type state = var list`): List is a builtin type in OCaml and represents a singly linked list. It's an immutable data structure.
* **Variant** (`type cmd = |... |... `): Type to represent a choice. `cmd` can either be `Store` or `Add_and_assign`. Each choice is called a constructor. A constructor can have zero or many arguments. 

`Store x` adds the given variable `x` to the state.

`Add_and_assign ("x", "y", "z")` sums the values associated with `"x"` and `"y"` and stores the result in a variable with name `"z"`. 

Here is a concrete example of a sequence of commands and the expected state:

```OCaml
let cmds = [
  Store {name = "x"; value = 1.}; 
  Store {name = "y"; value = 4.}; 
  Add_and_assign ("x", "y", "z"); 
]
```
After exectution of the above commands on an initial empty list state, we would then expect the resulting state:
```OCaml
let expected_state = [
  {name = "z"; value = 5.}; 
  {name = "y"; value = 4.}; 
  {name = "x"; value = 1.}; 
]
```
As far as OCaml is concerned we've just learned how to create values of the types we previously defined. 

Let's now write our first function in OCaml which will perform the execution of the state machine command:

```OCaml
let execute cmd state = 
  match cmd with
  | Store v -> v::state
  | Add_and_assign (xname, yname, zname) -> 
    let xvar = List.find (fun v -> v.name = xname) state in 
    let yvar = List.find (fun v -> v.name = yname) state in 
    {name = zname; value = xvar.value +. yvar.value} :: state
```

Let's look into more details at a few constructs:

**`match cmd with | Store ..-> ... | ...`**

This `match with` expression performs a proof by case logic. The OCaml compiler has special support to detect missing cases which helps a lot for finding bugs early. This construct is called **pattern matching** and is heavily used in OCaml. 

**`v::state`**

The expression `v::state` is simply the builtin syntax for to append a value at the head of the list.

**`fun x -> ...`**

OCaml is a functional language; you can create anonymous function using `(fun x -> ...)` expression. 

**`yvar.value`**

Finally we see that record fields access is using the classic `.` (dot) syntax. (`yvar.value`). 



> Notice the lack of type annotation in the above code! In fact the OCaml compiler infer all the types and guarantees type safety. The syntax is minimal without sacrifying program correctness.




