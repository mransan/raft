### Functional log replication in RAFT

The RAFT protocol is a consensus protocol for a log data structure. Each server 
in a RAFT cluster will eventually have the same log data structure. 

The RAFT protocol is based on the leader election mechanism; the initial phase of the protocol consists 
in deciding which server will be the leader. From then on, the leader server is responsible to append 
new log entries to its internal log as well as replicating the new entries in all the follower servers. 

In this post we will look at a practical performance problem related to implementing log replication 
both efficiently using solely functional data structures.

**Log Entry data structure**

```OCaml 
type log_entry = {
  index : int;
  term : int;
  data : bytes;
}
```

The `index` is monotically increasing (by 1) and starting at 1. 
The `term` corresponds to the election term that this `log entry` was created at. 

It's important to notice that both index and term are needed to uniquely identify a log 
entry. In fact, in certain circumstances where a previous leader crashed without fully 
replicating a log entry, it is possible that 2 servers will have log entries with 
the same index but different terms. (RAFT protocol ensure that only one is eventually persisted). 

**Log data structure**

The log is the collection of `log_entry`s:

```OCaml
{index = 10;term = 2}::{index = 9; term=1}:: .... ::{index = 1; term=1}::[]
```

The log ordering is from lastest to earliest log.  

**Replication protocol**

During log replication the leader must replicate its `log` to all the followers and the RAFT protocol 
defines the following (simplified) request to be send from the leader to each followers.

```OCaml
type request = {
  leader_id       : int;
  prev_log_index  : int;
  prev_log_term   : int;
  rev_log_entries : log_entry list;
}
```

Besides its `id` and the `log_entry`s to be replicated, the leader is also sending 
the `index` and `term` of the last log it believed was replicated on the follower. This extra
information helps both parties to synchronize. 

The (simplified) response replied by the follower to its leader is:

```OCaml
type response =
  | Success of {last_log_index : int} 
  | Failure of {last_log_index : int; last_log_term : int} 
```

In case of succesfful replication the follower server sends the index of the log it replicated (In general this would 
be the latest log sent in the query).  In case of failure the follower additionally sends the corresponding term. 

A failure happens in the follower when the `index`/`term` information sent by the leader is not matching 
the follower state. 2 main reaons:
a) The entry is simply not in the follower log. The follower is lagging behind and 
   needs earlier log entries than the ones sent in that request. 
b) The entry has different term. This could happen during a leader crash. That log index was 
   replicated on the follower, however the index was never commited.

**Normal operation**

Let's look at what the server is doing when computing the replication request to be 
sent to a follower. We assume here that the leader is keeping track of the `prev_log_index`
for each of the followers.

```OCaml
let collect (prev_log_index:int) (leader_id:int) (leader_log:log_entry list) = 

  let rec aux rev_log_entries = function
    | [] -> (
      (* This can happen when no logs hae previously been 
       * replicated on the follower. 
       * By convention the first log index is 1 and therefore
       * the expected `prev_log_index` should be 0. 
       *)
      assert(prev_log_index = 0); 
      {leader_id; prev_log_index = 0; prev_log_term = 0; rev_log_entries} 
    )

    | {index; term; _}::tl when index = prev_log_index -> 
      (* The prev_log_index is successfully found 
       * in the leader log. 
       *)
      {leader_id; prev_log_index; prev_log_term = term; rev_log_entries} 
    
    | log_entry::tl -> 
      (* More logs need to be accumulated. 
       *)
      aux (log_entry::rev_log_entries) tl 
  in
  aux [] log_entry
```

The code above iterates from latest to earlier `log entry`s until the `prev_log_index` is found. Log entries
are accumulated in a list in reverse order (hence the name `rev_log_entries`). 

In normal mode of operation, the follower is not lagging too much behind the leader; concequently only 
small number of iteration should be required to compute the requests. 
The `list` data structure used to store the logs is particularly efficient: adding new log at the head of the 
`list` is constant time and computing the replication request is linear in terms of logs to replicate. 

**Corner cases (1)**

From time to time servers will go offline. This can happen in case of a machine failure or if a machine needs to 
go through maintenance. No matter what the reason, when the follower will join the replication process it will be lagging
behind massively. 

The first problem is that in reality replication requests have a `max` number of log entries. It's impractical to send 
gigantic messages and the RAFT protocol accomodates nicely for the leader to send a limited number of log entries.

Here is the code to get the subset of log for the replication request 

```OCaml
let rec keep_first_n l = function
  | 0 -> []
  | n ->
    begin match l with
    | hd::tl -> hd::(keep_first_n tl (n - 1))
    | []     -> []
    end

let collect_with_max max prev_log_index leader_id leader_log = 
  let request = collect prev_log_index leader_id leader_log in 
  {request with 
   rev_log_entries = keep_first_n request.rev_log_entries max}
```

Let's define :
* `n` the number of log entries to be replicated 
* `m` the max number of log entries that can be sent in a single replication request

The replication complexity will therefore be 
> 0(n) = n + (n - m) + (n - 2\*m) + ... (n - (n * m)/m)
> = n^2

Note that during normal operation mode the `m` does not play a role since it's likely `>n` 
which makes the complexity `O(n)` as previously mentioned.

The impact of such slow performance is quite large. Not only the replication for one of the 
follower is slow, but also the entire leader is getting slower. 

The RAFT protocol relies on heartbeat messages from the leader to 
the followers to notify that it is still active; if the replication request 
takes too long then followers will not receive a heartbeat and they 
will start a new leader election.  
