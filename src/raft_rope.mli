(** Rope data structure for int intervals 
   
    The rope data structure stores interval of data where 
    interval boundaries are monotically increasing integer. 

    For instance one could store the soccer world champions
    in such a data structure:

    ]1929; 1933] Uruguay 
    ]1933; 1937] Italy 
    ]1937; 1949] France
    ]1949; 1953] Brazil 

    Internally those intervals are stored in a binary tree fashion. 
    
    Here are some list of characteristics:
    {ul
    {- Inserts are limited to append} 
    {- Removal can be done only in a backward fashion}
    {- Inserts are log(n) with a very small constant and requires
       no re-balancing}
    {- Search is linear and allow searching based on a value inside
       an interval}
    {- Interval values can be replaced but interval boundaries cannot}
    }

  *)

(** {2 Types} *)

type 'a leaf = {
  leaf_prev : int; 
  leaf_last : int;
  leaf_data : 'a;
}

and 'a append = {
  append_height : int;
  append_last : int;
  append_lhs : 'a tree;
  append_rhs : 'a tree;
}

and 'a tree = 
  | Interval    of 'a leaf
  | Append  of 'a append 

and 'a t = 'a tree option 

(** {2 Accessors} *)

val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b 
(** [fold f e rope] iterates over all the data in [rope] in 
    chronological order.
 *)

val find : index:int -> 'a t -> 'a 
(** [find ~index rope] returns the value associated with the 
    interval which contain the [index].
    
    raises [Not_found] if no interval could be found. 
 *) 

val last_entry_index_in_rope : 'a t -> int 
(** [last_entry_index_in_rope rope] reports the upper bound 
    value of the last interval

    If there are no interval then 0 is returned (TODO this 
    needs to change)
 *)

val last_interval : 'a t -> 'a leaf option
(** [last_leaf rope] returns the last interval in [rope]. 
    
    return [None] if no interval is in [rope].
  *)

(** {2 Mutators} *)

val replace : int -> 'a -> 'a t -> 'a t
(** [replace prev_index data rope] replace the data associated with 
    the interval identified by [prev_index] with [data]. 

    raise [Failure] if no interval with [prev_index] could be 
    found. 
  *)

val add : int -> int -> 'a -> 'a t -> 'a t 
(** [add prev last data rope] adds an interval at the end of [rope]. 
    
    TODO verification must be done. 
  *) 

val remove_backward_while : ('a leaf -> bool) -> 'a t -> 'a t 
(** [remove_backward_while f rope] keep on removing the latest interval
    in [rope] as long as [f interval] returns true. 
  *)

(** {2 Format} *)

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit 
(** [pp pp_a] returns the format function for the [rope] *)
