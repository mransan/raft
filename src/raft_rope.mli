(** Rope data structure for int intervals *)

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
  | Leaf    of 'a leaf
  | Append  of 'a append 

and 'a t = 'a tree option 

(** {2 Accessors} *)

val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b 

val find : index:int -> 'a t -> 'a 

val last_entry_index_in_rope : 'a t -> int 

val last_leaf : 'a t -> 'a leaf option

(** {2 Mutators} *)

val replace : int -> 'a -> 'a t -> 'a t

val add : int -> int -> 'a -> 'a t -> 'a t 

val remove_backward_while : ('a leaf -> bool) -> 'a t -> 'a t 

(** {2 Format} *)

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit 
