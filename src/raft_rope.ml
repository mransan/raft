type 'a leaf = {
  leaf_prev : int; 
  leaf_last : int;
  leaf_data : 'a;
}

and 'a append = {
  append_height : int; (* duplicated data for speed *)
  append_last : int;   (* duplicated data for speed *)
  append_lhs : 'a tree;
  append_rhs : 'a tree;
}

and 'a tree = 
  | Interval of 'a leaf
  | Append of 'a append 

and 'a t = 'a tree option 

let empty  = None 
  
let fold f e0 = function 
  | None -> e0
  | Some tree ->
    let rec aux acc = function
      | Interval {leaf_data; _ } -> f acc leaf_data
      | Append {append_lhs;append_rhs; _} -> aux (aux acc append_lhs) append_rhs
    in
    (aux e0 tree) 

let last_entry_index_in_rope = function 
  | None -> None 
  | Some Interval {leaf_last; _} -> Some leaf_last
  | Some Append {append_last; _} -> Some append_last

let last_entry_index_in_tree = function 
  | Interval {leaf_last; _} -> leaf_last
  | Append {append_last; _} -> append_last

let replace ~prev ~data = function 
  | None -> assert(false)
  | Some tree ->  
    let rec aux = function
      | Interval leaf -> 
        assert(leaf.leaf_prev = prev );  
        Interval {leaf with leaf_data = data}

      | Append ({append_rhs; append_lhs; _} as append)  -> 
        let lhs_last = last_entry_index_in_tree append_lhs in 
        if prev >= lhs_last 
        then Append { append with append_rhs = aux append_rhs }
        else Append { append with append_lhs = aux append_lhs }
    in
    Some (aux tree)

let rope_height = function
  | Interval _ -> 0 
  | Append  {append_height; _} -> append_height 
  
(*
 * The rope is an append-only data structure and therefore we don't need
 * to keep the tree data structure perfectly balanced. 
 *
 * The diagaram below show the [add] operation to the right side of the 
 * tree.
 *
 * |----------------------|
 * | I0              (0)  | 
 * |----------------------|
 * |     A                |
 * |    / \          (1)  |
 * | I0    I1             |
 * |----------------------|
 * |        A             | 
 * |      /  \            |
 * |     A    I2     (2)  |
 * |    / \               | 
 * | I0    I1             |
 * |----------------------|
 * |         A            |
 * |       /   \          |
 * |     A      A    (3)  |
 * |    / \    /  \       |
 * | I0    I1 I2   I3     |
 * |----------------------|
 *)
let add ~prev ~last ~data t = 

  let new_leaf = Interval {leaf_data=data;leaf_prev=prev; leaf_last=last}  in 

  let rec aux = function 
    | Interval x -> 
      (* case 1 & 3 *)
      Append {
        append_height = 1; 
        append_lhs = Interval x; 
        append_rhs = new_leaf; 
        append_last = last;
      }

    | (Append {append_height; append_rhs; append_lhs; _ }  as append)-> 
      let lhs_height = rope_height append_lhs in 
      let rhs_height = rope_height append_rhs in 
      if lhs_height = rhs_height
      then 
        (* balanced - case 2 *)
        Append {
          append_height = append_height + 1; 
          append_lhs = append; 
          append_rhs = new_leaf;
          append_last = last}
      else begin 
        assert(lhs_height > rhs_height); 
        Append {append_height; append_lhs ; append_rhs = aux append_rhs ; append_last= last} 
      end  
  in
  match t with
  | None -> 
    (* case 0 *)
    Some new_leaf 

  | Some tree -> 
    let current_last = last_entry_index_in_tree tree in 
    assert(last > current_last); 
    assert(prev >= current_last); 
      (* Here we don't enfore that the intervals are continuous but only
       * monotically increasing
       *)
    Some (aux tree) 
  
let find ~index t =  

  match t with 
  | None -> raise Not_found
  | Some tree ->
      
    let rec aux = function
      | Interval leaf-> 
        if index <= leaf.leaf_prev || 
           index > leaf.leaf_last
        then raise Not_found
        else leaf.leaf_data 

      | Append {append_rhs; append_lhs; _} -> 
        let lhs_last = last_entry_index_in_tree append_lhs in 
        if index > lhs_last 
        then aux append_rhs 
        else aux append_lhs  
    in
    aux tree

let remove_backward_while f = function
  | None -> None 
  | Some tree -> 
    let rec aux = function
      | Interval leaf -> 
        if f leaf
        then None
        else Some (Interval leaf)

      | Append ({append_lhs; append_rhs; _ } as append) ->
        match aux append_rhs with
        | None -> aux append_lhs 
        | Some tree -> Some (Append {append with append_rhs = tree; }) 
    in 
    aux tree

let pp f fmt = function
  | None -> Format.fprintf fmt "Empty"
  | Some tree ->  
    let rec pp_sub fmt = function
      | Interval {leaf_prev;leaf_last; leaf_data} -> 
        Format.fprintf fmt "]prev:%i; last:%i] = data:%a," leaf_prev leaf_last f leaf_data
      | Append {append_rhs; append_lhs; _ } ->
        Format.fprintf fmt "%a %a" pp_sub append_lhs pp_sub append_rhs 
    in
    pp_sub fmt tree
