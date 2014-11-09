type 'a queue = Nil | (* empty queue *)
                Node of 'a node_desc
(* auxiliary code *)
and 'a node_desc = { key : 'a; (* value in node *)
               height : int;   (* length of right path from node to leaf *)
               left : 'a queue;
               right : 'a queue; }

exception Empty

(* Getters *)
let getKey = function
  | Nil -> failwith "Nil node"
  | Node n -> n.key;;

let getLeft = function
  | Nil -> failwith "Nil node"
  | Node n -> n.left;;

let getRight = function
  | Nil -> failwith "Nil node"
  | Node n -> n.right;;

let is_empty = function
  | Nil -> true
  | _   -> false
;;

(* Returns the right height of given node, return 0 if node is nonexistent *)
let getHeight = function
  | Nil    -> 0
  | Node n -> n.height
;;

(* Constructors *)
let mkNode key left right = Node { key = key; height = 1 + (getHeight right); left = left; right = right; };;
let empty = Nil;;

let mkSingleton key = mkNode key empty empty;;

let rec join a b =
  (* Self-explanatory *)
  let order a b =
    if getHeight a < getHeight b then
      (b, a)
    else
      (a, b)
  in
    if is_empty a then
      b (* Trivial case *)
    else
      if is_empty b then
        a (* Another trivial case *)
      else if getKey a <= getKey b then
        let (ls, rs) = order                    (* Find the proper ordering of children *)
                        (join b (getRight a))   (* Recursive join *)
                        (getLeft a)
        in
          mkNode (getKey a) ls rs
      else
        join b a  (* symmetric case *)
;;


let add x = join (mkSingleton x);;

let delete_min q =
  if is_empty q then
    raise Empty
  else
    (getKey q, join (getLeft q) (getRight q))
;;

