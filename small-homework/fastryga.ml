(* type tree = Node of int * tree * tree * tree ref | Null;; *)

let fastryguj tr =
  let max_on_first (a, b) (c, d) =
    if a < c then
      (c, d)
    else
      (a, b) in
  (* return value: pair (Some largest value, node with such value) or (None, Null) *)
  (* Trick: None < Some x *)
  let rec trav tr = match tr with
    | Null -> (None, Null)
    | Node (v, l, r, rf) ->
        let (maxval, maxnode) = max_on_first (Some v, tr)
          (max_on_first (trav l) (trav r))
        in
          rf := maxnode;
          (maxval, maxnode)
  in
    ignore (trav tr)
