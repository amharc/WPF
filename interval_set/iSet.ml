(* Krzysztof Pszeniczny *)
open List;;

type t =
  |  Empty
  | Node of node
and node = { left : t
           ; right : t
           ; segment : int * int
           ; cover : int
           ; height : int }

exception EmptyTree

let empty = Empty

let getHeight = function
  | Empty -> 0
  | Node n -> n.height

let getCover = function
  | Empty -> 0
  | Node n -> n.cover

let (@@) f g = f g

let mkNode l (a, b) r = 
  Node
    { left = l
    ; right = r
    ; segment = (a, b)
    ; cover = getCover l + (b - a + 1) + getCover r
    ; height = 1 + max (getHeight l) (getHeight r)
    }

let bal l k r = 
  if getHeight l > getHeight r + 2 then
    match l with
      | Node n ->
          if getHeight n.left >= getHeight n.right then
            mkNode n.left n.segment (mkNode n.right k r)
          else
            (match n.right with
              | Node m ->
                  mkNode
                    (mkNode n.left n.segment m.left)
                    m.segment
                    (mkNode m.right k r)
              | Empty  -> assert false)
      | Empty  -> assert false
    else if getHeight r > getHeight l + 2 then
      match r with
        | Node n ->
          if getHeight n.right >= getHeight n.left then
            mkNode (mkNode l k n.left) n.segment n.right
          else
            (match n.left with
              | Node m ->
                  mkNode
                    (mkNode l k m.left)
                    m.segment
                    (mkNode m.right n.segment n.right)
              | Empty  -> assert false)
        | Empty  -> assert false
    else
      mkNode l k r

let is_empty n = n = Empty

let rec min_seg = function
  | Node n when is_empty n.left -> n.segment
  | Node n -> min_seg n.left
  | Empty -> raise EmptyTree

let rec max_seg = function
  | Node n when is_empty n.right -> n.segment
  | Node n -> max_seg n.right
  | Empty -> raise EmptyTree

let rec remove_min_seg = function
  | Node n when is_empty n.left -> n.right
  | Node n -> bal (remove_min_seg n.left) n.segment n.right
  | Empty -> raise EmptyTree

let merge t1 t2 =
  match t1, t2 with
  | Empty, _ -> t2
  | _, Empty -> t1
  | _ ->
      let k = min_seg t2 in
      bal t1 k (remove_min_seg t2)

(* Adds a segment to the tree. This segment MUST be disjoint from others. *)
let rec addSegment x = function
  | Node n ->
      let c = compare (fst x) (fst n.segment) in
      if c = 0 then
        mkNode (n.left) x (n.right)
      else if c < 0 then
        let nl = addSegment x n.left in
        bal nl (n.segment) (n.right)
      else
        let nr = addSegment x n.right in
        bal (n.left) (n.segment) nr
  | Empty -> mkNode Empty x Empty

let rec join l v r =
  match (l, r) with
    | Empty, _ -> addSegment v r
    | _, Empty -> addSegment v l 
    | Node n, Node m ->
        if n.height > 2 + m.height then
          bal n.left n.segment (join n.right v r)
      else if m.height > 2 + n.height then
          bal (join l v m.left) m.segment m.right
        else
          mkNode l v r

(* f x should return -1 if x should be in the left part, 1 if in the right part, 0 if pivot *)
let splitBy f set =
  let rec loop = function
    | Empty -> (Empty, None, Empty)
    | Node n ->
        let c = f n.segment in
        if c = 0 then
          (n.left, Some n.segment, n.right)
        else if c > 0 then
          let (l', v', r') = loop n.left
          in
            (l', v', join r' n.segment n.right)
        else
          let (l', v', r') = loop n.right
          in
            (join n.left n.segment l', v', r')
  in
    loop set

let dec x =
  if x = min_int then
    min_int
  else
    x - 1

let inc x =
  if x = max_int then
    max_int
  else
    x + 1

let cmpSeg x (a, b) = 
  if b < x then
    -1
  else if a <= x then
    0
  else
    1

let extend (a, b) = function
  | None -> (a, b)
  | Some (c, d) -> (min a c, max b d)

let add (a, b) set =
  let (l, v, r) = splitBy (cmpSeg @@ dec a) set
  in let (_, u, r') = splitBy (cmpSeg @@ inc b) r
  in
    let ns = merge l r'
    and nv = extend (extend (a, b) v) u
    in
      addSegment nv ns

let split x set =
  match splitBy (cmpSeg x) set with
    | l, Some (a, b), r ->
        ((if a < x then
          addSegment (a, dec x) l
        else
          l), 
        true,
        (if x < b then
          addSegment (inc x, b) r
        else
          r))
    | l, None, r ->
        (l, false, r)

let remove (a, b) set =
  let (l, _, r) = split a set
  in let (_, _, r) = split b r
  in
    merge l r

let mem x set =
  match split x set with
    | _, r, _ -> r

let rec iter f = function
  | Empty -> ()
  | Node n ->
      iter f n.left;
      f n.segment;
      iter f n.right

let rec fold f set acc =
  match set with
    | Empty -> acc
    | Node n ->
        let    acc = fold f n.left acc
        in let acc = f n.segment acc
        in           fold f n.right acc

let elements set =
  rev (fold (fun seg acc -> seg :: acc) set [])

(* We hold the cover as an int. The maximal cover is max_int - min_int + 1 = 2^63. 
 * This unfortunately overflows int, but in a very limited way: if the held cover is negative,
 * this is obviously because of an overflow, so we may return max_int.
 * Conversely, if the answer is positive, it means that the correct one is of the form k + 2^63 l, where
 * k is positive and l is nonnegative. But comparing this to the maximal cover of 2^63 we see that it will be
 * always the correct answer held.
 *
 * The only problematic case is 0. It may mean either 0 or 2^63, so we have to check it. *)

let below n set = 
  let (l, v, _) = split n set
  in
    let c = getCover l + (if v then 1 else 0)
    in
      if c > 0 then c
      else if c < 0 then max_int
      else (* c = 0 *)
        if l = empty then
          0
        else
          max_int

let string_of_segment (a, b) = "(" ^ string_of_int a ^ ", " ^ string_of_int b ^ ")"
let rec printSet = function
  | Empty -> print_string "empty"
  | Node n -> print_string "mkNode ("; printSet n.left; print_string (") " ^ string_of_segment n.segment ^ " ("); printSet n.right; print_string ")"
