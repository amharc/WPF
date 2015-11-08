open List

(* Point is represented as a pair of coordinates. For simplicity, vectors are represented as points *)
type point = float * float

type kartka = point -> int


(* Vector addition, substraction, multiplication by scalar and inner product *)
let ($+) (x, y) (t, u) = (x +. t, y +. u);;
let ($-) (x, y) (t, u) = (x -. t, y -. u);;
let ($*) c      (x, y) = (c *. x, c *. y);;
let ($.) (x, y) (t, u) = x *. t +. y *. u;;
let normsq v = v $. v;;

(* Accepts a boolean and returns 1 if true, 0 if false *)
let iverson = function
  | true -> 1
  | false -> 0
;;

let det (a, b) (c, d) = a *. d -. b *. c;;

let prostokat (a, b) (c, d) (x, y) = iverson (a <= x && x <= c && b <= y && y <= d);;

let kolko c r v = iverson (normsq (v $- c) <= r *. r);;

(* Reflection of q across vector p *)
let reflect p q =
  ((2. *. (p $. q) /. (p $. p)) $* p) $- q
;;

let zloz p1 p2 orig q =
  match det (q $- p1) (p2 $- p1) with
    | 0.            -> orig q
    | d when d > 0. -> 0
    | _             -> 
      let nq = p1 $+ (reflect (p2 $- p1) (q $- p1))
      in
        orig q + orig nq
;;

let skladaj lst orig=
  List.fold_left
    (fun acc (p1, p2) -> zloz p1 p2 acc)
    orig 
    lst
;;
