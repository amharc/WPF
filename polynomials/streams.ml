open Lazy;;

(* Definition of streams *)
type 'a streamCell = Nil | Cons of 'a * ('a stream)
and  'a stream = 'a streamCell Lazy.t;;

(* Exception *)
exception StreamTooShort;;

(* Return an empty stream *)
let empty' = lazy (
  Nil
);;

(* Equivalent of :: operator *)
let put' a st = lazy (
  Cons (a, st)
);;

(* Basic operations -------------------------------------------------------- *)

(* Return a singleton stream *)
let singleton' x = put' x empty';;

(* Return the first element of stream. *)
let head' st = lazy (
  match force st with
  | Nil               -> raise StreamTooShort
  | Cons (head, tail) -> head
);;

(* Return all but the first element. *)
let tail' st = lazy (
  match force st with
  | Nil               -> raise StreamTooShort
  | Cons (head, tail) -> force tail
);;

(* Return the last element. *)
let last' st = 
  let rec aux st =
    match st with
    | Nil               -> raise StreamTooShort
    | Cons (head, tail) ->
        match force tail with
          | Nil -> head
          | x   -> aux x
  in
  lazy(aux (force st))
;;

(* Return all but the last element. *)
let rec init' st = lazy (
  match force st with
  | Nil               -> raise StreamTooShort
  | Cons (head, tail) ->
      match force tail with
        | Nil -> Cons(head, empty')
        | x   -> Cons(head, init' (lazy x))
        );;

let null' st = lazy (
  match force st with
  | Nil -> true
  | _   -> false
);;

(* convert to/from lists *)
let rec stream_of_list = function
  | []    -> lazy Nil
  | x::xs -> lazy (Cons (x, stream_of_list xs))
;;

let rec list_of_stream st =
  match force st with
    | Nil               -> []
    | Cons (head, tail) -> head :: (list_of_stream tail)
;;

(* Helper functions *)
let hd st = force(head' st);;
let nl st = force(null' st);;

(* Generate an infinite stream [a, a, a, ...] *)
let rec repeat' a = lazy(force(put' a (repeat' a)));;

(* Generate an infinite stream [acc, fn acc, fn (fn acc), ...] *)
let rec iterate' fn acc = lazy(force(put' acc (iterate' fn (fn acc))));;

(* Basic operations -------------------------------------------------------- *)

(* Return the first n elements. *)
let rec take' n st = lazy(force(
  if n = 0 then
    empty'
  else
    if nl st then
      raise StreamTooShort
    else
      put' (hd st) (take' (n - 1) (tail' st))
));;

(* Return all but the first n elements. *)
let rec drop' n st = lazy(force(
  if n = 0 then
    st
  else
    if nl st then
      empty'
    else
      drop' (n - 1) (tail' st)
));;

(* Concatenate two streams. *)
let rec concatenate' stA stB = lazy(force(
  if nl stA then
    stB
  else
    put' (hd stA) (concatenate' (tail' stA) stB)
));;

(* Self-evident. *)
let splitAt' n st = (take' n st, drop' n st)

(* Basic higher-level operations ------------------------------------------- *)

(* Convert [a1, a2, ...] to [f a1, f a2, ... ] *)
let rec map' fn st = lazy(force(
  if nl st then
    empty'
  else
    put' (fn (hd st)) (map' fn (tail' st))
));;

(* Select those elements of the stream that satisfy the predicate fn *)
let rec filter' fn st = lazy(force(
  if nl st then
    empty'
  else
    let h = hd st
    and v = filter' fn (tail' st)
    in
      if fn h then
        put' h v
      else
        v
));;

(* Given a stream [a1, a2, ...] compute: (...(fn (fn acc a1) a2) ...) *)
let foldl' fn acc st =
  let rec aux acc st = 
    if nl st then
      acc
    else
      aux (fn acc (hd st)) (tail' st)
  in
    lazy(aux acc st)
;;

(* Given a stream [a1, a2, ...] compute: [acc, fn acc a1, fn (fn acc a1) a2, ...] *)
let rec scanl' fn acc st =
  let snd = lazy(force(
    if nl st then
      empty'
    else
      scanl' fn (fn acc (hd st)) (tail' st)
  ))
  in
    put' acc snd
;;

(* Symmetric variants of foldl and scanl *)
let rec foldr' fn acc st = lazy(
  if nl st then
    acc
  else
    fn (hd st) (force (foldr' fn acc (tail' st)))
);;

let rec scanr' fn acc st = lazy(force(
  if nl st then
    put' acc empty'
  else
    let hh = scanr' fn acc (tail' st)
    in
      put' (fn (hd st) (hd hh)) hh
));;

(* Return the longest prefix consisting of elements satisfying fn *)
let rec takeWhile' fn st = lazy(force(
  if nl st then
    empty'
  else if fn (hd st) then
    put' (hd st) (takeWhile' fn (tail' st))
  else
    empty'
));;

(* Drop the longest prefix consisting of elements satisfying fn *)
let rec dropWhile' fn st = lazy(force(
  if nl st then
    empty'
  else if fn (hd st) then
    dropWhile' fn (tail' st)
  else
    st
));;

(* Self-evident. *)
let reverse' st = lazy (force (force (foldl' (fun x y -> put' y x) empty' st)));;
let length' st = ((foldl' (fun len _ -> len + 1) 0 st) : (int Lazy.t));;

(* Given a stream of streams, concatenate all those inner streams *)
let concat' s = force (foldr' (concatenate') (lazy Nil) s);;

(* Self-evident. *)
let concatMap' fn vals = concat' (map' fn vals);;

(* Given streams [a1, ...] and [b1, ...] compute [f a1 b1, f a2 b2, ...] *)
let rec zipWith' f stA stB = lazy(force(
  if nl stA || nl stB then
    empty'
  else 
    put' (f (hd stA) (hd stB)) (zipWith' f (tail' stA) (tail' stB))
));;


(* Given streams [a1, ...] and [b1, ...] compute [(a1, b1), (a2, b2), ...] *)
let zip' stA stB = zipWith' (fun a b -> (a, b)) stA stB;;


(* utilities --------------------------------------------------------------- *)

(* Check if any of elements of the stream satisfies the predicate *)
let rec any' fn st = lazy(
  if nl st then
    false
  else if fn (hd st) then
    true
  else
    force (any' fn (tail' st))
);;

(* Check if all of elements of the stream satisfy the predicate *)
let rec all' fn st = lazy (
  if nl st then
    true
  else if fn (hd st) then
    force (all' fn (tail' st))
  else
    false
);;

(* Check if x is an element of a given stream *)
let elem' x = any' (fun y -> x = y)

(* Given [a1, ..., an] return [a1, ..., an, a1, ..., an, a1, ... ] *)
let cycle' s = concatMap' (fun _ -> s) (repeat' ());;


(* Generate stream [v, v+1, v+2, ..] *)
let enumFrom' v = iterate' (fun x -> x + 1) v;;

(* Generate stream of primes *)
let primes =
  let rec sieve st = lazy(force(
    if nl st then
      empty'
    else
      let h = hd st
      in
        put' h (sieve (filter' (fun n -> n mod h <> 0) (tail' st)))
  ))
  and
    nums = enumFrom' 2
  in
    sieve nums
;;


(* Generate a stream [a, ..., a] of length n *)
let rec replicate' n a = take' n (repeat' a);; 

(* sorting ----------------------------------------------------------------- *)
(* cmp x y <=> x < y *)

let rec mergeBy' cmp xs ys = lazy(force(
  if nl xs then
    ys
  else if nl ys then
    xs
  else if cmp (force (head' ys)) (force (head' xs)) then
    put' (force (head' ys)) (mergeBy' cmp xs (tail' ys))
  else
    put' (force (head' xs)) (mergeBy' cmp (tail' xs) ys)
));;

let rec sortBy' cmp xs = lazy(force(
  if force(length' xs) < 2 then
    xs
  else
    let (left, right) = force(foldr' (fun elem (a, b) -> (b, put' elem a)) (empty', empty') xs)
    in
      mergeBy' cmp (sortBy' cmp left) (sortBy' cmp right)
));;

let merge' x y = mergeBy' (<) x y;;
let sort' x = sortBy' (<) x;;
