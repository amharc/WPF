module Streams = struct
  exception StreamTooShort;;
  type 'a stream = Nil | Cons of ('a * (unit -> 'a stream));;

  let force f = f ()

  let map' f = function
    | Nil -> Nil
    | Cons (head, tail) -> Cons (f head, fun _ -> map' f (force tail))
  ;;

  let filter' f = function
    | Nil -> Nil
    | Cons (head, tail) when f head -> Cons (head, fun _ -> filter' f (force tail))
    | Cons (_,    tail)             -> filter' f (force tail)
  ;;

  let foldl' f acc = function
    | Nil -> acc
    | Cons (head, tail) -> foldl' f (f acc head) (force tail)
  ;;

  let take' n st = 
    if n = 0 then
      Nil
    else
      match st with
        | Nil -> raise StreamTooShort
        | Cons (head, tail) -> Cons (head, fun _ -> take' (n - 1) (force tail))
  ;;

  let drop' n st =
    if n = 0 then
      st
    else
      match st with ->
        | Nil -> raise StreamTooShort
        | Cons (_, tail) -> drop' (n - 1) (force tail)
  ;;

  let takeWhile' f = function
    | Cons (head, tail) when f head -> Cons (head, fun _ -> takeWhile' f (force tail))
    | _ -> Nil
  ;;

  let dropWhile' f = function
    | Cons (head, tail) when f head -> dropWhile' f (force tail)
    | s -> s
  ;;

  let zip' = function
    | Nil -> Nil
    | Cons (head1, tail1) -> function
      | Nil -> Nil
      | Cons (head2, tail2) -> Cons ((head1, head2), 

