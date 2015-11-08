let rec zapisz = function
  | 0                  -> []
  | n when n mod 2 = 0 -> 0 :: zapisz (n/(-2))
  | n                  -> 1 :: zapisz ((n-1)/(-2))

let prepend_zero = function
  | [] -> []
  | x -> 0::x

let rec inc = function
  | []   -> [1]
  | 0::t -> 1::t
  | _::t -> prepend_zero (dec t)

and     dec = function
  | []   -> [1; 1]
  | 1::t -> prepend_zero t
  | _::t -> 1::(inc t)
;;

let plus a b = 
  let rec apply t = function
    | 0 -> t
    | 1 -> inc t
    | _ -> dec t

  and aux a b c =
    match (a, b) with
    | ([], t) -> apply t c
    | (t, []) -> apply t c
    | (x::xs, y::ys) ->
        match x + y + c with
          | nd when nd mod 2 = 0 -> prepend_zero (aux xs ys (nd / (-2)))
          | nd                   -> 1 :: aux xs ys ((nd - 1) / (-2))

  in
    aux a b 0;;
