let silnia n =
  let rec aux n acc =
    match n with
    | 0 -> acc
    | _ -> aux (n - 1) (acc * n)
  in
    aux n 1
