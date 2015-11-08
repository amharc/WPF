let monotoniczny arr =
  if Array.length arr = 0 then
    0
  else
    let nonincreasing cmp =
      (let mx, _, _ = Array.fold_left
        (fun (mx, len, last) cur ->
          if cmp last cur then
            (max mx (len + 1), len + 1, cur)
          else
            (mx, 1, cur))
        (0, 0, arr.(0))
        arr
      in
        mx)
    in
      max (nonincreasing (<=)) (nonincreasing (>=))
