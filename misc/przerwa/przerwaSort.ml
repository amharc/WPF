open List

let przerwa lst = 
  let lst = sort Pervasives.compare lst in
  let res, _, _ = fold_left
    (fun (bestval, bestdist, prev) x -> (* prev and x are two consecutive elements of the list *)
      let cand = (x + prev) / 2 in
        if cand - prev > bestdist then
          (cand, cand - prev, x)
        else
          (bestval, bestdist, x)
    )
    (-1, -1, (hd lst))
    (tl lst)
  in
    res

