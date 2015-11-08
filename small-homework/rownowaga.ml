let (@@) f g = f g

let rownowaga t = 
  fst @@ fold_tree
    (* Accumulated value: (number of balanced vertices in the subtree, total number of vertices in the subtree *)
    (fun _ (lbal, lcnt) (rbal, rcnt) ->
      (lbal + rbal + 
        (if lcnt = rcnt then 1 else 0),  (* Check if current vertex is balanced *)
       lcnt + rcnt + 1))
    (0, 0)
    t
