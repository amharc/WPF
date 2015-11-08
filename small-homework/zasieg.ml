let zasieg arr y =
  (* total count of elements equal to y *)
  let total_y = Array.fold_left (fun acc x -> if x = y then acc + 1 else acc) 0 arr
  and total   = Array.length arr in
  let (res, _, _ ) = Array.fold_left
    (* res is the result, or -1 if yet to be determined
     * cnt_y is the number of so far processed elements equal to cnt_y
     * cnt is the total number of processed elements *)
    (fun (res, cnt_y, cnt) elem ->
      (* after_ny is the number of elements in the array:
        * 1) appearing after the current position and
        * 2) not equal to y *)
      let cnt_y = cnt_y + (if elem = y then 1 else 0)
      and cnt = cnt + 1 in
      let after_ny = (total - cnt) - (total_y - cnt_y) in
      if (after_ny = cnt_y) && (cnt <> total) then
        (cnt, cnt_y, cnt) (* current position is the result *)
      else
        (res, cnt_y, cnt))
    (-1, 0, 0)
    arr
  in
    res

