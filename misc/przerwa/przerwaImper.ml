let przerwa lst =
  (* We may see that the optimal y will always be the average of two consecutive
   * elements of the list sorted in ascending order. To achieve O(n) time complexity
   * we divide the given numbers into n buckets, each holding numbers from some (real)
   * interval. We make sure that the intervals are of the same length. This means
   * that either:
     * 1) for each interval, there is exactly one number from the list in it,
     *    so we have sorted the list
     * 2) some interval is empty, so the maximal difference between consecutive elements
     *    of the sorted list is not less than the length of the interval
   * In both cases, it is sufficient to check the smallest and largest element in each
   * bucket, and handle only that pairs of elements. *)
  let len = List.length lst
  in let buckets = Array.make len []
  and mn = List.fold_left min (List.hd lst) lst
  and mx = List.fold_left max (List.hd lst) lst
  in let scale x = (x - mn) * (len - 1) / (mx - mn) (* Number of the bucket, to which x belongs *)
  in
    begin
      List.iter (* Fill the bucket *)
        (fun vl ->
          buckets.(scale vl) <- vl :: buckets.(scale vl))
        lst;
      let res, _, _ = Array.fold_left
        (* Accumulated value: best y, min |x_i - y| for that y and the smaller element to consider,
         * so that (prev, x) is a pair of consecutive elements of the sorted list *)
        (fun (bestval, bestdist, prev) x ->
          if x = [] then (* empty bucket, just loop through *)
            (bestval, bestdist, prev) 
          else 
            let bmm = List.fold_left min (List.hd x) x (* Minimum and maximum in the bucket *)
            and bmx = List.fold_left max (List.hd x) x
            in
              let cand = (prev + bmm) / 2
              in let canddist = min (cand - prev) (bmm - cand)
              in
                if canddist > bestdist then
                  (cand, canddist, bmx)
                else
                  (bestval, bestdist, bmx)
        )
        (-1, -1, mn) (* Sentinel values *)
        buckets
    in
      res
  end

