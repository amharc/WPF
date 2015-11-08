(* Time complexity: O(n)
 * Space complexity: O(sqrt(n)) *)
let segmenty arr = 
  let n = Array.length arr in
  let s = int_of_float (sqrt (float_of_int n)) in
  (* Case 0: result is <= sqrt(n) *)
  let small =
    (* brr.(i) -- number of segments of length i containing only true *)
    let brr = Array.make (s + 1) 0 in
    ignore (Array.fold_left
      (fun cnt vl ->
        if vl then begin
          brr.(min s (cnt + 1)) <- brr.(min s (cnt + 1)) + 1;
          cnt + 1
        end else
          0)
      0
      arr);

    (* Now brr only knows about segments that are maximally extended to the left so we have to
     * compute suffix sums *)
    for i = s downto 1 do
      brr.(i - 1) <- brr.(i - 1) + brr.(i)
    done;

    (* And now the final computation *)
    let res = ref 0 in
    for i = 1 to s do
      if brr.(i) >= i then
        res := i
    done;

    !res

  (* Case 1: result is > sqrt(n) *)
  and big =
    (* Step 1.0: Compute a sorted list of maximal segments of length > s
     * Length of this list is O(s), so we may sort it *)
    let flush (lst, cnt) =
      if cnt > s then
        (cnt :: lst, 0)
      else
        (lst, 0) in

    let lengths = ref (List.sort compare (fst (flush (Array.fold_left
      (fun (lst, cnt) vl ->
        if not vl then
          flush (lst, cnt)
        else
          (lst, cnt + 1))
      ([], 0)
      arr)))) in
    let cnt = ref (List.length !lengths)
    and sum = ref (List.fold_left (+) 0 !lengths)
    and res = ref 0 in

    (* Step 1.1: Calculation: a maximal segment of length k > s
     * contains exactly k - l + 1 segments of length l *)

    for length = s to n do
      (* Delete segments too short -- the following loop reduces the length of
       * list lengths, so it will always terminate. Moreover, total number of
       * iterations will not exceed O(s) *)
      while !lengths <> [] && List.hd !lengths < length do
        sum := !sum - List.hd !lengths;
        decr cnt;
        lengths := List.tl !lengths;
      done;

      (* Sum of k - (l - 1) over all remaining sements *)
      let vl = !sum - (length - 1) * !cnt in
      if vl >= length then
        res := length
    done;

    !res in
  max small big

