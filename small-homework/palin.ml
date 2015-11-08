let prefikso_palindrom arr =
  let n = Array.length arr in
  if n = 0 then
    0
  else begin
    (* Performs the Manacher algorithm, if odd = 1, searches for
     * odd palindromes, if odd = 0 -- for even ones. *)
    let manacher odd =
      let rad = Array.make (n + 1) 0
      and i, j = ref 1, ref 0 in
      while !i < n do
        (* Extend the current pal!indrome *)
        while !i + rad.(!i) + odd < n && !i - rad.(!i) - 1 >= 0 
          && arr.(!i - rad.(!i) - 1) = arr.(!i + rad.(!i) + odd) do
            rad.(!i) <- rad.(!i) + 1
        done;

        (* Perform updates using relations between palindromes' radii *)
        j := 1;
        while !j <= rad.(!i) && rad.(!i - !j) <> rad.(!i) - !j do
          rad.(!i + !j) <- min rad.(!i - !j) (rad.(!i) - !j);
          incr j
        done;

        if !j <= rad.(!i) then
          rad.(!i + !j) <- min rad.(!i - !j) (rad.(!i) - !j);
        i := !i + !j;
      done;

      (* Get the length of the longest palindrome starting at 0 *)
      let best = ref 0 in
      for i = 0 to n - 1 do
        if rad.(i) = i then
          best := i
      done;
      2 * !best + odd
      in
        max (manacher 0) (manacher 1)
  end;;
