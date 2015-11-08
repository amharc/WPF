(* Precondition: n >= 1, p - prime *)
(* Postcondition: podzielnosc n p = k <=> p^k | n, but not p^(k + 1) | n *)
let podzielnosc n p =
  (* Precondition: n >= 0, k >= 1 *)
  (* Postcondition: cntDivisible n k = number of such x's (1 <= x <= n) 
   * that are divisible by k *)
  let cntDivisible n k = n / k

  (* Precondition: aux >= 0. aux + (how many times does p divide m!) = 
   * (how many times does p divide n!) *)
  (* Postcondition: iterate m aux = (how many times does p divide n!) *)
  in let rec iterate m aux =
    if m < p then
      (* it is certain, that p does not divide n! *)
      aux
    else
      (* let z = cntDivisible m p, then clearly z! * p^z = p * (2p) * (3p) * ... * (zp)
       * is the product of those numbers from 1 to m which are divisible by p.
       * So (how many times does p divide m!) = (how many times does p divide z! * p^z) = 
       * = z + (how many times does p divide z!), so the precondition of the recursive call
       * is satisfied. We may also see, that m decreases with each call and stays positive
       * so the recursion will be finite. *)
      iterate (cntDivisible m p) (aux + cntDivisible m p)
  in
    iterate n 0;;
