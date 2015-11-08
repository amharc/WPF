open List;;

let split l =
  (* Precondition: acc is a nonempty list of lists, representing the split into minimal number of increasing sequences of some sequence X *)
  (* Postcondition: (aux elem acc) -- nonempty list of lists, representing the split into minimal number of increasing sequences of sequence (elem :: X) *)
  let aux elem acc =
    match hd acc with
      | (x::_) as l when elem < x -> (elem :: l) :: (tl acc)  (* extend the first list *)
      | _                         -> [elem] :: acc            (* create new list *)
  in
    filter (fun l -> l <> []) (* remove sentinel empty list *)
      (fold_right aux l [[]])
;;

