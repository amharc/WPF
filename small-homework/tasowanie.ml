(* Postcondition: shuffle a b =
  * if one of the lists is empty, the result is the other one
  * if both are non-empty, the result is a list satisfying task requirements, but beginning with head of the *second* list *)
let rec shuffle a = function
  | []     -> a
  | h :: t -> h :: (shuffle t a)
;;
