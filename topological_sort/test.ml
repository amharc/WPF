open Topol;;

let rec gen = function
  | 0 -> []
  | n -> (n, [n / 2; n / 3; n / 5]) :: gen (n - 1)

(*let main () = List.iter (fun x -> print_endline (string_of_int x))
  (topol [(1, [2; 3]); (3, [4; 2]); (5, [4; 6]); (6, [3; 10])]);; *)
let main () = topol (gen 100000);;

main ();;
