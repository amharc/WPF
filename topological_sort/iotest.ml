open Topol;;

let t, n, p = Scanf.scanf "%d %d %f" (fun t n p -> (t, n, p));;
let rec _gen acc = function
  | i when i <= 0 -> acc
  | i             -> _gen (i :: acc) (i - 1)
;;
let gen = _gen [];;

let test testno = 
  begin
    Printf.eprintf "(\027[34m*\027[0m) \027[34m Test %05d\027[0m... " testno;

    let edgecnt = ref 0 in
    let data = (List.fold_left
      (fun acc i ->
        (i, List.fold_left
          (fun acc d ->
            if Random.float 1. <= p then
              (incr edgecnt; d :: acc)
            else
              acc)
          [] (gen (i - 1)))::acc)
      [] (gen n)) in
    Printf.eprintf "\027[0m%5d\027[33m vertices \027[0m%7d\027[33m edges" n !edgecnt;

    let ts = topol data in

    Printf.eprintf "\027[0m... ";

    let idxs = Array.make (n + 1) 0 in

    List.iter (fun (id, vl) -> idxs.(vl) <- id) (List.combine (gen n) ts);

    let wa = ref 0 in

    List.iter (fun (v, adj) ->
      List.iter (fun ngh ->
        if idxs.(v) > idxs.(ngh) then begin
            Printf.printf "%d should be placed before %d!\n" v ngh;
            incr wa
        end) adj) data;

    if !wa = 0 then
      Printf.eprintf "\027[32m OK:)\027[0m\n"
    else begin
      Printf.eprintf "\027[31m WA!\027[0m\n\n";
      List.iter (fun (v, adj) ->
        Printf.printf "\t (%d, [" v;
        if adj != [] then begin
          Printf.printf "%d" (List.hd adj);
          List.iter (Printf.printf "; %d") (List.tl adj)
        end;
        Printf.printf "])\n") data;
      Printf.printf "]\n";
      exit 1
    end
  end;;

for tc = 1 to t do
  test tc
done
