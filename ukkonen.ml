module CharMap = Map.Make(struct
  type t = char;;
  let compare = compare;
end);;

type  node = { leaf_no : int
             ; mutable suf : node
             ; mutable edges : edge CharMap.t }
and  edge  = { mutable dst : node
             ; mutable l : int
             ; mutable r : int}



let build_tree txt =
  let mk_node l s = { leaf_no = l
                    ; suf     = s
                    ; edges   = CharMap.empty } in
  let rec bot  = { leaf_no = -1
                 ; suf     = bot
                 ; edges   = CharMap.empty } in
  let     root = mk_node (-1) bot
  and     n    = String.length txt 
  and     lcnt = ref 0
  in

  let canonize e =
    if e.l <= e.r then begin
      let ed = ref (CharMap.find txt.[e.l] e.dst.edges) in
      while !ed.r - !ed.l <= e.r - e.l do
        e.l <- e.l + !ed.r - !ed.l + 1;
        e.dst <- !ed.dst;
        if e.l <= e.r then
          ed := CharMap.find txt.[e.l] e.dst.edges
      done
    end in

  let test_and_split w e =
    w := e.dst;
    if e.l <= e.r then begin
      let edp = CharMap.find txt.[e.l] e.dst.edges in
      let ed = { dst = edp.dst; l = edp.l; r = edp.r } in (* shallow copy *)
      if txt.[e.r + 1] = txt.[ed.l + e.r - e.l + 1] then
        true
      else begin
        w := mk_node (-1) bot;
        (CharMap.find txt.[e.l] e.dst.edges).r <- ed.l + e.r - e.l;
        (CharMap.find txt.[e.l] e.dst.edges).dst <- !w;
        ed.l <- ed.l + e.r - e.l + 1;
        !w.edges <- CharMap.add txt.[ed.l] ed !w.edges;
        false
      end
    end else
      CharMap.mem txt.[e.l] e.dst.edges in

  let update e =
    let prev, w = ref root, ref root in
    
    while not (test_and_split w e) do
      let ed = { dst = mk_node !lcnt root
               ; l   = e.r + 1
               ; r   = n - 1 } in
      incr lcnt;

      !w.edges <- CharMap.add txt.[ed.l] ed !w.edges;
      
      if !prev != root then
        !prev.suf <- !w;

      prev := !w;
      e.dst <- e.dst.suf;
      canonize e
    done;

    if !prev != root then
      !prev.suf <- !w

  in
    for i = 0 to n - 1 do
      bot.edges <- CharMap.add txt.[i] 
        { dst = root;
          l   = -i;
          r   = -i }
       bot.edges 
    done;

    let e = { dst = root;
              l   = 0;
              r   = -1 } in

    for i = 0 to n - 1 do
      update e;
      e.r <- e.r + 1;
      canonize e
    done;
  root;;

let sufsort txt =
  let t = build_tree txt in
  let rec trav nd = 
    if nd.leaf_no <> -1 then
      Printf.printf "%d " nd.leaf_no;
      CharMap.iter (fun _ ed -> trav ed.dst) nd.edges
  in
    trav t;
    Printf.printf "\n";

