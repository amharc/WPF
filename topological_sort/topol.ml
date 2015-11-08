open List

exception Cykliczne

(* Graph will be internally held as a map binding a vertex with its indegree and list of outcoming edges *)

let toMap data =
  let idxs   = flatten (map snd data)

  (* Increment indegree of vertex *)
  and incvrt = fun m v ->
    let (indeg, adj) = PMap.find v m
      in
         PMap.add v (indeg + 1, adj) m

  (* First of all, create empty bindings for all vertices with at least one incoming edge *)
  in let m = fold_left (fun m x        -> PMap.add x (0, [])  m) PMap.empty idxs
  (* Then, add adjacency lists for all vertices with at least one outcoming edge *)
  in let m = fold_left (fun m (x, adj) -> PMap.add x (0, adj) m) m          data
  in
             fold_left incvrt                                    m          (flatten (map snd data))

(* Extract one vertex with indegree 0 and return updated map and stack. Result is accumulated on acc *)
let step (m, s, acc) =
  match s with
    | []    -> raise Cykliczne (* some vertices left, but no vertex has indegree 0 *)
    | v::vs ->

        (* Update map and stack of vertices with indegree 0 *)
        let (m, s) = fold_left
          (fun (m, s) node ->
            let (indeg, adj) = PMap.find node m
            in let m = PMap.add node (indeg - 1, adj) m (* Update indeg *)
            in
              if indeg = 1 then (* Now indeg node will become 0 *)
                (m, node :: s)
              else
                (m, s)
          )
          (m, vs)
          (snd (PMap.find v m)) (* adjacency list of v *)
        in
          (m, s, v :: acc)

(* Kahn algorithm *)
let topol data =
  let    m = toMap data
  in let s = PMap.foldi (* Get all vertices with indegree 0 *)
            (fun node (indeg, _) s ->
              if indeg = 0 then
                node :: s
              else
                s)
            m
            []
  in
    let (_, _, acc) = PMap.fold (* Just to make n steps without recursiom *)
                        (fun _ a -> step a)
                        m
                        (m, s, [])
    in
      rev acc

