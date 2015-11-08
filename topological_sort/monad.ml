open List

exception Cykliczne

(* State monad *)
type ('s, 'a) state = ('s -> ('a * 's))

let runState s x = s x
let return x s = (x, s)
let (>>=) a b s = let (x, s') = runState a s in runState (b x) s'
let (>>) a b = a >>= (fun _ -> b)

let get s = (s, s)
let put x s = ((), x)
let modify f = get >>= (fun x -> put (f x))
let sequence ms = 
  let k m m' = 
    m  >>= fun x ->
    m' >>= fun xs ->
    return (x :: xs)
  in
    List.fold_right k ms (return [])

let liftM f m = m >>= (fun x -> f x)

let mapM f a = sequence (List.map f a)

(* Auxiliary functions *)
let mapFst f (a, b) = (f a, b)
let pmapOper f v m = PMap.add v (f (PMap.find v m)) m
let (@@) f g = f g

(* Graph will be internally held as a map binding a vertex with its indegree and list of outcoming edges *)
(* State monad will hold a pair (map, stack of vertices with indegree 0) *)

let toMap data =
  let idxs   = flatten (map snd data)
  in

  mapM (fun x -> modify @@ mapFst @@ PMap.add x (0, [])) idxs >>
  mapM (fun (x, adj) -> modify @@ mapFst @@ PMap.add x (0, adj)) data >>
  mapM (fun x -> modify @@ mapFst (pmapOper (mapFst (fun x -> x + 1)) x)) idxs

(* Extract one vertex with indegree 0 and update map and stack. Return the vertex *)
(* x is a dummy variable, used because ocaml has weird monomorphism restriction *)
let step x =
  (get >>= fun (m, s) ->
  match s with
    | [] -> raise Cykliczne
    | v :: vs ->
        modify (fun (m, _) -> (m, vs)) >> (* pop *)
        liftM (fun (m, _) -> return @@ snd @@ PMap.find v m) get >>= fun adj -> (* Get adjacency list of node v *)
        mapM (fun node ->
          modify (fun (m, s) ->
            let (indeg, adj) = PMap.find node m
            in
              (PMap.add node (indeg - 1, adj) m,
               if indeg = 1 then node :: s else s)
            ))
          adj >>
        return v) x

let elems m = PMap.foldi (fun k v s -> (k, v) :: s) m []

(* Kahn algorithm *)
let topol data =
  fst @@ runState
    (toMap data >>
     get >>= fun (m, _) ->
     let vrts = PMap.foldi (fun node _ s -> node :: s) m [] in
     let s    = PMap.foldi (fun node (indeg, _) s ->
                             if indeg = 0 then
                               node :: s
                             else
                               s)
                            m [] in
     put (m, s) >>
     mapM (fun _ -> step) vrts) (* Repeat n times *)
    (PMap.empty, []) (* Initial state *)
