let rec y1 f x = f (y1 f) x;;

let y2 f =
  let r = ref (fun _ -> failwith "fixed point") in
  let g x = f !r x in
  r := g; !r;;

type 'a recc = Wrap of ('a recc -> 'a);;
let unwrap (Wrap f) = f;;
let fix f =
  (fun x a -> f (unwrap x x) a) (Wrap (fun x a -> f (unwrap x x) a));;

let fix_poly = fun l -> fix (fun self l -> List.map (fun li x -> li (self l) x) l) l

let memoizer (type w) (type u) (f : (w -> u) -> (w -> u)) = 
  let module M = Map.Make(struct
    type t = w
    let compare = compare end) in
    let db = ref (M.empty) in
    let rec run x =
      if not (M.mem x !db) then
        (let res = f run x in
        db := M.add x res !db);
      M.find x !db
    in
      run;;


module type QUEUE =
  sig
    type 'a queue
    exception EmptyQueue
    val init : unit -> 'a queue
    val is_empty : 'a queue -> bool
    val put_first : 'a queue -> 'a -> unit
    val put_last : 'a queue -> 'a -> unit
    val first : 'a queue -> 'a
    val last : 'a queue -> 'a
    val remove_first : 'a queue -> unit
    val remove_last : 'a queue -> unit
    val merge : 'a queue -> 'a queue -> unit
    val rev : 'a queue -> unit
  end;;

module type FIND_UNION = sig
  type 'a set
  val make_set : 'a -> 'a set
  val find : 'a set -> 'a
  val equivalent : 'a set -> 'a set -> bool
  val union : 'a set -> 'a set -> unit
  val elements : 'a set -> 'a list
  val n_of_sets : unit->int
end;;

let sort (type s) (module Set : Set.S with type elt = s) l =
  Set.elements (List.fold_right Set.add l Set.empty);;

let make_set (type s) cmp =
  let module S = Set.Make(struct
    type t = s
    let compare = cmp
  end) in
    (module S : Set.S with type elt = s);;


    (* module IntSet = Set.Make(struct type t = int;; let compare = Pervasives.compare end);; *)
module IntSet :
  sig
    type elt = int
    type t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
    val find : elt -> t -> elt
  end


module IntMap :
  sig
    type key = int
    type +'a t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val max_binding : 'a t -> key * 'a
    val choose : 'a t -> key * 'a
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  end

module Hashtbl :
 sig
    type ('a, 'b) t = ('a, 'b) Hashtbl.t
    val create : ?random:bool -> int -> ('a, 'b) t
    val clear : ('a, 'b) t -> unit
    val reset : ('a, 'b) t -> unit
    val copy : ('a, 'b) t -> ('a, 'b) t
    val add : ('a, 'b) t -> 'a -> 'b -> unit (* przykrywa, nie kasuje! *)
    val find : ('a, 'b) t -> 'a -> 'b
    val find_all : ('a, 'b) t -> 'a -> 'b list
    val mem : ('a, 'b) t -> 'a -> bool
    val remove : ('a, 'b) t -> 'a -> unit
    val replace : ('a, 'b) t -> 'a -> 'b -> unit
    val iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit (* z rownych najnowsze dodane najpierw *)
    val fold : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
    val length : ('a, 'b) t -> int
    val randomize : unit -> unit
end

 (* create ~random:true 100 -- przewiduje 100 miejsc, ale sie rozrosnie *)
