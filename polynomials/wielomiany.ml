open Lazy;;
open Streams;; (* wlasny modul do strumieni - wole leniwosc *)

(* Polynomial is represented as a (lazy) stream of coefficients (in order of increasing exponents) of respective type. No trailing zeroes are allowed. *)
type wielomian = float stream;;
type wielomian_c = Complex.t stream;;

(* Old ocaml version *)
let (@@) f g = f g;;

(** Evaluate a complex polynomial wiel in point x. Time compl.: linear, Space compl.:  linear *)
let oblicz_c wiel x = 
  let fn ai acc = Complex.add ai @@ Complex.mul acc x
  in
    foldr' fn Complex.zero wiel
;;

(** Admissible error *)
let const_epsilon = 1e-12;;

(** Checks if a complex number is small *)
let epsilon_c x = Complex.norm x < const_epsilon;;
(** Checks if a real number is small *)
let epsilon x = abs_float(x) < const_epsilon;;

(** Remove trailing zeroes (real polynomials). Time compl.: linear, space compl.: linear (but has to generate the result) *)
let shorten y = reverse' (dropWhile' epsilon (reverse' y));;
(** Remove trailing zeroes (complex polynomials). Time compl.: linear, space compl.: linear (but has to generate the result) *)
let shorten_c y = reverse' (dropWhile' epsilon_c (reverse' y));;

(** Append trailing zeroes. Time compl.: linear, space compl.: linear (but has to generate the resuly) *)
let expand_c x y =
  let cnt = force(length' y) - force(length' x)
  in
    concatenate' x (replicate' cnt Complex.zero)

(* Linear space complexity, but so much space is required for the result *)
let suma_c x y =
  let (xx, yy) = (expand_c x y, expand_c y x)
  in
    shorten_c @@ zipWith' (Complex.add) xx yy
;;

let enumFromFloat x = iterate' (Complex.add Complex.one) x;;

(* Linear space complexity, but so much space is required for the result *)
let pochodna_c x =
  drop' 1 @@ zipWith' (Complex.mul) x (enumFromFloat Complex.zero)
;;

(* Linear space complexity, but so much space is required for the result *)
let calka_c x =
  put' Complex.zero @@ zipWith' (Complex.div) x (enumFromFloat Complex.one)
;;

(* Const. space complexity, linear time complexity *)
let stopien_c x = lazy(-1 + force(length' x));;


(* Fourier transform *)
(* Strict operation, splits polynomial in (even, odd) indexes *)
let rec split st =
  if force (null' st) then
    (empty', empty')
  else
    let    (x, y, rest) = (force(head' st), force(head' @@ tail' st), tail' @@ tail' st)
    in let (xx, yy)     = split rest
    in
      (put' x xx, put' y yy)
;;

(* Computes DFT iff mode = 1, inverse DFT iff mode = -1 *)
let rec dft mode st =
  if force(length' st) < 2 then
    st
  else
    let    (aa, bb) = split st
    and    omegaexp = { Complex.re = 0.; Complex.im = 2. *. 4. *. mode *. atan 1. /. (float_of_int (force(length' st)))}
    in let (xx, yy) = (dft mode aa, dft mode bb)
    and    omega    = Complex.exp omegaexp
    in let omegas   = iterate' (Complex.mul omega) Complex.one
    in let vals     = zip' (zip' xx yy) omegas
    in let f x y =
      match x with ((fst, snd), ex) ->
        match y with (ff, ss) ->
          let (nf, ns) = (Complex.add fst @@ Complex.mul ex snd, Complex.sub fst @@ Complex.mul ex snd)
          in
            (put' nf ff, put' ns ss)
    in let (first, second) = force(foldr' f (empty', empty') vals)
    in
      concatenate' first second
;;


(* Smallest power of two greater or equal than n *)
let smallestPow2 n = force(head' @@ dropWhile' (fun x -> x < n) @@ iterate' (fun x -> x * 2) 1);;

(* Add trailing zeroes to polynomial, such that its length equals len *)
let setlength x len = concatenate' x (replicate' (len - force(length' x)) Complex.zero);;

let razy_c x y =
  let    (l1, l2) = (force(length' x), force(length' y))
  in let newlen   = smallestPow2 (l1 + l2)
  in let (x, y)   = (setlength x newlen, setlength y newlen)
  in let (fx, fy) = (dft 1.0 x, dft 1.0 y)
  in let newpoly  = zipWith' Complex.mul fx fy
  in let divisor  = { Complex.re = (float_of_int newlen); Complex.im = 0. }
  in
    shorten_c @@ map' (fun x -> Complex.div x divisor) @@ dft (-1.0) newpoly
;;

(* Conversions *)
let getre x = match x with { Complex.re = r } -> r;;

let wielomian_c_of_wielomian = map' (fun x -> { Complex.re = x; Complex.im = 0. });;
let wielomian_of_wielomian_c x = shorten @@ map' getre x;;

(* Operations on real polynomials *)
let apply1 f x = wielomian_of_wielomian_c @@ f (wielomian_c_of_wielomian x);;
let apply2 f x y = wielomian_of_wielomian_c @@ f (wielomian_c_of_wielomian x) (wielomian_c_of_wielomian y);;
let oblicz f x = lazy(getre @@ force @@ oblicz_c (wielomian_c_of_wielomian f) {Complex.re = x; Complex.im = 0.});;
let suma = apply2 suma_c;;
let razy = apply2 razy_c;;
let pochodna = apply1 pochodna_c;;
let calka = apply1 calka_c;;
let stopien x = lazy(-1 + force(length' x));;

(* Computation of zeroes of polynomials *)

(* Precondition: f(a) > 0, f(b) < 0 *)
let rec bisect f a b =
  let mid = (a +. b) /. 2.
  in
    if epsilon (b -. a) then
      mid
    else
      let v = force @@ oblicz f mid
      in 
        if v > const_epsilon then
          bisect f mid b
        else
          bisect f a mid
;;

let findroot f a b =
  if epsilon (b -. a) then
    singleton' ((a +. b)/.2.)
  else
    match (force @@ oblicz f a, force @@ oblicz f b) with
      | (va, _) when epsilon va -> singleton' a
      | (_, vb) when epsilon vb -> singleton' b 
      | (va, vb) when va < 0. && vb > 0. -> singleton' (bisect f b a)
      | (va, vb) when va > 0. && vb < 0. -> singleton' (bisect f a b)
      | _ -> empty'
;;

let rec zeroes = function
  | a when force(null' a) -> failwith "Zero polynomial"
  | a when force(length' a) = 1 -> empty'
  | f ->
    let ddzeroes   = zeroes (pochodna f)
    and bound      = abs_float(3. +. (1. /. force(head' (reverse' f))) *. (force (foldl' (+.) 0. (map' abs_float f)))) (* bound on abs of roots *)
    in let dzeroes = concat' @@ put' (singleton' (-.bound)) @@ put' ddzeroes @@ put' (singleton' bound) @@ empty'
    in
      concatMap' (fun (a, b) -> findroot f a b) (zip' dzeroes (tail' dzeroes))
;;

(* Sparse polynomials -------------------------------------------------------------------------------------------------------------------- *)

(* Sparse polynomial is represented as a stream of pairs (exponent, coefficient). Coefficients must not satisfy predicate epsilon.
 * No two elements of the stream may have the same exponent. Stream must be sorted by increasing exponents. An empty stream means a zero polynomial *)
type rzadki = (int * float) stream;;

(* Remove terms with small coefficients *)
let remove_epsilons = filter' (fun x -> not @@ epsilon (snd x));;

(* Merge values with the same exponent *)
let merge_dense p = 
  (* Prepend x to accumulator, or merge it with its head if they have the same exponent *)
  let fn x acc =
    if force(null' acc) then
      singleton' x
    else
      if fst (force (head' acc)) = fst x then
        put' (fst x, snd x +. snd (force (head' acc))) (tail' acc)
      else
        put' x acc
  in
    foldr' (fn) empty' p
;;

(* Add two sparse polynomials, but do NOT remove epsilon terms *)
let suma_rz_wepsilons x y = force(merge_dense (mergeBy' (fun x y -> (fst x < fst y)) x y));;

let suma_rz x y = remove_epsilons (suma_rz_wepsilons x y);;
let stopien_rz x = lazy(if force(null' x) then -1 else fst(force(last' x)));;
let pochodna_rz x = tail' (map' (fun (a, b) -> (a - 1, b *. (float_of_int a))) x);;
let calka_rz x = map' (fun (a, b) -> (a + 1, b /. (1. +. float_of_int a))) x;;
let rec razy_rz x y = 
  let fn acc (a, b) =
    suma_rz acc (map' (fun (c, d) -> (a + c, b *. d)) x)
  in
    force (foldl' fn empty' y)
;;

let rzadki_of_wielomian x = remove_epsilons (zip' (enumFrom' 0) x);;
let wielomian_of_rzadki x =
  let zero = zip' (enumFrom' 0) (replicate' (1 + force(stopien_rz x)) 0.)
  in
    map' (snd) (suma_rz_wepsilons x zero)
;;

let rec pow a = function
  | 0 -> 1.
  | b when b mod 2 <> 0 ->
      a *. (pow a (b - 1))
  | b ->
      let x = pow a (b / 2)
      in
        x *. x
;;

let oblicz_rz f x = foldl' (+.) 0. (map' (fun (a, b) -> b *. (pow x a)) f);;
