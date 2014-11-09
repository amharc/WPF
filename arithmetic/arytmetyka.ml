type wartosc = Interval of float * float (* wartosc znajduje sie w przedziale [x, y] dla x <= y *)
               | Complement of float * float (* wartosc znajduje sie poza przedzialem (x, y) dla x < y *)

let wartosc_dokladnosc x p =
  let diff = (p /. 100.) *. x (* maksymalny dopuszczalny blad bezwzgledny *)
  in Interval (x -. diff, x +. diff)

let wartosc_od_do x y = Interval (x, y)

let wartosc_dokladna x = Interval (x, x)

let in_wartosc x y =
  match x with
    | Interval (mn, mx)   -> mn <= y && y <= mx
    | Complement (mn, mx) -> y <= mn || mx <= y

let min_wartosc = function
  | Interval (mn, mx) -> mn
  | _                 -> neg_infinity

let max_wartosc = function
  | Interval (mn, mx) -> mx
  | _                 -> infinity

let sr_wartosc = function
  | Interval (mn, mx) when mn <> neg_infinity && mx <> infinity ->
      (mn +. mx) /. 2.
  | _ -> nan

(* jesli t jest dopelnieniem przedzialu (x, y) dla x > y, czyli przedzialu pustego, to tak naprawde jest przedzialem (-inf, inf) *)
let reduce = function 
  | Complement (mn1, mn2) when mn1 >= mn2 -> Interval (neg_infinity, infinity)
  | a                                     -> a

let rec plus = function
  | Complement (mn1, mx1) -> (function (* pierwszy argument jest dopelnieniem przedzialu *)
    | Interval (mn2, mx2) -> 
        let left  = mn1 +. mx2 (* ograniczenie gorne na sume b i elementu z pierwszego przedzialu a *)
        and right = mx1 +. mn2 (* ograniczenie dolne na sume b i elementu z drugiego przedzialu a *)
        in reduce (Complement (left, right))
    | Complement _ -> Interval (neg_infinity, infinity) (* suma moze byc jakakolwiek *)
    )
  | (Interval (mn1, mx1) as a) -> (function (* pierwszy argument jest przedzialem *)
    | Interval (mn2, mx2)          -> Interval ((mn1 +. mn2), (mx1 +. mx2)) (* trywialne dodanie *)
    | (Complement (mn2, mx2) as b) -> plus b a (* z przemiennosci dodawania, przypadek przedzial + dopelnienie mamy rozwazony wyzej *)
    ) 

let neg = function (* dla danego t, zwraca -t *)
  | Interval (mn, mx)   -> Interval (-.mx, -.mn)
  | Complement (mn, mx) -> Complement (-.mx, -.mn)

let minus a b = plus a (neg b)

let signum = function (* zwraca znak liczby *)
  | 0.            -> 0
  | x when x > 0. -> 1
  | _             -> -1

let rec razy = function
  | (Complement (mn1, mx1) as a) -> (function
    | Interval (0., 0.) -> (* mnozenie przez 0 *)
        Interval (0., 0.)
    | b when in_wartosc b 0. -> (* b zawiera liczby dowolnie bliskie na modul od zera i zero, a wiec kazda wartosc jest osiagalna *)
        Interval (neg_infinity, infinity)
    | Interval (mn2, mx2) when mn2 > 0. ->
        let left  = max (mn1 *. mx2) (mn1 *. mn2) (* ograniczenie gorne na iloczyn b i elementu z pierwszego przedzialu a *)
        and right = min (mx1 *. mx2)  (mx2 *. mn2)  (* ograniczenie dolne na iloczyn b i elementu z drugiego przedzialu a *)
        in reduce (Complement (left, right))
    | (Interval _  as b) -> (* kod analogiczny jak wyzej, wiec sie wywolajmy ponownie *)
        neg (razy a (neg b))
    | Complement _ when in_wartosc a 0. -> (* a zawiera liczby dowolnie bliskie na modul od zera i zero, a wiec kazda wartosc jest osiagalna *)
        Interval (neg_infinity, infinity)
    | Complement (mn2, mx2) ->
        let left  = max (mn1 *. mx2) (mn2 *. mx1) (* ograniczenie gorne na iloczyn b i elementu z pierwszego przedzialu a *)
        and right = min(mx1 *. mx2) (mn1 *. mn2)  (* ograniczenie dolne na iloczyn b i elementu z drugiego przedzialu a *)
        in reduce (Complement (left, right))
    )
  | (Interval (mn1, mx1) as a) -> (function
    | (Complement _ as b) -> razy b a (* juz napisalismy to wyzej, a mnozenie jest przemienne *)
    | Interval (mn2, mx2) ->
        let bounds = List.filter (* chcemy policzyc mozliwe skrajne wartosci przedzialu wynikowego *)
          (fun x -> x = x) (* ale moze byc 0 * infinity = nan, a tego nie chcemy. Wszystko poza nanem spełnia x = x, wiec to jest dobry warunek *)
          [mn1 *. mn2; mn1 *. mx2; mx1 *. mn2; mx1 *. mx2] (* mozliwe ograniczenia *)
        in let left  = List.fold_left (min) infinity bounds (* i liczymy najmniejsze i najwieksze z nich *)
           and right = List.fold_left (max) neg_infinity bounds
           in Interval (left, right)
    )

(* funkcje pomocnicze w celu stworzenia przedzialu lub dopelnienia, gdy znamy wartosci brzegowe, ale nie wiemy, w jakiej wystepuja kolejnosci *)
let mkInterval a b = Interval (min a b, max a b)
let mkComplement a b = Complement (min a b, max a b)
           
(* liczenie odwrotnosci liczby *)
let inv = function
  | Interval (mn, mx) when (signum mn) * (signum mx) < 0 -> (* przedzial zawiera 0 we wnętrzu, a wiec po odwroceniu sie 'wywinie' *)
      mkComplement (1. /. mx) (1. /. mn)
  | Interval (mn, 0.) -> (* przedzial zawierajacy 0 na brzegu, zmienia sie w przedzial jednostronnie nieskonczony *)
      Interval (neg_infinity, 1. /. mn)
  | Interval (0., mx) -> (* j.w. *)
      Interval (1. /. mx, infinity)
  | Interval (mn, mx) -> (* przedzial niezawierajacy zera przechodzi na inny przedzial (skonczony) *)
      mkInterval (1. /. mx) (1. /. mn)
  | Complement (mn, mx) when (signum mn) * (signum mx) < 0 -> (* dopelnienie przedzialu, ktore nie zawiera 0, przejdzie na przedzial zawierajacy 0 *)
      mkInterval (1. /. mx) (1. /. mn)
  | Complement (mn, mx) -> (* dowolne inne dopelnienie przedzialu przechodzi na dopelnienie przedzialu *)
      mkComplement (1. /. mx) (1. /. mn)

let podzielic a b = razy a (inv b)
