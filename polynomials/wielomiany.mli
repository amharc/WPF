open Lazy
open Streams

type wielomian
type wielomian_c
type rzadki

val oblicz : wielomian -> float -> float Lazy.t
val suma : wielomian -> wielomian -> wielomian
val razy : wielomian -> wielomian -> wielomian
val pochodna : wielomian -> wielomian 
val calka : wielomian -> wielomian 
val stopien : wielomian -> int Lazy.t

val oblicz_c : wielomian_c -> Complex.t -> Complex.t Lazy.t
val suma_c : wielomian_c -> wielomian_c -> wielomian_c
val razy_c : wielomian_c -> wielomian_c -> wielomian_c
val pochodna_c : wielomian_c -> wielomian_c 
val calka_c : wielomian_c -> wielomian_c 
val stopien_c : wielomian_c -> int Lazy.t

val oblicz_rz : rzadki -> float -> float Lazy.t
val suma_rz : rzadki -> rzadki -> rzadki
val razy_rz : rzadki -> rzadki -> rzadki
val pochodna_rz : rzadki -> rzadki 
val calka_rz : rzadki -> rzadki 
val stopien_rz : rzadki -> int Lazy.t

val zeroes : wielomian -> float stream

val wielomian_of_wielomian_c : wielomian_c -> wielomian
val wielomian_c_of_wielomian : wielomian -> wielomian_c

val rzadki_of_wielomian : wielomian -> rzadki
val wielomian_of_rzadki : rzadki -> wielomian
