type 'a stream
exception StreamTooShort

val empty' : 'a stream
val put' : 'a -> 'a stream -> 'a stream
val null' : 'a stream -> bool Lazy.t

val singleton' : 'a -> 'a stream
val head' : 'a stream -> 'a Lazy.t
val tail' : 'a stream -> 'a stream

val last' : 'a stream -> 'a Lazy.t
val init' : 'a stream -> 'a stream

val map' : ('a -> 'b) -> 'a stream -> 'b stream
val filter' : ('a -> bool) -> 'a stream -> 'a stream

val foldr' : ('a -> 'b -> 'b) -> 'b -> 'a stream -> 'b Lazy.t
val foldl' : ('a -> 'b -> 'a) -> 'a -> 'b stream -> 'a Lazy.t

val scanr' : ('a -> 'b -> 'b) -> 'b -> 'a stream -> 'b stream
val scanl' : ('a -> 'b -> 'a) -> 'a -> 'b stream -> 'a stream

val take' : int -> 'a stream -> 'a stream
val drop' : int -> 'a stream -> 'a stream
val splitAt' : int -> 'a stream -> 'a stream * 'a stream

val takeWhile' : ('a -> bool) -> 'a stream -> 'a stream
val dropWhile' : ('a -> bool) -> 'a stream -> 'a stream

val zip' : 'a stream -> 'b stream -> ('a * 'b) stream
val zipWith' : ('a -> 'b -> 'c) -> 'a stream -> 'b stream -> ('c) stream

val repeat' : 'a -> 'a stream
val iterate' : ('a -> 'a) -> 'a -> 'a stream
val replicate' : int -> 'a -> 'a stream
val cycle' : 'a stream -> 'a stream

val concatenate' : 'a stream -> 'a stream -> 'a stream
val concat' : 'a stream stream -> 'a stream
val concatMap' : ('a -> 'b stream) -> 'a stream -> 'b stream
val length' : 'a stream -> int Lazy.t
val reverse' : 'a stream -> 'a stream

val mergeBy' : ('a -> 'a -> bool) -> 'a stream -> 'a stream -> 'a stream
val sortBy' : ('a -> 'a -> bool) -> 'a stream -> 'a stream

val merge' : 'a stream -> 'a stream -> 'a stream
val sort' : 'a stream -> 'a stream

val stream_of_list : 'a list -> 'a stream
val list_of_stream : 'a stream -> 'a list

val primes : int stream
val enumFrom' : int -> int stream

