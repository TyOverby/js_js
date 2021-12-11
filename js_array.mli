open! Base

type 'a t [@@deriving sexp, equal, compare, bin_io]

include Indexed_container.S1 with type 'a t := 'a t
include Binary_searchable.S1 with type 'a t := 'a t
include Comparator.Derived with type 'a t := 'a t
include Blit.S1 with type 'a t := 'a t
include Monad.S with type 'a t := 'a t

val empty : unit -> 'a t
val clone : 'a t -> 'a t
val of_array : 'a array -> 'a t
val get : 'a t -> int -> 'a option
val set_exn : 'a t -> int -> 'a -> unit
val get_exn : 'a t -> int -> 'a
val set_unchecked : 'a t -> int -> 'a -> unit
val get_unchecked : 'a t -> int -> 'a
val push : 'a t -> 'a -> unit
val unshift : 'a t -> 'a -> unit
val concat : 'a t -> 'a t -> 'a t
val pop : 'a t -> 'a option
val shift : 'a t -> 'a option
val pop_exn : 'a t -> 'a
val shift_exn : 'a t -> 'a
val rev : 'a t -> unit
val sort : 'a t -> compare:('a -> 'a -> int) -> unit
val sorted : 'a t -> compare:('a -> 'a -> int) -> 'a t
val slice : ?end_:int -> start:int -> 'a t -> 'a t
val remove : 'a t -> int -> unit
val swap_remove : 'a t -> int -> unit
val remove_range : 'a t -> int -> len:int -> unit
val mapi : 'a t -> f:(int -> 'a -> 'b) -> 'b t
val filter : 'a t -> f:('a -> bool) -> 'a t
val filteri : 'a t -> f:(int -> 'a -> bool) -> 'a t
val insert : 'a t -> idx:int -> 'a -> unit

val rev_iter : 'a t -> f:('a -> unit) -> unit
val rev_iteri : 'a t -> f:(int -> 'a -> unit) -> unit
