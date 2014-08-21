(* Copyright 2014 Nicolas Ojeda Bar <n.oje.bar@gmail.com>.
   All rights reserved.
   Distributed under the Q Public License, version 1.0. *)

exception Break

exception Nil of int

exception Out_of_bounds of int

val print : string -> unit

val printi : int -> unit

val flush : unit -> unit

val getchar : unit -> string

val ord : string -> int

val chr : int -> string

val size : string -> int

val substring : string -> int -> int -> string

val concat : string -> string -> string

val not : bool -> bool

val exit : int -> unit

val get : 'a array -> int -> int -> 'a

val set : 'a array -> int -> 'a -> int -> unit

val run : (unit -> 'a) -> 'a
