(* Copyright (c) 2014, Nicolas Ojeda Bar <n.oje.bar@gmail.com>

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
   REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
   AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
   INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
   LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
   OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
   PERFORMANCE OF THIS SOFTWARE. *)

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

exception Break

exception Nil of int * int * int

exception Division_by_zero of int * int * int

exception Out_of_bounds of int * int * int
