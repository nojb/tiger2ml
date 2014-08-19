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

let _print =
  print_string

let _printi =
  print_int

let _flush () =
  flush stdout

let _getchar () =
  let c = input_char stdin in
    (String.make 1 c)

let _ord s =
  let c = s.[0] in
    int_of_char c

let _chr n =
  let s = char_of_int n in
    String.make 1 s

let _size =
  String.length

let _substring =
  String.sub

let _concat s1 s2 =
  s1 ^ s2

let _not n =
  if n = 0 then 1 else 0

let _exit n =
  ignore (exit n)

exception Nil of int
