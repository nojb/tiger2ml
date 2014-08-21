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

let print =
  print_string

let printi =
  print_int

let flush () =
  flush stdout

let getchar () =
  try
    String.make 1 (input_char stdin)
  with
    End_of_file -> ""

let ord s =
  match String.length s with
    0 -> -1
  | _ -> int_of_char s.[0]

let chr n =
  try
    String.make 1 (char_of_int n)
  with
    _ -> exit 1

let size =
  String.length

let substring =
  String.sub

let concat s1 s2 =
  s1 ^ s2

let not =
  not

let exit =
  exit

exception Break

exception Nil of int * int * int

exception Division_by_zero of int * int * int

exception Out_of_bounds of int * int * int
