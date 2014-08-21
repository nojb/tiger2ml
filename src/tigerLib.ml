(* Copyright 2014 Nicolas Ojeda Bar <n.oje.bar@gmail.com>.
   All rights reserved.
   Distributed under the Q Public License, version 1.0. *)

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
