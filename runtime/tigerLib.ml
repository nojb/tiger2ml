(* Copyright 2014 Nicolas Ojeda Bar <n.oje.bar@gmail.com>.
   All rights reserved.
   Distributed under the Q Public License, version 1.0. *)

exception Break

exception Nil of int

exception Out_of_bounds of int

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
    _ -> failwith "chr"

let size =
  String.length

let substring s pos len =
  try
    String.sub s pos len
  with
    _ -> failwith "substring"

let concat s1 s2 =
  s1 ^ s2

let not =
  not

let exit =
  exit

let get a i line =
  if i < 0 || i >= Array.length a then
    raise (Out_of_bounds line)
  else
    Array.unsafe_get a i

let set a i x line =
  if i < 0 || i >= Array.length a then
    raise (Out_of_bounds line)
  else
    Array.unsafe_set a i x

let run f =
  try
    f ()
  with
    Break ->
      assert false
  | Nil line ->
      Printf.eprintf "Failure (line %i): nil record\n%!" line;
      exit 1
  | Division_by_zero ->
      Printf.eprintf "Failure: division by zero";
      exit 1
  | Out_of_bounds line ->
      Printf.eprintf "Failure (line %i): index out of bounds\n%!" line;
      exit 1
  | Failure err ->
      Printf.eprintf "Failure: %s\n%!" err;
      exit 1
