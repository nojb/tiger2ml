(* Copyright 2014 Nicolas Ojeda Bar <n.oje.bar@gmail.com>.
   All rights reserved.
   Distributed under the Q Public License, version 1.0. *)

open Format
open Tiger

let parse_file ppf inputfile =
  Location.input_name := inputfile;
  let lexbuf = Lexing.from_channel (open_in inputfile) in
  let prg = Parser.program Lexer.token lexbuf in
  let typs, prg = Typing.exp prg in
  let m = Emit.emit_ocaml typs prg in
  fprintf ppf "@[%a@]@." Pprintast.structure m

let usage =
  Printf.sprintf "Usage: %s <options> <file>\nOptions are:" (Filename.basename Sys.executable_name)

let main () =
  try
    Arg.parse [] (parse_file Format.err_formatter) usage
  with
    Error.Error (loc, err) ->
      eprintf "%aError: %a.@." Location.print loc Error.report err
  | Parsing.Parse_error ->
      eprintf "Parsing error.@."
  | Failure e ->
      eprintf "Internal error: %s@." e

let _ =
  main ()
