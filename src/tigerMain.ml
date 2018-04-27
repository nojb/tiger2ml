(* Copyright 2014 Nicolas Ojeda Bar <n.oje.bar@gmail.com>.
   All rights reserved.
   Distributed under the Q Public License, version 1.0. *)

open Format
open TigerError

let parse_file ppf inputfile =
  Location.input_name := inputfile;
  let lexbuf = Lexing.from_channel (open_in inputfile) in
  let prg = TigerParser.program TigerLexer.token lexbuf in
  let typs, prg = TigerTyping.exp prg in
  let m = TigerEmit.emit_ocaml typs prg in
  fprintf ppf "@[%a@]@." Pprintast.structure m

let usage =
  "Usage: tiger2ml <options> <file>\nOptions are:"

let main () =
  try
    Arg.parse [] (parse_file Format.err_formatter) usage
  with
    Error (loc, err) ->
      eprintf "%aError: %a.@." Location.print loc TigerError.report err
  | Parsing.Parse_error ->
      eprintf "Parsing error.@."
  | Failure e ->
      eprintf "Internal error: %s@." e

let _ =
  main ()
