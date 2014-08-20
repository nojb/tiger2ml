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

open Format
open TigerError

let parse_file ppf inputfile =
  let lexbuf = Lexing.from_channel (open_in inputfile) in
  let prg = TigerParser.program TigerLexer.token lexbuf in
  let typs, prg = TigerTyping.exp prg in
  let m = TigerEmit.emit_ocaml typs prg in
  printf "%a@." Pprintast.default # structure m
  
let usage =
  "Usage: tiger2ml <options> <file>\nOptions are:"

let main () =
  try
    Arg.parse [] (parse_file Format.err_formatter) usage
  with
    Error (loc, err) ->
      eprintf "%aError: %a.@." Location.print loc TigerError.report err
  | Parsing.Parse_error ->
      eprintf "Parser error (where?).@."
  | Failure e ->
      eprintf "Internal error: %s@." e

let _ =
  main ()
