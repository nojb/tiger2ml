open Format
open Error

let get_lexbuf () =
  if Array.length Sys.argv = 1 then
    Lexing.from_channel stdin
  else begin
    Syntax.currfilename := Sys.argv.(1);
    Lexing.from_channel (open_in Sys.argv.(1))
  end

let _ =
  try
    let lexbuf = get_lexbuf () in
    let prg = Parser.program Lexer.token lexbuf in
    let (_, typedprg) = Typecheck.exp Typecheck.std_env prg in
    Transl.emit_ocaml typedprg
  with
    Error (p, s) ->
      eprintf "%a: %s@." Location.print p s
  | Parsing.Parse_error ->
      eprintf "Parser error (where?). Terminating.@."
  | Failure e ->
      eprintf "Internal error: %s@." e
