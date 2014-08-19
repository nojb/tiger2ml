open Printf
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
      let (_, typedprg) = Typecheck.typecheck Typecheck.std_env
        Typecheck.error_break prg in
        Transl.emit_ocaml typedprg;
        flush stdout
  with
  | Error (p, s) ->
    fprintf stderr "%sine %d, column %d: %s\n" (if p.Lexing.pos_fname = ""
    then "L" else "File " ^ p.Lexing.pos_fname ^ ", l")
      p.Lexing.pos_lnum (p.Lexing.pos_cnum - p.Lexing.pos_bol+1) s; exit(2)
  | Parser.Error ->
      fprintf stderr "Parser error (where?). Terminating.\n"; exit(2)
  | Failure e -> printf "Internal error: %s\n" e; exit(2)
