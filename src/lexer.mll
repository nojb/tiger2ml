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

{
open Parser

let str_buf =
  Buffer.create 100

let incr_linenum lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <- { pos with
    Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
    Lexing.pos_bol = pos.Lexing.pos_cnum
  }
}

rule token = parse
    [' ' '\t']     { token lexbuf }
  | '\n'            { incr_linenum lexbuf; token lexbuf }
  | eof                 { EOF }
  | "/*"                { comment 0 lexbuf }
  | "/"                 { DIV }
  | '&'                 { AND }
  | '|'                 { OR }
  | "let"               { LET }
  | "if"                { IF }
  | "in"                { IN }
  | "then"              { THEN }
  | "else"              { ELSE }
  | "end"               { END }
  | "var"               { VAR }
  | "type"              { TYPE }
  | "function"          { FUNCTION }
  | "array"             { ARRAY }
  | "of"                { OF }
  | '{'                 { LC }
  | '}'                 { RC }
  | "nil"               { NIL }
  | "break"             { BREAK }
  | "while"             { WHILE }
  | "do"                { DO }
  | "for"               { FOR }
  | "to"                { TO }
  | ['0'-'9']+ as lxm   { INT (int_of_string lxm) }
  | '+'                 { PLUS }
  | '-'                 { MINUS }
  | '*'                 { TIMES }
  | ":="                { COLONEQ }
  | '='                 { EQ }
  | "<>"                { NEQ }
  | "<="                { LE }
  | ">="                { GE }
  | '<'                 { LT }
  | '>'                 { GT }
  | ';'                 { SEMI }
  | ':'                 { COLON }
  | '.'                 { DOT }
  | ','                 { COMMA }
  | '('                 { LP }
  | ')'                 { RP }
  | '['                 { LB }
  | ']'                 { RB }
  | '"'                 { str lexbuf }
  | ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9''_']* as lxm { IDENT lxm }

and comment level = parse
  | "*/"          { if level == 0 then token lexbuf else comment (level - 1)
  lexbuf }
  | "/*"          { comment (level+1) lexbuf }
  | '\n'          { incr_linenum lexbuf; comment level lexbuf }
  | eof           { EOF }
  | _             { comment level lexbuf }

and str = parse
  | '"'           { let s = Buffer.contents str_buf in Buffer.clear
  str_buf; STRING s }
  | '\n'          { incr_linenum lexbuf; Buffer.add_char str_buf '\n'; str lexbuf }
  | "\\n"         { Buffer.add_char str_buf '\n'; str lexbuf }
  | "\\t"         { Buffer.add_char str_buf '\t'; str lexbuf }
  | "\\\""        { Buffer.add_char str_buf '"'; str lexbuf }
  | "\\" ((['0'-'9']['0'-'9']['0'-'9']) as lxm)
                  { Buffer.add_char str_buf (char_of_int (int_of_string lxm));
                  str lexbuf }
  | "\\\\"          { Buffer.add_char str_buf '\\'; str lexbuf }
  | '\\' [' ''\t' '\r']   { skip_whitespace lexbuf }
  | '\\' '\n'     { incr_linenum lexbuf; skip_whitespace lexbuf }
  | eof           { EOF }
  | _ as c           { Buffer.add_char str_buf c; str lexbuf }

and skip_whitespace = parse
  | '\\'          { str lexbuf }
  | '\n'          { incr_linenum lexbuf; skip_whitespace lexbuf }
  | [' ' '\t' '\r']  { skip_whitespace lexbuf }
  | eof           { EOF }
