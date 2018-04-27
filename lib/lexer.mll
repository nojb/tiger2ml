(* Copyright 2014 Nicolas Ojeda Bar <n.oje.bar@gmail.com>.
   All rights reserved.
   Distributed under the Q Public License, version 1.0. *)

{
open Parser

let string_buf =
  Buffer.create 100
}

rule token = parse
    [' ' '\t']     { token lexbuf }
  | '\n'            { Lexing.new_line lexbuf; token lexbuf }
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
  | ['0'-'9']+ { INT (int_of_string (Lexing.lexeme lexbuf)) }
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
  | '\"'
      { Buffer.clear string_buf; string lexbuf }
  | ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9''_']*
      { IDENT (Lexing.lexeme lexbuf) }

and comment level = parse
  | "*/"
    { if level = 0 then token lexbuf else comment (level - 1) lexbuf }
  | "/*"          { comment (level + 1) lexbuf }
  | '\n'          { Lexing.new_line lexbuf; comment level lexbuf }
  | eof           { EOF }
  | _             { comment level lexbuf }

and string = parse
  | '\"'
      { STRING (Buffer.contents string_buf) }
  | '\n'
      { Lexing.new_line lexbuf; Buffer.add_char string_buf '\n';
        string lexbuf }
  | "\\n"         { Buffer.add_char string_buf '\n'; string lexbuf }
  | "\\t"         { Buffer.add_char string_buf '\t'; string lexbuf }
  | "\\\""        { Buffer.add_char string_buf '\"'; string lexbuf }
  | "\\" ((['0'-'9']['0'-'9']['0'-'9']) as lxm)
                  { Buffer.add_char string_buf (char_of_int (int_of_string lxm));
                    string lexbuf }
  | "\\\\"          { Buffer.add_char string_buf '\\'; string lexbuf }
  | '\\' [' ' '\t' '\r']* '\n' { Lexing.new_line lexbuf; skip_whitespace lexbuf }
  | eof           { EOF }
  | _ as c           { Buffer.add_char string_buf c; string lexbuf }

and skip_whitespace = parse
  | '\n'          { Lexing.new_line lexbuf; skip_whitespace lexbuf }
  | [' ' '\t' '\r']  { skip_whitespace lexbuf }
  | eof           { EOF }
  | _ as c { Buffer.add_char string_buf c; string lexbuf }
