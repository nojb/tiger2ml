/* Copyright (c) 2014, Nicolas Ojeda Bar <n.oje.bar@gmail.com>

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
   REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
   AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
   INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
   LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
   OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
   PERFORMANCE OF THIS SOFTWARE. */

%{
open TigerSyntax

let loc_nth i =
  { Location.loc_start = Parsing.rhs_start_pos i;
    loc_end = Parsing.rhs_end_pos i;
    loc_ghost = false }

let loc () =
  { Location.loc_start = Parsing.symbol_start_pos ();
    loc_end = Parsing.symbol_end_pos ();
    loc_ghost = false }

let merge_loc loc1 loc2 =
  { Location.loc_start = loc1.Location.loc_start;
    loc_end = loc2.Location.loc_end;
    loc_ghost = false }
%}

%token <int> INT
%token <string> STRING
%token <string> IDENT
%token PLUS
%token TIMES
%token EQ
%token NEQ
%token LE
%token LT
%token GT
%token GE
%token LP
%token RP
%token COLONEQ
%token DOT
%token LB
%token RB
%token EOF
%token SEMI
%token COMMA
%token LET
%token IF
%token THEN
%token IN
%token END
%token ELSE
%token VAR
%token TYPE
%token FUNCTION
%token ARRAY
%token OF
%token LC
%token RC
%token COLON
%token NIL
%token BREAK
%token WHILE
%token DO
%token FOR
%token TO
%token MINUS
%token AND
%token OR
%token DIV

%nonassoc COLONEQ
%nonassoc DO
%right OF
%nonassoc THEN
%nonassoc ELSE
%left OR
%left AND
%nonassoc EQ NEQ LT LE GT GE
%left PLUS MINUS
%left TIMES DIV
%left UMINUS

%start program

%type <TigerSyntax.exp> program

%%

program
  : exp EOF
      { $1 }
  ;

id
  : IDENT
      { (loc (), $1) }
  ;

exp_seq
  : exp
    { $1 }
  | exp SEMI exp_seq
    { PSeqExp (loc (), $1, $3) }
  ;

exp_list
  : exp
      { $1 :: [] }
  | exp COMMA exp_list
      { $1 :: $3 }
  ;

record_field_list
  : id EQ exp
      { ($1, $3) :: [] }
  | id EQ exp COMMA record_field_list
      { ($1, $3) :: $5 }
  ;

exp
  : exp PLUS exp
      { PBinExp (loc (), $1, "+", $3) }
  | exp TIMES exp
      { PBinExp (loc (), $1, "*", $3) }
  | exp DIV exp
      { PBinExp (loc (), $1, "/", $3) }
  | exp MINUS exp
      { PBinExp (loc (), $1, "-", $3) }
  | exp AND exp
      { PBinExp (loc (), $1, "&", $3) }
  | exp EQ exp
      { PBinExp (loc (), $1, "=", $3) }
  | exp OR exp
      { PBinExp (loc (), $1, "|", $3) }
  | exp NEQ exp
      { PBinExp (loc (), $1, "<>", $3) }
  | exp LT exp
      { PBinExp (loc (), $1, "<", $3) }
  | exp LE exp
      { PBinExp (loc (), $1, "<=", $3) }
  | exp GT exp
       { PBinExp (loc (), $1, ">", $3) }
  | exp GE exp
      { PBinExp (loc (), $1, ">=", $3) }
  | MINUS exp %prec UMINUS
      { PUnaryExp (loc (), "-", $2) }
  | INT
      { PIntExp (loc (), $1) }
  | STRING
      { PStringExp (loc (), $1) }
  | WHILE exp DO exp
      { PWhileExp (loc (), $2, $4) }
  | FOR id COLONEQ exp TO exp DO exp
      { PForExp (loc (), $2, $4, $6, $8) }
  | NIL
      { PNilExp (loc ()) }
  | BREAK
      { PBreakExp (loc ()) }
  | LP RP
      { PUnitExp (loc ()) }
  | LP exp_seq RP
      { $2 }
  | var
      { PVarExp (loc (), $1) }
  | var COLONEQ exp
      { PAssignExp (loc (), $1, $3) }
  | id LP RP
      { PCallExp (loc (), $1, []) }
  | id LP exp_list RP
      { PCallExp (loc (), $1, $3) }
  | var LB exp RB OF exp
      { match $1 with
          PNameVar (loc, id) -> PArrayExp (loc, id, $3, $6)
        | _ -> raise Parse_error }
  | id LC RC
      { PRecordExp (loc (), $1, []) }
  | id LC record_field_list RC
      { PRecordExp (loc (), $1, $3) }
  | LET decs IN exp_seq END
      { List.fold_right (fun d e -> PLetExp (loc_dec d, d, e)) $2 $4 }
  | IF exp THEN exp
      { PIfExp (loc (), $2, $4, None) }
  | IF exp THEN exp ELSE exp
      { PIfExp (loc (), $2, $4, Some $6) }
  | error
      { TigerError.error (loc ()) TigerError.BadParse }
  ;

decs
  : var_dec
      { PVarDec (loc (), $1) :: [] }
  | typ_dec
      { PTypeDec (loc (), [$1]) :: [] }
  | fun_dec
      { PFunctionDec (loc (), [$1]) :: [] }
  | var_dec decs
      { PVarDec (loc_nth 1, $1) :: $2 }
  | typ_dec decs
      { match $2 with
          PTypeDec (loc1, typs) :: decs ->
            PTypeDec (merge_loc (loc_nth 1) loc1, $1 :: typs) :: decs
        | _ as decs ->
            PTypeDec (loc_nth 1, [$1]) :: decs }
  | fun_dec decs
      { match $2 with
          PFunctionDec (loc1, funs) :: decs ->
            PFunctionDec (merge_loc (loc_nth 1) loc1, $1 :: funs) :: decs
        | _ as decs ->
            PFunctionDec (loc_nth 1, [$1]) :: decs }
  ;

var_dec
  : VAR id COLONEQ exp
      { ($2, None, $4) }
  | VAR id COLON id COLONEQ exp
      { ($2, Some $4, $6) }
  ;

typ_dec
  : TYPE id EQ typ
    { ($2, $4) }
  ;
  
type_field_list
  : id COLON id
      { ($1, $3) :: [] }
  | id COLON id COMMA type_field_list
      { ($1, $3) :: $5 }
  ;

typ
  : id
      { PNameTyp (loc (), $1) }
  | ARRAY OF typ
      { PArrayTyp (loc (), $3) }
  | LC RC
      { PRecordTyp (loc (), []) }
  | LC type_field_list RC
      { PRecordTyp (loc (), $2) }
  ;

var
  : id
      { PNameVar (loc (), $1) }
  | var DOT id
      { PFieldVar (loc (), $1, $3) }
  | var LB exp RB
      { PIndexVar (loc (), $1, $3) }
  ;

fun_dec
  : FUNCTION id LP RP EQ exp
      { ($2, [], None, $6) }
  | FUNCTION id LP type_field_list RP EQ exp
      { ($2, $4, None, $7) }
  | FUNCTION id LP RP COLON id EQ exp
      { ($2, [], Some $6, $8) }
  | FUNCTION id LP type_field_list RP COLON id EQ exp
      { ($2, $4, Some $7, $9) }
  ;
