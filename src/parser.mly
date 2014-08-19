%{
  open Syntax

  let mkexp p e = { posn = p; desc = e }
  let mkref p r = { posn = p; desc = r }
  let mkdec p d = { posn = p; desc = d }
  let mkid p s = { posn = p; desc = s }
%}

%token <int> INT
%token <string> STR
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

%type <Syntax.exp Syntax.pos> program
%type <Syntax._ref Syntax.pos> lval
%type <Syntax.exp Syntax.pos> exp

%%

program: exp EOF
    { $1 }
;

exp: exp PLUS exp
    { mkexp $startpos (Aexp_bin ($1, Add, $3)) }
  | exp TIMES exp
    { mkexp $startpos (Aexp_bin ($1, Times, $3)) }
  | exp DIV exp
    { mkexp $startpos (Aexp_bin ($1, Div, $3)) }
  | exp MINUS exp
    { mkexp $startpos (Aexp_bin ($1, Minus, $3)) }
  | exp AND exp
    { mkexp $startpos (Aexp_bin ($1, And, $3)) }
  | exp EQ exp
    { mkexp $startpos (Aexp_bin ($1, Eq, $3)) }
  | exp OR exp
    { mkexp $startpos (Aexp_bin ($1, Or, $3)) }
  | exp NEQ exp
    { mkexp $startpos (Aexp_bin ($1, Neq, $3)) }
  | exp LT exp
    { mkexp $startpos (Aexp_bin ($1, Lt, $3)) }
  | exp LE exp
    { mkexp $startpos (Aexp_bin ($1, Le, $3)) }
  | exp GT exp
    { mkexp $startpos (Aexp_bin ($1, Gt, $3)) }
  | exp GE exp
    { mkexp $startpos (Aexp_bin ($1, Ge, $3)) }
  | MINUS exp %prec UMINUS
    { mkexp $startpos (Aexp_unary (Neg, $2)) }
  | INT
    { mkexp $startpos (Aexp_int $1) }
  | STR
    { mkexp $startpos (Aexp_str $1) }
  | WHILE exp DO exp
    { mkexp $startpos (Aexp_while ($2, $4)) }
  | FOR IDENT COLONEQ exp TO exp DO exp
    { mkexp $startpos (Aexp_for ($2, $4, $6, $8)) }
  | NIL
    { mkexp $startpos Aexp_nil }
  | BREAK
    { mkexp $startpos Aexp_break }
  | LP x = separated_list(SEMI,exp) RP
    { mkexp $startpos (Aexp_seq x) }
  | lval
    { mkexp $startpos (Aexp_ref $1) }
  | lval COLONEQ exp
    { mkexp $startpos (Aexp_set ($1, $3)) }
  | y = lval LP x = separated_list(COMMA,exp) RP
    { match y with
      | { posn = p; desc = Aref_name s } ->
        mkexp p (Aexp_call (s, x))
      | _ -> $syntaxerror }
  | lval LB exp RB OF exp
    { match $1 with
      | { posn = p; desc = Aref_name s } -> mkexp p (Aexp_array (s, $3, $6))
      | _ -> $syntaxerror }
  | x = lval LC y = separated_list (COMMA, separated_pair(IDENT,EQ,exp)) RC
    { match x with
      | { posn = p; desc = Aref_name s } ->
        mkexp p (Aexp_record (s, y))
      | _ -> $syntaxerror }
  | LET x = list(dec) IN y = separated_list(SEMI,exp) END
    { mkexp $startpos (Aexp_let (x, y)) }
  | IF x = exp THEN y = exp z = ioption(preceded(ELSE,exp))
    { mkexp $startpos (Aexp_if (x, y, z)) }
  | error { Error.error $startpos "parser error" }
;

dec: VAR x = IDENT y = option(preceded(COLON,ident)) COLONEQ z = exp
    { mkdec $startpos (Adec_var (x, y, z)) }
  | x = nonempty_list(typdec)
    { mkdec $startpos (Adec_typ x) }
  | x = nonempty_list(fundec)
    { mkdec $startpos (Adec_fun x) }
;

%inline typdec: TYPE x = IDENT EQ y = typ
    { (x, y) }
;
  
%inline fundec:
    FUNCTION x = IDENT LP
      y = separated_list(COMMA,separated_pair(IDENT,COLON,ident)) RP
      z = option(preceded(COLON,ident)) EQ w = exp
    { (x, y, z, w) }
;

ident: IDENT
  { mkid $startpos $1 }
;

typ: IDENT
    { Atyp_alias $1 }
  | ARRAY OF typ
    { Atyp_array $3 }
  | LC x = separated_list(COMMA,separated_pair(IDENT,COLON,ident)) RC
    { Atyp_record x }
;

lval: ident
    { mkref $startpos (Aref_name $1) }
  | lval DOT ident
    { mkref $startpos (Aref_field ($1, $3)) }
  | lval LB exp RB
    { mkref $startpos (Aref_index ($1, $3)) }
;
