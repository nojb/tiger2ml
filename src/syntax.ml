let currfilename = ref ""

type 'a pos = {
  posn : Lexing.position;
  desc : 'a;
}

let posn { posn = p; desc = _ } = p
let desc { posn = _; desc = d } = d

type _ref =
  | Aref_name of string pos
  | Aref_field of _ref pos * string pos
  | Aref_index of _ref pos * exp pos

and binop =
  | Add | Times | Eq | Minus | And | Or | Div
  | Neq | Lt | Le | Ge | Gt

and unaryop =
  Neg

and exp =
  | Aexp_bin of exp pos * binop * exp pos
  | Aexp_unary of unaryop * exp pos
  | Aexp_int of int
  | Aexp_str of string
  | Aexp_set of _ref pos * exp pos
  | Aexp_ref of _ref pos
  | Aexp_call of string pos * exp pos list
  | Aexp_seq of exp pos list
  | Aexp_let of dec pos list * exp pos list
  | Aexp_if of exp pos * exp pos * exp pos option
  | Aexp_nil
  | Aexp_break
  | Aexp_while of exp pos * exp pos
  | Aexp_for of string * exp pos * exp pos * exp pos
  | Aexp_array of string pos * exp pos * exp pos
  | Aexp_record of string pos * (string * exp pos) list

and dec =
  | Adec_var of string * string pos option * exp pos
  | Adec_typ of (string * typ) list
  | Adec_fun of (string * (string * string pos) list * string pos option * exp pos) list

and typ =
  | Atyp_record of (string * string pos) list
  | Atyp_array of typ
  | Atyp_alias of string
