(* Copyright 2014 Nicolas Ojeda Bar <n.oje.bar@gmail.com>.
   All rights reserved.
   Distributed under the Q Public License, version 1.0. *)

type loc = Location.t

type id =
  loc * string

type var =
    PNameVar of loc * id
  | PFieldVar of loc * var * id
  | PIndexVar of loc * var * exp

and exp =
    PBinExp of loc * exp * string * exp
  | PUnaryExp of loc * string * exp
  | PIntExp of loc * int
  | PStringExp of loc * string
  | PAssignExp of loc * var * exp
  | PVarExp of loc * var
  | PCallExp of loc * id * exp list
  | PUnitExp of loc
  | PSeqExp of loc * exp * exp
  | PLetExp of loc * dec * exp
  | PIfExp of loc * exp * exp * exp option
  | PNilExp of loc
  | PBreakExp of loc
  | PWhileExp of loc * exp * exp
  | PForExp of loc * id * exp * exp * exp
  | PArrayExp of loc * id * exp * exp
  | PRecordExp of loc * id * (id * exp) list

and dec =
    PVarDec of loc * (id * id option * exp)
  | PTypeDec of loc * typ list
  | PFunctionDec of loc * (id * (id * id) list * id option * exp) list

and typ =
    PRecordTyp of loc * id * (id * id) list
  | PArrayTyp of loc * id * id
  | PNameTyp of loc * id * id

let typ_name =
  function
    PRecordTyp (_, id, _)
  | PArrayTyp (_, id, _)
  | PNameTyp (_, id, _) -> id

let loc_exp =
  function
    PBinExp (loc, _, _, _)
  | PUnaryExp (loc, _, _)
  | PIntExp (loc, _)
  | PStringExp (loc, _)
  | PAssignExp (loc, _, _)
  | PVarExp (loc, _)
  | PCallExp (loc, _, _)
  | PUnitExp loc
  | PSeqExp (loc, _, _)
  | PLetExp (loc, _, _)
  | PIfExp (loc, _, _, _)
  | PNilExp loc
  | PBreakExp loc
  | PWhileExp (loc, _, _)
  | PForExp (loc, _, _, _, _)
  | PArrayExp (loc, _, _, _)
  | PRecordExp (loc, _, _) -> loc

let loc_var =
  function
    PNameVar (loc, _)
  | PFieldVar (loc, _, _)
  | PIndexVar (loc, _, _) -> loc

let loc_dec =
  function
    PVarDec (loc, _)
  | PTypeDec (loc, _)
  | PFunctionDec (loc, _) -> loc
