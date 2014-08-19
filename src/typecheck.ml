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

open Syntax
open Error

let map_default f x = function
    Some a -> f a
  | None -> x

type mutable_flag =
    Immutable
  | Mutable of bool ref

type exp =
    TIntExp of int
  | TStringExp of string
  | TNilExp
  | TBreakExp
  | TUnitExp
  | TSeqExp of exp * exp
  | TIfExp of exp * exp * exp option
  | TWhileExp of exp * exp * bool
  | TForExp of string * exp * exp * exp * bool
  | TCallExp of string * exp list
  | TVarExp of var
  | TAssignExp of var * exp
  | TArrayExp of exp * exp
  | TLetExp of string * exp * mutable_flag * exp
  | TLetRecExp of (string * (string * mutable_flag) list * exp) list * exp
  | TRecordExp of (string * exp) list

and var =
    TNameVar of string * mutable_flag
  | TFieldVar of var * string
  | TIndexVar of var * exp

type typ =
    TArrayTyp of typ
  | TRecordTyp of (string * typ) list
  | TForwardTyp of typ option ref
  | TUnitTyp
  | TIntTyp
  | TStringTyp
  | TAnyTyp

module SMap = Map.Make (String)

type value =
    Var of typ * mutable_flag
  | Fun of typ list * typ

type loop_flag =
    InLoop of bool ref
  | NoLoop

type env = {
  values : value SMap.t;
  types : typ SMap.t;
  in_loop : loop_flag;
  record_types : (int * string list * typ) list ref
}

let enter_loop env =
  let has_break = ref false in
  { env with in_loop = InLoop has_break }, has_break

let new_frame env =
  { env with in_loop = NoLoop }

let empty_env () ={
  values = SMap.empty;
  types = SMap.empty;
  in_loop = NoLoop;
  record_types = ref []
}

let find_type env (loc, name) =
  try
    SMap.find name env.types
  with
    Not_found ->
      error loc TypeNotFound
;;

let find_type_opt env (_, name) =
  try
    Some (SMap.find name env.types)
  with
    Not_found -> None
;;

let rec actual_type =
  function
    TForwardTyp r ->
      begin
        match !r with
          None ->
            invalid_arg "actual_type"
        | Some t ->
            actual_type t
      end
  | _ as x ->
      x
;;

let find_var env (loc, name) =
  try
    match SMap.find name env.values with
      Var (t, mut) ->
        (t, mut)
    | _ ->
        raise Not_found
  with
    Not_found ->
      error loc VariableNotFound
;;

let rec find_record_typ env (loc, name) =
  try
    match actual_type (SMap.find name env.types) with
      TRecordTyp flds as t ->
        t, flds
    | _ ->
        error loc RecordTypeExpected
  with
    Not_found ->
      error loc TypeNotFound
;;

let rec find_array_typ env (loc, name) =
  try
    match actual_type (SMap.find name env.types) with
      TArrayTyp t1 as t ->
        t, t1
    | _ ->
        error loc ArrayTypeExpected
  with
    Not_found ->
      error loc TypeNotFound
;;

let find_fun env (loc, name) =
  try
    match SMap.find name env.values with
      Fun (argt, t) ->
        argt, t
    | _ ->
        raise Not_found
  with
    Not_found ->
      error loc FunctionNotFound
;;

let add_var env name t mut =
  { env with values = SMap.add name (Var (t, mut)) env.values }
;;

let add_fun env name args =
  let rec loop r =
    function
      [] -> assert false
    | [t] -> List.rev r, t
    | t :: ts -> loop (t :: r) ts
  in
  let argt, t = loop [] args in
  { env with values = SMap.add name (Fun (argt, t)) env.values }
;;

let add_typ env name t =
  { env with types = SMap.add name t env.types }
;;

let equal_types t1 t2 =
  match actual_type t1, actual_type t2 with
    (TArrayTyp _ | TRecordTyp _), (TArrayTyp _ | TRecordTyp _) ->
      t1 == t2
  | TAnyTyp, _
  | _, TAnyTyp ->
      true
  | _ ->
      t1 = t2 
;;

let is_record_type t =
  match actual_type t with
    TRecordTyp _ -> true
  | _ -> false
;;

let rec typ env =
  function
    PNameTyp (_, id) ->
      begin
        match find_type_opt env id with
          None ->
            TForwardTyp (ref None)
        | Some t ->
            t
      end
  | PArrayTyp (_, t) ->
      TArrayTyp (typ env t)
  | PRecordTyp (_, flds) ->
      TRecordTyp (List.map (fun ((_, name), typid) -> name, find_type env typid) flds)
;;

let rec var env =
  function
    PNameVar (_, (loc, name as id)) ->
      let t, mut = find_var env id in
      t, TNameVar (name, mut)
  | PIndexVar (_, v, index) ->
      begin
        let rt, rc = var env v in
        let index = int_exp env index in
        match rt with
          TArrayTyp t ->
            t, TIndexVar (rc, index)
        | _ ->
            error (loc_var v) ArrayExpected
      end
  | PFieldVar (_, v, (loc, name)) ->
      begin
        let rt, rc = var env v in
        match rt with
          TRecordTyp flds ->
            begin
              try
                let t = List.assoc name flds in
                t, TFieldVar (rc, name)
              with
                Not_found -> error loc FieldNotFound
            end
        | _ ->
            error (loc_var v) RecordExpected
      end

and int_exp env e =
  let t, e' = exp env e in
  if equal_types t TIntTyp then
    e'
  else
    error (loc_exp e) IntExpected

and bool_exp env e =
  int_exp env e (* FIXME *)

and unit_exp env e =
  let t, e' = exp env e in
  if equal_types t TUnitTyp then
    e'
  else
    error (loc_exp e) UnitExpected

and typ_exp env t e =
  let t', e' = exp env e in
  if equal_types t t' then
    e'
  else
    error (loc_exp e) TypeMismatch

and int_or_string_exp env e =
  let t, e' = exp env e in
  match actual_type t with
    TIntTyp
  | TStringTyp ->
      t, e'
  | _ ->
      error (loc_exp e) IntOrStringExpected

and nil_exp env t e =
  match e with
    PNilExp loc ->
      if is_record_type t then
        TNilExp
      else
        error loc TypeMismatch
  | _ as e ->
      let t1, e1 = exp env e in
      if equal_types t t1 then
        e1
      else
        error (loc_exp e) TypeMismatch

and exp env =
  function
    PIntExp (_, n) ->
      TIntTyp, TIntExp n
  | PStringExp (_, s) ->
      TStringTyp, TStringExp s
  | PUnitExp _ ->
      TUnitTyp, TUnitExp
  | PSeqExp (_, e1, e2) ->
      let _, e1 = exp env e1 in
      let t, e2 = exp env e2 in
      t, TSeqExp (e1, e2)
  | PNilExp p ->
      error p BadNil
  | PBreakExp loc ->
      begin
        match env.in_loop with
          InLoop has_break ->
            has_break := true;
            TAnyTyp, TBreakExp
        | NoLoop ->
            error loc BadBreak
      end
  | PBinExp (_, e1, ("+" | "-" | "*" | "/" as op), e2) ->
      let e1 = int_exp env e1 in
      let e2 = int_exp env e2 in
      TIntTyp, TCallExp (op, [e1; e2])
  | PBinExp (_, e1, "&", e2) ->
      let e1 = int_exp env e1 in
      let e2 = int_exp env e2 in
      TIntTyp, TCallExp ("&&", [e1; e2])
  | PBinExp (_, e1, "|", e2) ->
      let e1 = int_exp env e1 in
      let e2 = int_exp env e2 in
      TIntTyp, TCallExp ("||", [e1; e2])
  | PBinExp (_, e1, ("<" | "<=" | ">=" | ">" as op), e2) ->
      let t1, e1 = int_or_string_exp env e1 in
      let e2 = typ_exp env t1 e2 in
      TIntTyp, TCallExp (op, [e1; e2])
      (*   | "=" | "<>" -> *)
      (*       begin *)
      (*         match t with *)
      (*           TIntTyp *)
      (*         | TUnitTyp *)
      (*         | TStringTyp -> *)
      (*             TIntTyp, TBinExp (e1c, op, e2c) *)
      (*         | TRecordTyp _ *)
      (*         | TArrayTyp _ -> *)
      (*             TIntTyp, TBinExp (e1c, "==", e2c) *)
      (*         | _ -> *)
      (*             assert false *)
      (*       end *)
      (*   | _ -> *)
      (*       assert false *)
      (* end *)
  | PUnaryExp (_, "-", e) ->
      let e = int_exp env e in
      TIntTyp, TCallExp ("~-", [e])
  | PIfExp (_, e1, e2, None) ->
      let e1 = int_exp env e1 in
      let e2 = unit_exp env e2 in
      TUnitTyp, TIfExp (e1, e2, None)
  | PIfExp (_, e1, e2, Some e3) ->
      let e1 = bool_exp env e1 in
      let e2t, e2c = exp env e2 in
      let e3t, e3c = exp env e3 in
      if equal_types e2t e3t then
        e2t, TIfExp (e1, e2c, Some e3c)
      else
        error (loc_exp e3) TypeMismatch
  | PWhileExp (_, e1, e2) ->
      let e1 = bool_exp env e1 in
      let env, has_break = enter_loop env in
      let e2 = unit_exp env e2 in
      TUnitTyp, TWhileExp (e1, e2, !has_break)
  | PCallExp (_, (loc, name as id), el) ->
      begin
        let argt, t = find_fun env id in
        let args2 = List.map (exp env) el in
        try
          if List.for_all2 (fun t1 (t2, _) -> equal_types t1 t2) argt args2 then
            t, TCallExp (name, List.map (fun (_, x) -> x) args2)
          else
            error loc TypeMismatch
        with
          Invalid_argument _ ->
            error loc BadArgumentCount
      end
  | PAssignExp (_, v, e) ->
      let rt, rc = var env v in
      let et, ec = exp env e in
      if equal_types rt et then
        begin
          begin
            match rc with
              TNameVar (_, Immutable) ->
                error (loc_exp e) ImmutableAssignment
            | TNameVar (_, Mutable m) ->
                m := true
            | _ -> ()
          end;
          TUnitTyp, TAssignExp (rc, ec)
        end
      else
        error (loc_exp e) TypeMismatch
  | PVarExp (_, v) ->
      let rt, rc = var env v in
      rt, TVarExp rc
  | PLetExp (_, d, e) ->
      dec env d e
  | PForExp (_, (_, index), start, finish, body) ->
      let start = int_exp env start in
      let finish = int_exp env finish in
      let env = add_var env index TIntTyp Immutable in
      let env, has_break = enter_loop env in
      let body = unit_exp env body in
      TUnitTyp, TForExp (index, start, finish, body, !has_break)
  | PArrayExp (_, typid, size, init) ->
      let size = int_exp env size in
      let initt, initc = exp env init in
      let t, typ = find_array_typ env typid in
      if equal_types typ initt then
        t, TArrayExp (size, initc)
      else
        error (loc_exp init) TypeMismatch
  | PRecordExp (_, typid, fields) ->
      let t, t1 = find_record_typ env typid in
      let flds =
        List.map2
          (fun (f1n, f1tn) ((_, f2n), f2init) ->
             if f1n = f2n then
               let f2initt, f2initc = exp env f2init in
               if equal_types f2initt f1tn then
                 f2n, f2initc
               else
                 error (loc_exp f2init) TypeMismatch
             else
               error (loc_exp f2init) BadFieldName) t1 fields
      in
      t, TRecordExp flds

and dec env d e =
  match d with
    PVarDec (_, ((_, name), None, init)) ->
      let initt, init1 = exp env init in
      let mf = Mutable (ref false) in
      let env = add_var env name initt mf in
      let t, e = exp env e in
      t, TLetExp (name, init1, mf, e)
  | PVarDec (_, ((_, name), Some t, init)) ->
      let vart = find_type env t in
      let init1 = nil_exp env vart init in
      let mf = Mutable (ref false) in
      let env = add_var env name vart mf in
      let t, e = exp env e in
      t, TLetExp (name, init1, mf, e)
  | PTypeDec (_, typs) ->
      assert false
      (* List.fold_left *)
      (*   (fun env (name, typ) -> *)
      (*      add_typ env name (typecheck_typ env typ)) env tl, td *)
  | PFunctionDec (_, funs) ->
      let env1 =
        List.fold_left
          (fun env ((_, name), args, ret_typid, _) ->
             let rett = map_default (find_type env) TUnitTyp ret_typid in
             add_fun env name ((List.map (fun (_, y) -> find_type env y) args) @ [rett]))
          env funs
      in
      let t, e = exp env1 e in
      let funs =
        List.map
          (fun ((_, name), args, ret_typid, body) ->
             let env1 = new_frame env1 in
             let env2, muts =
               List.fold_left
                 (fun (env, muts) ((_, arg_name), arg_typid) ->
                    let mut = Mutable (ref false) in
                    add_var env arg_name (find_type env arg_typid) mut, mut :: muts)
                 (env1, []) args
             in
             let bodyt, bodyc = exp env2 body in
             let rett = map_default (find_type env) TUnitTyp ret_typid in
             if equal_types bodyt rett then
               (name, List.map2 (fun ((_, x), _) mut -> (x, mut)) args (List.rev muts), bodyc)
             else
               error (loc_exp body) TypeMismatch)
          funs
      in
      t, TLetRecExp (funs, e)
         
let string = TStringTyp
let int = TIntTyp
let unit = TUnitTyp
  
let (@->) t1 t2 = t1 :: t2
let ret t = t :: []

let primitives = [
  "print", (string @-> ret unit);
  "printi", (int @-> ret unit);
  "flush", (unit @-> ret unit);
  "getchar", (unit @-> ret string);
  "ord", (string @-> ret int);
  "chr", (int @-> ret string);
  "size", (string @-> ret int);
  "substring", (string @-> int @-> int @-> ret string);
  "concat", (string @-> string @-> ret string);
  "not", (int @-> ret int);
  "exit", (int @-> ret unit)
]

let std_types = [
  "int", int;
  "string", string
]

let std_env =
  let env = empty_env () in
  let env = List.fold_left (fun env (name, args) -> add_fun env name args) env primitives in
  let env = List.fold_left (fun env (name, t) -> add_typ env name t) env std_types in
  env
