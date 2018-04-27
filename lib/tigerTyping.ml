(* Copyright 2014 Nicolas Ojeda Bar <n.oje.bar@gmail.com>.
   All rights reserved.
   Distributed under the Q Public License, version 1.0. *)

open TigerSyntax
open TigerError

type mutable_flag =
    Immutable
  | Mutable of bool ref

type primitive_flag =
    User
  | Primitive

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
  | TFieldVar of var * string * int
  | TIndexVar of var * exp * int

type typ =
    TArray of typ
  | TRecord of string * (string * typ) list
  | TForward of typ option ref
  | TUnit
  | TInt
  | TString
  | TAny
  | TBool
  | TNil

let next_sym =
  let count = ref (-1) in
  fun s ->
    incr count;
    Printf.sprintf "%s_%i" s !count

module SMap = Map.Make (String)

type value =
    Var of (string * typ * mutable_flag)
  | Fun of (string * typ list * typ * primitive_flag)

type loop_flag =
    InLoop of bool ref
  | NoLoop

type env = {
  values : value SMap.t;
  types : typ SMap.t;
  in_loop : loop_flag;
  record_types : (string * (string * typ) list) list list ref
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

let save_record_types env =
  function
    [] ->
      ()
  | _ as typs ->
      env.record_types := typs :: !(env.record_types)

let find_type env (loc, name) =
  try
    SMap.find name env.types
  with
    Not_found ->
      error loc (TypeNotFound name)

let find_type_opt env (_, name) =
  try
    Some (SMap.find name env.types)
  with
    Not_found -> None

let rec actual_type =
  function
    TForward r ->
      begin
        match !r with
          None ->
            assert false
        | Some t ->
            actual_type t
      end
  | _ as x ->
      x

let find_var env (loc, name) =
  try
    match SMap.find name env.values with
      Var v ->
        v
    | _ ->
        raise Not_found
  with
    Not_found ->
      error loc (VariableNotFound name)

let find_record_type env (loc, name) =
  try
    match actual_type (SMap.find name env.types) with
      TRecord (_, flds) as t ->
        t, flds
    | _ ->
        error loc RecordTypeExpected
  with
    Not_found ->
      error loc (TypeNotFound name)

let find_array_type env (loc, name) =
  try
    match actual_type (SMap.find name env.types) with
      TArray t1 as t ->
        t, t1
    | _ ->
        error loc ArrayTypeExpected
  with
    Not_found ->
      error loc (TypeNotFound name)

let find_fun env (loc, name) =
  try
    match SMap.find name env.values with
      Fun f ->
        f
    | _ ->
        raise Not_found
  with
    Not_found ->
      error loc (FunctionNotFound name)

let add_var env name t mut =
  let name' = next_sym name in
  name', { env with values = SMap.add name (Var (name', t, mut)) env.values }

let add_fun env name args rty =
  let name' = next_sym name in
  name', { env with values = SMap.add name (Fun (name', args, rty, User)) env.values }

let add_prim env name args rty =
  let name' = "TigerLib." ^ name in
  { env with values = SMap.add name (Fun (name', args, rty, Primitive)) env.values }

let add_type env (_, name) t =
  { env with types = SMap.add name t env.types }

let is_record_type t =
  match actual_type t with
    TRecord _ -> true
  | _ -> false

let rec var env =
  function
    PNameVar (_, (_, _ as id)) ->
      let name, t, mut = find_var env id in
      t, TNameVar (name, mut)
  | PIndexVar (_, v, index) ->
      begin
        let rt, rc = var env v in
        let index' = int_exp env index in
        match actual_type rt with
          TArray t ->
            let line = (loc_exp index).Location.loc_start.Lexing.pos_lnum in
            t, TIndexVar (rc, index', line)
        | _ ->
            error (loc_var v) ArrayExpected
      end
  | PFieldVar (_, v, (loc, name)) ->
      begin
        let rt, rc = var env v in
        match actual_type rt with
          TRecord (_, flds) ->
            begin
              try
                let t = List.assoc name flds in
                let line = loc.Location.loc_start.Lexing.pos_lnum in
                t, TFieldVar (rc, name, line)
              with
                Not_found -> error loc (FieldNotFound name)
            end
        | _ ->
            error (loc_var v) RecordExpected
      end

and int_exp env e =
  let t, e' = exp env e in
  match actual_type t with
    TAny
  | TInt ->
      e'
  | TBool ->
      TIfExp (e', TIntExp 1, Some (TIntExp 0))
  | _ ->
    error (loc_exp e) IntExpected

and bool_exp env e =
  let t, e' = exp env e in
  match actual_type t with
    TAny
  | TBool ->
      e'
  | TInt ->
      TCallExp ("<>", [e'; TIntExp 0])
  | _ ->
      error (loc_exp e) IntExpected

and unit_exp env e =
  let t, e' = exp env e in
  match actual_type t with
    TAny
  | TUnit ->
      e'
  | _ ->
      error (loc_exp e) UnitExpected

and typ_exp env t e =
  let t', e' = exp env e in
  match actual_type t, actual_type t' with
    TBool, TInt ->
      TCallExp ("<>", [e'; TIntExp 0])
  | TInt, TBool ->
      TIfExp (e', TIntExp 1, Some (TIntExp 0))
  | TBool, TBool | TInt, TInt | TUnit, TUnit | TString, TString
  | TNil, TRecord _ | TRecord _, TNil
  | TAny, _ | _, TAny ->
      e'
  | (TRecord _ | TArray _ as t1), (TRecord _ | TArray _ as t2) when t1 == t2 ->
      e'
  | _ ->
      error (loc_exp e) TypeMismatch

and int_or_string_exp env e =
  let t, e' = exp env e in
  match actual_type t with
    TAny
  | TInt
  | TBool
  | TString ->
      t, e'
  | _ ->
      error (loc_exp e) IntOrStringExpected

and exp env =
  function
    PIntExp (_, n) ->
      TInt, TIntExp n
  | PStringExp (_, s) ->
      TString, TStringExp s
  | PUnitExp _ ->
      TUnit, TUnitExp
  | PSeqExp (_, e1, e2) ->
      let _, e1 = exp env e1 in
      let t, e2 = exp env e2 in
      t, TSeqExp (e1, e2)
  | PNilExp _ ->
      TNil, TNilExp
  | PBreakExp loc ->
      begin
        match env.in_loop with
          InLoop has_break ->
            has_break := true;
            TAny, TBreakExp
        | NoLoop ->
            error loc BadBreak
      end
  | PBinExp (_, e1, ("+" | "-" | "*" | "/" as op), e2) ->
      let e1 = int_exp env e1 in
      let e2 = int_exp env e2 in
      TInt, TCallExp (op, [e1; e2])
  | PBinExp (_, e1, ("&" | "|" as op), e2) ->
      let e1 = bool_exp env e1 in
      let e2 = bool_exp env e2 in
      let op = match op with "&" -> "&&" | "|" -> "||" | _ -> assert false in
      TBool, TCallExp (op, [e1; e2])
  | PBinExp (_, e1, ("<" | "<=" | ">=" | ">" as op), e2) ->
      let t1, e1 = int_or_string_exp env e1 in
      let e2 = typ_exp env t1 e2 in
      TBool, TCallExp (op, [e1; e2])
  | PBinExp (_, e1, ("=" | "<>" as op), e2) ->
      let t1, e1 = exp env e1 in
      let e2 = typ_exp env t1 e2 in
      let op' = match op with "=" -> "==" | "<>" -> "!=" | _ -> assert false in
      begin
        match actual_type t1 with
          TString | TInt | TBool | TAny | TUnit ->
            TBool, TCallExp (op, [e1; e2])
        | TRecord _ | TArray _ ->
            TBool, TCallExp (op', [e1; e2])
        | _ ->
            assert false
      end
  | PBinExp _ ->
      assert false
  | PUnaryExp (_, "-", e) ->
      let e = int_exp env e in
      TInt, TCallExp ("~-", [e])
  | PUnaryExp _ ->
      assert false
  | PIfExp (_, e1, e2, None) ->
      let e1 = bool_exp env e1 in
      let e2 = unit_exp env e2 in
      TUnit, TIfExp (e1, e2, None)
  | PIfExp (_, e1, e2, Some e3) ->
      let e1 = bool_exp env e1 in
      let e2t, e2c = exp env e2 in
      let e3c = typ_exp env e2t e3 in
      e2t, TIfExp (e1, e2c, Some e3c)
  | PWhileExp (_, e1, e2) ->
      let e1 = bool_exp env e1 in
      let env, has_break = enter_loop env in
      let e2 = unit_exp env e2 in
      TUnit, TWhileExp (e1, e2, !has_break)
  | PCallExp (_, (loc, _ as id), el) ->
      let name, argt, t, _ = find_fun env id in
      if List.length argt <> List.length el then error loc BadArgumentCount;
      let args2 = List.map2 (typ_exp env) argt el in
      t, TCallExp (name, args2)
  | PAssignExp (_, v, e) ->
      let rt, rc = var env v in
      let ec = typ_exp env rt e in
      begin
        match rc with
          TNameVar (name, Immutable) ->
            error (loc_exp e) (ImmutableAssignment name)
        | TNameVar (_, Mutable m) ->
            m := true
        | _ -> ()
      end;
      TUnit, TAssignExp (rc, ec)
  | PVarExp (_, v) ->
      let rt, rc = var env v in
      rt, TVarExp rc
  | PLetExp (_, d, e) ->
      dec env d e
  | PForExp (_, (_, index), start, finish, body) ->
      let start = int_exp env start in
      let finish = int_exp env finish in
      let index, env = add_var env index TInt Immutable in
      let env, has_break = enter_loop env in
      let body = unit_exp env body in
      TUnit, TForExp (index, start, finish, body, !has_break)
  | PArrayExp (_, typid, size, init) ->
      let size = int_exp env size in
      let t, typ = find_array_type env typid in
      let initc = typ_exp env typ init in
      t, TArrayExp (size, initc)
  | PRecordExp (_, typid, fields) ->
      let t, t1 = find_record_type env typid in
      let flds =
        List.map2
          (fun (f1n, f1tn) ((_, f2n), f2init) ->
             if f1n = f2n then
               let f2initc = typ_exp env f1tn f2init in
               f2n, f2initc
             else
               error (loc_exp f2init) BadFieldName) t1 fields
      in
      t, TRecordExp flds

and dec env d e =
  match d with
    PVarDec (_, ((_, name), None, init)) ->
      let initt, init1 = exp env init in
      let mf = Mutable (ref false) in
      let name, env = add_var env name initt mf in
      let t, e = exp env e in
      t, TLetExp (name, init1, mf, e)
  | PVarDec (_, ((_, name), Some t, init)) ->
      let vart = find_type env t in
      let init1 = typ_exp env vart init in
      let mf = Mutable (ref false) in
      let name, env = add_var env name vart mf in
      let t, e = exp env e in
      t, TLetExp (name, init1, mf, e)
  | PTypeDec (_, typs) ->
      let rec scan env refs =
        function
          [] ->
            env, List.rev refs
        | t :: typs ->
            let r = ref None in
            let env = add_type env (typ_name t) (TForward r) in
            scan env (r :: refs) typs
      in
      let env, refs = scan env [] typs in
      let update r t = r := Some t; t in
      let saved_types = ref [] in
      let save name flds = saved_types := (name, flds) :: !saved_types in
      let mktype r =
        function
          PRecordTyp (_, (_, id), flds) ->
            let flds =
              List.map
                (fun ((_, fid), tid) -> fid, find_type env tid)
                flds
            in
            save id flds;
            update r (TRecord (id, flds))
        | PArrayTyp (_, _, tid) ->
            update r (TArray (find_type env tid))
        | PNameTyp (_, _, tid) ->
            update r (find_type env tid)
      in
      let typs = List.map2 mktype refs typs in
      save_record_types env (List.rev !saved_types);
      let rec check seenr visited t =
        if List.memq t visited then
          if seenr then () else error Location.none BadTypeCycle
        else
          match t with
            TUnit | TInt | TString | TAny | TBool | TNil ->
              ()
          | TRecord (_, flds) ->
              List.iter (fun (_, t1) -> check true (t :: visited) t1) flds
          | TArray t1 ->
              check seenr (t :: visited) t1
          | TForward r ->
              begin
                match !r with
                  None ->
                    assert false
                | Some t1 ->
                    check seenr visited t1
              end
      in
      List.iter (check false []) typs;
      exp env e
  | PFunctionDec (_, funs) ->
      let funs', env1 =
        List.fold_left
          (fun (names', env) ((_, name), args, ret_typid, _) ->
             let rett =
               match ret_typid with
                 None -> TUnit
               | Some t -> find_type env t
             in
             let name', env = add_fun env name (List.map (fun (_, y) -> find_type env y) args) rett in
             name' :: names', env)
          ([], env) funs
      in
      let funs' = List.rev funs' in
      let t, e = exp env1 e in
      let funs =
        List.map2
          (fun name' (_, args, ret_typid, body) ->
             let env1 = new_frame env1 in
             let env2, muts =
               List.fold_left
                 (fun (env, muts) ((_, arg_name), arg_typid) ->
                    let mut = Mutable (ref false) in
                    let arg_name, env = add_var env arg_name (find_type env arg_typid) mut in
                    env, (arg_name, mut) :: muts)
                 (env1, []) args
             in
             let rett =
               match ret_typid with
                 None -> TUnit
               | Some t -> find_type env t
             in
             let bodyc = typ_exp env2 rett body in
             name', List.map (fun (x, mut) -> (x, mut)) (List.rev muts), bodyc)
          funs' funs
      in
      t, TLetRecExp (funs, e)

let primitives =
  [
    "print"    , [ TString ], TUnit;
    "printi"   , [ TInt ], TUnit;
    "flush"    , [  ], TUnit;
    "getchar"  , [  ], TString;
    "ord"      , [ TString ], TInt;
    "chr"      , [ TInt ], TString;
    "size"     , [ TString ], TInt;
    "substring", [ TString; TInt; TInt ], TString;
    "concat"   , [ TString; TString ], TString;
    "not"      , [ TBool ], TBool;
    "exit"     , [ TInt ], TUnit
  ]

let std_types =
  [
    "int"   , TInt;
    "string", TString
  ]

let std_env () =
  let env = empty_env () in
  let env = List.fold_left (fun env (name, args, rty) -> add_prim env name args rty) env primitives in
  let env = List.fold_left (fun env (name, t) -> add_type env (Location.none, name) t) env std_types in
  env

let exp e =
  let env = std_env () in
  let _, e = exp env e in
  List.rev !(env.record_types), e
