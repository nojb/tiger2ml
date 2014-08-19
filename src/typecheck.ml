open Syntax
open Error

let map_default f x = function
    Some a -> f a
  | None -> x

type var_kind =
    Assigned
  | Immutable
  | NonAssigned

type exp =
    TIntExp of int
  | TStringExp of string
  | TBinExp of exp * binop * exp
  | TNilExp
  | TBreakExp
  | TUnitExp
  | TSeqExp of exp * exp
  | TIfExp of exp * exp * exp option
  | TWhileExp of exp * exp * bool ref
  | TForExp of string * exp * exp * exp * bool ref
  | TCallExp of string * exp list
  | TVarExp of var
  | TAssignExp of var * exp
  | TArrayExp of exp * exp
  | TLetExp of string * exp * var_kind ref * exp
  | TLetRecExp of (string * (string * var_kind ref) list * exp) list * exp
  | TRecordExp of (string * exp) list

and var =
    TNameVar of string * var_kind ref
  | TFieldVar of var * string
  | TIndexVar of var * exp

type typ =
    TAliasTyp of string
  | TArrayTyp of int * typ (* int is type_id *)
  | TRecordTyp of int * (string * typ) list (* same *)
  | TNilTyp
  | TUnitTyp
  | TIntTyp
  | TStringTyp

let type_id_count = ref 0

let new_type_id () =
  incr type_id_count;
  !type_id_count

module SMap = Map.Make (String)

type var_fun_data =
  | Var of typ * var_kind ref
  | Fun of typ list * typ

type env = {
  vars_funs : var_fun_data SMap.t;
  types : typ SMap.t;
}

let empty_env = {
  vars_funs = SMap.empty;
  types = SMap.empty;
}

let find_typ env (loc, name) =
  try
    SMap.find name env.types
  with
    Not_found -> error loc "type not found"

let rec resolve_typ env loc = function
  | TAliasTyp s -> resolve_typ env loc (find_typ env (loc, s))
  | _ as x -> x

let find_var env (loc, name) =
  try
    match SMap.find name env.vars_funs with
      Var (t, mut) ->
        (t, mut)
    | _ ->
        error loc "should be variable, is function"
  with
    Not_found -> error loc "var not found"

let rec find_record_typ env (loc, name) =
  try
    match SMap.find name env.types with
      TRecordTyp (tid, fields) ->
        tid, fields
    | TAliasTyp s2 ->
        find_record_typ env (loc, s2)
    | _ ->
        error loc "var not of record type"
  with
    Not_found -> error loc "type not found"

let rec find_array_typ env (loc, name) =
  try
    match SMap.find name env.types with
      TArrayTyp (tid, t) ->
        tid, t
    | TAliasTyp s2 ->
        find_array_typ env (loc, s2)
    | _ ->
        error loc "var not of array type"
  with
    Not_found -> error loc "type not found"

let find_fun env (loc, name) =
  try
    match SMap.find name env.vars_funs with
      Fun (argt, t) ->
        argt, t
    | _ -> error loc "should be function, is variable"
  with
    Not_found -> error loc "fun not found"

let add_var env name t mut =
  { env with vars_funs = SMap.add name (Var (t, mut)) env.vars_funs }

let add_fun env name args =
  let rec loop r =
    function
      [] -> assert false
    | [t] -> List.rev r, t
    | t :: ts -> loop (t :: r) ts
  in
  let argt, t = loop [] args in
  { env with vars_funs = SMap.add name (Fun (argt, t)) env.vars_funs }

let add_typ env name t =
  { env with types = SMap.add name t env.types }

(* check that in fact this can never fail to find the involved types --
 * theoretically this has been checked when the declarations where processed *)
let rec eq_typ env t1 t2 =
  match t1, t2 with
  | TAliasTyp s, t2 ->
      eq_typ env (find_typ env (Location.none, s)) t2
  | _ as t1, TAliasTyp s ->
      eq_typ env t1 (find_typ env (Location.none, s))
  | TArrayTyp (x,_), TArrayTyp (y,_)
  | TRecordTyp (x,_), TRecordTyp (y,_) ->
      x = y
  | TRecordTyp _, TNilTyp
  | TNilTyp, TRecordTyp _ ->
      true
  | _ ->
      t1 = t2 

let rec typecheck_typ env = function
  | PNameTyp (_, (_, name)) ->
      TAliasTyp name
  | PArrayTyp (_, t) ->
      TArrayTyp (new_type_id (), typecheck_typ env t)
  | PRecordTyp (_, fields) ->
      TRecordTyp (new_type_id (), List.map (fun ((_, name), typid) -> name, find_typ env typid) fields)

let error_break loc =
  error loc "break outside for/while loop"

(* val typecheck :: env -> exp pos -> typ * texp *)

let rec var env breaks =
  function
    PNameVar (_, (loc, name as id)) ->
      let t, mut = find_var env id in
      resolve_typ env loc t, TNameVar (name, mut)
  | PIndexVar (_, v, index) ->
      begin
        let rt, rc = var env breaks v in
        let indext, indexc = exp env breaks index in
        match rt with
          TArrayTyp (_,t) ->
            if eq_typ env indext TIntTyp then
              resolve_typ env (loc_var v) t, TIndexVar (rc, indexc)
            else
              error (loc_exp index) "index expression is not integer valued"
        | _ ->
            error (loc_var v) "variable is not of array type"
      end
  | PFieldVar (_, v, (loc, name)) ->
      begin
        let rt, rc = var env breaks v in
        match rt with
          TRecordTyp (_,fields) ->
            begin
              try
                let t = List.assoc name fields in
                resolve_typ env loc t, TFieldVar (rc, name)
              with
                Not_found -> error loc "non-existent field"
            end
        | _ ->
            error (loc_var v) "variable is not of record type"
      end

and exp env breaks =
  function
    PIntExp (_, n) ->
      TIntTyp, TIntExp n
  | PStringExp (_, s) ->
      TStringTyp, TStringExp s
  | PUnitExp _ ->
      TUnitTyp, TUnitExp
  | PSeqExp (_, e1, e2) ->
      let _, e1 = exp env breaks e1 in
      let t, e2 = exp env breaks e2 in
      t, TSeqExp (e1, e2)
  | PNilExp p ->
      error p "illegal nil"
  | PBreakExp p ->
      breaks p;
      TUnitTyp, TBreakExp
  | PBinExp (_, e1, op, e2) ->
      typecheck_bin env breaks e1 op e2
  | PUnaryExp (_, op, e) ->
      typecheck_unary env breaks op e
  | PIfExp (_, e1, e2, e3) ->
      let e1t, e1c = exp env breaks e1 in
      if eq_typ env e1t TIntTyp then
        let (e2t,e2c) = exp env breaks e2 in
        match e3 with
          None ->
            if eq_typ env e2t TUnitTyp then
              TUnitTyp, TIfExp (e1c, e2c, None)
            else
              error (loc_exp e2) "then-part of if-then should be void valued"
        | Some e3 ->
            let e3t, e3c = exp env breaks e3 in
            if eq_typ env e2t e3t then
              e2t, TIfExp (e1c, e2c, Some e3c)
            else
              error (loc_exp e3) "then-part and else-part should have the same type"
      else
        error (loc_exp e1) "condition in if expression should be integer valued"
  | PWhileExp (_, e1, e2) ->
      let e1t, e1c = exp env breaks e1 in
      if eq_typ env e1t TIntTyp then
        let does_break = ref false in
        let breaks _ = does_break := true in
        let e2t, e2c = exp env breaks e2 in
        if eq_typ env e2t TUnitTyp then
          TUnitTyp, TWhileExp (e1c, e2c, does_break)
        else
          error (loc_exp e2) "body of while loop should return no value"
      else
        error (loc_exp e1) "condition of while loop should be integer valued"
  | PCallExp (_, (loc, name as id), el) ->
      begin
        let argt, t = find_fun env id in
        let args2 = List.map (exp env breaks) el in
        try
          if List.for_all2 (fun t1 (t2, _) -> eq_typ env t1 t2) argt args2 then
            resolve_typ env loc t, TCallExp (name, List.map (fun (_, x) -> x) args2)
          else
            error loc "type mismatch in parameter list"
        with
          Invalid_argument _ -> error loc "incorrect number of parameters"
      end
  | PAssignExp (_, v, e) ->
      let (rt, rc) = var env breaks v in
      let (et, ec) = exp env breaks e in
      if eq_typ env rt et then
        begin
          begin
            match rc with
              TNameVar (_, {contents = Immutable}) ->
                error (loc_exp e) "immutable variable"
            | TNameVar (_, mut) ->
                mut := Assigned
            | _ -> ()
          end;
          TUnitTyp, TAssignExp (rc, ec)
        end
      else
        error (loc_exp e) "type mismatch in assignment"
  | PVarExp (_, v) ->
      let rt, rc = var env breaks v in
      rt, TVarExp rc
  | PLetExp (_, d, e) ->
      dec env breaks d e
  | PForExp (_, (_, index), start, finish, body) ->
      let startt, startc = exp env breaks start in
      let finisht, finishc = exp env breaks finish in
      let does_break = ref false in
      let breaks _ = does_break := true in
      let bodyt, bodyc = exp (add_var env index TIntTyp (ref Immutable)) breaks body in
      if eq_typ env startt TIntTyp then
        if eq_typ env finisht TIntTyp then
          if eq_typ env bodyt TUnitTyp then
            TUnitTyp, TForExp (index, startc, finishc, bodyc, does_break)
          else
            error (loc_exp body) "body of for loop should return void"
        else
          error (loc_exp finish) "end expression in for loop should be integer valued"
      else
        error (loc_exp start) "start expression in for loop should be integer valued"
  | PArrayExp (_, typid, size, init) ->
      let sizet, sizec = exp env breaks size in
      let initt, initc = exp env breaks init in
      let tid, typ = find_array_typ env typid in
      if eq_typ env typ initt then
        if eq_typ env sizet TIntTyp then
          TArrayTyp (tid, typ), TArrayExp (sizec, initc)
        else
          error (loc_exp size) "size expression not of integer type"
      else
        error (loc_exp init) "init expression does not match array def"
  | PRecordExp (_, typid, fields) ->
      let tid, t = find_record_typ env typid in
      let flds =
        List.map2
          (fun (f1n, f1tn) ((_, f2n), f2init) ->
             if f1n = f2n then
               let f2initt, f2initc = exp env breaks f2init in
               if eq_typ env f2initt f1tn then
                 f2n, f2initc
               else
                 error (loc_exp f2init) "field type mismatch"
             else
               error (loc_exp f2init) "field name mismatch") t fields
      in
      TRecordTyp (tid, t), TRecordExp flds

and typecheck_unary env breaks Neg e =
  let et, ec = exp env breaks e in
  if eq_typ env et TIntTyp then
    TIntTyp, assert false (* Texp_bin_int (Texp_int 0, Minus, ec) *)
  else
    error (loc_exp e) "unary negation requires integer valued exp"

and dec env breaks d e =
  match d with
    PVarDec (_, ((_, name), typid, init)) ->
      let initt, init1 = exp env breaks init in
      let vart =
        match typid with
          None ->
            if eq_typ env initt TNilTyp then
              error (loc_exp init) "can't determine variable type"
            else
              initt
        | Some t ->
            find_typ env t
      in
      if eq_typ env vart initt then 
        let mut = ref NonAssigned in
        let env = add_var env name vart mut in
        let t, e = exp env breaks e in
        t, TLetExp (name, init1, mut, e)
      else
        error (loc_exp init) "type mismatch in variable declaration"
  | PTypeDec (_, typs) ->
      assert false
      (* List.fold_left *)
      (*   (fun env (name, typ) -> *)
      (*      add_typ env name (typecheck_typ env typ)) env tl, td *)
  | PFunctionDec (_, funs) ->
      assert false
      (* let env1 = List.fold_left (fun env (name, args, ret_typid, _) -> *)
      (*     let rett = map_default (find_typ env) TUnitTyp ret_typid in *)
      (*     add_fun env name *)
      (*       (List.map (fun (_, y) -> find_typ env y) args) rett) *)
      (*     env fl *)
      (* in (env1, TFunctionDec (List.map *)
      (*                           (fun (name, args, ret_typid, body) -> *)
      (*                              let (env2, muts) = List.fold_left (fun (env, muts) (arg_name, arg_typid) -> *)
      (*                                  let mut = ref NonAssigned in *)
      (*                                  (add_var env arg_name (find_typ env arg_typid) mut, mut :: muts)) *)
      (*                                  (env1, []) args in *)
      (*                              let (bodyt, bodyc) = typecheck env2 error_break body in *)
      (*                              let rett = map_default (find_typ env) Ttyp_void ret_typid in *)
      (*                              begin *)
      (*                                if eq_typ env bodyt rett then *)
      (*                                  (name, List.map2 (fun (x, _) mut -> (x, mut)) args (List.rev muts), bodyc) *)
      (*                                else *)
      (*                                  error (posn body) "body of function does not match return type" *)
      (*                              end) fl) :: td) *)
         
and typecheck_bin env breaks e1 op e2 =
  assert false
  (* let e1t, e1c = exp env breaks e1 in *)
  (* let e2t, e2c = exp env breaks e2 in *)
  (* match op with *)
  (*   Add | Minus | Times | Div -> *)
  (*     if eq_typ env e1t Ttyp_int then *)
  (*       if eq_typ env e2t Ttyp_int then *)
  (*         (Ttyp_int, Texp_bin_math (e1c, op, e2c)) *)
  (*       else *)
  (*         error (posn e2) "integer exp required" *)
  (*     else *)
  (*       error (posn e1) "integer exp required" *)
  (* | And | Or -> *)
  (*     if eq_typ env e1t Ttyp_int then *)
  (*       if eq_typ env e2t Ttyp_int then *)
  (*         (Ttyp_int, Texp_bin_int (e1c, op, e2c)) *)
  (*       else *)
  (*         error (posn e2) "integer exp required" *)
  (*     else *)
  (*       error (posn e1) "integer exp required" *)
  (* | Lt | Le | Gt | Ge -> *)
  (*     if eq_typ env e1t Ttyp_int then *)
  (*       if eq_typ env e2t Ttyp_int then *)
  (*         (Ttyp_int, Texp_bin_cmp (e1c, op, e2c)) *)
  (*       else *)
  (*         error (posn e2) "integer exp required" *)
  (*     else if eq_typ env e1t Ttyp_string then *)
  (*       if eq_typ env e2t Ttyp_string then *)
  (*         (Ttyp_int, Texp_bin_cmp (e1c, op, e2c)) *)
  (*       else *)
  (*         error (posn e2) "string exp required" *)
  (*     else *)
  (*       error (posn e1) "integer or string exp required" *)
  (* | Eq | Neq -> *)
  (*     if eq_typ env e1t Ttyp_int then *)
  (*       (Ttyp_int, Texp_bin_cmp (e1c, op, e2c)) *)
  (*     else if eq_typ env e1t Ttyp_string then *)
  (*       (Ttyp_int, Texp_bin_cmp (e1c, op, e2c)) *)
  (*     else if eq_typ env e1t e2t then *)
  (*       (Ttyp_int, Texp_bin_gen (e1c, op, e2c)) *)
  (*     else *)
  (*       error (posn e2) "comparing expressions of different types" *)

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
  let env = List.fold_left (fun env (name, args) -> add_fun env name args) empty_env primitives in
  let env = List.fold_left (fun env (name, t) -> add_typ env name t) env std_types in
  env
