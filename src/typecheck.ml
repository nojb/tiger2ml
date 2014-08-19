open Syntax
open Error

let map_default f x = function
  | Some a -> f a
  | None -> x

type var_kind =
  | Assigned
  | Immutable
  | NonAssigned

type texp =
  | Texp_int of int
  | Texp_str of string
  | Texp_bin_int of texp * binop * texp
  | Texp_bin_cmp of texp * binop * texp
  | Texp_bin_math of texp * binop * texp
  | Texp_bin_gen of texp * binop * texp
  | Texp_nil
  | Texp_break
  | Texp_seq of texp list
  | Texp_if of texp * texp * texp
  | Texp_while of texp * texp * bool ref
  | Texp_for of string * texp * texp * texp * bool ref
  | Texp_call of string * texp list
  | Texp_ref of tref
  | Texp_set of tref * texp
  | Texp_array of texp * texp
  | Texp_let of tdec list * texp
  | Texp_record of (string * texp) list

and tdec =
  | Tdec_var of string * texp * var_kind ref
  | Tdec_fun of (string * (string * var_kind ref) list * texp) list

and tref =
  | Tref_name of string * var_kind ref
  | Tref_field of tref * string
  | Tref_index of tref * texp

type ttyp =
  | Ttyp_alias of string
  | Ttyp_array of int * ttyp (* int is type_id *)
  | Ttyp_record of int * (string * ttyp) list (* same *)
  | Ttyp_nil
  | Ttyp_void
  | Ttyp_int
  | Ttyp_string

let type_id_count = ref 0
let new_type_id () =
  incr type_id_count;
  !type_id_count

module StringMap = Map.Make(String)

type var_fun_data =
  | Var of ttyp * var_kind ref
  | Fun of ttyp list * ttyp

type env = {
  vars_funs : var_fun_data StringMap.t;
  types : ttyp StringMap.t;
}

let empty_env = {
  vars_funs = StringMap.empty;
  types = StringMap.empty;
}

let find_typ env s =
  try StringMap.find (desc s) env.types
  with Not_found -> error (posn s) "type not found"

let rec resolve_typ env p = function
  | Ttyp_alias s -> resolve_typ env p (find_typ env { posn = p; desc = s })
  | x -> x

let find_var env ( s : string pos ) =
  try match StringMap.find (desc s) env.vars_funs with
  | Var (t, mut) -> (t, mut)
  | _ -> error (posn s) "should be variable, is function"
  with Not_found -> error (posn s) "var not found"

let rec find_record_typ env s =
  try match StringMap.find (desc s) env.types with
  | Ttyp_record (tid,fields) -> (tid,fields)
  | Ttyp_alias s2 -> find_record_typ env { posn = (posn s); desc = s2 }
  | _ -> error (posn s) "var not of record type"
  with Not_found -> error (posn s) "type not found"

let rec find_array_typ env s =
  try match StringMap.find (desc s) env.types with
  | Ttyp_array (tid,t) -> (tid,t)
  | Ttyp_alias s2 -> find_array_typ env { posn = (posn s); desc = s2 }
  | _ -> error (posn s) "var not of array type"
  with Not_found -> error (posn s) "type not found"

let find_fun env s =
  try match StringMap.find (desc s) env.vars_funs with
  | Fun (argt,t) -> (argt,t)
  | _ -> error (posn s) "should be function, is variable"
  with Not_found -> error (posn s) "fun not found"

let add_var env name t mut =
  { env with vars_funs = StringMap.add name (Var (t, mut)) env.vars_funs }

let add_fun env name argt t =
  { env with vars_funs = StringMap.add name (Fun (argt, t)) env.vars_funs }

let add_typ env name t =
  { env with types = StringMap.add name t env.types }

(* check that in fact this can never fail to find the involved types --
 * theoretically this has been checked when the declarations where processed *)
let rec eq_typ env t1 t2 =
  match t1, t2 with
  | Ttyp_alias s, t2 ->
      eq_typ env (find_typ env { posn = Lexing.dummy_pos; desc = s }) t2
  | t1, Ttyp_alias s ->
      eq_typ env t1 (find_typ env { posn = Lexing.dummy_pos; desc = s })
  | Ttyp_array (x,_), Ttyp_array (y,_) -> x = y
  | Ttyp_record (x,_), Ttyp_record (y,_) -> x = y
  | Ttyp_record _, Ttyp_nil
  | Ttyp_nil, Ttyp_record _ -> true
  | _, _ -> t1 = t2 

let rec typecheck_typ env = function
  | Atyp_alias s -> Ttyp_alias s
  | Atyp_array t -> Ttyp_array (new_type_id (), typecheck_typ env t)
  | Atyp_record fields ->
      Ttyp_record (new_type_id (),
        List.map (fun (name, typid) -> (name, find_typ env typid)) fields)

let error_break p = error p "break outside for/while loop"

(* val typecheck :: env -> exp pos -> typ * texp *)

let rec typecheck env breaks e =
  match desc e with
  | Aexp_int n -> typecheck_int n
  | Aexp_str s -> typecheck_str s
  | Aexp_seq el -> typecheck_seq env breaks el
  | Aexp_nil -> (Ttyp_nil, Texp_nil)
  | Aexp_break -> breaks (posn e); (Ttyp_void, Texp_break)
  | Aexp_bin (e1, op, e2) -> typecheck_bin env breaks e1 op e2
  | Aexp_unary (op, e) -> typecheck_unary env breaks op e
  | Aexp_if (e1, e2, e3) -> typecheck_if env breaks e1 e2 e3
  | Aexp_while (e1, e2) -> typecheck_while env breaks e1 e2
  | Aexp_call (s, el) -> typecheck_call env breaks s el
  | Aexp_set (r, e) -> typecheck_set env breaks r e
  | Aexp_ref r ->
      let (rt, rc) = typecheck_ref env breaks r
      in (rt, Texp_ref rc)
  | Aexp_let (dl, el) -> typecheck_let env breaks dl el
  | Aexp_for (index, start, finish, body) ->
      typecheck_for env breaks index start finish body
  | Aexp_array (typid, size, init) ->
      typecheck_array env breaks typid size init
  | Aexp_record (typid, fields) ->
      typecheck_record env breaks typid fields

and typecheck_unary env breaks Neg e =
  let (et, ec) = typecheck env breaks e in
  if eq_typ env et Ttyp_int then (Ttyp_int, Texp_bin_int (Texp_int 0, Minus, ec))
  else error (posn e) "unary negation requires integer valued exp"

and typecheck_record env breaks typid fields =
  let (tid,t) = find_record_typ env typid in
  let flds = List.map2 (fun (f1n,f1tn) (f2n,f2init) ->
    if f1n = f2n then
      let (f2initt, f2initc) = typecheck env breaks f2init
      in if eq_typ env f2initt f1tn
        then (f2n, f2initc)
        else error (posn f2init) "field type mismatch"
    else error (posn f2init) "field name mismatch") t fields
  in (Ttyp_record (tid,t), Texp_record flds)

and typecheck_array env breaks typid size init =
  let (sizet,sizec) = typecheck env breaks size in
  let (initt,initc) = typecheck env breaks init in
  let (tid,typ) = find_array_typ env typid
  in if (eq_typ env typ initt) then
    if (eq_typ env sizet Ttyp_int) then
      (Ttyp_array (tid,typ), Texp_array (sizec, initc))
    else error (posn size) "size expression not of integer type"
  else error (posn init) "init expression does not match array def"

and typecheck_for env breaks index start finish body =
  let (startt, startc) = typecheck env breaks start in
  let (finisht, finishc) = typecheck env breaks finish in
  let does_break = ref false in
  let breaks _ = does_break := true in
  let (bodyt, bodyc) = typecheck (add_var env index Ttyp_int (ref Immutable))
    breaks body in
  if (eq_typ env startt Ttyp_int) then
    if (eq_typ env finisht Ttyp_int) then
      if (eq_typ env bodyt Ttyp_void) then
        (Ttyp_void, Texp_for (index, startc, finishc, bodyc, does_break))
      else error (posn body) "body of for loop should return void"
    else error (posn finish) "end expression in for loop should be integer valued"
  else error (posn start) "start expression in for loop should be integer valued"

and typecheck_dec breaks (env, td) d =
  match desc d with
  | Adec_var (name, typid, init) -> begin
    let (initt, init1) = typecheck env breaks init in
    let vart = match typid with
    | None ->
        if eq_typ env initt Ttyp_nil then
          error (posn init) "can't determine variable type"
        else
          initt
    | Some t -> find_typ env t
    in if eq_typ env vart initt then 
      let mut = ref NonAssigned in
        (add_var env name vart mut, (Tdec_var (name, init1, mut))::td)
    else
      error (posn init) "type mismatch in variable declaration"
  end
  | Adec_typ tl ->
      (List.fold_left
        (fun env (name, typ) ->
          add_typ env name (typecheck_typ env typ)) env tl, td)
  | Adec_fun fl -> typecheck_fundecs env td fl

and typecheck_fundecs env td fl =
  let env1 = List.fold_left (fun env (name, args, ret_typid, _) ->
    let rett = map_default (find_typ env) Ttyp_void ret_typid in
      add_fun env name
        (List.map (fun (_, y) -> find_typ env y) args) rett)
        env fl
  in (env1, Tdec_fun (List.map
    (fun (name, args, ret_typid, body) ->
      let (env2, muts) = List.fold_left (fun (env, muts) (arg_name, arg_typid) ->
        let mut = ref NonAssigned in
          (add_var env arg_name (find_typ env arg_typid) mut, mut :: muts))
        (env1, []) args in
      let (bodyt, bodyc) = typecheck env2 error_break body in
      let rett = map_default (find_typ env) Ttyp_void ret_typid in
        begin
          if eq_typ env bodyt rett then
            (name, List.map2 (fun (x, _) mut -> (x, mut)) args (List.rev muts), bodyc)
          else
            error (posn body) "body of function does not match return type"
        end) fl) :: td)
  
and typecheck_let env breaks dl body =
  let (env1, td) = List.fold_left (typecheck_dec breaks) (env, []) dl in
  let body1t = List.rev_map (typecheck env1 breaks) body in
  let ret_typ = match body1t with [] -> Ttyp_void | (bodyt,_)::_ -> bodyt
  in (ret_typ, Texp_let (List.rev td, Texp_seq (List.rev (List.map
      (fun (_,x) -> x) body1t))))
    
and typecheck_set env breaks r e =
  let (rt, rc) = typecheck_ref env breaks r in
  let (et, ec) = typecheck env breaks e
  in if eq_typ env rt et then begin
    (match rc with
    | Tref_name (_, mut) ->
        if !mut = Immutable then error (posn e) "immutable variable"
        else mut := Assigned
    | _ -> ());
    (Ttyp_void, Texp_set (rc, ec))
  end else
    error (posn e) "type mismatch in assignment"

and typecheck_ref env breaks r =
  match (desc r) with
  | Aref_name s ->
      let (t, mut) = find_var env s in
        (resolve_typ env (posn s) t, Tref_name (desc s, mut))
  | Aref_index (r, index) -> begin
    let (rt, rc) = typecheck_ref env breaks r in
    let (indext, indexc) = typecheck env breaks index
    in match rt with
    | Ttyp_array (_,t) ->
      if eq_typ env indext Ttyp_int
      then (resolve_typ env (posn r) t, Tref_index (rc, indexc))
      else error (posn index) "index expression is not integer valued"
    | _ -> error (posn r) "variable is not of array type"
  end
  | Aref_field (r, s) -> begin
    let (rt, rc) = typecheck_ref env breaks r in
    match rt with
    | Ttyp_record (_,fields) -> begin
      try let t = List.assoc (desc s) fields in
        (resolve_typ env (posn s) t, Tref_field (rc, desc s))
      with Not_found -> error (posn s) "non-existent field"
    end
    | _ -> error (posn r) "variable is not of record type"
  end

and typecheck_int n =
  (Ttyp_int, Texp_int n)

and typecheck_str s =
  (Ttyp_string, Texp_str s)

and typecheck_call env breaks s args =
  let (argt, t) = find_fun env s in
  let args2 = List.map (typecheck env breaks) args
  in try if List.for_all2 (fun t1 (t2, _) -> eq_typ env t1 t2) argt args2 then
    (resolve_typ env (posn s) t, Texp_call (desc s, List.map (fun (_,x) -> x) args2))
  else error (posn s) "type mismatch in parameter list"
  with Invalid_argument _ -> error (posn s) "incorrect number of parameters"

and typecheck_while env breaks e1 e2 =
  let (e1t,e1c) = typecheck env breaks e1 in
    if eq_typ env e1t Ttyp_int then
      let does_break = ref false in
      let breaks _ = does_break := true in
      let (e2t,e2c) = typecheck env breaks e2
      in if eq_typ env e2t Ttyp_void then
        (Ttyp_void, Texp_while (e1c, e2c, does_break))
      else error (posn e2) "body of while loop should return no value"
    else error (posn e1) "condition of while loop should be integer valued"

and typecheck_if env breaks e1 e2 e3 =
  let (e1t,e1c) = typecheck env breaks e1 in
    if eq_typ env e1t Ttyp_int then
    let (e2t,e2c) = typecheck env breaks e2 in
      match e3 with
      | None -> begin
        if eq_typ env e2t Ttyp_void then
          (Ttyp_void, Texp_if (e1c, e2c, Texp_seq []))
        else error (posn e2) "then-part of if-then should be void valued"
      end
      | Some e3 -> begin
        let (e3t,e3c) = typecheck env breaks e3 in
          if eq_typ env e2t e3t then
            (e2t, Texp_if (e1c, e2c, e3c))
          else
            error (posn e3) "then-part and else-part should have the same type"
      end
    else
      error (posn e1) "condition in if expression should be integer valued"

and typecheck_bin env breaks e1 op e2 =
  let (e1t,e1c) = typecheck env breaks e1 in
  let (e2t,e2c) = typecheck env breaks e2 in
    match op with
    | Add | Minus | Times | Div ->
      if eq_typ env e1t Ttyp_int then
        if eq_typ env e2t Ttyp_int then
          (Ttyp_int, Texp_bin_math (e1c, op, e2c))
        else
          error (posn e2) "integer exp required"
      else
        error (posn e1) "integer exp required"
    | And | Or ->
        if eq_typ env e1t Ttyp_int then
          if eq_typ env e2t Ttyp_int then
            (Ttyp_int, Texp_bin_int (e1c, op, e2c))
          else
            error (posn e2) "integer exp required"
        else
          error (posn e1) "integer exp required"
    | Lt | Le | Gt | Ge ->
      if eq_typ env e1t Ttyp_int then
        if eq_typ env e2t Ttyp_int then
          (Ttyp_int, Texp_bin_cmp (e1c, op, e2c))
        else
          error (posn e2) "integer exp required"
      else if eq_typ env e1t Ttyp_string then
        if eq_typ env e2t Ttyp_string then
          (Ttyp_int, Texp_bin_cmp (e1c, op, e2c))
        else
          error (posn e2) "string exp required"
      else
        error (posn e1) "integer or string exp required"
    | Eq | Neq ->
      if eq_typ env e1t Ttyp_int then
        (Ttyp_int, Texp_bin_cmp (e1c, op, e2c))
      else if eq_typ env e1t Ttyp_string then
        (Ttyp_int, Texp_bin_cmp (e1c, op, e2c))
      else if eq_typ env e1t e2t then
        (Ttyp_int, Texp_bin_gen (e1c, op, e2c))
      else
        error (posn e2) "comparing expressions of different types"

and typecheck_seq env breaks el =
  let el2 = List.rev_map (typecheck env breaks) el in
  match el2 with
  | [] -> (Ttyp_void, Texp_seq [])
  | (t,_)::_ -> (t, Texp_seq (List.rev (List.map (fun (_,x) -> x) el2)))

let primitives = [
  "print", [Ttyp_string], Ttyp_void;
  "printi", [Ttyp_int], Ttyp_void;
  "flush", [], Ttyp_void;
  "getchar", [], Ttyp_string;
  "ord", [Ttyp_string], Ttyp_int;
  "chr", [Ttyp_int], Ttyp_string;
  "size", [Ttyp_string], Ttyp_int;
  "substring", [Ttyp_string; Ttyp_int; Ttyp_int], Ttyp_string;
  "concat", [Ttyp_string; Ttyp_string], Ttyp_string;
  "not", [Ttyp_int], Ttyp_int;
  "exit", [Ttyp_int], Ttyp_void
]

let std_types = [
  "int", Ttyp_int;
  "string", Ttyp_string;
]

let std_env =
  let env = List.fold_left
    (fun env (name, args, ret) ->
      add_fun env name args ret) empty_env primitives in
  let env = List.fold_left
    (fun env (name, t) -> add_typ env name t) env std_types
  in env
