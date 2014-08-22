(* Copyright 2014 Nicolas Ojeda Bar <n.oje.bar@gmail.com>.
   All rights reserved.
   Distributed under the Q Public License, version 1.0. *)

open TigerTyping

open Ast_mapper
open Parsetree
open Asttypes
  
let map_opt f =
  function
    None ->
      None
  | Some x ->
      Some (f x)

let mkident ?(loc = Location.none) s =
  Location.mkloc (Longident.parse s) loc

let rec emit_type =
  function
    TInt ->
      T.constr (mkident "int") []
  | TString ->
      T.constr (mkident "string") []
  | TUnit ->
      T.constr (mkident "unit") []
  | TArray t ->
      T.constr (mkident "array") [ emit_type t ]
  | TAny ->
      assert false
  | TBool ->
      T.constr (mkident "bool") []
  | TRecord (id, _) ->
      T.constr (mkident "option") [ T.constr (mkident id) [] ]
  | TForward r ->
      begin
        match !r with
          None ->
            assert false
        | Some t ->
            emit_type t
      end

let emit_top_types typs =
  let mkrecord fields = {
    ptype_params = [];
    ptype_cstrs = [];
    ptype_kind =
      Ptype_record
        (List.map (fun (name, t) -> Location.mknoloc name, Mutable, emit_type t, Location.none) fields);
    ptype_private = Public;
    ptype_manifest = None;
    ptype_variance = [];
    ptype_loc = Location.none
  }
  in
  let typs = List.map (fun (name, fields) -> Location.mknoloc name, mkrecord fields) typs in
  M.type_ typs

let rec expr =
  function
    TBreakExp ->
      E.apply_nolabs (E.lid "raise") [E.construct (mkident "TigerLib.Break") None false]
  | TNilExp ->
      E.construct (mkident "None") None false
  | TIntExp n ->
      E.constant (Const_int n)
  | TStringExp s ->
      E.constant (Const_string s)
  | TUnitExp ->
      E.construct (mkident "()") None false
  | TSeqExp (e1, e2) ->
      E.sequence (expr e1) (expr e2)
  | TLetExp (id, init, mf, e) ->
      let d =
        match mf with
          Mutable m when !m ->
            [P.var (Location.mknoloc id), E.apply_nolabs (E.lid "ref") [expr init]]
        | _ ->
            [P.var (Location.mknoloc id), expr init]
      in
      E.let_ Nonrecursive d (expr e)
  | TLetRecExp (funs, e) ->
      let makefun =
        function
          (id, [], e) ->
            P.var (Location.mknoloc id),
            E.function_ "" None [P.construct (mkident "()") None false, (expr e)]
        | (id, args, e) ->
            let e =
              List.fold_right
                (fun (a, m) e ->
                   let e = E.function_ "" None [P.var (Location.mknoloc a), e] in
                   match m with
                     TigerTyping.Mutable m when !m ->
                       E.let_ Nonrecursive [P.var (Location.mknoloc a), E.apply_nolabs (E.lid "ref") [E.lid a]] e
                   | _ ->
                       e)
                args (expr e) in
            P.var (Location.mknoloc id), e
      in
      E.let_ Recursive (List.map makefun funs) (expr e)
  | TVarExp v ->
      var v
  | TAssignExp (TNameVar (name, _), e) ->
      E.apply_nolabs (E.lid ":=") [E.lid name; expr e]
  | TAssignExp (TFieldVar (v, name, line), e) ->
      E.match_ (var v)
        [P.construct (mkident "None") None false,
         E.apply_nolabs (E.lid "raise")
           [E.construct (mkident "TigerLib.Nil") (Some (E.constant (Const_int line))) false];
         P.construct (mkident "Some") (Some (P.var (Location.mknoloc "x"))) false,
         E.setfield (E.lid "x") (mkident name) (expr e)]
  | TAssignExp (TIndexVar (v, e', line), e) ->
      E.apply_nolabs (E.lid "TigerLib.set") [var v; expr e'; expr e; E.constant (Const_int line)]
  | TForExp (index, start, finish, body, false) ->
      E.for_ (Location.mknoloc index) (expr start) (expr finish) Upto (expr body)
  | TForExp (index, start, finish, body, true) ->
      E.try_
        (E.for_ (Location.mknoloc index) (expr start) (expr finish) Upto (expr body))
        [P.construct (mkident "TigerLib.Break") None false, E.construct (mkident "()") None false]
  | TCallExp (name, []) ->
      E.apply_nolabs (E.lid name) [E.construct (mkident "()") None false]
  | TCallExp (name, args) ->
      E.apply_nolabs (E.lid name) (List.map expr args)
  | TArrayExp (size, init) ->
      E.apply_nolabs (E.lid "Array.make") [expr size; expr init]
  | TRecordExp fields ->
      let mkfield (id, e) = mkident id, expr e in
      E.construct (mkident "Some") (Some (E.record (List.map mkfield fields) None)) false
  | TIfExp (e1, e2, e3) ->
      E.ifthenelse (expr e1) (expr e2) (map_opt expr e3)
  | TWhileExp (e1, e2, false) ->
      E.while_ (expr e1) (expr e2)
  | TWhileExp (e1, e2, true) ->
      E.try_
        (E.while_ (expr e1) (expr e2))
        [P.construct (mkident "TigerLib.Break") None false, E.construct (mkident "()") None false]

and var =
  function
    TNameVar (s, Mutable m) when !m ->
      E.apply_nolabs (E.lid "!") [E.lid s]
  | TNameVar (s, _) ->
      E.lid s
  | TFieldVar (v, name, line) ->
      E.match_
        (var v)
        [P.construct (mkident "None") None false, E.apply_nolabs (E.lid "raise")
           [E.construct (mkident "TigerLib.Nil") (Some (E.constant (Const_int line))) false];
         P.construct (mkident "Some") (Some (P.var (Location.mknoloc "x"))) false,
         E.field (E.lid "x") (mkident name)]
  | TIndexVar (v, e, line) ->
      E.apply_nolabs (E.lid "TigerLib.get") [var v; expr e; E.constant (Const_int line)]

let emit_ocaml typs e =
  let typs = List.map emit_top_types typs in
  let e = expr e in
  let e1 =
    M.value Nonrecursive
      [P.var (Location.mknoloc "main"), E.function_ "" None [P.construct (mkident "()") None false, e]]
  in
  let e2 = M.eval (E.apply_nolabs (E.lid "TigerLib.run") [E.lid "main"]) in
  let m = typs @ [e1; e2] in
  m
