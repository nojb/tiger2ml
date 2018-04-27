(* Copyright 2014 Nicolas Ojeda Bar <n.oje.bar@gmail.com>.
   All rights reserved.
   Distributed under the Q Public License, version 1.0. *)

open Typing

open Parsetree
open Asttypes
open Ast_helper

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
      Typ.constr (mkident "int") []
  | TString ->
      Typ.constr (mkident "string") []
  | TUnit ->
      Typ.constr (mkident "unit") []
  | TArray t ->
      Typ.constr (mkident "array") [ emit_type t ]
  | TAny ->
      assert false
  | TBool ->
      Typ.constr (mkident "bool") []
  | TRecord (id, _) ->
      Typ.constr (mkident "option") [ Typ.constr (mkident id) [] ]
  | TForward r ->
      begin
        match !r with
          None ->
            assert false
        | Some t ->
            emit_type t
      end
  | TNil ->
      assert false

let emit_top_types typs =
  let mkrecord (name, fields) = {
    ptype_name = Location.mknoloc name;
    ptype_params = [];
    ptype_cstrs = [];
    ptype_kind =
      Ptype_record
        (List.map (fun (name, t) -> {pld_name = Location.mknoloc name;
                                     pld_mutable = Mutable;
                                     pld_type = emit_type t;
                                     pld_loc = Location.none;
                                     pld_attributes = []}
                  ) fields);
    ptype_private = Public;
    ptype_manifest = None;
    ptype_attributes = [];
    ptype_loc = Location.none
  }
  in
  let typs = List.map mkrecord typs in
  Str.type_ Recursive typs

let rec expr =
  function
    TBreakExp ->
      Exp.apply (Exp.ident (mkident "raise")) [Nolabel, Exp.construct (mkident "TigerLib.Break") None]
  | TNilExp ->
      Exp.construct (mkident "None") None
  | TIntExp n ->
      Exp.constant (Pconst_integer (string_of_int n, None))
  | TStringExp s ->
      Exp.constant (Pconst_string (s, None))
  | TUnitExp ->
      Exp.construct (mkident "()") None
  | TSeqExp (e1, e2) ->
      Exp.sequence (expr e1) (expr e2)
  | TLetExp (id, init, mf, e) ->
      let d =
        match mf with
          Mutable m when !m ->
            {pvb_pat = Pat.var (Location.mknoloc id);
             pvb_expr = Exp.apply (Exp.ident (mkident "ref")) [Nolabel, expr init];
             pvb_attributes = [];
             pvb_loc = Location.none}
        | _ ->
            {pvb_pat = Pat.var (Location.mknoloc id);
             pvb_expr = expr init;
             pvb_attributes = [];
             pvb_loc = Location.none}
      in
      Exp.let_ Nonrecursive [d] (expr e)
  | TLetRecExp (funs, e) ->
      let makefun =
        function
          (id, [], e) ->
            {pvb_pat = Pat.var (Location.mknoloc id);
             pvb_expr =
               Exp.function_ [{pc_lhs = Pat.construct (mkident "()") None;
                               pc_guard = None;
                               pc_rhs = expr e}];
             pvb_attributes = [];
             pvb_loc = Location.none}
        | (id, args, e) ->
            let e =
              List.fold_right
                (fun (a, m) e ->
                   let e = Exp.function_ [{pc_lhs = Pat.var (Location.mknoloc a);
                                           pc_guard = None;
                                           pc_rhs = e}] in
                   match m with
                     Typing.Mutable m when !m ->
                       Exp.let_ Nonrecursive [{pvb_pat = Pat.var (Location.mknoloc a);
                                               pvb_expr =
                                                 Exp.apply (Exp.ident (mkident "ref"))
                                                   [Nolabel, Exp.ident (mkident a)];
                                               pvb_attributes = [];
                                               pvb_loc = Location.none}] e
                   | _ ->
                       e)
                args (expr e) in
            {pvb_pat = Pat.var (Location.mknoloc id);
             pvb_expr = e;
             pvb_attributes = [];
             pvb_loc = Location.none}
      in
      Exp.let_ Recursive (List.map makefun funs) (expr e)
  | TVarExp v ->
      var v
  | TAssignExp (TNameVar (name, _), e) ->
      Exp.apply (Exp.ident (mkident ":=")) [Nolabel, Exp.ident (mkident name); Nolabel, expr e]
  | TAssignExp (TFieldVar (v, name, line), e) ->
      Exp.match_ (var v)
        [{pc_lhs = Pat.construct (mkident "None") None;
          pc_guard = None;
          pc_rhs =
            Exp.apply (Exp.ident (mkident "raise"))
              [Nolabel, Exp.construct (mkident "TigerLib.Nil")
                 (Some (Exp.constant (Pconst_integer (string_of_int line, None))))]};
         {pc_lhs = Pat.construct (mkident "Some") (Some (Pat.var (Location.mknoloc "x")));
          pc_guard = None;
          pc_rhs = Exp.setfield (Exp.ident (mkident "x")) (mkident name) (expr e)}]
  | TAssignExp (TIndexVar (v, e', line), e) ->
      Exp.apply (Exp.ident (mkident "TigerLib.set"))
        [Nolabel, var v;
         Nolabel, expr e';
         Nolabel, expr e;
         Nolabel, Exp.constant (Pconst_integer (string_of_int line, None))]
  | TForExp (index, start, finish, body, false) ->
      Exp.for_ (Pat.var (Location.mknoloc index)) (expr start) (expr finish) Upto (expr body)
  | TForExp (index, start, finish, body, true) ->
      Exp.try_
        (Exp.for_ (Pat.var (Location.mknoloc index)) (expr start) (expr finish) Upto (expr body))
        [{pc_lhs = Pat.construct (mkident "TigerLib.Break") None;
          pc_guard = None;
          pc_rhs = Exp.construct (mkident "()") None}]
  | TCallExp (name, []) ->
      Exp.apply (Exp.ident (mkident name)) [Nolabel, Exp.construct (mkident "()") None]
  | TCallExp (name, args) ->
      Exp.apply (Exp.ident (mkident name)) (List.map (fun e -> Nolabel, expr e) args)
  | TArrayExp (size, init) ->
      Exp.apply (Exp.ident (mkident "Array.make")) [Nolabel, expr size; Nolabel, expr init]
  | TRecordExp fields ->
      let mkfield (id, e) = mkident id, expr e in
      Exp.construct (mkident "Some") (Some (Exp.record (List.map mkfield fields) None))
  | TIfExp (e1, e2, e3) ->
      Exp.ifthenelse (expr e1) (expr e2) (map_opt expr e3)
  | TWhileExp (e1, e2, false) ->
      Exp.while_ (expr e1) (expr e2)
  | TWhileExp (e1, e2, true) ->
      Exp.try_
        (Exp.while_ (expr e1) (expr e2))
        [{pc_lhs = Pat.construct (mkident "TigerLib.Break") None;
          pc_guard = None;
          pc_rhs = Exp.construct (mkident "()") None}]

and var =
  function
    TNameVar (s, Mutable m) when !m ->
      Exp.apply (Exp.ident (mkident "!")) [Nolabel, Exp.ident (mkident s)]
  | TNameVar (s, _) ->
      Exp.ident (mkident s)
  | TFieldVar (v, name, line) ->
      Exp.match_
        (var v)
        [{pc_lhs = Pat.construct (mkident "None") None;
          pc_guard = None;
          pc_rhs =
            Exp.apply (Exp.ident (mkident "raise"))
              [Nolabel,
               Exp.construct (mkident "TigerLib.Nil")
                 (Some (Exp.constant (Pconst_integer (string_of_int line, None))))]};
         {pc_lhs = Pat.construct (mkident "Some") (Some (Pat.var (Location.mknoloc "x")));
          pc_guard = None;
          pc_rhs = Exp.field (Exp.ident (mkident "x")) (mkident name)}]
  | TIndexVar (v, e, line) ->
      Exp.apply (Exp.ident (mkident "TigerLib.get"))
        [Nolabel, var v;
         Nolabel, expr e;
         Nolabel, Exp.constant (Pconst_integer (string_of_int line, None))]

let emit_ocaml typs e =
  let typs = List.map emit_top_types typs in
  let e = expr e in
  let e1 =
    Str.value Nonrecursive
      [{pvb_pat = Pat.var (Location.mknoloc "main");
        pvb_expr = Exp.function_ [{pc_lhs = Pat.construct (mkident "()") None;
                                   pc_guard = None;
                                   pc_rhs = e}];
        pvb_attributes = [];
        pvb_loc = Location.none}]
  in
  let e2 = Str.eval (Exp.apply (Exp.ident (mkident "TigerLib.run")) [Nolabel, Exp.ident (mkident "main")]) in
  let m = typs @ [e1; e2] in
  m
