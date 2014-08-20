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

open TigerTyping

open Ast_mapper
open Parsetree
open Asttypes
  
(* let ident2ocaml s = "_" ^ s *)

let map_opt f =
  function
    None ->
      None
  | Some x ->
      Some (f x)

let mkident ?(loc = Location.none) s =
  Location.mkloc (Longident.parse s) loc

let rec typ =
  function
    TInt -> T.constr (mkident "int") []
  | TString -> T.constr (mkident "string") []
  | TUnit -> T.constr (mkident "unit") []
  | TArray t -> T.constr (mkident "array") [ typ t ]
  | TAny -> invalid_arg "emit_type"
  | TBool -> T.constr (mkident "bool") []

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
  | TAssignExp (TFieldVar (v, name), e) ->
      E.setfield (var v) (mkident name) (expr e)
  | TAssignExp (TIndexVar (v, e'), e) ->
      E.apply_nolabs (E.lid "Array.set") [var v; expr e'; expr e]
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
  | TFieldVar (v, name) ->
      E.match_
        (var v)
        [P.construct (mkident "None") None false, E.assertfalse (); (* FIXME report error ! *)
         P.construct (mkident "Some") (Some (P.var (Location.mknoloc "x"))) false,
         E.field (E.lid "x") (mkident name)] (* FIXME 'x' can be captured ?! *)
  | TIndexVar (v, e) ->
      E.apply_nolabs (E.lid "Array.get") [var v; expr e]

let emit_ocaml e =
  let e = expr e in
  Format.printf "%a@." Pprintast.default # structure_item (M.eval e)
