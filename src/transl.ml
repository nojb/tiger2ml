(* open Syntax *)
open Typecheck

open Ast_mapper
open Parsetree
open Asttypes
open Longident
  
(* let ident2ocaml s = "_" ^ s *)

(* let mathop_impl = function *)
(*   | Add -> "+" | Minus -> "-" | Times -> "*" | Div -> "/" *)
(*   | _ -> assert false *)

(* let cmpop_impl = function *)
(*   | Eq -> "=" | Neq -> "<>" | Ge -> ">=" *)
(*   | Gt -> ">" | Le -> "<=" | Lt -> "<" *)
(*   | _ -> assert false *)

let map_opt f = function None -> None | Some x -> Some (f x)

let mkident ?(loc = Location.none)  s =
  {txt = Longident.parse s; loc }

let rec expr = function
    TBreakExp ->
      E.apply_nolabs (E.lid "raise") [E.construct (mkident "Tigerlib.Break") None false]
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
  | TLetExp (id, init, mut, e) ->
      let r, d =
        if !mut = Assigned then
          Nonrecursive, [P.var (Location.mknoloc id), E.apply_nolabs (E.lid "ref") [expr init]]
        else
          Nonrecursive, [P.var (Location.mknoloc id), expr init]
      in
      E.let_ r d (expr e)
  | TVarExp r ->
      expr_ref r
  | TAssignExp (r, e) ->
      transl_assign r e
  | TForExp (index, start, finish, body, {contents = false}) ->
      E.for_ {loc=Location.none; txt=index} (expr start) (expr finish) Upto (expr body)
  | TForExp (index, start, finish, body, {contents = true}) ->
      E.try_
        (E.for_ {loc=Location.none; txt=index} (expr start) (expr finish) Upto (expr body))
        [P.construct (mkident "Tigerlib.Break") None false, E.construct (mkident "()") None false]
  (* | Texp_call (name, []) -> *)
  (*     fprintf out "@[%s@ ()@]" (ident2ocaml name) *)
  (* | Texp_call (name, args) -> *)
  (*     fprintf out "@[%s@ @[<2>" (ident2ocaml name); *)
  (*     separated (fun out () -> fprintf out "@ ") *)
  (*       (fun out x -> fprintf out "(%a)" transl x) out args; *)
  (*     fprintf out "@]@]" *)
  (* | Texp_bin_int (e1, And, e2) -> *)
  (*     fprintf out "@[(if@ (%a)@,<>@,0@,&&@,(%a)@,<>@,0@ then@ 1@ else@ 0)@]" *)
  (*       transl e1 transl e2 *)
  (* | Texp_bin_int (e1, Or, e2) -> *)
  (*     fprintf out "@[(if@ (%a)@,<>@,0@,||@,(%a)@,<>@,0@ then@ 1@ else@ 0)@]" *)
  (*       transl e1 transl e2 *)
  (* | Texp_bin_int (_, _, _) -> assert false *)
  (* | Texp_bin_math (e1, op, e2) -> *)
  (*     fprintf out "@[(%a)@,%s@,(%a)@]" *)
  (*       transl e1 (mathop_impl op) transl e2 *)
  (* | Texp_bin_cmp (e1, op, e2) -> *)
  (*     fprintf out "@[(if@ (%a)@,%s@,(%a)@ then@ 1@ else@ 0)@]" transl e1 (cmpop_impl op) transl e2 *)
  (* | Texp_bin_gen (e1, Eq, e2) -> *)
  (*     fprintf out "@[(if@ (%a)@,==@,(%a)@ then@ 1@ else@ 0)@]" transl e1 transl e2 *)
  (* | Texp_bin_gen (e1, Neq, e2) -> *)
  (*     fprintf out "@[(if@ (%a)@,!=@,(%a)@ then@ 1@ else@ 0)@]" transl e1 transl e2 *)
  (* | Texp_bin_gen (_, _, _) -> assert false *)
  | TArrayExp (size, init) ->
      E.apply_nolabs (E.lid "Array.make") [expr size; expr init]
  (* | Texp_record fields -> *)
  (*     fprintf out "Some{%a}" *)
  (*       (separated (fun out () -> fprintf out ";@ ") *)
  (*       (fun out (name, init) -> fprintf out "%s=%a" (ident2ocaml *)
  (*         (name)) transl init)) fields *)
  | TIfExp (e1, e2, e3) ->
      E.ifthenelse (expr e1) (expr e2) (map_opt expr e3)
  | TWhileExp (e1, e2, {contents = false}) ->
      E.while_ (expr e1) (expr e2)
  | TWhileExp (e1, e2, {contents = true}) ->
      E.try_ (E.while_ (expr e1) (expr e2))
        [P.construct (mkident "Tigerlib.Break") None false, E.construct (mkident "()") None false]

and expr_ref = function
  | TNameVar (s, mut) ->
      if !mut = Assigned then
        E.apply_nolabs (E.lid "!") [E.lid s]
      else
        E.lid s
  (* | Tref_field (r, name) -> *)
  (*     fprintf out "@[Option.get@ ("; *)
  (*     transl_ref out r; *)
  (*     fprintf out ")."; *)
  (*     fprintf out "%s@]" (ident2ocaml name) *)
  | TIndexVar (r, index) ->
      E.apply_nolabs (E.lid "Array.get") [expr_ref r; expr index]

(* and transl_fundec out = function *)
(*   | (name, [], body) -> *)
(*       fprintf out "%s@ ()@ =@ @[<2>%a@]@\n" *)
(*         (ident2ocaml name) transl body *)
(*   | (name, args, body) -> *)
(*       fprintf out "%s@ %a@ =@\n" *)
(*         (ident2ocaml name) *)
(*         (separated (fun out () -> fprintf out "@ ") *)
(*           (fun out (s, _) -> fprintf out "%s" (ident2ocaml s))) args; *)
(*       List.iter *)
(*         (fun (name, mut) -> *)
(*           if !mut = Assigned then *)
(*             let name = ident2ocaml name in *)
(*             fprintf out "let@ %s=ref@ %s@ in@\n" name name) *)
(*         args; *)
(*       fprintf out "@[<2>%a@]@\n" transl body *)

and transl_assign r e =
  match r with
  | TNameVar (name, _) ->
      E.apply_nolabs (E.lid ":=") [E.lid name; expr e]
  (* | Tref_field (r, name) -> *)
  (*     fprintf out "%a.%s<-(%a)" transl_ref r (ident2ocaml name) transl e *)
  | TIndexVar (r, e') ->
      E.apply_nolabs (E.lid "Array.set") [expr e'; expr e]

let emit_ocaml e =
  let e = expr e in
  Format.printf "%a@." Pprintast.default # structure_item (M.eval e)
