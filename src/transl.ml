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
    Texp_break ->
      E.apply_nolabs (E.lid "raise") [E.construct (mkident "Tigerlib.Break") None false]
  | Texp_nil ->
      E.construct (mkident "None") None false
  | Texp_int n ->
      E.constant (Const_int n)
  | Texp_str s ->
      E.constant (Const_string s)
  | Texp_seq [] ->
      E.construct (mkident "()") None false
  | Texp_seq [a] ->
      expr a
  | Texp_seq (e :: el) ->
      List.fold_left (fun e1 e2 -> E.sequence e1 (expr e2)) (expr e) el
  | Texp_let (ds, body) ->
      List.fold_right (fun d b -> let f, pl = transl_dec d in E.let_ f pl b) ds (expr body)
  | Texp_ref r ->
      expr_ref r
  | Texp_set (r, e) ->
      transl_assign r e
  | Texp_for (index, start, finish, body, {contents = false}) ->
      E.for_ {loc=Location.none; txt=index} (expr start) (expr finish) Upto (expr body)
  | Texp_for (index, start, finish, body, {contents = true}) ->
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
  | Texp_array (size, init) ->
      E.apply_nolabs (E.lid "Array.make") [expr size; expr init]
  (* | Texp_record fields -> *)
  (*     fprintf out "Some{%a}" *)
  (*       (separated (fun out () -> fprintf out ";@ ") *)
  (*       (fun out (name, init) -> fprintf out "%s=%a" (ident2ocaml *)
  (*         (name)) transl init)) fields *)
  | Texp_if (e1, e2, e3) ->
      E.ifthenelse (expr e1) (expr e2) (map_opt expr e3)
  | Texp_while (e1, e2, {contents = false}) ->
      E.while_ (expr e1) (expr e2)
  | Texp_while (e1, e2, {contents = true}) ->
      E.try_ (E.while_ (expr e1) (expr e2))
        [P.construct (mkident "Tigerlib.Break") None false, E.construct (mkident "()") None false]

and expr_ref = function
  | Tref_name (s, mut) ->
      if !mut = Assigned then
        E.apply_nolabs (E.lid "!") [E.lid s]
      else
        E.lid s
  (* | Tref_field (r, name) -> *)
  (*     fprintf out "@[Option.get@ ("; *)
  (*     transl_ref out r; *)
  (*     fprintf out ")."; *)
  (*     fprintf out "%s@]" (ident2ocaml name) *)
  | Tref_index (r, index) ->
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

and transl_dec = function
  | Tdec_var (s, init, mut) ->
      if !mut = Assigned then
        Nonrecursive, [P.var {loc=Location.none; txt=s}, E.apply_nolabs (E.lid "ref") [expr init]]
      else
        Nonrecursive, [P.var {loc=Location.none; txt=s}, expr init]
  (* | Tdec_fun (a :: b) -> *)
  (*     fprintf out "@[let@ rec@ %a" transl_fundec a; *)
  (*     let rec loop = function *)
  (*       | [] -> fprintf out "in@]@ " *)
  (*       | a :: b -> *)
  (*           fprintf out "and@ %a" transl_fundec a; *)
  (*           loop b *)
  (*     in loop b *)
  (* | Tdec_fun [] -> assert false *)

and transl_assign r e =
  match r with
  | Tref_name (name, _) ->
      E.apply_nolabs (E.lid ":=") [E.lid name; expr e]
  (* | Tref_field (r, name) -> *)
  (*     fprintf out "%a.%s<-(%a)" transl_ref r (ident2ocaml name) transl e *)
  | Tref_index (r, e') ->
      E.apply_nolabs (E.lid "Array.set") [expr e'; expr e]

let emit_ocaml e =
  let e = expr e in
  Format.printf "%a@." Pprintast.default # structure_item (M.eval e)
