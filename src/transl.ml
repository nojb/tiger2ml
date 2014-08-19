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
  (* | Texp_let (decs, body) -> *)
  (*     List.iter (transl_dec out) decs; *)
  (*     transl out body *)
  (* | Texp_ref r -> *)
  (*     transl_ref out r *)
  (* | Texp_set (r, e) -> *)
  (*     transl_assign out r e *)
  (* | Texp_for (index, start, finish, body, does_break) -> *)
  (*     let forloop = E.mk (Pexp_for (index, transl start, transl finish, Upto, transl body)) in *)
  (*     E.mk (Pexp_try (forloop, [P.mk (Ppat_construct ("Break", ...)), E.mk (Pexp_construct ("()", None, false))])) *)
      (* if !does_break then *)
      (*   fprintf out "@[try@\n@[<2>" *)
      (* else *)
      (*   fprintf out "@["; *)
      (* fprintf out *)
      (*   "for@ %s=(%a)@ to@ (%a)@ do@\n@[<hv 2>%a@]@\ndone" *)
      (*   (ident2ocaml index) transl start transl finish transl body; *)
      (* if !does_break then *)
      (*   fprintf out "@]@\nwith@ Break->()@]" *)
      (* else *)
      (*   fprintf out "@]" *)
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
  (* | Texp_array (size, init) -> *)
  (*     fprintf out "Array.make@ (%a)@ (%a)" *)
  (*       transl size transl init *)
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

(* and transl_ref out = function *)
(*   | Tref_name (s, mut) -> *)
(*       if !mut = Assigned then *)
(*         fprintf out "!%s" (ident2ocaml s) *)
(*       else *)
(*         fprintf out "%s" (ident2ocaml s) *)
(*   | Tref_field (r, name) -> *)
(*       fprintf out "@[Option.get@ ("; *)
(*       transl_ref out r; *)
(*       fprintf out ")."; *)
(*       fprintf out "%s@]" (ident2ocaml name) *)
(*   | Tref_index (r, index) -> *)
(*       fprintf out "@["; *)
(*       transl_ref out r; *)
(*       fprintf out ".("; *)
(*       transl out index; *)
(*       fprintf out ")@]" *)

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

(* and transl_dec out = function *)
(*   | Tdec_var (s, init, mut) -> *)
(*       if !mut = Assigned then *)
(*         fprintf out "let@ @[%s@ =@ ref@ " (ident2ocaml s) *)
(*       else *)
(*         fprintf out "let@ @[%s@ =@ " (ident2ocaml s); *)
(*       fprintf out "(%a)@]@ in@\n" transl init *)
(*   | Tdec_fun (a :: b) -> *)
(*       fprintf out "@[let@ rec@ %a" transl_fundec a; *)
(*       let rec loop = function *)
(*         | [] -> fprintf out "in@]@ " *)
(*         | a :: b -> *)
(*             fprintf out "and@ %a" transl_fundec a; *)
(*             loop b *)
(*       in loop b *)
(*   | Tdec_fun [] -> assert false *)

(* and transl_assign out r e = *)
(*   match r with *)
(*   | Tref_name (name, _) -> *)
(*       fprintf out "%s:=" (ident2ocaml name); *)
(*       transl out e *)
(*   | Tref_field (r, name) -> *)
(*       fprintf out "%a.%s<-(%a)" transl_ref r (ident2ocaml name) transl e *)
(*   | Tref_index (r, e') -> *)
(*       fprintf out "%a.(%a)<-(%a)" *)
(*         transl_ref r transl e' transl e *)

let emit_ocaml e =
  let e = expr e in
  Format.printf "%a@." Pprintast.default # structure_item (M.eval e)
