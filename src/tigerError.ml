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

open Format
open TigerSyntax

type error =
    TypeMismatch
  | ImmutableAssignment
  | BadArgumentCount
  | BadFieldName
  | BadNil
  | TypeNotFound
  | VariableNotFound
  | ArrayTypeExpected
  | RecordTypeExpected
  | FunctionNotFound
  | ArrayExpected
  | FieldNotFound
  | RecordExpected
  | IntExpected
  | UnitExpected
  | BadBreak
  | BadParse
  | IntOrStringExpected

exception Error of Location.t * error

let error loc err =
  raise (Error (loc, err))

let report ppf =
  function
    TypeMismatch ->
      fprintf ppf "Type mismatch"
  | ImmutableAssignment ->
      fprintf ppf "Illegal assignment to an immutable variable"
  | BadArgumentCount ->
      fprintf ppf "Wrong number of arguments"
  | BadFieldName ->
      fprintf ppf "Wrong field name"
  | BadNil ->
      fprintf ppf "Ambiguous usage of `nil'"
  | TypeNotFound ->
      fprintf ppf "Type not found"
  | VariableNotFound ->
      fprintf ppf "Variable not found"
  | ArrayTypeExpected ->
      fprintf ppf "Array type name expected"
  | RecordTypeExpected ->
      fprintf ppf "Record type name expected"
  | FunctionNotFound ->
      fprintf ppf "Function not found"
  | ArrayExpected ->
      fprintf ppf "Array expression expected"
  | FieldNotFound ->
      fprintf ppf "Field not found"
  | RecordExpected ->
      fprintf ppf "Record expression expected"
  | IntExpected ->
      fprintf ppf "integer expression expected"
  | UnitExpected ->
      fprintf ppf "unit expression expected"
  | BadBreak ->
      fprintf ppf "Illegal usage of `break'"
  | BadParse ->
      fprintf ppf "Parsing error"
  | IntOrStringExpected ->
      fprintf ppf "Int or String expected"

(*let get_file_line p =
  let ic = open_in p.pos_filename in begin
    seek_in ic (p.pos_offset-p.pos_column);
    let line = input_line ic in begin
      close_in ic;
      line
    end
  end*)
