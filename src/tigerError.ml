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
  | ImmutableAssignment of string
  | BadArgumentCount
  | BadFieldName
  | BadNil
  | TypeNotFound of string
  | VariableNotFound of string
  | ArrayTypeExpected
  | RecordTypeExpected
  | FunctionNotFound of string
  | ArrayExpected
  | FieldNotFound of string
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
  | ImmutableAssignment name ->
      fprintf ppf "Variable %s cannot be assigned to" name
  | BadArgumentCount ->
      fprintf ppf "Wrong number of arguments"
  | BadFieldName ->
      fprintf ppf "Wrong field name"
  | BadNil ->
      fprintf ppf "Illegal use of `nil'; type cannot be determined"
  | TypeNotFound name ->
      fprintf ppf "Type %s not found" name
  | VariableNotFound name ->
      fprintf ppf "Variable %s not found" name
  | ArrayTypeExpected ->
      fprintf ppf "Array type name expected"
  | RecordTypeExpected ->
      fprintf ppf "Record type name expected"
  | FunctionNotFound name ->
      fprintf ppf "Function %s not found" name
  | ArrayExpected ->
      fprintf ppf "Array expression expected"
  | FieldNotFound name ->
      fprintf ppf "Field %s not found" name
  | RecordExpected ->
      fprintf ppf "Record expression expected"
  | IntExpected ->
      fprintf ppf "Integer expression expected"
  | UnitExpected ->
      fprintf ppf "Unit expression expected"
  | BadBreak ->
      fprintf ppf "Illegal use of `break'; can only appear inside a loop"
  | BadParse ->
      fprintf ppf "Parsing error"
  | IntOrStringExpected ->
      fprintf ppf "Int or String expected"