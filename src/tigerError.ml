(* Copyright 2014 Nicolas Ojeda Bar <n.oje.bar@gmail.com>.
   All rights reserved.
   Distributed under the Q Public License, version 1.0. *)

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
  | BadTypeCycle

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
  | BadTypeCycle ->
      fprintf ppf "Bad type cycle"
