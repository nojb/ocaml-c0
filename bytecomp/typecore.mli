(* The MIT License (MIT)

   Copyright (c) 2014 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
   FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
   COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
   IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *)

open Parsetree
open Lambda

type error =
  | Type_not_found of string
  | Variable_not_found of string
  | Function_not_found of string
  | Field_not_found of string
  | Repeated_field of string
  | Repeated_type of string
  | Bad_recursive_type of string list
  | Repeated_function of string
  | Array_type_expected of string * Types.type_expr
  | Record_type_expected of string * Types.type_expr
  | Illegal_break
  | Illegal_nil
  | Array_expr_expected of Types.type_expr
  | Record_expr_expected of Types.type_expr
  | Type_mismatch of Types.type_expr * Types.type_expr
  | Immutable_var
  | Wrong_arg_count of int * int
  | Repeated_argument of string
  | Wrong_field_count of int * int
  | Wrong_field_name of string * string
  | Int_or_string_expr_expected of Types.type_expr
  | Unfilled_struct of string
  | Pointer_expr_expected of Types.type_expr
  | Illegal_large_type of Types.type_expr
  | Illegal_continue

exception Error of Location.t * error

val program : stmt -> lambda

open Format

val report_error : formatter -> Location.t -> error -> unit
