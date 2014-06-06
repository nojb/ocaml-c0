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

type constant =
  | Const_bool of bool
  | Const_char of char
  | Const_int of nativeint
  | Const_string of string
  | Const_null

type comparison =
  | Ceq | Cneq | Clt | Cgt | Cle | Cge

type arith_operator =
  | Aop_add | Aop_sub | Aop_mul | Aop_div | Aop_mod
  | Aop_lsl | Aop_asr | Aop_and | Aop_or | Aop_xor

(* type logic_operator = *)
(*   | Lop_and *)
(*   | Lop_or *)

type asnop =
  | ArithAssign of arith_operator
  | Assign

val string_of_asnop : asnop -> string

type binary_operator =
  | Bop_arith of arith_operator
  (* | Bop_logic of logic_operator *)
  | Bop_cmp of comparison

val string_of_arith_operator : arith_operator -> string

val string_of_binary_operator : binary_operator -> string
  
type unary_operator =
  | Uop_neg
  | Uop_not
  | Uop_lnot

val string_of_unary_operator : unary_operator -> string

open Format

val print_constant : formatter -> constant -> unit
  
