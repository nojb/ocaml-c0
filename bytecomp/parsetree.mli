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

open Asttypes

type 'a loc = {
  txt : 'a;
  loc : Location.t
}

type tp =
  | Ptyp_bool
  | Ptyp_char
  | Ptyp_int
  | Ptyp_string
  | Ptyp_pointer of tp
  | Ptyp_array of tp
  | Ptyp_struct of string loc
  | Ptyp_name of string loc

type expr =
  | Pexp_const of constant
  | Pexp_ident of string loc
  | Pexp_getfield of expr loc * string loc
  | Pexp_get of expr loc * expr loc
  | Pexp_getptr of expr loc
  | Pexp_binop of expr loc * binary_operator * expr loc
  | Pexp_unop of unary_operator * expr loc
  | Pexp_cond of expr loc * expr loc * expr loc
  | Pexp_call of string loc * expr loc list
  | Pexp_alloc of tp
  | Pexp_allocarray of tp * expr loc

type stmt =
  | Pstm_empty
  | Pstm_assign of string loc * asnop * expr loc
  | Pstm_setfield of expr loc * string loc * asnop * expr loc
  | Pstm_set of expr loc * expr loc * asnop * expr loc
  | Pstm_setptr of expr loc * asnop * expr loc
  | Pstm_expr of expr loc
  | Pstm_def of tp * string loc * expr loc option * stmt
  | Pstm_ifthenelse of expr loc * stmt * stmt
  | Pstm_while of expr loc * stmt
  (* | SFor of stmt * expr loc * stmt * stmt *)
  | Pstm_return of expr loc option
  | Pstm_seq of stmt * stmt
  | Pstm_assert of expr loc
  | Pstm_error of expr loc
  | Pstm_break
  | Pstm_continue

type function_definition = {
  pfun_name : string loc;
  pfun_arguments : (tp * string loc) list;
  pfun_return_type : tp option;
  pfun_body : stmt
}
