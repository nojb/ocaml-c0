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
  | Ptyp_void
  | Ptyp_bool
  | Ptyp_char
  | Ptyp_int
  | Ptyp_string
  | Ptyp_pointer of tp
  | Ptyp_array of tp
  | Ptyp_struct of string loc
  | Ptyp_name of string loc

type rexpr =
  | Pexp_const of constant
  | Pexp_binop of rexpr loc * binary_operator * rexpr loc
  | Pexp_unop of unary_operator * rexpr loc
  | Pexp_cond of rexpr loc * rexpr loc * rexpr loc
  | Pexp_call of string loc * rexpr loc list
  | Pexp_alloc of tp
  | Pexp_allocarray of tp * rexpr loc
  | Pexp_valof of lexpr loc

and lexpr =
  | Pexp_ident of string loc
  | Pexp_index of rexpr loc * rexpr loc
  | Pexp_field of rexpr loc * string loc
  | Pexp_deref of rexpr loc
        
type stmt =
  | Pstm_empty
  | Pstm_assign of lexpr loc * rexpr loc
  | Pstm_assignop of lexpr loc * arith_operator * rexpr loc
  | Pstm_expr of rexpr loc
  | Pstm_def of tp * string loc * rexpr loc option * stmt
  | Pstm_ifthenelse of rexpr loc * stmt * stmt
  | Pstm_while of rexpr loc * stmt
  (* | SFor of stmt * expr loc * stmt * stmt *)
  | Pstm_return of rexpr loc option
  | Pstm_seq of stmt * stmt
  | Pstm_assert of rexpr loc
  | Pstm_error of rexpr loc
  | Pstm_break
  | Pstm_continue

type defn =
  | Pdef_struct of string loc * (tp * string loc) list
  | Pdef_fun of tp * string loc * (tp * string loc) list * stmt
  | Pdef_type of tp * string loc

type decl =
  | Pdec_fun of tp * string loc * (tp * string loc) list
  | Pdec_struct of string loc
