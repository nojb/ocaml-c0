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

open Format
open Lexing
open Location
open Asttypes
open Parsetree

let fmt_position ppf loc =
  fprintf ppf "%s[%d,%d+%d]" loc.pos_fname loc.pos_lnum loc.pos_bol (loc.pos_cnum - loc.pos_bol)

let fmt_location ppf loc =
  fprintf ppf "(%a..%a)" fmt_position loc.loc_start fmt_position loc.loc_end
    
let fmt_string_loc ppf x =
  fprintf ppf "\"%s\" %a" x.txt fmt_location x.loc
    
let line i f s (* ... *) =
  fprintf f "%s" (String.make ((2 * i) mod 72) ' ');
  fprintf f s (* ... *)

let string_loc i ppf id =
  line i ppf "%a\n" fmt_string_loc id

let rec list i f ppf l =
  match l with
  | [] -> line i ppf "[]\n"
  | _ :: _ ->
    line i ppf "[\n";
    List.iter (f (i+1) ppf) l;
    line i ppf "]\n"

let option i f ppf = function
  | None ->
    line i ppf "None\n";
  | Some x ->
    line i ppf "Some\n";
    f (i+1) ppf x

(* let record_type_field i ppf (id, tid) = *)
(*   line i ppf "%a %a\n" fmt_string_loc id fmt_string_loc tid *)

(* let type_expr i ppf typ = *)
(*   match typ with *)
(*   | Ptyp_name id -> *)
(*     line i ppf "Ptyp_name %a\n" fmt_string_loc id *)
(*   | Ptyp_record fields -> *)
(*     line i ppf "Ptyp_record\n"; *)
(*     list i record_type_field ppf fields *)
(*   | Ptyp_array id -> *)
(*     line i ppf "Ptyp_array %a\n" fmt_string_loc id *)

(* let typ_decl i ppf (id, typ) = *)
(*   string_loc i ppf id; *)
(*   type_expr (i+1) ppf typ *)

let rec tp i ppf t =
  line i ppf "type\n";
  let i = i+1 in
  match t with
  | Ptyp_int ->
    line i ppf "Ptyp_int\n"
  | Ptyp_bool ->
    line i ppf "Ptyp_bool\n"
  | Ptyp_string ->
    line i ppf "Ptyp_string\n"
  | Ptyp_char ->
    line i ppf "Ptyp_char\n"
  | Ptyp_pointer t ->
    line i ppf "Ptyp_pointer\n";
    tp i ppf t
  | Ptyp_array t ->
    line i ppf "Ptyp_array\n";
    tp i ppf t
  | Ptyp_struct id ->
    line i ppf "Ptyp_struct %a\n" fmt_string_loc id
  | Ptyp_name id ->
    line i ppf "Ptyp_name %a\n" fmt_string_loc id

(* let string_of_arithop = function *)
(*   | Add -> "+" *)
(*   | Sub -> "-" *)
(*   | Mul -> "*" *)
(*   | Div -> "/" *)
(*   | Mod -> "%" *)
(*   | Lsl -> "<<" *)
(*   | Asr -> ">>" *)
(*   | And -> "&" *)
(*   | Or -> "|" *)
(*   | Xor -> "^" *)

(* let string_of_comparison = function *)
(*   | Eq -> "==" *)
(*   | Le -> "<=" *)
(*   | Ne -> "!=" *)
(*   | Lt -> "<" *)
(*   | Ge -> ">=" *)
(*   | Gt -> ">" *)

(* let string_of_binop = function *)
(*   | Arith op -> string_of_arithop op *)
(*   | Cmp c -> string_of_comparison c *)
(*   | Land -> "&&" *)
(*   | Lor -> "||" *)

(* let string_of_unop = function *)
(*   | Not -> "!" *)
(*   | Lnot -> "~" *)
(*   | Neg -> "-" *)

let rec expr i ppf e =
  line i ppf "expression %a\n" fmt_location e.loc;
  let i = i+1 in
  match e.txt with
  | Pexp_const cst ->
    line i ppf "Pexp_const %a\n" print_constant cst
  | Pexp_ident id ->
    line i ppf "Pexp_ident %a\n" fmt_string_loc id
  | Pexp_binop (e1, op, e2) ->
    line i ppf "Pexp_binop %S\n" (string_of_binary_operator op);
    expr i ppf e1;
    expr i ppf e2
  | Pexp_unop (op, e) ->
    line i ppf "Pexp_unop %S\n" (string_of_unary_operator op);
    expr i ppf e
  | Pexp_cond (e1, e2, e3) ->
    line i ppf "Pexp_cond\n";
    expr i ppf e1;
    expr i ppf e2;
    expr i ppf e3
  | Pexp_call (id, el) ->
    line i ppf "Pexp_call %a\n" fmt_string_loc id;
    list i expr ppf el
  | Pexp_getfield (e, id) ->
    line i ppf "Pexp_getfield %a\n" fmt_string_loc id;
    expr i ppf e
  | Pexp_get (e1, e2) ->
    line i ppf "Pexp_get\n";
    expr i ppf e1;
    expr i ppf e2
  | Pexp_load e ->
    line i ppf "Pexp_load\n";
    expr i ppf e
  | Pexp_alloc t ->
    line i ppf "Pexp_alloc\n";
    tp i ppf t
  | Pexp_allocarray (t, e) ->
    line i ppf "Pexp_allocarray\n";
    tp i ppf t;
    expr i ppf e

let string_of_asnop = function
  | ArithAssign op ->
    string_of_arith_operator op ^ "="
  | Assign -> "="

let rec stmt i ppf s =
  line i ppf "statement\n";
  let i = i+1 in
  match s with
  | Pstm_empty ->
    line i ppf "Pstm_empty\n"
  | Pstm_assign (id, op, e) ->
    line i ppf "Pstm_assign %a %S\n" fmt_string_loc id (string_of_asnop op);
    expr i ppf e
  | Pstm_setfield (e1, id, op, e2) ->
    line i ppf "Pstm_setfield %a %S\n" fmt_string_loc id (string_of_asnop op);
    expr i ppf e1;
    expr i ppf e2
  | Pstm_set (e1, e2, op, e3) ->
    line i ppf "Pstm_set %S\n" (string_of_asnop op);
    expr i ppf e1;
    expr i ppf e2;
    expr i ppf e3
  | Pstm_store (e1, op, e2) ->
    line i ppf "Pstm_store %S\n" (string_of_asnop op);
    expr i ppf e1;
    expr i ppf e2
  | Pstm_expr e ->
    line i ppf "Pstm_expr\n";
    expr i ppf e
  | Pstm_def (t, id, e, s) ->
    line i ppf "Pstm_def %a\n" fmt_string_loc id;
    tp i ppf t;
    option i expr ppf e;
    stmt i ppf s
  | Pstm_ifthenelse (e, s1, s2) ->
    line i ppf "Pstm_ifthenelse\n";
    expr i ppf e;
    stmt i ppf s1;
    stmt i ppf s2
  | Pstm_while (e, s) ->
    line i ppf "Pstm_while\n";
    expr i ppf e;
    stmt i ppf s
  | Pstm_return e ->
    line i ppf "Pstm_return\n";
    option i expr ppf e
  | Pstm_seq (s1, s2) ->
    line i ppf "Pstm_seq\n";
    stmt i ppf s1;
    stmt i ppf s2
  | Pstm_assert e ->
    line i ppf "Pstm_assert\n";
    expr i ppf e
  | Pstm_error e ->
    line i ppf "Pstm_error\n";
    expr i ppf e
  | Pstm_break ->
    line i ppf "Pstm_break\n"
  | Pstm_continue ->
    line i ppf "Pstm_continue\n"
      
  (* | Pexp_get (e1, e2) -> *)
(*     line i ppf "Pexp_get\n"; *)
(*     expr i ppf e1; *)
(*     expr i ppf e2 *)
(*   | Pexp_set (e1, e2, e3) -> *)
(*     line i ppf "Pexp_set\n"; *)
(*     expr i ppf e1; *)
(*     expr i ppf e2; *)
(*     expr i ppf e3 *)
(*   | Pexp_getfield (e, id) -> *)
(*     line i ppf "Pexp_getfield %a\n" fmt_string_loc id; *)
(*     expr i ppf e *)
(*   | Pexp_setfield (e1, id, e2) -> *)
(*     line i ppf "Pexp_setfield %a\n" fmt_string_loc id; *)
(*     expr i ppf e1; *)
(*     expr i ppf e2 *)
(*   | Pexp_ifthenelse (e1, e2, e3) -> *)
(*     line i ppf "Pexp_ifthenelse\n"; *)
(*     expr i ppf e1; *)
(*     expr i ppf e2; *)
(*     option i expr ppf e3 *)
(*   | Pexp_while (e1, e2) -> *)
(*     line i ppf "Pexp_while\n"; *)
(*     expr i ppf e1; *)
(*     expr i ppf e2 *)
(*   | Pexp_for (id, e1, e2, e3) -> *)
(*     line i ppf "Pexp_for %a\n" fmt_string_loc id; *)
(*     expr i ppf e1; *)
(*     expr i ppf e2; *)
(*     expr i ppf e3 *)
(*   | Pexp_sequence (e1, e2) -> *)
(*     line i ppf "Pexp_sequence\n"; *)
(*     expr i ppf e1; *)
(*     expr i ppf e2 *)
(*   | Pexp_unit -> *)
(*     line i ppf "Pexp_unit\n" *)
(*   | Pexp_break -> *)
(*     line i ppf "Pexp_break\n" *)
(*   | Pexp_nil -> *)
(*     line i ppf "Pexp_nil\n" *)
(*   | Pexp_let (d, e) -> *)
(*     line i ppf "Pexp_let\n"; *)
(*     list i decl ppf d; *)
(*     expr i ppf e *)
(*   | Pexp_array (id, e1, e2) -> *)
(*     line i ppf "Pexp_array %a\n" fmt_string_loc id; *)
(*     expr i ppf e1; *)
(*     expr i ppf e2 *)
(*   | Pexp_record (id, fields) -> *)
(*     line i ppf "Pexp_record %a\n" fmt_string_loc id; *)
(*     list i record_field ppf fields *)
(*   | Pexp_call (id, args) -> *)
(*     line i ppf "Pexp_call %a\n" fmt_string_loc id; *)
(*     list i expr ppf args *)
(*   | Pexp_binary (e1, op, e2) -> *)
(*     line i ppf "Pexp_binary %S\n" (string_of_binary_operator op); *)
(*     expr i ppf e1; *)
(*     expr i ppf e2 *)
(*   | Pexp_unary (op, e) -> *)
(*     line i ppf "Pexp_unary %S\n" (string_of_unary_operator op); *)
(*     expr i ppf e *)
  
(* and record_field i ppf (id, e) = *)
(*   line i ppf "%a\n" fmt_string_loc id; *)
(*   expr i ppf e *)

(* and decl i ppf d = *)
(*   line i ppf "declaration %a\n" fmt_location d.loc; *)
(*   let i = i+1 in *)
(*   match d.txt with *)
(*   | Pdec_variable (id, tid, e) -> *)
(*     line i ppf "Pdec_variable %a\n" fmt_string_loc id; *)
(*     option i string_loc ppf tid; *)
(*     expr i ppf e *)
(*   | Pdec_type typs -> *)
(*     line i ppf "Pdec_type\n"; *)
(*     list i typ_decl ppf typs *)
(*   | Pdec_function fns -> *)
(*     line i ppf "Pdec_function\n"; *)
(*     list i fun_decl ppf fns *)

(* and fun_decl i ppf fn = *)
(*   line i ppf "<function>\n"; *)
(*   let i=i+1 in *)
(*   line i ppf "pfun_name = %a\n" fmt_string_loc fn.pfun_name; *)
(*   line i ppf "pfun_arguments =\n"; *)
(*   list (i+1) fun_arg ppf fn.pfun_arguments; *)
(*   line i ppf "pfun_return_type =\n"; *)
(*   option (i+1) string_loc ppf fn.pfun_return_type; *)
(*   line i ppf "pfun_body =\n"; *)
(*   expr (i+1) ppf fn.pfun_body *)

(* and fun_arg i ppf (id, tid) = *)
(*   line i ppf "<argument>\n"; *)
(*   let i=i+1 in *)
(*   string_loc i ppf id; *)
(*   string_loc i ppf tid *)

let program ppf s =
  fprintf ppf "@[%a@]" (stmt 0) s
