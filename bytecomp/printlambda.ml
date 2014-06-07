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
open Asttypes
open Lambda

let print_primitive ppf p f e =
  match p with
  | Palloc n -> fprintf ppf "alloc(%i,@ %a)" n f e
  | Pallocarray n -> fprintf ppf "allocarray(%i,@ %a)" n f e
  | Perror l -> fprintf ppf "error(%i,@ %a)" l f e

type associativity = LtoR | RtoL | NA

let precedence = function
  | Lconst _
  | Lstackaddr _ -> (16, NA)
  | Lprim _
  | Lload _ -> (15, RtoL)
  | Lbinop (_, Bop_arith Aop_mul, _)
  | Lbinop (_, Bop_arith Aop_div, _)
  | Lbinop (_, Bop_arith Aop_mod, _) -> (13, LtoR)
  | Lbinop (_, Bop_arith Aop_add, _)
  | Lbinop (_, Bop_arith Aop_sub, _) -> (12, LtoR)
  | Lbinop (_, Bop_arith Aop_lsl, _)
  | Lbinop (_, Bop_arith Aop_asr, _) -> (10, LtoR)
  | Lbinop (_, Bop_cmp _, _) -> (10, LtoR)
  | Lbinop (_, Bop_arith Aop_and, _) -> (8, LtoR)
  | Lbinop (_, Bop_arith Aop_xor, _) -> (7, LtoR)
  | Lbinop (_, Bop_arith Aop_or, _) -> (6, LtoR)

let rec expr1 ppf (prec, e) =
  let prec', assoc = precedence e in
  let prec1, prec2 =
    if assoc = LtoR then (prec', prec' + 1) else (prec' + 1, prec')
  in
  if prec' < prec then fprintf ppf "@[<hov 2>(" else fprintf ppf "@[<hov 2>";
  begin match e with
  | Lconst cst ->
    print_constant ppf cst
  | Lstackaddr off ->
    fprintf ppf "&%i" off
  | Lload e ->
    fprintf ppf "[%a]" expr e
  | Lprim (p, el) ->
    print_primitive ppf p args el
  | Lcall (id, el) ->
    fprintf ppf "%a(%a)" Ident.print id args el
  | Lcond (e1, e2, e3) ->
    fprintf ppf "%a ?@ %a :@ %a" expr e1 expr e2 expr e3
  | Lbinop (e1, op, e2) ->
    fprintf ppf "%a@ %s %a" expr1 (prec1, e1) (string_of_binary_operator op) expr1 (prec2, e2)
  | Lunop (op, e) ->
    fprintf ppf "%s %a" (string_of_unary_operator op) expr1 (prec', e)
  end;
  if prec' < prec then fprintf ppf ")@]" else fprintf ppf "@]"
      
and args ppf = function
  | [] -> ()
  | e :: [] ->
    expr ppf e
  | e :: el ->
    fprintf ppf "%a" expr e;
    List.iter (fun e -> fprintf ppf ",@ %a" expr e) el

and expr ppf e =
  expr1 ppf (0, e)

let rec stmt ppf = function
  | Lempty -> ()
  | Lstore (e1, e2) ->
    fprintf ppf "@[<hv 2>[%a] =@ %a;@]" expr e1 expr e2
  | Lexpr e ->
    fprintf ppf "%a;" expr e
  | Lifthenelse (e1, s1, s2) ->
    fprintf ppf "@[<v 2>if (%a) {@ %a@;<1 -2>} else {@ %a@;<1 -2>}@]"
      expr e1 stmt s1 stmt s2
  | Lloop s ->
    fprintf ppf "@[<v 2>loop {@ %a@;<1 -2>}@]" stmt s
  | Lblock s ->
    fprintf ppf "@[<v 3>{{ %a@;<0 -3>}}@]" stmt s
  | Lexit n ->
    fprintf ppf "exit %i;" n
  | Lseq (s1, s2) ->
    fprintf ppf "%a@ %a" seq s1 seq s2
  | Lreturn None ->
    fprintf ppf "return;"
  | Lreturn (Some e) ->
    fprintf ppf "return %a;" expr e

and seq ppf = function
  | Lseq (s1, s2) ->
    fprintf ppf "%a@ %a" seq s1 seq s2
  | _ as s ->
    stmt ppf s

let lambda_fun ppf (Lfun (id, args, body)) =
  let prargs ppf args = List.iter (fun arg -> fprintf ppf "@ %a" Ident.print arg) args in
  fprintf ppf "@[<v>%a@,{@;<0 2>@[<v>%a@]@,}@]@." Ident.print id stmt body

let program ppf fns =
  List.iter (lambda_fun ppf) fns
