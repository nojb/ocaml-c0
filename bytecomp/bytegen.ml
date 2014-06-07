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
open Lambda
open Instruct

let optimise = ref true

let label_counter = ref 0
let max_stack_used = ref 0

let new_label () =
  incr label_counter;
  !label_counter

module IdentMap = Map.Make (Ident)

type env = int IdentMap.t

let find id env =
  try IdentMap.find id env
  with Not_found -> failwith (Format.sprintf "Bytegen.find: '%s' not found" (Ident.to_string id))

let add_var id pos env =
  IdentMap.add id pos env

let label_code cont =
  if !optimise then
    match cont with
    | Kbranch lbl :: _ as cont -> (lbl, cont)
    | Klabel lbl :: _ as cont -> (lbl, cont)
    | cont -> let lbl = new_label () in (lbl, Klabel lbl :: cont)
  else
    let lbl = new_label () in (lbl, Klabel lbl :: cont)

let branch_to label cont =
  if !optimise then
    match cont with
    | Klabel label0 :: _ when label = label0 -> cont
    | _ -> Kbranch label :: cont
  else
    Kbranch label :: cont

let rec add_pop n cont =
  if !optimise then
    if n = 0 then cont
    else match cont with
      | Kpop m :: cont -> add_pop (n + m) cont
      | Kreturn m :: cont -> Kreturn (n + m) :: cont
      | _ -> Kpop n :: cont
  else
    Kpop n :: cont
    
let rec discard_dead_code cont =
  if !optimise then
    match cont with
    | [] -> []
    | Klabel _ :: _ as cont -> cont
    | _ :: cont -> discard_dead_code cont
  else
    cont

let rec is_tailcall = function
  | Kreturn _ :: _ -> true
  | Klabel _ :: c -> is_tailcall c
  | Kpop _ :: c -> is_tailcall c
  | _ -> false

let comp_primitive = function
  | Pallocarray sz -> Kallocarray sz
  | _ -> failwith "comp_primitive: not implemented"

let comp_binop = function
  | Bop_arith Aop_add -> Kaddint
  | Bop_arith Aop_sub -> Ksubint
  | Bop_arith Aop_mul -> Kmulint
  | Bop_arith Aop_div -> Kdivint
  | Bop_arith Aop_mod -> Kmodint
  | Bop_arith Aop_lsl -> Klslint
  | Bop_arith Aop_asr -> Kasrint
  | Bop_arith Aop_and -> Kandint
  | Bop_arith Aop_or -> Korint
  | Bop_arith Aop_xor -> Kxorint
  
let rec comp_expr env e cont =
  match e with
  | Lconst cst ->
    Kconst cst :: cont
  | Lstackaddr off ->
    Kaccess off :: cont
  | Lload (Lstackaddr off) ->
    Kloadi off :: cont
  | Lload e ->
    comp_expr env e (Kload :: cont)
  | Lprim (p, el) ->
    comp_args env el (comp_primitive p :: cont)
  | Lcall (f, []) ->
    let lbl = find f env in
    let lbl_cont, cont = label_code cont in
    if is_tailcall cont then
      Ktailcall (0, lbl) :: discard_dead_code cont
    else
      Kpush_retaddr lbl_cont :: Kcall (0, lbl) :: cont
  | Lcall (f, el) ->
    let nargs = List.length el in
    let lbl = find f env in
    if is_tailcall cont then
      comp_args env el
        (Kpush :: Ktailcall (nargs, lbl) :: discard_dead_code cont)
    else
      let lbl_cont, cont = label_code cont in
      Kpush_retaddr lbl_cont :: comp_args env el (Kpush :: Kcall (nargs, lbl) :: cont)
  | Lcond (e1, e2, e3) ->
    comp_cond env e1 e2 e3 cont
  | Lbinop (e1, op, e2) ->
    comp_expr env e2 (comp_expr env e1 (comp_binop op :: cont))
    
and comp_args env argl cont =
  comp_expr_list env (List.rev argl) cont

and comp_expr_list env argl cont =
  match argl with
  | [] -> cont
  | exp :: [] ->
    comp_expr env exp cont
  | exp :: exps ->
    comp_expr env exp (Kpush :: comp_args env exps cont)

and comp_cond env cond ifso ifnot cont =
  (* FIXME - optimise *)
  let lbl_cont, cont = label_code cont in
  let lbl_ifno, ifno = label_code (comp_expr env ifnot cont) in
  comp_expr env cond
    (Kbranchifnot lbl_ifno :: comp_expr env ifso (branch_to lbl_cont ifno))

let rec comp_stmt env s lexit cont =
  match s with
  | Lempty ->
    cont
  | Lstore (Lstackaddr off, e) ->
    comp_expr env e (Kstorei off :: cont)
  | Lstore (e1, e2) ->
    comp_expr env e1 (Kpush :: comp_expr env e2 (Kstore :: cont))
  | Lexpr e ->
    comp_expr env e cont
  | Lifthenelse (e1, e2, e3) ->
    comp_binary_test env e1 e2 e3 lexit cont
  | Lloop s ->
    let lbl = new_label () in
    Klabel lbl :: comp_stmt env s lexit (Kbranch lbl :: discard_dead_code cont)
  | Lblock s ->
    let lbl, cont = label_code cont in
    comp_stmt env s (lbl :: lexit) cont
  | Lexit n ->
    branch_to (List.nth lexit n) (discard_dead_code cont)
  | Lseq (e1, e2) ->
    comp_stmt env e1 lexit (comp_stmt env e2 lexit cont)
  | Lreturn None ->
    Kreturn (-1) (* arity *) :: discard_dead_code cont
  | Lreturn (Some e) ->
    comp_expr env e (Kreturn (-1) (* arity *) :: discard_dead_code cont)

and comp_binary_test env cond ifso ifnot lexit cont =
  (* FIXME - optimise *)
  let lbl_cont, cont = label_code cont in
  let lbl_ifno, ifno = label_code (comp_stmt env ifnot lexit cont) in
  comp_expr env cond
    (Kbranchifnot lbl_ifno :: comp_stmt env ifso lexit (branch_to lbl_cont ifno))

let comp_function env lbl (Lfun (id, args, body)) cont =
  (* let arity = args in *)
  Klabel lbl :: comp_stmt env body [] cont
  
let compile_program fns =
  label_counter := 0;
  max_stack_used := 0;
  let fns = List.map (fun fn -> new_label (), fn) fns in
  let env =
    List.fold_left (fun env (lbl, Lfun (id, _, _)) -> IdentMap.add id lbl env)
      IdentMap.empty fns
  in
  List.fold_left (fun cont (lbl, fn) -> comp_function env lbl fn cont) [Kstop] fns
