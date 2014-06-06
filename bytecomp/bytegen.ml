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

let comp_primitive = function
  | Paddint -> Kaddint
  | Psubint -> Ksubint
  | Pmulint -> Kmulint
  | Pdivint -> Kdivint
  | Pmodint -> Kmodint
  | Plslint -> Klslint
  | Pasrint -> Kasrint
  | Pandint -> Kandint
  | Porint -> Korint
  | Pxorint -> Kxorint
  | Pnegint -> Knegint
  | Pload -> Kload
  | Pstore -> Kstore
  | Pallocarray sz -> Kallocarray sz
  | _ -> failwith "comp_primitive: not implemented"

let rec comp_expr env e sz lexit cont =
  if sz > !max_stack_used then max_stack_used := sz;
  match e with
  | Lconst cst ->
    Kconst cst :: cont
  | Lident id ->
    let pos = find id env in
    Kaccess (sz - pos) :: cont
  | Lassign (id, e) ->
    let pos = find id env in
    comp_expr env e sz lexit (Kassign (sz - pos) :: cont)
  | Lifthenelse (e1, e2, e3) ->
    comp_binary_test env e1 e2 e3 sz lexit cont
  | Lprim (p, args) ->
    comp_args env args sz lexit (comp_primitive p :: cont)
  | Lcall (f, []) ->
    let lbl = find f env in
    let lbl_cont, cont = label_code cont in
    Kpush_retaddr lbl_cont :: Kcall lbl :: cont
  | Lcall (f, el) ->
    let lbl = find f env in
    let lbl_cont, cont = label_code cont in
    Kpush_retaddr lbl_cont :: comp_args env el sz lexit (Kpush :: Kcall lbl :: cont)
  | Lloop e ->
    let lbl = new_label () in
    Klabel lbl :: comp_expr env e sz lexit (Kbranch lbl :: discard_dead_code cont)
  | Lblock e ->
    let lbl, cont = label_code cont in
    comp_expr env e sz ((sz, lbl) :: lexit) cont
  | Lexit n ->
    let szold, lbl = List.nth lexit n in
    let cont = discard_dead_code cont in
    add_pop (sz - szold) (branch_to lbl cont)
  | Lseq (e1, e2) ->
    comp_expr env e1 sz lexit (comp_expr env e2 sz lexit cont)
  | Ldef (id, e1, e2) ->
    comp_expr env e1 sz lexit
      (Kpush :: comp_expr (add_var id sz env) e2 (sz+1) lexit (add_pop 1 cont))
  | Lreturn None ->
    Kreturn sz :: discard_dead_code cont
  | Lreturn (Some e) ->
    comp_expr env e sz lexit (Kreturn sz :: discard_dead_code cont)

and comp_args env argl sz lexit cont =
  comp_expr_list env (List.rev argl) sz lexit cont

and comp_expr_list env argl sz lexit cont =
  match argl with
  | [] -> cont
  | exp :: [] ->
    comp_expr env exp sz lexit cont
  | exp :: exps ->
    comp_expr env exp sz lexit
      (Kpush :: comp_args env exps (sz+1) lexit cont)

and comp_binary_test env cond ifso ifnot sz lexit cont =
  (* FIXME - optimise *)
  let lbl_cont, cont = label_code cont in
  let lbl_ifno, ifno = label_code (comp_expr env ifnot sz lexit cont) in
  comp_expr env cond sz lexit
    (Kbranchifnot lbl_ifno :: comp_expr env ifso sz lexit (branch_to lbl_cont ifno))

let comp_function env lbl (Lfun (id, args, body)) cont =
  let arity = List.length args in
  let rec positions env pos = function
    | [] -> env
    | id :: rem -> IdentMap.add id pos (positions env (pos+1) rem)
  in
  let env = positions env 0 args in
  Klabel lbl :: comp_expr env body arity [] cont
  
let compile_program fns =
  label_counter := 0;
  max_stack_used := 0;
  let fns = List.map (fun fn -> new_label (), fn) fns in
  let env =
    List.fold_left (fun env (lbl, Lfun (id, _, _)) -> IdentMap.add id lbl env)
      IdentMap.empty fns
  in
  List.fold_left (fun cont (lbl, fn) -> comp_function env lbl fn cont) [Kstop] fns
