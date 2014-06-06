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

(** Stack layout (grows downwards):

    argument n
    argument n-1
    ...
    argument 2
    argument 1
    saved static link
    returns address
    dynamic link (old fp) <- fp, sp at function entry
    local 1
    local 2 <- sp
    garbage

    so argument i is at fp + 2 + i and local i is at fp - i

    returns (lvl, n):
      pc <- *(fp-1);
      fp <- *fp;
      D[lvl] <- *(fp-2)
      sp -= n + 3

    call n, L:
      *++sp = D[n];
      *++sp = pc;
      *++sp = fp;
      fp <- sp;
      D[n] <- fp;
      pc <- L

    access (i, j):
      a <- *(D[i] + j)
*)

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

type function_to_compile = {
  params : Ident.t list;
  body : lambda;
  level : int;
  label : label;
  env : env
}

let functions_to_compile : function_to_compile Stack.t = Stack.create ()

let comp_primitive p args =
  match p with
  | Pmulint -> Kmulint
  | Pdivint -> Kdivint
  | Psubint -> Ksubint
  | Paddint -> Kaddint
  | Pnegint -> Knegint
  | Pget l -> Kget l
  | Pset l -> Kset l
  | Pgetfield i -> Kgetfield i
  | Psetfield i -> Ksetfield i
  | Pallocarray sz -> Kallocarray sz

let comp_arithop = function
  | Aop_add -> Kaddint
  | Aop_sub -> Ksubint
  | Aop_mul -> Kmulint

let rec comp_expr env e sz lexit cont =
  if sz > !max_stack_used then max_stack_used := sz;
  match e with
  | Lconst cst ->
    Kconst cst :: cont
  | Lident id ->
    let pos = find id env in
    Kaccess (sz - pos) :: cont
  | Lassign (id, Assign, e) ->
    let pos = find id env in
    comp_expr env e sz lexit (Kassign (sz - pos) :: cont)
  | Lassign (id, ArithAssign op, e) ->
    let pos = find id env in
    Kaccess (sz - pos) :: Kpush :: comp_expr env e (sz+1) lexit
      (comp_arithop op :: Kassign (sz - pos) :: cont)
  | Lifthenelse (e1, e2, e3) ->
    comp_binary_test env e1 e2 e3 sz lexit cont
  | Lprim (p, args) ->
    comp_args env args sz lexit (comp_primitive p args :: cont)
  | Lcall (f, el) -> (* FIXME *)
    let lbl = find f env in
    comp_args env el sz lexit (Kpush :: Kcall lbl :: cont)
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
  (* | Lletrec (fns, e) -> *)
  (*   let labels = List.map (fun _ -> new_label ()) fns in *)
  (*   let funid (Lfunction (id, _, _)) = id in *)
  (*   let env = *)
  (*     List.fold_right2 (fun fn lbl env -> add_var (funid fn) (lvl+1) lbl env) fns labels env *)
  (*   in *)
  (*   let comp_fun (Lfunction (_, params, body)) label = *)
  (*     let to_compile = { params; body; level = lvl+1; label; env } in *)
  (*     Stack.push to_compile functions_to_compile *)
  (*   in *)
  (*   List.iter2 comp_fun fns labels; *)
(*   comp_expr env e lvl sz break cont *)

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

(* let comp_function fc cont = *)
(*   let arity = List.length fc.params in *)
(*   let rec positions env pos = function *)
(*     | [] -> env *)
(*     | id :: rem -> IdentMap.add id (fc.level, pos) (positions env (pos-1) rem) *)
(*   in *)
(*   let env = positions fc.env (-3) fc.params in *)
(*   let cont = comp_expr env fc.body fc.level 1 None (Kreturn arity :: cont) in *)
(*   Klabel fc.label :: cont *)

(* let comp_remainder cont = *)
(*   let rec loop cont = *)
(*     if Stack.is_empty functions_to_compile then cont *)
(*     else loop (comp_function (Stack.pop functions_to_compile) cont) *)
(*   in *)
(*   loop cont *)

let compile_program expr =
  label_counter := 0;
  max_stack_used := 0;
  Stack.clear functions_to_compile;
  let prog = comp_expr IdentMap.empty expr 0 [] [] in
  let label, prog = label_code prog in
  Kcall label :: Kstop :: (* comp_remainder *) prog
