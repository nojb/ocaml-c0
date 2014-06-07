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

let rec branch_to_cont2 lbl n cont = function
  | Klabel _ :: c -> branch_to_cont2 lbl n cont c
  | Kpop m :: c -> branch_to_cont2 lbl (n+m) cont c
  | Kreturn m :: _ -> (Kreturn (n+m), cont)
  | _ ->
    match lbl with
    | Some lbl ->
      Kbranch lbl, cont
    | None ->
      let lbl = new_label () in
      Kbranch lbl, Klabel lbl :: cont

let branch_to_cont cont =
  match cont with
  | (Kbranch _ as br) :: _ -> br, cont
  | (Kreturn _ as ret) :: _ -> ret, cont
  | Klabel lbl :: _ -> branch_to_cont2 (Some lbl) 0 cont cont
  | _ -> branch_to_cont2 None 0 cont cont

let code_will_jump l lexit =
  match l with
  | Lexit n -> Some (List.nth lexit n)
  | _ -> None

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
  
let rec comp_expr env e sz cont =
  match e with
  | Lconst cst ->
    Kconst cst :: cont
  | Lident id ->
    let pos = find id env in
    Kaccess (sz - pos) :: cont
  | Lload (Lident id) ->
    let pos = find id env in
    Kloadi (sz - pos) :: cont
  | Lload e ->
    comp_expr env e sz (Kload :: cont)
  | Lprim (p, el) ->
    comp_args env el sz (comp_primitive p :: cont)
  | Lcall (f, []) ->
    let lbl = find f env in
    let lbl_cont, cont = label_code cont in
    if is_tailcall cont then
      Ktailcall (sz, 0, lbl) :: discard_dead_code cont
    else
      Kpush_retaddr lbl_cont :: Kcall (0, lbl) :: cont
  | Lcall (f, el) ->
    let nargs = List.length el in
    let lbl = find f env in
    if is_tailcall cont then
      comp_args env el sz
        (Kpush :: Ktailcall (sz, nargs, lbl) :: discard_dead_code cont)
    else
      let lbl_cont, cont = label_code cont in
      Kpush_retaddr lbl_cont :: comp_args env el (sz+1) (Kpush :: Kcall (nargs, lbl) :: cont)
  | Lcond (e1, e2, e3) ->
    comp_cond env e1 e2 e3 sz cont
  | Lbinop (e1, op, e2) ->
    comp_expr env e2 sz (Kpush :: comp_expr env e1 (sz+1) (comp_binop op :: cont))
    
and comp_args env argl sz cont =
  comp_expr_list env (List.rev argl) sz cont

and comp_expr_list env argl sz cont =
  match argl with
  | [] -> cont
  | exp :: [] ->
    comp_expr env exp sz cont
  | exp :: exps ->
    comp_expr env exp sz (Kpush :: comp_expr_list env exps (sz+1) cont)

and comp_cond env cond ifso ifnot sz cont =
  let cond_cont =
    if ifnot = Lconst (Const_int 0n) then
      let lbl_end, cont = label_code cont in
      Kbranchifnot lbl_end :: comp_expr env ifso sz cont
    else
      let branch_end, cont1 = branch_to_cont cont in
      let lbl2, cont2 = label_code (comp_expr env ifnot sz cont1) in
      Kbranchifnot lbl2 :: comp_expr env ifso sz (branch_end :: cont2)
  in
  comp_expr env cond sz cond_cont

let rec comp_stmt env s sz lexit cont =
  match s with
  | Lempty ->
    cont
  | Lstore (Lident id, e) ->
    let pos = find id env in
    comp_expr env e sz (Kstorei (sz - pos) :: cont)
  | Lstore (e1, e2) ->
    comp_expr env e1 sz (Kpush :: comp_expr env e2 (sz+1) (Kstore :: cont))
  | Lexpr e ->
    comp_expr env e sz cont
  | Lifthenelse (e1, e2, e3) ->
    comp_binary_test env e1 e2 e3 sz lexit cont
  | Lloop s ->
    let lbl = new_label () in
    Klabel lbl :: comp_stmt env s sz lexit (Kbranch lbl :: discard_dead_code cont)
  | Lblock s ->
    let lbl, cont = label_code cont in
    comp_stmt env s sz (lbl :: lexit) cont
  | Lexit n ->
    branch_to (List.nth lexit n) (discard_dead_code cont)
  | Llet (id, e, s) ->
    comp_expr env e sz (Kpush :: comp_stmt (add_var id sz env) s (sz+1) lexit cont)
  | Lseq (e1, e2) ->
    comp_stmt env e1 sz lexit (comp_stmt env e2 sz lexit cont)
  | Lreturn None ->
    Kreturn sz :: discard_dead_code cont
  | Lreturn (Some e) ->
    comp_expr env e sz (Kreturn sz :: discard_dead_code cont)

and comp_binary_test env cond ifso ifnot sz lexit cont =
  let cond_cont =
    if ifnot = Lempty then
      let lbl_end, cont = label_code cont in
      Kbranchifnot lbl_end :: comp_stmt env ifso sz lexit cont
    else
      match code_will_jump ifso lexit with
      | Some lbl ->
        let cont = comp_stmt env ifnot sz lexit cont in
        Kbranchif lbl :: cont
      | None ->
        match code_will_jump ifnot lexit with
        | Some lbl ->
          let cont = comp_stmt env ifso sz lexit cont in
          Kbranchifnot lbl :: cont
        | None ->
          let branch_end, cont1 = branch_to_cont cont in
          let lbl2, cont2 = label_code (comp_stmt env ifnot sz lexit cont1) in
          Kbranchifnot lbl2 :: comp_stmt env ifso sz lexit (branch_end :: cont2)
  in
  comp_expr env cond sz cond_cont

let comp_function env lbl (Lfun (id, args, body)) cont =
  let arity = List.length args in (* FIXME *)
  let rec add_args env pos = function
    | [] -> env
    | id :: ids -> add_args (add_var id pos env) (pos+1) ids
  in
  let env = add_args env 0 args in
  Klabel lbl :: comp_stmt env body arity [] cont
  
let compile_program fns =
  label_counter := 0;
  max_stack_used := 0;
  let fns = List.map (fun fn -> new_label (), fn) fns in
  let env =
    List.fold_left (fun env (lbl, Lfun (id, _, _)) -> IdentMap.add id lbl env)
      IdentMap.empty fns
  in
  List.fold_left (fun cont (lbl, fn) -> comp_function env lbl fn cont) [Kstop] fns
