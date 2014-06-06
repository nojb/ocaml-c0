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

open Misc
open Asttypes
open Parsetree
open Lambda
open Types

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

let type_of_constant = function
  | Const_bool _ -> Tbool
  | Const_char _ -> Tchar
  | Const_int _ -> Tint
  | Const_string _ -> Tstring
  | Const_null -> Tnull
    
type value_description =
  | Val_fun of type_expr list * type_expr * Ident.t
  | Val_var of type_expr * Ident.t
  | Val_type of type_expr

type struct_body =
  | Unfilled
  | Filled of (string * type_expr) list

type venv = value_description Tbl.t
type tenv = struct_body Tbl.t

let add_var id t venv =
  let id1 = Ident.fresh id.txt in
  id1, Tbl.add id.txt (Val_var (t, id1)) venv

let find_var id venv =
  try
    match Tbl.find id.txt venv with
    | Val_var (t, id) -> id, t
    | _ -> raise Not_found
  with
  | Not_found ->
    raise (Error (id.loc, Variable_not_found id.txt))

let find_fun id venv =
  try
    match Tbl.find id.txt venv with
    | Val_fun (ts, t, id) -> ts, t, id
    | _ -> raise Not_found
  with
  | Not_found ->
    raise (Error (id.loc, Function_not_found id.txt))

let find_type id venv =
  try
    match Tbl.find id.txt venv with
    | Val_type t -> t
    | _ -> raise Not_found
  with
  | Not_found ->
    raise (Error (id.loc, Type_not_found id.txt))

let add_fun id ts t venv =
  Tbl.add id.txt (Val_fun (ts, t, Ident.fresh id.txt)) venv

let add_type id t venv =
  Tbl.add id.txt (Val_type t) venv

let index_of tenv sid fid =
  let rec loop i = function
    | [] ->
      raise (Error (fid.loc, Field_not_found fid.txt))
    | (id1, t) :: flds ->
      if id1 = fid.txt then (t, i) else loop (i+1) flds
  in
  try
    match Tbl.find sid.txt tenv with
    | Filled fields ->
      loop 0 fields
    | Unfilled ->
      raise Not_found
  with
  | Not_found ->
    raise (Error (fid.loc, Unfilled_struct sid.txt)) (* FIXME loc *)

let rec tp venv = function
  | Ptyp_bool -> Tbool
  | Ptyp_char -> Tchar
  | Ptyp_int -> Tint
  | Ptyp_string -> Tstring
  | Ptyp_pointer t -> Tpointer (tp venv t)
  | Ptyp_array t -> Tarray (tp venv t)
  | Ptyp_struct id -> Tstruct id
  | Ptyp_name id -> find_type id venv
                      
  (* let typ_name (id, _) = id.txt in *)
  (* begin match list_has_duplicate typ_name tdecs with *)
  (*   | Some (a, _) -> raise (Error (a.loc, Repeated_type a.txt)) *)
  (*   | None -> () *)
  (* end; *)
  (* let find_type_or_forward id tenv = *)
  (*   try Tbl.find id.txt tenv with Not_found -> Tname id *)
  (* in *)
  (* let rec get id1 = function *)
  (*   | Ptyp_name id -> *)
  (*     find_type_or_forward id tenv *)
  (*   | Ptyp_record flds -> *)
  (*     let fld_name (id, _) = id.txt in *)
  (*     begin match list_has_duplicate fld_name flds with *)
  (*       | Some (fld, _) -> *)
  (*         raise (Error (fld.loc, Repeated_field fld.txt)) *)
  (*       | None -> *)
  (*         Trecord (id1.txt, List.map (fun (id, t) -> (id.txt, find_type_or_forward t tenv)) flds) *)
  (*     end *)
  (*   | Ptyp_array id -> *)
  (*     Tarray (id1.txt, find_type_or_forward id tenv) *)
  (* in *)
  (* let rec fixup tenv seen seen_record = function *)
  (*   | Tany *)
  (*   | Tint *)
  (*   | Tunit *)
  (*   | Tstring -> () *)
  (*   | Tname id when List.mem id.txt seen -> *)
  (*     if not seen_record then raise (Error (id.loc, Bad_recursive_type (List.rev seen))) *)
  (*   | Tname id -> *)
  (*     fixup tenv (id.txt :: seen) seen_record (find_type id tenv) *)
  (*   | Trecord (id, flds) -> *)
  (*     List.iter (fun (_, t) -> fixup tenv (id :: seen) true t) flds *)
  (*   | Tarray (id, t) -> *)
  (*     fixup tenv (id :: seen) seen_record t *)
  (* in *)
  (* let tenv = List.fold_left (fun tenv (id, t) -> add_type id (get id t) tenv) tenv tdecs in *)
  (* List.iter (fun (id, _) -> fixup tenv [] false (find_type id tenv)) tdecs; *)
  (* tenv *)

let const_int n = Lconst (Const_int (Nativeint.of_int n))

let default_init tenv = function
  | Tbool
  | Tchar
  | Tnull
  | Tpointer _
  | Tint -> Const_int 0n
  | Tstring -> Const_string ""
  | Tarray t -> failwith "Tarray"
  | _ -> assert false

let rec size_of tenv = function
  | Tbool
  | Tchar
  | Tnull
  | Tpointer _
  | Tint
  | Tstring
  | Tarray _ -> 1
  | Talias (_, t) -> size_of tenv t
  | Tstruct id ->
    try
      match Tbl.find id.txt tenv with
      | Filled fields ->
        List.fold_left (fun acc (_, ftp) -> acc + size_of tenv ftp) 0 fields
      | Unfilled ->
        raise Not_found
    with
    | Not_found ->
      raise (Error (id.loc, Unfilled_struct id.txt)) (* FIXME loc *)

let load_if_small t lam =
  if is_large t then lam else Lprim (Pload, [lam])

let rec expr venv tenv e =
  match e.txt with
  | Pexp_const cst ->
    Lconst cst, type_of_constant cst
  | Pexp_ident vid ->
    let vid, t = find_var vid venv in
    Lident vid, t
  | Pexp_getfield (e, fid) ->
    let rexp, sid = struct_expr venv tenv e in
    let t, i = index_of tenv sid fid in
    load_if_small t (Lprim (Pgetfield i, [rexp])), t
  | Pexp_get (e1, e2) ->
    let aexp, elty = array_expr venv tenv e1 in
    let iexp = expr_with_type Tint venv tenv e2 in
    let lnum = e2.loc.Location.loc_start.Lexing.pos_lnum in
    load_if_small elty (Lprim (Pget lnum, [aexp; iexp])), elty
  | Pexp_load e ->
    let e, t = pointer_expr venv tenv e in
    load_if_small t e, t
  (* | Pexp_binop (e1, Bop_arith op, e2) -> *)
  (*   let e1 = expr_with_type Tint venv tenv inloop e1 in *)
  (*   let e2 = expr_with_type Tint venv tenv inloop e2 in *)
  (*   let p = match op with *)
  (*     | Aop_add -> Paddint *)
  (*     | Aop_sub -> Psubint *)
  (*     | Aop_mul -> Pmulint *)
  (*     | Aop_div -> Pdivint *)
  (*   in *)
  (*   Lprim (p, [e1; e2]), Tint *)
  (* | Pexp_binop (e1, Bop_cmp op, e2) -> *)
  (*   begin match op with *)
  (*     | Ceq *)
  (*     | Cneq -> *)
  (*       let e1, t = expr venv tenv e1 in *)
  (*       begin match t with *)
  (*         | Tbool *)
  (*         | Tchar *)
  (*         | Tint *)
  (*         | Tpointer _ -> *)
  (*           let e2 = expr_with_type t venv tenv e2 in *)
  (*           Lprim (Pintcomp op, [e1; e2]) *)
  (*         | _ -> *)
  (*           failwith "eq neq" *)
  (*       end *)
  (*     | _ -> *)
  (*       let e1 = expr_with_type Tint venv tenv e1 in *)
  (*       let e2 = expr_with_type Tint venv tenv e2 in *)
  (*       Lprim (Pintcomp op, [e1; e2]), Tbool *)
  (*   end *)
  (* | Pexp_unop (Uop_minus, e) -> *)
  (*   let e = expr_with_type Tint venv tenv inloop e in *)
  (*   Lprim (Pnegint, [e]), Tint *)
  | Pexp_cond (e1, e2, e3) ->
    let e1 = expr_with_type Tbool venv tenv e1 in
    let e2, t = small_expr venv tenv e2 in
    let e3 = expr_with_type t venv tenv e3 in
    Lifthenelse (e1, e2, e3), t
  | Pexp_call (id, args) ->
    let atyps, rtyp, id1 = find_fun id venv in
    let check_arg a typ = expr_with_type typ venv tenv a in
    if List.length args <> List.length atyps then
      raise (Error (e.loc, Wrong_arg_count (List.length args, List.length atyps)));
    let args = List.map2 check_arg args atyps in
    Lcall (id1, args), rtyp
  | Pexp_alloc t ->
    let t = tp venv t in
    Lprim (Palloc (size_of tenv t), []), Tpointer t
  | Pexp_allocarray (t, e) ->
    let t = tp venv t in
    let e = expr_with_type Tint venv tenv e in
    Lprim (Pallocarray (size_of tenv t), [e]), Tarray t

and array_expr venv tenv e =
  let lam, t = expr venv tenv e in
  match t with
  | Tarray t -> lam, t
  | _ ->
    raise (Error (e.loc, Array_expr_expected t))

and struct_expr venv tenv e =
  let lam, t = expr venv tenv e in
  match t with
  | Tstruct id -> lam, id
  | _ ->
    raise (Error (e.loc, Record_expr_expected t))

and pointer_expr venv tenv e =
  let lam, t = expr venv tenv e in
  match t with
  | Tpointer t -> lam, t
  | _ ->
    raise (Error (e.loc, Pointer_expr_expected t))

and expr_with_type t venv tenv e =
  let lam, t1 = expr venv tenv e in
  if equal t t1 then lam
  else raise (Error (e.loc, Type_mismatch (t, t1)))

and small_expr venv tenv e =
  let lam, t = expr venv tenv e in
  if is_large t then raise (Error (e.loc, Illegal_large_type t));
  lam, t
  
let rec stmt venv tenv inloop s =
  match s with
  | Pstm_empty -> const_int 0
  | Pstm_assign (id, Assign, e) ->
    let id1, t = find_var id venv in
    if is_large t then raise (Error (id.loc, Illegal_large_type t));
    let e = expr_with_type t venv tenv e in
    Lassign (id1, Assign, e)
  | Pstm_setfield (e1, fid, op, e2) ->
    let e1, sid = struct_expr venv tenv e1 in
    let t, i = index_of tenv sid fid in
    let e2 = expr_with_type t venv tenv e2 in
    Lprim (Psetfield i, [e1; e2])
  | Pstm_set (e1, e2, op, e3) ->
    let aexp, elty = array_expr venv tenv e1 in
    let iexp = expr_with_type Tint venv tenv e2 in
    let oexp = expr_with_type elty venv tenv e3 in
    let lnum = e2.loc.Location.loc_start.Lexing.pos_lnum in
    Lprim (Pset lnum, [aexp; iexp; oexp])
  | Pstm_store (e1, Assign, e2) ->
    let e1, t = pointer_expr venv tenv e1 in
    let e2 = expr_with_type t venv tenv e2 in
    Lprim (Pstore, [e1; e2])
  | Pstm_expr e ->
    let e, _ = expr venv tenv e in
    e
  | Pstm_def (t, id, e, s) ->
    let t = tp venv t in
    if is_large t then raise (Error (id.loc, Illegal_large_type t));
    let e = match e with
      | Some e -> expr_with_type t venv tenv e
      | None -> Lconst (default_init tenv t)
    in
    let v, venv = add_var id t venv in
    let s = stmt venv tenv inloop s in
    Ldef (v, e, s)
  | Pstm_ifthenelse (e, s1, s2) ->
    let e = expr_with_type Tbool venv tenv e in
    let s1 = stmt venv tenv inloop s1 in
    let s2 = stmt venv tenv inloop s2 in
    Lifthenelse (e, s1, s2)
  | Pstm_while (e, s) ->
    let e = expr_with_type Tbool venv tenv e in
    let s = stmt venv tenv true s in
    Lblock (Lloop (Lblock (Lifthenelse (e, s, Lexit 1))))
  | Pstm_seq (s1, s2) ->
    let lam1 = stmt venv tenv inloop s1 in
    let lam2 = stmt venv tenv inloop s2 in
    Lseq (lam1, lam2)
  | Pstm_assert e ->
    let lam = expr_with_type Tbool venv tenv e in
    let lnum = e.loc.Location.loc_start.Lexing.pos_lnum in
    Lprim (Passert lnum, [lam])
  | Pstm_error e ->
    let lam = expr_with_type Tstring venv tenv e in
    let lnum = e.loc.Location.loc_start.Lexing.pos_lnum in
    Lprim (Perror lnum, [lam])
  | Pstm_break ->
    if inloop then Lexit 1
    else raise (Error (Location.dummy, Illegal_break))
  | Pstm_continue ->
    if inloop then Lexit 0
    else raise (Error (Location.dummy, Illegal_continue))

(* and decl (venv : venv) (tenv : tenv) inloop d k = *)
(*   match d.txt with *)
(*   | Pdec_type types -> *)
(*     let tenv = type_declarations tenv types in *)
(*     k venv tenv *)
(*   | Pdec_function fns -> *)
(*     let fun_name fn = fn.pfun_name.txt in *)
(*     begin match list_has_duplicate fun_name fns with *)
(*       | Some fn -> raise (Error (d.loc, Repeated_function fn.pfun_name.txt)) *)
(*       | None -> () *)
(*     end; *)
(*     let add venv fn = *)
(*       let atyps = List.map (fun (_, tid) -> find_type tid tenv) fn.pfun_arguments in *)
(*       let rtyp = match fn.pfun_return_type with None -> Tunit | Some tid -> find_type tid tenv in *)
(*       add_fun fn.pfun_name atyps rtyp venv *)
(*     in *)
(*     let venv = List.fold_left add venv fns in *)
(*     let dofun fn = *)
(*       let atyps, rtyp, kind = find_fun fn.pfun_name venv in *)
(*       let id = match kind with User id -> id | Primitive _ -> assert false in *)
(*       let arg_name (id, _) = id.txt in *)
(*       begin match list_has_duplicate arg_name fn.pfun_arguments with *)
(*         | Some (id, _) -> raise (Error (id.loc, Repeated_argument id.txt)) *)
(*         | None -> () *)
(*       end; *)
(*       let args = List.map (fun (id, tid) -> *)
(*           (Ident.fresh id.txt, find_type tid tenv)) fn.pfun_arguments in *)
(*       let add_arg venv (id, typ) = Tbl.add (Ident.name id) (Val_var (typ, Mutable, id)) venv in *)
(*       let venv = List.fold_left add_arg venv args in *)
(*       let e = expr_with_type rtyp venv tenv false fn.pfun_body in *)
(*       Lfunction (id, List.map fst args, e) *)
(*     in *)
(*     let fns = List.map dofun fns in *)
(*     let e, t = k venv tenv in *)
(*     Lletrec (fns, e), t *)

let program e =
  let lam = stmt Tbl.empty Tbl.empty false e in
  lam

open Format

let report_error ppf loc = function
  | Type_not_found id ->
    Location.report_error loc "type '%s' not found" id
  | Variable_not_found id ->
    Location.report_error loc "variable '%s' not found" id
  | Function_not_found id ->
    Location.report_error loc "function '%s' not found" id
  | Field_not_found id ->
    Location.report_error loc "record field '%s' not found" id
  | Repeated_field id ->
    Location.report_error loc "record field '%s' is repeated" id
  | Repeated_type id ->
    Location.report_error loc "type '%s' is repeated" id
  | Bad_recursive_type ids ->
    Location.report_error loc "recursive type definition must go \
                               through a record type: [%s]" (String.concat " " ids)
  | Repeated_function id ->
    Location.report_error loc "function '%s' is repeated" id
  | Array_type_expected (id, t) ->
    Location.report_error loc "expected '%s' to be an array type" id
  | Record_type_expected (id, t) ->
    Location.report_error loc "expected '%s' to be an record type" id
  | Illegal_break ->
    Location.report_error loc "'break' can only be used inside a loop"
  | Illegal_nil ->
    Location.report_error loc "'nil' must be used in a context where its type can be determined"
  | Array_expr_expected t ->
    Location.report_error loc "expected expression of array type"
  | Record_expr_expected t ->
    Location.report_error loc "expected expression of record type"
  | Type_mismatch (t1, t2) ->
    Location.report_error loc "type mismatch: expected '%a', found '%a'"
      Types.print t1 Types.print t2
  | Immutable_var ->
    Location.report_error loc "this variable is immutable"
  | Wrong_arg_count (found, expected) ->
    Location.report_error loc "this function takes %d arguments, found %d" expected found
  | Repeated_argument id ->
    Location.report_error loc "repated argument '%s'" id
  | Wrong_field_count (found, expected) ->
    Location.report_error loc "this record type has %d fields, found %d" expected found
  | Wrong_field_name (found, expected) ->
    Location.report_error loc "this record field's name should be '%s', is '%s'" expected found
  | Int_or_string_expr_expected t ->
    Location.report_error loc "'string' or 'int' expr expected, found '%a'" Types.print t
