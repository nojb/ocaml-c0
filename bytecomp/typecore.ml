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
  | Struct_already_defined
      
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
  let id1 = Ident.fresh id.txt in
  id1, Tbl.add id.txt (Val_fun (ts, t, id1)) venv

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
  | Ptyp_void -> Tvoid
  | Ptyp_bool -> Tbool
  | Ptyp_char -> Tchar
  | Ptyp_int -> Tint
  | Ptyp_string -> Tstring
  | Ptyp_pointer t -> Tpointer (tp venv t)
  | Ptyp_array t -> Tarray (tp venv t)
  | Ptyp_struct id -> Tstruct id
  | Ptyp_name id -> find_type id venv
                      
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

let mul e1 e2 =
  match e1, e2 with
  | Lconst (Const_int n), Lconst (Const_int m) -> Lconst (Const_int (Nativeint.mul n m))
  | Lconst (Const_int 1n), e1
  | e1, Lconst (Const_int 1n) -> e1
  | e1, e2 -> Lprim (Pmulint, [e1; e2])

let add e1 e2 =
  match e1, e2 with
  | Lconst (Const_int n), Lconst (Const_int m) -> Lconst (Const_int (Nativeint.add n m))
  | e1, Lconst (Const_int 0n)
  | Lconst (Const_int 0n), e1 -> e1
  | e1, e2 -> Lprim (Paddint, [e1; e2])

let load_if_small t start off =
  if is_large t then add start off
  else Lprim (Pload, [start; off])

let comp_arithop = function
  | Aop_add -> Paddint
  | Aop_sub -> Psubint
  | Aop_mul -> Pmulint
  | Aop_div -> Pdivint

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
    let tsz = size_of tenv t in
    load_if_small t rexp (const_int (tsz * i)), t
  | Pexp_get (e1, e2) ->
    let aexp, elty = array_expr venv tenv e1 in
    let tsz = size_of tenv elty in
    let iexp = expr_with_type Tint venv tenv e2 in
    let lnum = e2.loc.Location.loc_start.Lexing.pos_lnum in
    load_if_small elty aexp (mul iexp (const_int tsz)), elty
  | Pexp_getptr e ->
    let e, t = pointer_expr venv tenv e in
    load_if_small t e (const_int 0), t
  | Pexp_binop (e1, Bop_arith op, e2) ->
    let e1 = expr_with_type Tint venv tenv e1 in
    let e2 = expr_with_type Tint venv tenv e2 in
    Lprim (comp_arithop op, [e1; e2]), Tint
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

let rec stmt rt venv tenv inloop s =
  match s with
  | Pstm_empty -> const_int 0
  | Pstm_assign (id, ArithAssign op, e) ->
    let id1, t = find_var id venv in
    if is_large t then raise (Error (id.loc, Illegal_large_type t));
    let e = expr_with_type t venv tenv e in
    Lassign (id1, Lprim (comp_arithop op, [Lident id1; e]))
  | Pstm_assign (id, Assign, e) ->
    let id1, t = find_var id venv in
    if is_large t then raise (Error (id.loc, Illegal_large_type t));
    let e = expr_with_type t venv tenv e in
    Lassign (id1, e)
  | Pstm_setfield (e1, fid, ArithAssign op, e2) ->
    let e1, sid = struct_expr venv tenv e1 in
    let t, i = index_of tenv sid fid in
    (* FIXME check t = Tint *)
    let e2 = expr_with_type t venv tenv e2 in
    let str = Ident.fresh "str" in
    Ldef (str, e1, Lprim (Pstore, [Lident str; const_int i; Lprim (comp_arithop op, [Lident str; e2])]))
  | Pstm_setfield (e1, fid, Assign, e2) ->
    let e1, sid = struct_expr venv tenv e1 in
    let t, i = index_of tenv sid fid in
    if is_large t then raise (Error (fid.loc, Illegal_large_type t));
    let e2 = expr_with_type t venv tenv e2 in
    Lprim (Pstore, [e1; const_int i; e2])
  | Pstm_set (e1, e2, ArithAssign op, e3) ->
    let aexp, elty = array_expr venv tenv e1 in
    (* FIXME check elty = Tint *)
    let tsz = size_of tenv elty in
    let iexp = expr_with_type Tint venv tenv e2 in
    let oexp = expr_with_type elty venv tenv e3 in
    let lnum = e2.loc.Location.loc_start.Lexing.pos_lnum in
    let arr = Ident.fresh "arr" in
    let idx = Ident.fresh "idx" in
    Ldef (arr, aexp,
          Ldef (idx, mul iexp (const_int tsz), Lprim
                  (Pstore,
                   [Lident arr; Lident idx;
                    Lprim (comp_arithop op, [Lprim (Pload, [Lident arr; Lident idx]); oexp])])))
  | Pstm_set (e1, e2, Assign, e3) ->
    let aexp, elty = array_expr venv tenv e1 in
    if is_large elty then raise (Error (e1.loc, Illegal_large_type elty)); (* FIXME loc *)
    (* let tsz = size_of tenv elty in *)
    let iexp = expr_with_type Tint venv tenv e2 in
    let oexp = expr_with_type elty venv tenv e3 in
    let lnum = e2.loc.Location.loc_start.Lexing.pos_lnum in
    Lprim (Pstore, [aexp; iexp; oexp])
  | Pstm_setptr (e1, ArithAssign op, e2) ->
    let e1, t = pointer_expr venv tenv e1 in
    (* FIXME check t = Tint *)
    let e2 = expr_with_type t venv tenv e2 in
    let ptr = Ident.fresh "ptr" in
    Ldef (ptr, e1, Lprim (Pstore, [Lident ptr; const_int 0; Lprim (comp_arithop op, [Lident ptr; e2])]))
  | Pstm_setptr (e1, Assign, e2) ->
    let e1, t = pointer_expr venv tenv e1 in
    if is_large t then raise (Error (e2.loc, Illegal_large_type t)); (* FIXME loc *)
    let e2 = expr_with_type t venv tenv e2 in
    Lprim (Pstore, [e1; const_int 0; e2])
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
    let s = stmt rt venv tenv inloop s in
    Ldef (v, e, s)
  | Pstm_ifthenelse (e, s1, s2) ->
    let e = expr_with_type Tbool venv tenv e in
    let s1 = stmt rt venv tenv inloop s1 in
    let s2 = stmt rt venv tenv inloop s2 in
    Lifthenelse (e, s1, s2)
  | Pstm_while (e, s) ->
    let e = expr_with_type Tbool venv tenv e in
    let s = stmt rt venv tenv true s in
    Lblock (Lloop (Lblock (Lifthenelse (e, s, Lexit 1))))
  | Pstm_return None ->
    assert (rt = Tvoid); (* FIXME *)
    Lreturn None
  | Pstm_return (Some e) ->
    let e = expr_with_type rt venv tenv e in
    Lreturn (Some e)
  | Pstm_seq (s1, s2) ->
    let lam1 = stmt rt venv tenv inloop s1 in
    let lam2 = stmt rt venv tenv inloop s2 in
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

let outfuns : lambda_fun list ref = ref []

let define_struct id fields venv tenv =
  (* FIXME check for field repetitions *)
  let already_defined = try Tbl.find id.txt tenv <> Unfilled with Not_found -> false in
  if already_defined then raise (Error (id.loc, Struct_already_defined));
  Tbl.add id.txt (Filled (List.map (fun (t, id) -> id.txt, tp venv t) fields)) tenv

let defn venv tenv d =
  match d with
  | Pdef_struct (id, fields) ->
    venv, define_struct id fields venv tenv
  | Pdef_fun (rt, id, args, body) ->
    (* FIXME check for param repetitions *)    
    let ats = List.map (fun (t, _) -> tp venv t) args in
    let rt = tp venv rt in
    let id, venv0 = add_fun id ats rt venv in
    let ids, venv = List.fold_left2 (fun (ids, venv) (_, id) t ->
        let id, venv = add_var id t venv in id :: ids, venv) ([], venv0) args ats in
    let body = stmt rt venv tenv false body in
    outfuns := Lfun (id, List.rev ids, body) :: !outfuns;
    venv0, tenv
  | Pdef_type (t, id) ->
    add_type id (tp venv t) venv, tenv
  
let program ds =
  outfuns := [];
  let _ = List.fold_left (fun (venv, tenv) d -> defn venv tenv d) (Tbl.empty, Tbl.empty) ds in
  List.rev !outfuns

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
  | Pointer_expr_expected t ->
    Location.report_error loc "pointer expr expected, found '%a'" Types.print t
  | Illegal_large_type t ->
    Location.report_error loc "illegal large type '%a'" Types.print t
