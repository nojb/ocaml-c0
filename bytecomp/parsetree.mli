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

type 'a loc = {
  txt : 'a;
  loc : Location.t
}

type compop =
  | Eq | Ne | Le | Lt | Ge | Gt

type arithop =
  | Add | Sub | Mul | Div | Mod
  | Lsl | Asr | And | Or | Xor

type binop =
  | Arith of arithop
  | Cmp of compop
  | Land
  | Lor

type unop =
  | Not | Lnot | Neg

type asnop =
  | ArithAssign of arithop
  | Assign

type tp =
  | TInt
  | TBool
  | TString
  | TChar
  | TPointer of tp
  | TArray of tp
  | TStruct of string loc
  | TName of string loc

type expr =
  | EInt of int32
  | EString of string
  | EChar of char
  | EBool of bool
  | EIdent of string loc
  | EBinop of expr loc * binop * expr loc
  | EUnop of unop * expr loc
  | ECond of expr loc * expr loc * expr loc
  | ECall of string loc * expr loc list
  | EField of expr loc * string loc
  | EIndex of expr loc * expr loc
  | EDeref of expr loc
  | EAlloc of tp
  | EAllocArray of tp * expr loc

and lval =
  | LIdent of string loc
  | LField of lval loc * string loc
  | LIndex of lval loc * expr loc
  | LDeref of lval loc

type stmt =
  | SEmpty
  | SAssign of lval loc * asnop * expr loc
  | SExpr of expr loc
  | SDef of tp * string loc * expr loc option * stmt
  | SIf of expr loc * stmt * stmt
  | SWhile of expr loc * stmt
  (* | SFor of stmt * expr loc * stmt * stmt *)
  | SReturn of expr loc option
  | SSeq of stmt * stmt
  | SAssert of expr loc
  | SError of expr loc
  | SBreak
  | SContinue

type function_definition = {
  pfun_name : string loc;
  pfun_arguments : (tp * string loc) list;
  pfun_return_type : tp option;
  pfun_body : stmt
}
