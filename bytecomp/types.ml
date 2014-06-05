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

type type_expr =
  | Tnull
  | Tbool
  | Tchar
  | Tint
  | Tstring
  | Tstruct of string Parsetree.loc
  | Talias of string * type_expr
  | Tpointer of type_expr
  | Tarray of type_expr

let rec equal t1 t2 =
  match t1, t2 with
  | Tstring, Tstring
  | Tint, Tint
  | Tbool, Tbool
  | Tchar, Tchar -> true
  | Tstruct id1, Tstruct id2 -> id1 = id2
  | Talias (_, t1), _ -> equal t1 t2
  | _, Talias (_, t2) -> equal t1 t2
  | Tpointer t1, Tpointer t2
  | Tarray t1, Tarray t2 -> equal t1 t2
  | Tpointer _, Tnull
  | Tnull, Tpointer _
  | Tnull, Tnull -> true
  | _ -> false

let is_large = function
  | Tstruct _ -> true
  | _ -> false

open Format

let rec print ppf = function
  | Tint -> fprintf ppf "int"
  | Tstring -> fprintf ppf "string"
  | Tbool -> fprintf ppf "bool"
  | Tchar -> fprintf ppf "char"
  | Tstruct id -> fprintf ppf "struct %s" id.Parsetree.txt
  | Talias (id, t) -> fprintf ppf "%s = %a" id print t
  | Tpointer t -> fprintf ppf "%a *" print t
  | Tarray t -> fprintf ppf "%a []" print t
  | Tnull -> fprintf ppf "NULL"
