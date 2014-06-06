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

type primitive =
  | Palloc of int
  | Pallocarray of int
  | Paddint
  | Psubint
  | Pmulint
  | Pdivint
  | Pnegint
  | Pintcomp of comparison
  | Pload
  | Pstore
  (* | Pgetfield of int *)
  (* | Psetfield of asnop * int *)
  (* | Pget of int * int *)
  (* | Pset of int * int *)
  | Passert of int
  | Perror of int
    
type lambda =
  | Lconst of constant
  | Lident of Ident.t
  | Lassign of Ident.t * lambda
  | Lifthenelse of lambda * lambda * lambda
  | Lprim of primitive * lambda list
  | Lcall of Ident.t * lambda list
  | Lloop of lambda
  | Lblock of lambda
  | Lexit of int
  | Lseq of lambda * lambda
  | Ldef of Ident.t * lambda * lambda
  | Lreturn of lambda option

type fundef = {
  name : Ident.t;
  params : Ident.t list;
  body : lambda
}
