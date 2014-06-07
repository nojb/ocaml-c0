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
  
type label = int

type instruction =
  | Klabel of label
  | Kconst of constant
  | Kaccess of int
  | Kloadi of int
  | Kstorei of int
  | Kload
  | Kstore
  | Kpush
  | Kpop of int
  | Kbranch of label
  | Kbranchif of label
  | Kbranchifnot of label
  | Kstop
  | Kreturn of int
  | Kaddint
  | Ksubint
  | Kmulint
  | Kdivint
  | Kmodint
  | Klslint
  | Kasrint
  | Kandint
  | Korint
  | Kxorint
  | Knegint
  | Kcall of int * label
  | Ktailcall of int * label
  | Kallocarray of int
  | Kpush_retaddr of label
  (* | Kintcomp of comparison *)
  (* | Kstringcomp of comparison *)
