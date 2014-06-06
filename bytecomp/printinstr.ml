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

open Format
open Instruct

let instruction ppf = function
  | Klabel lbl -> fprintf ppf "L%i:" lbl
  | Kconst cst -> fprintf ppf "\tconst %a" Asttypes.print_constant cst
  | Kaccess n -> fprintf ppf "\taccess %i" n
  | Kassign n -> fprintf ppf "\tassign %i" n
  | Kload -> fprintf ppf "\tload"
  | Kstore -> fprintf ppf "\tstore"
  | Kpush -> fprintf ppf "\tpush"
  | Kpop n -> fprintf ppf "\tpop %i" n
  | Kbranch lbl -> fprintf ppf "\tbranch L%i" lbl
  | Kbranchif lbl -> fprintf ppf "\tbranchif L%i" lbl
  | Kbranchifnot lbl -> fprintf ppf "\tbranchifnot L%i" lbl
  | Kstop -> fprintf ppf "\tstop"
  | Kaddint -> fprintf ppf "\taddint"
  | Ksubint -> fprintf ppf "\tsubint"
  | Kmulint -> fprintf ppf "\tmulint"
  | Kdivint -> fprintf ppf "\tdivint"
  | Kmodint -> fprintf ppf "\tmodint"
  | Klslint -> fprintf ppf "\tlslint"
  | Kasrint -> fprintf ppf "\tasrint"
  | Kandint -> fprintf ppf "\tandint"
  | Korint -> fprintf ppf "\torint"
  | Kxorint -> fprintf ppf "\txorint"
  | Knegint -> fprintf ppf "\tnegint"
  | Kreturn n -> fprintf ppf "\treturn %i" n
  | Kcall lbl -> fprintf ppf "\tcall L%i" lbl
  | Kallocarray sz -> fprintf ppf "\tallocarray %i" sz
                   
let rec instruction_list ppf = function
  | [] -> ()
  | Klabel lbl :: il ->
    fprintf ppf "L%i:%a" lbl instruction_list il
  | instr :: il ->
    fprintf ppf "%a@ %a" instruction instr instruction_list il

let instrlist ppf il =
  fprintf ppf "@[<v 0>%a@]" instruction_list il
