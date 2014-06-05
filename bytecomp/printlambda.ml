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
open Asttypes
open Lambda

let primitive ppf = function
  | Palloc n -> fprintf ppf "alloc %i" n
  | Pallocarray n -> fprintf ppf "allocarray %i" n
  | Paddint -> fprintf ppf "+"
  | Psubint -> fprintf ppf "-"
  | Pmulint -> fprintf ppf "*"
  | Pdivint -> fprintf ppf "/"
  | Pnegint -> fprintf ppf "-"
  | Pintcomp op -> fprintf ppf "%s" (string_of_binary_operator (Bop_cmp op))
  | Pload -> fprintf ppf "load"
  | Pstore -> fprintf ppf "store"
  | Pgetfield i -> fprintf ppf "getfield %i" i
  | Psetfield i -> fprintf ppf "setfield %i" i
  | Pset l -> fprintf ppf "set %i" l
  | Pget l -> fprintf ppf "get %i" l
  | Passert l -> fprintf ppf "assert %i" l
  | Perror l -> fprintf ppf "error %i" l

let rec lambda ppf = function
  | Lconst cst ->
    print_constant ppf cst
  | Lident id ->
    Ident.print ppf id
  | Lassign (id, e) ->
    fprintf ppf "@[<2>(assign@ %a@ %a)@]" Ident.print id lambda e
  | Lifthenelse (e1, e2, e3) ->
    fprintf ppf "@[<2>(if@ %a@ %a@ %a)@]" lambda e1 lambda e2 lambda e3
  | Lprim (p, args) ->
    let lams ppf largs = List.iter (fun l -> fprintf ppf "@ %a" lambda l) largs in
    fprintf ppf "@[<2>(%a%a)@]" primitive p lams args
  (* | Lletrec (fns, e) -> *)
  (*   let rec prargs ppf = function *)
  (*     | [] -> () *)
  (*     | [arg] -> *)
  (*       Ident.print ppf arg *)
  (*     | a :: rest -> *)
  (*       fprintf ppf "%a@ %a" Ident.print a prargs rest *)
  (*   in *)
  (*   let aux ppf fns = *)
  (*     let spc = ref false in *)
  (*     List.iter (fun (Lfunction (id, args, e)) -> *)
  (*         if !spc then fprintf ppf "@ " else spc := true; *)
  (*         fprintf ppf "@[<2>(%a@ @[<2>(fun@ (%a)@ %a)@])@]" Ident.print id prargs args lambda e) fns *)
  (*   in *)
  (*   fprintf ppf "@[<2>(letrec@ (@[<hv 1>%a@])@ %a)@]" aux fns lambda e *)
  (* | Llet (id, e1, e2) -> *)
  (*   fprintf ppf "@[<2>(let@ @[<hv 1>(@[<2>%a@ %a@]" Ident.print id lambda e1; *)
  (*   let rec letbody = function *)
  (*     | Llet (id, e1, e2) -> *)
  (*       fprintf ppf "@ @[<2>%a@ %a@]" Ident.print id lambda e1; *)
  (*       letbody e2 *)
  (*     | _ as e -> e *)
  (*   in *)
  (*   let e = letbody e2 in *)
  (*   fprintf ppf ")@]@ %a)@]" lambda e *)
  | Lcall (id, args) ->
    let lams ppf largs = List.iter (fun l -> fprintf ppf "@ %a" lambda l) largs in
    fprintf ppf "@[<2>(%a%a)@]" Ident.print id lams args
  | Lloop lam ->
    fprintf ppf "@[<2>(loop@ %a)@]" lambda lam
  | Lblock lam ->
    fprintf ppf "@[<2>(block@ %a)@]" lambda lam
  | Lexit n ->
    fprintf ppf "@[<2>(exit@ %i)@]" n
  | Lseq (e1, e2) ->
    fprintf ppf "@[<2>(seq@ %a@ %a)@]" lambda e1 sequence e2
  | Ldef (id, e1, e2) ->
    fprintf ppf "@[<2>(def@ (%a@ %a)@ %a)@]" Ident.print id lambda e1 lambda e2
  | Lreturn None ->
    fprintf ppf "(return)"
  | Lreturn (Some e) ->
    fprintf ppf "@[<2>(return@ %a)@]" lambda e
      
and sequence ppf = function
  | Lseq (e1, e2) ->
    fprintf ppf "%a@ %a" sequence e1 sequence e2
  | _ as e ->
    lambda ppf e

let program ppf lam =
  fprintf ppf "@[%a@]@." lambda lam