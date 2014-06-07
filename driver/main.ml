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

let print_if ppf flag printer arg =
  if !flag then fprintf ppf "%a@." printer arg;
  arg

let (++) x f = f x

let file ppf inputfile parse_fun =
  let ic = open_in_bin inputfile in
  let ast =
    try
      seek_in ic 0;
      Location.input_name := inputfile;
      let lexbuf = Lexing.from_channel ic in
      Location.init lexbuf inputfile;
      parse_fun lexbuf
    with x -> close_in ic; raise x
  in
  close_in ic;
  ast

let implementation ppf sourcefile =
  file ppf sourcefile (Parser.implementation Lexer.token)
  ++ print_if ppf Clflags.dump_parsetree Printast.program
  ++ Typecore.program
  ++ print_if ppf Clflags.dump_lambda Printlambda.program
  ++ Bytegen.compile_program
  ++ print_if ppf Clflags.dump_instr Printinstr.instrlist

let process_file ppf name =
  if Filename.check_suffix name ".c0" then
    implementation ppf name
  else
    raise (Arg.Bad ("don't know what to do with " ^ name))

let usage = "Usage: cc0 <options> <files>\nOptions are:"

let ppf = Format.err_formatter

let anonymous filename =
  let _ = process_file ppf filename in
  ()

module Options = Main_args.Make_bytecomp_options (struct
    let set r () = r := true
    let unset r () = r := false
    let _dparsetree = set Clflags.dump_parsetree
    let _dlambda = set Clflags.dump_lambda
    let _dinstr = set Clflags.dump_instr
  end)

let main () =
  try
    Arg.parse Options.list anonymous usage
  with
  | Syntaxerr.Error err ->
    Syntaxerr.report_error Format.std_formatter err
  | Typecore.Error (loc, err) ->
    Typecore.report_error Format.std_formatter loc err

let _ =
  main ()
