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

type error =
  | Unclosed of Location.t * string * Location.t * string
  | Expecting of Location.t * string

exception Error of error

open Format

let report_error ppf = function
  | Unclosed (opening_loc, opening, closing_loc, closing) ->
    Location.report_error closing_loc
      ~sub:[opening_loc, Printf.sprintf "Error: This '%s' might be unmatched" opening]
      ~if_highlight:(Printf.sprintf "Syntax error: '%s' expected, \
                                      the highlighted '%s' might be unmatched"
                                      closing opening)
      "Error: Syntax error: '%s' expected" closing
  | Expecting (loc, nonterm) ->
    Location.report_error loc "Error: Syntax error: '%s' expected" nonterm
