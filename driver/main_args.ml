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

let mk_dparsetree f =
  "-dparsetree", Arg.Unit f, "show parse tree"

let mk_dlambda f =
  "-dlambda", Arg.Unit f, "show intermediate form"

let mk_dinstr f =
  "-dinstr", Arg.Unit f, "show virtual machine instructions"
  
module type Bytecomp_options = sig
  val _dparsetree : unit -> unit
  val _dlambda : unit -> unit
  val _dinstr : unit -> unit
end

module type Arg_list = sig
  val list : (string * Arg.spec * string) list
end

module Make_bytecomp_options (F : Bytecomp_options) = struct
  let list = [
    mk_dparsetree F._dparsetree;
    mk_dlambda F._dlambda;
    mk_dinstr F._dinstr
  ]
end
