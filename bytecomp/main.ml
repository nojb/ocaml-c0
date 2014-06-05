let main () =
  let lexbuf = Lexing.from_channel stdin in
  let p = Parser.program Lexer.token lexbuf in
  Printast.program Format.std_formatter p;
  let lam = Typecore.program p in
  Printlambda.program Format.std_formatter lam

let _ =
  try
    main ()
  with
  | Syntaxerr.Error err ->
    Syntaxerr.report_error Format.std_formatter err
  (* | Typecore.Error (loc, err) -> *)
    (* Typecore.report_error Format.std_formatter loc err *)
