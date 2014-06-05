{
open Lexing
open Misc
open Parser

type error =
  | Illegal_character of char
  | Illegal_escape of string
  | Unterminated_comment of Location.t
  | Unterminated_string
  | Unterminated_string_in_comment of Location.t * Location.t
  | Literal_overflow

exception Error of error * Location.t

let keyword_table =
  create_hashtable 37 [
    "break", BREAK;
    "else", ELSE;
    "for", FOR;
    "if", IF;
    "while", WHILE;
    "return", RETURN;
    "int", INT;
    "char", CHAR;
    "string", STRING;
    "true", TRUE;
    "false", FALSE;
    "assert", ASSERT;
    "error", ERROR;
    "alloc", ALLOC;
    "alloc_array", ALLOC_ARRAY;
    "break", BREAK;
    "continue", CONTINUE;
    "NULL", NULL
  ]

let string_buff = Buffer.create 64
let in_comment = ref false
let in_comment () = !in_comment
let string_start_loc = ref Location.dummy
let comment_start_loc = ref Location.dummy

let char_for_backslash = function
  | 'n' -> '\010'
  | 't' -> '\009'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | c -> c

let char_for_decimal_code lexbuf i =
  let c =
    100 * (Char.code (Lexing.lexeme_char lexbuf i) - 48) +
    10 * (Char.code (Lexing.lexeme_char lexbuf (i+1)) - 48) +
    (Char.code (Lexing.lexeme_char lexbuf (i+2)) - 48)
  in
  if c < 0 || c > 255 then
    if in_comment () then 'x'
    else raise (Error (Illegal_escape (Lexing.lexeme lexbuf), Location.curr lexbuf))
  else
    Char.chr c

let cvt_int_literal s =
  Nativeint.neg (Nativeint.of_string ("-" ^ s))

let update_loc lexbuf line chars =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with
      pos_lnum = pos.pos_lnum + line;
      pos_bol = pos.pos_cnum - chars }
  
open Format

let report_error ppf = function
  | Illegal_character c ->
    fprintf ppf "Illegal character (%s)" (Char.escaped c)
  | Illegal_escape s ->
    fprintf ppf "Illegal backslash escape in string or character (%s)" s
  | Unterminated_comment _ ->
    fprintf ppf "Comment not terminated"
  | Unterminated_string ->
    fprintf ppf "String literal not terminated"
  | Unterminated_string_in_comment (_, loc) ->
    fprintf ppf "This comment contains an unterminated string literal@.\
                 %aString literal begins here"
      Location.print_error loc
  | Literal_overflow ->
    fprintf ppf "Integer literal exceeds the range of representable integers"
}

let newline = ('\013'* '\010')
let blank = [' ' '\009' '\012']
let firstidentchar = ['A'-'Z' 'a'-'z' '_']
let identchar = ['A'-'Z' 'a'-'z' '_' '0'-'9']
let decimal_literal =
  ['0'-'9']+

rule token = parse
  | '\n'
      { Lexing.new_line lexbuf;
        token lexbuf }
  | newline
      { update_loc lexbuf 1 0;
        token lexbuf }
  | blank +
      { token lexbuf }
  | firstidentchar identchar * as s
      { try Hashtbl.find keyword_table s
        with Not_found -> IDENT s }
  | decimal_literal as s
      { try
          INTLIT (cvt_int_literal s)
        with Failure _ ->
          raise (Error (Literal_overflow, Location.curr lexbuf)) }
  | "->"
    { ARROW }
  | "++"
    { PLUSPLUS }
  | '+'
      { PLUS }
  | '*'
      { STAR }
  | "--"
    { MINUSMINUS }
  | '-'
      { MINUS }
  | '/'
      { SLASH }
  | "%="
    { PERCENTEQUAL }
  | '%'
    { PERCENT }
  | "&="
    { AMPERSANDEQUAL }
  | "&&"
    { AMPERSANDAMPERSAND }
  | '&'
      { AMPERSAND }
  | "|="
    { BAREQUAL }
  | "||"
    { BARBAR }
  | '|'
      { BAR }
  | ':'
      { COLON }
  | ','
      { COMMA }
  | '?'
    { QUESTION }
  | "=="
      { EQUALEQUAL }
  | "!="
    { BANGEQUAL }
  | '!'
    { BANG }
  | '~'
    { TILDE }
  | "^="
    { CARETEQUAL }
  | '^'
    { CARET }
  | "<<="
    { LESSLESSEQUAL }
  | "<<"
    { LESSLESS }
  | ">>="
    { GREATERGREATEREQUAL }
  | ">>"
    { GREATERGREATER }
  | '='
      { EQUAL }
  | "<="
      { LESSEQUAL }
  | '<'
      { LESS }
  | ">="
      { GREATEREQUAL }
  | '>'
      { GREATER }
  | '.'
      { DOT }
  | ';'
      { SEMI }
  | '{'
      { LBRACE }
  | '}'
      { RBRACE }
  | '['
      { LBRACKET }
  | ']'
      { RBRACKET }
  | '('
      { LPAREN }
  | ')'
      { RPAREN }
  | "/*"
      { comment 0 lexbuf }
  | '"'
      { string_start_loc := Location.curr lexbuf;
        Buffer.clear string_buff;
        string lexbuf;
        STRINGLIT (Buffer.contents string_buff) }
  | eof
      { EOF }
  | _ as lxm
      { raise (Error (Illegal_character lxm, Location.curr lexbuf)) }

and string = parse
  | '"'
      { () }
  | '\n'
      { Lexing.new_line lexbuf;
        Buffer.add_char string_buff '\n';
        string lexbuf }
  | "\\" ['\\' '\'' '"' 'n' 't' 'b' 'r' ' ']
      { Buffer.add_char string_buff (char_for_backslash (Lexing.lexeme_char lexbuf 1));
        string lexbuf }
  | "\\t"
      { Buffer.add_char string_buff '\t';
        string lexbuf }
  | "\\\""
      { Buffer.add_char string_buff '"';
        string lexbuf }
  | "\\" ['0'-'9'] ['0'-'9'] ['0'-'9']
      { Buffer.add_char string_buff (char_for_decimal_code lexbuf 1);
        string lexbuf }
  | "\\\\"
      { Buffer.add_char string_buff '\\';
        string lexbuf }
  | '\\' [' ' '\t']
      { skip_whitespace lexbuf }
  (* | '\\' '\n'
  { Lexing.new_line lexbuf; (* incr_linenum lexbuf; *)
    skip_whitespace lexbuf } *)
  | eof
      { raise (Error (Unterminated_string, !string_start_loc)) }
  | _ as c
      { Buffer.add_char string_buff c;
        string lexbuf }

and skip_whitespace = parse
  | '\\'
      { string lexbuf }
  | '\n'
      { Lexing.new_line lexbuf;
        skip_whitespace lexbuf }
  | [' ' '\t']+
    { skip_whitespace lexbuf }
  | eof
      { raise (Error (Unterminated_string, !string_start_loc)) }

and comment lvl = parse
  | "*/" { if lvl = 0 then token lexbuf else comment (lvl-1) lexbuf }
  | "/*" { comment (lvl+1) lexbuf }
  | _ { comment lvl lexbuf }
  | eof
  { EOF }
