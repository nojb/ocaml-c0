%{
open Location
open Asttypes
open Parsetree
(* open Syntaxerr *)

let mkloc d =
  { txt = d;
    loc = symbol_loc () }

let mkdummyloc d =
  { txt = d;
    loc = dummy }

let seq s1 s2 =
  match s1, s2 with
  | Pstm_empty, _ -> s2
  | _, Pstm_empty -> s1
  | _ -> Pstm_seq (s1, s2)

let unclosed opening_name opening_num closing_name closing_num =
  let open Syntaxerr in
  raise (Error (Unclosed (rhs_loc opening_num, opening_name, rhs_loc closing_num, closing_name)))

let expecting num name =
  let open Syntaxerr in
  raise (Error (Expecting (rhs_loc num, name)))

let rec lvalue pos lv op e =
  match lv with
  | Pexp_ident id -> Pstm_assign (id, op, e)
  | Pexp_getfield (e1, id) -> Pstm_setfield (e1, id, op, e)
  | Pexp_get (e1, e2) -> Pstm_set (e1, e2, op, e)
  | Pexp_load e1 -> Pstm_store (e1, op, e)
  | _ -> expecting pos "lvalue"

let const_true = mkdummyloc (Pexp_const (Const_bool true))
let const_false = mkdummyloc (Pexp_const (Const_bool false))
let const_zero = mkdummyloc (Pexp_const (Const_int 0n))
%}

%token AMPERSAND
%token BAR
%token BREAK
%token COLON
%token COMMA
%token DOT
%token ELSE
%token EOF
%token EQUAL
%token FOR
%token GREATER
%token GREATEREQUAL
%token <string> IDENT
%token IF
%token <nativeint> INTLIT
%token <char> CHARLIT
%token TRUE
%token FALSE
%token TILDE
%token BANG
%token INT
%token BOOL
%token CHAR
%token STRING
(* %token VOID *)
%token LBRACE
%token LBRACKET
%token LESS
%token LESSEQUAL
%token LPAREN
%token MINUS
%token PLUS
%token RBRACE
%token RBRACKET
%token RPAREN
%token SEMI
%token SLASH
%token STAR
%token <string> STRINGLIT
%token WHILE
%token LESSLESSEQUAL
%token GREATERGREATEREQUAL
%token PLUSEQUAL
%token MINUSEQUAL
%token STAREQUAL
%token SLASHEQUAL
%token PERCENT
%token PERCENTEQUAL
%token RETURN
%token EQUALEQUAL
%token BANGEQUAL
%token BARBAR
%token AMPERSANDAMPERSAND
%token LESSLESS
%token GREATERGREATER
%token CARET
%token BAREQUAL
%token AMPERSANDEQUAL
%token QUESTION
%token PLUSPLUS
%token MINUSMINUS
%token ERROR
%token ASSERT
%token STRUCT
%token ALLOC
%token ALLOC_ARRAY
%token CARETEQUAL
%token CONTINUE
%token ARROW
%token NULL

%type <Parsetree.stmt> program
%start program

%right QUESTION
%left BARBAR
%left AMPERSANDAMPERSAND
%left BAR
%left CARET
%left AMPERSAND
%left EQUALEQUAL BANGEQUAL
%left LESSEQUAL GREATEREQUAL GREATER LESS
%left LESSLESS GREATERGREATER
%left PLUS MINUS
%left STAR SLASH PERCENT
%right prec_unary_op
%left LBRACKET DOT ARROW

%%

program:
    stmt EOF
    { $1 }
  ;

ident:
    IDENT
    { mkloc $1 }
  ;

ident_or_fail:
    ident
    { $1 }
  | error
    { expecting 1 "identifier" }
  ;
  
tp:
    INT
    { Ptyp_int }
  | BOOL
    { Ptyp_bool }
  | STRING
    { Ptyp_string }
  | CHAR
    { Ptyp_char }
  | tp STAR
    { Ptyp_pointer $1 }
  | tp LBRACKET RBRACKET
    { Ptyp_array $1 }
  | tp LBRACKET error
    { unclosed "[" 2 "]" 3 }
  | STRUCT ident_or_fail
    { Ptyp_struct $2 }
  | ident
    { Ptyp_name $1 }
  ;

expr_list:
    x = separated_list(COMMA, expr)
    { x }
  ;

%inline binop:
    PLUS
    { Bop_arith Aop_add }
  | MINUS
    { Bop_arith Aop_sub }
  | STAR
    { Bop_arith Aop_mul }
  | SLASH
    { Bop_arith Aop_div }
  | PERCENT
    { Bop_arith Aop_mod }
  | LESSLESS
    { Bop_arith Aop_lsl }
  | GREATERGREATER
    { Bop_arith Aop_asr }
  | AMPERSAND
    { Bop_arith Aop_and }
  | BAR
    { Bop_arith Aop_or }
  | CARET
    { Bop_arith Aop_xor }
  | LESS
    { Bop_cmp Clt }
  | LESSEQUAL
    { Bop_cmp Cle }
  | GREATER
    { Bop_cmp Cgt }
  | GREATEREQUAL
    { Bop_cmp Cge }
  | EQUALEQUAL
    { Bop_cmp Ceq }
  | BANGEQUAL
    { Bop_cmp Cneq }
  ;

%inline unop:
    BANG
    { Uop_lnot }
  | TILDE
    { Uop_not }
  | MINUS
    { Uop_neg }
  ;

colon_or_fail:
     COLON
     { () }
   | error
     { expecting 1 ":" }
   ;

expr:
    LPAREN expr RPAREN
    { $2 }
  | INTLIT
    { mkloc (Pexp_const (Const_int $1)) }
  | STRINGLIT
    { mkloc (Pexp_const (Const_string $1)) }
  | CHARLIT
    { mkloc (Pexp_const (Const_char $1)) }
  | TRUE
    { mkloc (Pexp_const (Const_bool true)) }
  | FALSE
    { mkloc (Pexp_const (Const_bool false)) }
  | NULL
    { mkloc (Pexp_const (Const_null)) }
  | ident
    { mkloc (Pexp_ident $1) }
  | e1 = expr op = binop e2 = expr
    { mkloc (Pexp_binop (e1, op, e2)) }
  | expr AMPERSANDAMPERSAND expr
    { mkloc (Pexp_cond ($1, $3, const_false)) }
  | expr BARBAR expr
    { mkloc (Pexp_cond ($1, const_true, $3)) }
  | op = unop e = expr %prec prec_unary_op
    { mkloc (Pexp_unop (op, e)) }
  | expr QUESTION expr colon_or_fail expr
    { mkloc (Pexp_cond ($1, $3, $5)) }
  | ident inparen_or_fail(expr_list)
    { mkloc (Pexp_call ($1, $2)) }
  | expr DOT ident_or_fail
    { mkloc (Pexp_getfield ($1, $3)) }
  | expr ARROW ident_or_fail
    { mkloc (Pexp_getfield (mkloc (Pexp_load $1), $3)) }
  | expr LBRACKET expr RBRACKET
    { mkloc (Pexp_get ($1, $3)) }
  | expr LBRACKET expr error
    { unclosed "[" 2 "]" 4 }
  | STAR expr %prec prec_unary_op
    { mkloc (Pexp_load $2) }
  | ALLOC inparen_or_fail(tp)
    { mkloc (Pexp_alloc $2) }
  | alloc_array
    { $1 }
  ;

alloc_array:
    ALLOC_ARRAY LPAREN tp COMMA expr RPAREN
    { mkloc (Pexp_allocarray ($3, $5)) }
  | ALLOC_ARRAY LPAREN tp COMMA expr error
    { unclosed "(" 2 ")" 6 }
  ;

%inline asnop:
    PLUSEQUAL
    { ArithAssign Aop_add }
  | MINUSEQUAL
    { ArithAssign Aop_sub }
  | STAREQUAL
    { ArithAssign Aop_mul }
  | SLASHEQUAL
    { ArithAssign Aop_div }
  | PERCENTEQUAL
    { ArithAssign Aop_mod }
  | LESSLESSEQUAL
    { ArithAssign Aop_lsl }
  | GREATERGREATEREQUAL
    { ArithAssign Aop_asr }
  | AMPERSANDEQUAL
    { ArithAssign Aop_and }
  | BAREQUAL
    { ArithAssign Aop_or }
  | CARETEQUAL
    { ArithAssign Aop_xor }
  | EQUAL
    { Assign }
  ;

simple:
    lv = expr op = asnop e = expr
    { lvalue 1 lv.txt op e }
  | expr PLUSPLUS
    { lvalue 1 $1.txt (ArithAssign Aop_add) const_zero }
  | expr MINUSMINUS
    { lvalue 1 $1.txt (ArithAssign Aop_sub) const_zero }
  | expr
    { Pstm_expr $1 }
  ;

opt_equal_expr:
    x = option(preceded(EQUAL, expr))
    { x }
  ;

block_stmts:
    /* empty */
    { Pstm_empty }
  | stmt block_stmts
    { seq $1 $2 }
  | tp ident opt_equal_expr semi_or_fail block_stmts
    { Pstm_def ($1, $2, $3, $5) }
  ;

block:
    LBRACE block_stmts RBRACE
    { $2 }
  | LBRACE block_stmts error
    { unclosed "{" 1 "}" 3 }
  ;

semi_or_fail:
   SEMI
   { () }
 | error
   { expecting 1 ";" }
 ;

inparen_or_fail(X):
    LPAREN x = X RPAREN
    { x }
  | LPAREN X error
    { unclosed "(" 1 ")" 3 }
  ;

opt_simple:
    /* nothing */
    { Pstm_empty }
  | simple
    { $1 }
  ;

stmt:
    stmtu
    { $1 }
  | stmtm
    { $1 }
  ;

stmtu:
    IF inparen_or_fail(expr) stmtm ELSE stmtu
    { Pstm_ifthenelse ($2, $3, $5) }
  | IF inparen_or_fail(expr) stmt
    { Pstm_ifthenelse ($2, $3, Pstm_empty) }
  | WHILE inparen_or_fail(expr) stmtu
    { Pstm_while ($2, $3) }
  ;

for_loop:
    FOR LPAREN opt_simple SEMI expr SEMI opt_simple RPAREN stmtm
    { seq $3 (Pstm_while ($5, seq $9 $7)) }
  | FOR LPAREN tp ident opt_equal_expr SEMI expr SEMI opt_simple RPAREN stmtm
    { Pstm_def ($3, $4, $5, Pstm_while ($7, seq $11 $9)) }
  ;
  
stmtm:
    simple SEMI
    { $1 }
  | IF inparen_or_fail(expr) stmtm ELSE stmtm
    { Pstm_ifthenelse ($2, $3, $5) }
  | WHILE inparen_or_fail(expr) stmtm
    { Pstm_while ($2, $3) }
  | for_loop
    { $1 }
  | RETURN SEMI
    { Pstm_return None }
  | RETURN expr SEMI
    { Pstm_return (Some $2) }
  | block
    { $1 }
  | ASSERT inparen_or_fail(expr) semi_or_fail
    { Pstm_assert $2 }
  | ERROR inparen_or_fail(expr) semi_or_fail
    { Pstm_error $2 }
  | BREAK semi_or_fail
    { Pstm_break }
  | CONTINUE semi_or_fail
    { Pstm_continue }
  ;

(* rec_fun_decl: *)
(*     fun_decl_list *)
(*     { mkloc (Pdec_function $1) } *)
(*   ; *)
  
(* fun_decl_list: *)
(*     fun_decl *)
(*     { $1 :: [] } *)
(*   | fun_decl fun_decl_list *)
(*     { $1 :: $2 } *)
(*   ; *)

(* fun_decl: *)
(*     FUNCTION ident LPAREN opt_ident_colon_ident_comma_list RPAREN opt_colon_ident EQUAL expr *)
(*     { *)
(*       { pfun_name = $2; pfun_arguments = $4; pfun_return_type = $6; pfun_body = $8 } *)
(*     } *)
(*   ; *)
