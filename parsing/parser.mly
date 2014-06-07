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

let const_true = mkdummyloc (Pexp_const (Const_bool true))
let const_false = mkdummyloc (Pexp_const (Const_bool false))
let const_int n = mkdummyloc (Pexp_const (Const_int (Nativeint.of_int n)))
%}

%token ALLOC
%token ALLOC_ARRAY
%token AMPERSAND
%token AMPERSANDAMPERSAND
%token AMPERSANDEQUAL
%token ARROW
%token ASSERT
%token BANG
%token BANGEQUAL
%token BAR
%token BARBAR
%token BAREQUAL
%token BOOL
%token BREAK
%token CARET
%token CARETEQUAL
%token CHAR
%token <char> CHARLIT
%token COLON
%token COMMA
%token CONTINUE
%token DOT
%token ELSE
%token EOF
%token EQUAL
%token EQUALEQUAL
%token ERROR
%token FALSE
%token FOR
%token GREATER
%token GREATEREQUAL
%token GREATERGREATER
%token GREATERGREATEREQUAL
%token <string> IDENT
%token IF
%token INT
%token <nativeint> INTLIT
%token LBRACE
%token LBRACKET
%token LESS
%token LESSEQUAL
%token LESSLESS
%token LESSLESSEQUAL
%token LPAREN
%token MINUS
%token MINUSEQUAL
%token MINUSMINUS
%token NULL
%token PERCENT
%token PERCENTEQUAL
%token PLUS
%token PLUSEQUAL
%token PLUSPLUS
%token QUESTION
%token RBRACE
%token RBRACKET
%token RETURN
%token RPAREN
%token SEMI
%token SLASH
%token SLASHEQUAL
%token STAR
%token STAREQUAL
%token STRING
%token <string> STRINGLIT
%token STRUCT
%token TILDE
%token TRUE
%token TYPEDEF
%token <string> TYPE_IDENT
%token VOID
%token WHILE

%type <Parsetree.defn list> implementation
%start implementation

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

implementation
  : x = list(gdefn) EOF
    { x }
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

type_ident
  : TYPE_IDENT
    { mkloc $1 }
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
  | type_ident
    { Ptyp_name $1 }
  ;

rexpr_list:
    x = separated_list(COMMA, rexpr)
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

rexpr:
    LPAREN rexpr RPAREN
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
  | lexpr
    { mkloc (Pexp_valof $1) }
  | e1 = rexpr op = binop e2 = rexpr
    { mkloc (Pexp_binop (e1, op, e2)) }
  | rexpr AMPERSANDAMPERSAND rexpr
    { mkloc (Pexp_cond ($1, $3, const_false)) }
  | rexpr BARBAR rexpr
    { mkloc (Pexp_cond ($1, const_true, $3)) }
  | op = unop e = rexpr %prec prec_unary_op
    { mkloc (Pexp_unop (op, e)) }
  | rexpr QUESTION rexpr colon_or_fail rexpr
    { mkloc (Pexp_cond ($1, $3, $5)) }
  | ident inparen_or_fail(rexpr_list)
    { mkloc (Pexp_call ($1, $2)) }
  | ALLOC inparen_or_fail(tp)
    { mkloc (Pexp_alloc $2) }
  | alloc_array
    { $1 }
  ;

alloc_array:
    ALLOC_ARRAY LPAREN tp COMMA rexpr RPAREN
    { mkloc (Pexp_allocarray ($3, $5)) }
  | ALLOC_ARRAY LPAREN tp COMMA rexpr error
    { unclosed "(" 2 ")" 6 }
  ;

%inline asnop:
    PLUSEQUAL
    { Aop_add }
  | MINUSEQUAL
    { Aop_sub }
  | STAREQUAL
    { Aop_mul }
  | SLASHEQUAL
    { Aop_div }
  | PERCENTEQUAL
    { Aop_mod }
  | LESSLESSEQUAL
    { Aop_lsl }
  | GREATERGREATEREQUAL
    { Aop_asr }
  | AMPERSANDEQUAL
    { Aop_and }
  | BAREQUAL
    { Aop_or }
  | CARETEQUAL
    { Aop_xor }
  ;

lexpr
  : ident
    { mkloc (Pexp_ident $1) }
  | rexpr ARROW ident
    { mkloc (Pexp_field (mkloc (Pexp_valof (mkloc (Pexp_deref $1))), $3)) }
  | rexpr DOT ident
    { mkloc (Pexp_field ($1, $3)) }
  | rexpr LBRACKET rexpr RBRACKET
    { mkloc (Pexp_index ($1, $3)) }
  | rexpr LBRACKET rexpr error
    { unclosed "[" 2 "]" 4 }
  | STAR rexpr %prec prec_unary_op
    { mkloc (Pexp_deref $2) }
  ;

simple:
    lv = lexpr op = asnop e = rexpr
    { Pstm_assignop (lv, op, e) }
  | lexpr EQUAL rexpr
    { Pstm_assign ($1, $3) }
  | lexpr PLUSPLUS
    { Pstm_assignop ($1, Aop_add, const_int 1) }
  | lexpr MINUSMINUS
    { Pstm_assignop ($1, Aop_sub, const_int 1) }
  | rexpr
    { Pstm_expr ($1) }
  ;

opt_equal_expr:
    x = option(preceded(EQUAL, rexpr))
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
    IF inparen_or_fail(rexpr) stmtm ELSE stmtu
    { Pstm_ifthenelse ($2, $3, $5) }
  | IF inparen_or_fail(rexpr) stmt
    { Pstm_ifthenelse ($2, $3, Pstm_empty) }
  | WHILE inparen_or_fail(rexpr) stmtu
    { Pstm_while ($2, $3) }
  ;

for_loop:
    FOR LPAREN opt_simple SEMI rexpr SEMI opt_simple RPAREN stmtm
    { seq $3 (Pstm_while ($5, seq $9 $7)) }
  | FOR LPAREN tp ident opt_equal_expr SEMI rexpr SEMI opt_simple RPAREN stmtm
    { Pstm_def ($3, $4, $5, Pstm_while ($7, seq $11 $9)) }
  ;
  
stmtm:
    simple SEMI
    { $1 }
  | IF inparen_or_fail(rexpr) stmtm ELSE stmtm
    { Pstm_ifthenelse ($2, $3, $5) }
  | WHILE inparen_or_fail(rexpr) stmtm
    { Pstm_while ($2, $3) }
  | for_loop
    { $1 }
  | RETURN SEMI
    { Pstm_return None }
  | RETURN rexpr SEMI
    { Pstm_return (Some ($2)) }
  | block
    { $1 }
  | ASSERT inparen_or_fail(rexpr) semi_or_fail
    { Pstm_assert ($2) }
  | ERROR inparen_or_fail(rexpr) semi_or_fail
    { Pstm_error ($2) }
  | BREAK semi_or_fail
    { Pstm_break }
  | CONTINUE semi_or_fail
    { Pstm_continue }
  ;

fields
  : x = list(terminated(pair(tp, ident), SEMI))
    { x }
  ;

params
  : x = separated_list(COMMA, pair(tp, ident))
    { x }
  ;

return_type
  : VOID
    { Ptyp_void }
  | tp
    { $1 }
  ;

gdefn
  : STRUCT ident LBRACE fields RBRACE semi_or_fail
    { Pdef_struct ($2, $4) }
  | return_type ident LPAREN params RPAREN block
    { Pdef_fun ($1, $2, $4, $6) }
  | TYPEDEF tp ident semi_or_fail
    { Pdef_type ($2, $3) }
  ;

gdecl
  : STRUCT ident SEMI
    { Pdec_struct $2 }
  | tp ident LPAREN params RPAREN SEMI
    { Pdec_fun ($1, $2, $4) }
  ;
