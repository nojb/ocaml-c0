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

let maptxt f d =
  { d with txt = f d.txt }

let rec lval = function
  | EIdent id -> LIdent id
  | EField (e, id) -> LField (maptxt lval e, id)
  | EIndex (e1, e2) -> LIndex (maptxt lval e1, e2)
  | _ -> raise Exit

let seq s1 s2 =
  match s1, s2 with
  | SEmpty, _ -> s2
  | _, SEmpty -> s1
  | _ -> SSeq (s1, s2)

let for_to_while start cond step body =
  let rec loop s k =
    match s with
    | SEmpty -> k
    | SDef (t, id, e, body) ->
      SDef (t, id, e, loop body k)
    | SSeq (s1, s2) ->
      seq s1 (loop s2 k)
    | _ ->
      seq s k
  in
  loop start (SWhile (cond, seq body step))

let unclosed opening_name opening_num closing_name closing_num =
  let open Syntaxerr in
  raise (Error (Unclosed (rhs_loc opening_num, opening_name, rhs_loc closing_num, closing_name)))

let expecting num name =
  let open Syntaxerr in
  raise (Error (Expecting (rhs_loc num, name)))
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
%token <int32> INTLIT
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
    { TInt }
  | BOOL
    { TBool }
  | STRING
    { TString }
  | CHAR
    { TChar }
  | tp STAR
    { TPointer $1 }
  | tp LBRACKET RBRACKET
    { TArray $1 }
  | tp LBRACKET error
    { unclosed "[" 2 "]" 3 }
  | STRUCT ident_or_fail
    { TStruct $2 }
  | ident
    { TName $1 }
  ;

expr_list:
    x = separated_list(COMMA, expr)
    { x }
  ;

%inline binop:
    PLUS
    { Arith Add }
  | MINUS
    { Arith Sub }
  | STAR
    { Arith Mul }
  | SLASH
    { Arith Div }
  | PERCENT
    { Arith Mod }
  | LESSLESS
    { Arith Lsl }
  | GREATERGREATER
    { Arith Asr }
  | AMPERSAND
    { Arith And }
  | BAR
    { Arith Or }
  | CARET
    { Arith Xor }
  | LESS
    { Cmp Lt }
  | LESSEQUAL
    { Cmp Le }
  | GREATER
    { Cmp Gt }
  | GREATEREQUAL
    { Cmp Ge }
  | EQUALEQUAL
    { Cmp Eq }
  | BANGEQUAL
    { Cmp Ne }
  | AMPERSANDAMPERSAND
    { Land }
  | BARBAR
    { Lor }
  ;

%inline unop:
    BANG
    { Not }
  | TILDE
    { Lnot }
  | MINUS
    { Neg }
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
    { mkloc (EInt $1) }
  | STRINGLIT
    { mkloc (EString $1) }
  | CHARLIT
    { mkloc (EChar $1) }
  | TRUE
    { mkloc (EBool true) }
  | FALSE
    { mkloc (EBool false) }
  | ident
    { mkloc (EIdent $1) }
  | e1 = expr op = binop e2 = expr
    { mkloc (EBinop (e1, op, e2)) }
  | op = unop e = expr %prec prec_unary_op
    { mkloc (EUnop (op, e)) }
  | expr QUESTION expr colon_or_fail expr
    { mkloc (ECond ($1, $3, $5)) }
  | ident inparen_or_fail(expr_list)
    { mkloc (ECall ($1, $2)) }
  | expr DOT ident_or_fail
    { mkloc (EField ($1, $3)) }
  | expr ARROW ident_or_fail
    { mkloc (EField (mkloc (EDeref $1), $3)) }
  | expr LBRACKET expr RBRACKET
    { mkloc (EIndex ($1, $3)) }
  | expr LBRACKET expr error
    { unclosed "[" 2 "]" 4 }
  | STAR expr %prec prec_unary_op
    { mkloc (EDeref $2) }
  | ALLOC inparen_or_fail(tp)
    { mkloc (EAlloc $2) }
  | alloc_array
    { $1 }
  ;

alloc_array:
    ALLOC_ARRAY LPAREN tp COMMA expr RPAREN
    { mkloc (EAllocArray ($3, $5)) }
  | ALLOC_ARRAY LPAREN tp COMMA expr error
    { unclosed "(" 2 ")" 6 }
  ;

%inline asnop:
    PLUSEQUAL
    { ArithAssign Add }
  | MINUSEQUAL
    { ArithAssign Sub }
  | STAREQUAL
    { ArithAssign Mul }
  | SLASHEQUAL
    { ArithAssign Div }
  | PERCENTEQUAL
    { ArithAssign Mod }
  | LESSLESSEQUAL
    { ArithAssign Lsl }
  | GREATERGREATEREQUAL
    { ArithAssign Asr }
  | AMPERSANDEQUAL
    { ArithAssign And }
  | BAREQUAL
    { ArithAssign Or }
  | CARETEQUAL
    { ArithAssign Xor }
  | EQUAL
    { Assign }
  ;

inbrackets(X):
    LBRACKET x = X RBRACKET
    { x }
  | LBRACKET X error
    { unclosed "[" 1 "]" 3 }
  ;

lval:
    expr
    { try maptxt lval $1 with Exit -> expecting 1 "lvalue" }
  ;

simple:
    lv = lval op = asnop e = expr
    { SAssign (lv, op, e) }
  | lval PLUSPLUS
    { SAssign ($1, ArithAssign Add, mkdummyloc (EInt 1l)) }
  | lval MINUSMINUS
    { SAssign ($1, ArithAssign Sub, mkdummyloc (EInt 1l)) }
  | expr
    { SExpr $1 }
  ;

opt_equal_expr:
    x = option(preceded(EQUAL, expr))
    { x }
  ;

block_stmts:
    /* empty */
    { SEmpty }
  | stmt block_stmts
    { seq $1 $2 }
  | tp ident opt_equal_expr semi_or_fail block_stmts
    { SDef ($1, $2, $3, $5) }
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

simple_or_defn:
    /* nothing */
    { SEmpty }
  | simple
    { $1 }
  | tp ident opt_equal_expr
    { SDef ($1, $2, $3, SEmpty) }
  ;

opt_simple:
    /* nothing */
    { SEmpty }
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
    { SIf ($2, $3, $5) }
  | IF inparen_or_fail(expr) stmt
    { SIf ($2, $3, SEmpty) }
  | WHILE inparen_or_fail(expr) stmtu
    { SWhile ($2, $3) }
  ;

stmtm:
    simple SEMI
    { $1 }
  | IF inparen_or_fail(expr) stmtm ELSE stmtm
    { SIf ($2, $3, $5) }
  | WHILE inparen_or_fail(expr) stmtm
    { SWhile ($2, $3) }
  | FOR LPAREN simple_or_defn SEMI expr SEMI opt_simple RPAREN stmtm
    { for_to_while $3 $5 $7 $9 }
  | RETURN SEMI
    { SReturn None }
  | RETURN expr SEMI
    { SReturn (Some $2) }
  | block
    { $1 }
  | ASSERT inparen_or_fail(expr) semi_or_fail
    { SAssert $2 }
  | ERROR inparen_or_fail(expr) semi_or_fail
    { SError $2 }
  | BREAK semi_or_fail
    { SBreak }
  | CONTINUE semi_or_fail
    { SContinue }
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
