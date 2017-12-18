/* Ocamlyacc parser for Pixelman 
*  Teresa Choe 
*  Anthony Chan
*  Brian Tsau
*  Gabriel Kramer-Garcia 
*/

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA COLON 
%token LMATBRACK RMATBRACK
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT INTCAST FLOATCAST
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token RETURN IF ELSE FOR WHILE INT FLOAT BOOL VOID DEF STRING CHAR IMAGE

%token NOVECLBRACKET
%token BREAK CONTINUE
%token LSHIFT RSHIFT BITAND BITXOR BITOR MOD 
%token <int> INT_LITERAL
%token <string> ID
%token <char> CHAR_LITERAL
%token <float> FLOAT_LITERAL
%token <string> STRING_LITERAL
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%nonassoc NOVECLBRACKET
%nonassoc LMATBRACK
%nonassoc LBRACKET
%right ASSIGN
%left OR
%left AND
%left BITOR
%left BITXOR
%left BITAND 
%left EQ NEQ
%left LT GT LEQ GEQ
%left LSHIFT RSHIFT
%right INTCAST FLOATCAST
%left PLUS MINUS
%left TIMES DIVIDE MOD DIVINT
%right NOT NEG

%start program
%type <Ast.program> program

%%

program:
  decls EOF { $1 }

decls:
   /* nothing */ { [], [] }
 | decls vdecl { ($2 :: fst $1), snd $1 }
 | decls fdecl { fst $1, ($2 :: snd $1) }

fdecl:
   DEF typ ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
     { { typ = $2;
	 fname = $3;
	 formals = $5;
	 locals = List.rev $8;
	 body = List.rev $9 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    typ ID                   { [($1,$2)] }
  | formal_list COMMA typ ID { ($3,$4) :: $1 }

typ:
    INT { Int }
  | BOOL { Bool }
  | FLOAT { Float } 
  | CHAR { Char } 
  | STRING { String } 
  | VOID { Void } 
  | im_t { $1 }
/*  | vec_t { $1 } 
  | mat_t { $1 } */

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
   typ ID SEMI { ($1, $2) }
   | vec_t { $1 } 
   | mat_t { $1 } 
 

vec_t: 
  typ LBRACKET expr RBRACKET ID SEMI { (Vector($1, $3), $5) }  

mat_t: 
  typ LBRACKET expr RBRACKET LBRACKET expr RBRACKET ID SEMI { (Matrix($1, $3, $6), $8) }

/*
vec_t:
   typ LBRACKET expr RBRACKET %prec NOVECLBRACKET { Vector($1, $3) }
   
mat_t:
   typ LBRACKET expr RBRACKET LBRACKET expr RBRACKET { Matrix($1, $3, $6) }
*/

im_t:
   IMAGE LBRACKET expr COMMA expr RBRACKET { Image($3, $5) }


stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI { Expr $1 }
  | RETURN SEMI { Return Noexpr }
  | RETURN expr SEMI { Return $2 }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
     { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }
  | BREAK SEMI { Break }  
  | CONTINUE SEMI { Continue } 

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr: 
  literals { $1 } 
  | ID               { Id($1) }
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr MOD    expr { Binop($1, Mod,   $3) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | expr AND    expr { Binop($1, And,   $3) }
  | expr OR     expr { Binop($1, Or,    $3) }
  | expr LSHIFT expr { Binop($1, Shiftleft, $3) } 
  | expr RSHIFT     expr { Binop($1, Shiftright, $3) } 
  | expr BITAND     expr { Binop($1, Bitand, $3) } 
  | expr BITOR      expr { Binop($1, Bitor, $3) } 
  | expr BITXOR     expr { Binop($1, Bitxor, $3) } 
  | MINUS expr %prec NEG { Unop(Neg, $2) }
  | NOT expr         { Unop(Not, $2) }
  | INTCAST expr    { Unop(IntCast, $2) }
  | FLOATCAST expr  { Unop(FloatCast, $2) }
  | ID ASSIGN expr   { Assign($1, $3) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN { $2 }
  | ID LBRACKET expr RBRACKET { VecAccess($1, $3) }
  | ID LBRACKET expr RBRACKET LBRACKET expr RBRACKET { MatAccess($1, $3, $6) } 


primitive_literals:
    INT_LITERAL      { Int_Literal($1) }
  | STRING_LITERAL   { String_Literal($1) }
  | FLOAT_LITERAL    { Float_Literal($1) }
  | CHAR_LITERAL     { Char_Literal($1) }
  | TRUE             { Bool_Literal(true) }
  | FALSE            { Bool_Literal(false) }

literals: 
  primitive_literals { $1 } 
  | LBRACKET array_literal RBRACKET { Vector_Literal(List.rev $2) } 
  | LMATBRACK matrix_literal RMATBRACK { Matrix_Literal(List.rev $2) }

matrix_literal: 
  LBRACKET array_literal RBRACKET { [$2] } 
  | matrix_literal BITAND LBRACKET array_literal RBRACKET { $4 :: $1 } 

array_literal: 
  primitive_literals { [$1] } 
  | array_literal COMMA primitive_literals { $3 :: $1 } 

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
