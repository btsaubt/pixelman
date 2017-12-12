/* Ocamlyacc parser for MicroC */

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA DOT COLON 
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token RETURN IF ELSE FOR WHILE INT FLOAT BOOL VOID DEF STRING CHAR PIXEL
IMAGE
%token BREAK CONTINUE
%token LSHIFT RSHIFT BITAND BITXOR BITOR MOD DIVINT 
%token <int> INT_LITERAL
%token <string> ID
%token <char> CHAR_LITERAL
%token <float> FLOAT_LITERAL
%token <string> STRING_LITERAL
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left BITOR
%left BITXOR
%left BITAND 
%left EQ NEQ
%left LT GT LEQ GEQ
%left LSHIFT RSHIFT 
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
  | PIXEL { Pixel } 
  | IMAGE { Image } 

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }
  | vdecl_list vecdecl { $2 :: $1 }
  | vdecl_list matdecl { $2 :: $1 }

vdecl:
   typ ID SEMI { ($1, $2) }

vecdecl:
   typ LBRACKET expr RBRACKET ID SEMI { ($1, $5, $3) }

matdecl:
   typ LBRACKET expr RBRACKET LBRACKET expr RBRACKET ID SEMI { ($1, $8, $3, $6) }

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
    INT_LITERAL      { Int_Literal($1) }
  | STRING_LITERAL   { String_Literal($1) }
  | FLOAT_LITERAL    { Float_Literal($1) } 
  | CHAR_LITERAL     { Char_Literal($1) } 
  | TRUE             { BoolLit(true) }
  | FALSE            { BoolLit(false) }
  | ID               { Id($1) }
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr DIVINT expr { Binop($1, Divint,   $3) }
  | expr MOD expr { Binop($1, Mod,   $3) }
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
  | expr ASSIGN expr   { Assign($1, $3) }
  /*| ID MULTASSIGN expr { Assign($1, $3) } 
  | ID DIVASSIGN  expr { Assign($1, $3) } 
  | ID PLUSASSIGN expr { Assign($1, $3) } 
  | ID SUBASSIGN expr { Assign($1, $3) }
  | ID RASSIGN expr { Assign($1, $3) }
  | ID LASSIGN expr { Assign($1, $3) }
  | ID ANDASSIGN expr { Assign($1, $3) }
  | ID NOTASSIGN expr { Assign($1, $3) }
*/
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN { $2 }
  | ID LBRACKET expr RBRACKET { VecAccess($1, $3) }
  | ID LBRACKET expr RBRACKET LBRACKET expr RBRACKET { MatAccess($1, $3, $6) }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
