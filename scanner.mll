(* Ocamllex scanner for pixelman *)

{ open Parser }

let character = [' '-'!' '#'-'[' ']'-'~'] | ('\\' ['\\' ''' '"' 'n' 'r' 't'])
let digit = ['0'-'9']

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| ":)"     { comment lexbuf }           (* Comments *)

    (* Separators *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| ';'      { SEMI }
| ','      { COMMA }
| "["      { LBRACKET } 
| "]"      { RBRACKET } 
(*| ':'      { COLON }
| "."      { DOT } *)

	(* Assignment Operators *)
| '='      { ASSIGN }
(*| "*="     { MULTASSIGN } 
| "/="     { DIVASSIGN }
| "%="     { MODASSIGN } 
| "+="     { PLUSASSIGN } 
| "-="     { SUBASSIGN } 
| ">>="    { RASSIGN } 
| "<<="    { LASSIGN } 
| "&="     { ANDASSIGN } 
| "!="     { NOTASSIGN } 
*)

    (* Binary Arithmetic Operators *)
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| "%"      { MOD } 
| "//"     { DIVINT } 

	(* Binary Comparison Operators *)
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }

	(* Binary Boolean Operators *)
| "&&"     { AND }
| "||"     { OR }

	(* Unary Boolean Operators *)
| "!"      { NOT }

	(* Binary Bitwise Operators *)
| "|"      { BITOR }
| "<<"     { LSHIFT } 
| ">>"     { RSHIFT }
| "&"      { BITAND }
| "^"      { BITXOR } 

	(* Branching Control *)
| "if"       { IF }
| "else"     { ELSE }
| "for"      { FOR }
| "while"    { WHILE }
| "continue" { CONTINUE } 
| "break"    { BREAK } 
| "return"   { RETURN }

	(* function definition *)
| "def"      { DEF } 

	(* Data and Return Types *)
| "char"   { CHAR }
| "int"    { INT }
| "float"  { FLOAT } 
| "bool"   { BOOL }
| "string" { STRING }
| "void"   { VOID }
| "true"   { TRUE }
| "false"  { FALSE }
(*| "Pixel"  { PIXEL } 
| "Image"  { IMAGE } *)

(* Literals *)
| ('-'?) digit+    as lxm    { INT_LITERAL(int_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| ''' character '''    as lxm    { CHAR_LITERAL(lxm.[1]) }
| '"' (character*) '"' as lxm    { STRING_LITERAL(lxm) }
| ('-'?) (digit+) ['.'] digit+ as lxm   { FLOAT_LITERAL(float_of_string lxm) } 

| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  ":)" { token lexbuf }
| _    { comment lexbuf }
