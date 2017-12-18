(* Abstract Syntax Tree and functions for printing it 
 * Anthony Chan
 * Gabriel Kramer-Garcia
 * Brian Tsau
 * Teresa Choe
*)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or | Divint | Shiftleft | Shiftright | Bitand | Bitor | Bitxor |
          Mod

type uop = Neg | Not | IntCast | FloatCast

type expr =
    Int_Literal of int
  | Float_Literal of float
  | Char_Literal of char
  | String_Literal of string 
  | Vector_Literal of expr list 
  | Matrix_Literal of expr list list 
  | Bool_Literal of bool
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of expr * expr
  | Call of string * expr list
  | VecAccess of string * expr
  | MatAccess of string * expr * expr
  | Noexpr

type typ = Int | Bool | Float | Char | String | Void | Image of expr * expr | Vector of typ * expr | Matrix of typ * expr * expr

type bind = typ * string

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  (* | Break
  | Continue  *)

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    locals : bind list;
    body : stmt list;
  }

type program = bind list * func_decl list

(* Pretty-printing functions *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"
  | Divint -> "//"
  | Mod -> "%"
  | Shiftleft -> "<<"
  | Shiftright -> ">>"
  | Bitand -> "&"
  | Bitor -> "|"
  | Bitxor -> "^"

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"
  | IntCast -> "(Int) "
  | FloatCast -> "(Float) "

let rec string_of_vector el = 
  "[" ^ String.concat ", " (List.map (fun e -> string_of_expr e) el) ^ "]"

and(* rec *) string_of_matrix el = "[|" ^
   String.concat " | " (List.map (fun v -> string_of_vector v) el) ^ "|]"

and(* rec *) string_of_expr = function
    Int_Literal(i) -> string_of_int i
  | Float_Literal(f) -> string_of_float f
  | Char_Literal(c) -> Char.escaped c
  | String_Literal(s) -> s
  | Bool_Literal(b) -> if b then "true" else "false"
  | Vector_Literal(el) -> string_of_vector el   
  | Matrix_Literal(el) -> string_of_matrix el
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(e1, e2) -> string_of_expr e1  ^ " = " ^ string_of_expr e2
(*   | Assign(v, e2) -> v ^ " = " ^ string_of_expr e2 *)
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | VecAccess(v, e) -> v ^ "[" ^ string_of_expr e ^ "]"
  | MatAccess(v, e1, e2) -> v ^ "[" ^ string_of_expr e1 ^ "]" ^ 
                                "[" ^ string_of_expr e2 ^ "]"
  | Noexpr -> ""

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
(*   | Break -> "break;" 
  | Continue -> "continue;" *)

let rec string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Char -> "char"
  | Float -> "float"
  | String -> "string"
  | Void -> "void"
  | Image(h, w) -> "Image[" ^ string_of_expr h ^ "," ^ string_of_expr w ^ "]"
  | Vector(t, e) -> string_of_typ t ^ "[" ^ string_of_expr e ^ "]"
  | Matrix(t, e1, e2) -> string_of_typ t ^ "[" ^ string_of_expr e1 ^ "][" ^ string_of_expr e2 ^ "]"

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  "def " ^ string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
