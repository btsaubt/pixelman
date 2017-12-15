(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or | Divint | Shiftleft | Shiftright | Bitand | Bitor | Bitxor |
          Mod

type uop = Neg | Not

type expr =
    Int_Literal of int
  | Float_Literal of float
  | Char_Literal of char
  | String_Literal of string 
  | Vector_Literal of expr list 
  | Matrix_Literal of expr list list 
  | Pixel of expr * expr * expr * expr * expr
  | Image of expr * expr 
  | BoolLit of bool
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of string * expr
  | Call of string * expr list
(*  | VecAccess of string * expr
  | MatAccess of string * expr * expr *)
  | Noexpr

type typ = Int | Bool | Float | Char | String | Void | (*Image | Pixel |*) Vector of typ * expr | Matrix of typ * expr * expr

type bind = typ * string

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | Break
  | Continue 

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

let string_of_vector el = 
  let rec string_of_vector_literal = function
      [] -> "]" 
    | [hd] -> (match hd with
              Int_Literal(i) -> string_of_int i 
            | Float_Literal(f) -> string_of_float f
            | _ -> raise( Failure("Illegal type for vector list")))
    | hd::tl -> (match hd with 
                  Int_Literal(i) -> string_of_int i ^ ", " 
                | Float_Literal(f) -> string_of_float f ^ ", "
                | _ -> raise( Failure("Illegal type for vector list")))
  in 
  "[" ^ string_of_vector_literal el 

let rec string_of_expr = function
    Int_Literal(i) -> string_of_int i
  | Float_Literal(f) -> string_of_float f
  | Char_Literal(c) -> Char.escaped c
  | String_Literal(s) -> s
  | Vector_Literal(el) -> string_of_vector el   

  (*| Pixel(r, g, b, x, y) -> "Pixel(" ^ string_of_expr r ^ ", " ^ string_of_expr g ^ ", " ^ 
                            string_of_expr b ^ ", " ^ string_of_expr x ^ ", " ^ 
                            string_of_expr y 
  | Image(h, w) -> string_of_expr h ^ ", " ^ string_of_expr w
  *)  
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  (* | Assign(e1, e2) -> string_of_expr e1  ^ " = " ^ string_of_expr e2 *)
  | Assign(v, e2) -> v ^ " = " ^ string_of_expr e2
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
(*  | VecAccess(v, e) -> v ^ "[" ^ string_of_expr e ^ "]"
  | MatAccess(v, e1, e2) -> v ^ "[" ^ string_of_expr e1 ^ "]" ^ 
                                "[" ^ string_of_expr e2 ^ "]" *)
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
  | Break -> "break;" 
  | Continue -> "continue;"

let rec string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Char -> "char"
  | Float -> "float"
  | String -> "string"
  | Void -> "void"
  (*| Pixel -> "Pixel" 
  | Image -> "Image" *)
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
