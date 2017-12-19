(* pixelman's Semantically Checked Abstract Syntax Tree and functions for printing it 
 * Gabriel Kramer-Garcia
 * Anthony Chan
 * Teresa Choe
 * Brian Tsau
*)

open Ast

(* sexpressions, ssome swith sdatatype sas sadditional sinformation *)
type sexpr =
    SInt_Literal of int
  | SFloat_Literal of float
  | SChar_Literal of char
  | SString_Literal of string
  | SBool_Literal of bool
  | SVector_Literal of sexpr list * typ
  | SMatrix_Literal of sexpr list list * typ
  | SId of string * typ
  | SBinop of sexpr * op * sexpr * typ
  | SUnop of uop * sexpr * typ
  | SAssign of sexpr * sexpr * typ
  | SVecAccess of string * sexpr * typ
  | SMatAccess of string * sexpr * sexpr * typ
  | SMatRow of string * sexpr * typ
  | SMatCol of string * sexpr * typ
  | SCall of string * sexpr list * typ
  | SSizeOf of string * typ
  | SNoexpr

(* sstatements *)
type sstmt = 
    SBlock of sstmt list
  | SExpr of sexpr
  | SReturn of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SFor of sexpr * sexpr * sexpr * sstmt
  | SWhile of sexpr * sstmt
  (* | SBreak
  | SContinue  *)

(* sfunction sdeclarations *)
type sfunc_decl = {
  styp : typ;
  sfname : string;
  sformals : bind list;
  slocals : bind list;
  sbody : sstmt list;
}

(* sprogram *)
type sprogram = bind list * func_decl list
