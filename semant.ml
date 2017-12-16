(* Semantic checking for the MicroC compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of a program. Returns void if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check (globals, functions) =

  (* Raise an exception if the given list has a duplicate *)
  let report_duplicate exceptf list =
    let rec helper = function
	n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
      | _ :: t -> helper t
      | [] -> ()
    in helper (List.sort compare list)
  in

  (* Raise an exception if a given binding is to a void type *)
  let check_not_void exceptf = function
      (Void, n) -> raise (Failure (exceptf n))
    | _ -> ()
  in
   
  (**** Checking Global Variables ****)

  List.iter (check_not_void (fun n -> "illegal void global " ^ n)) globals;
   
  report_duplicate (fun n -> "duplicate global " ^ n) (List.map snd globals);

  (**** Checking Functions ****)

  let protected_functions = ["print"; "print_string"; "perror"; "scan"; "size"; "load"; "write";
                                 "display"; "resize"; "transform"; "print_float"; "printb"] in
  let rec check_protected = function
    [] -> ()
    | h :: t -> if List.mem h (List.map (fun fd -> fd.fname) functions)
        then raise (Failure ("function" ^ h ^ "may not be defined"))
        else ignore (check_protected t)
  in check_protected protected_functions;

  report_duplicate (fun n -> "duplicate function " ^ n)
    (List.map (fun fd -> fd.fname) functions);

  (* Function declaration for a named function *)
  let built_in_decls =  StringMap.add "print"
     { typ = Void; fname = "print"; formals = [(Int, "x")];
       locals = []; body = [] } (StringMap.add "printb"
     { typ = Void; fname = "printb"; formals = [(Bool, "x")];
       locals = []; body = [] } (StringMap.add "printbig"
     { typ = Void; fname = "printbig"; formals = [(Int, "x")];
       locals = []; body = [] } (StringMap.add "print_string"
     { typ = Void; fname = "print_string"; formals = [(String, "x")];
       locals = []; body = [] } (StringMap.singleton "print_float" 
     { typ = Void; fname = "print_float"; formals = [(Float, "x")];
       locals = []; body = [] } ))))
   in
     
  let function_decls = List.fold_left (fun m fd -> StringMap.add fd.fname fd m)
                         built_in_decls functions
  in

  let function_decl s = try StringMap.find s function_decls
       with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = function_decl "main" in (* Ensure "main" is defined *)

  let fdecl_to_sfdecl func =

    List.iter (check_not_void (fun n -> "illegal void formal " ^ n ^
      " in " ^ func.fname)) func.formals;

    report_duplicate (fun n -> "duplicate formal " ^ n ^ " in " ^ func.fname)
      (List.map snd func.formals);

    report_duplicate (fun n -> "duplicate local " ^ n ^ " in " ^ func.fname)
      (List.map snd func.locals);

    (* Type of each variable (global, formal, or local) *)
    let symbols = List.fold_left (fun m (t, n) -> StringMap.add n t m)
	StringMap.empty (globals @ func.formals @ func.locals )
    in

    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    let access_type = function
      Vector(t, _) -> t
      | Matrix(t, _, _) -> t
      | _ -> raise (Failure ("illegal matrix/vector access"))
    in

    let get_sexpr_type se = match se with
        SInt_Literal(_) -> Int
        | SFloat_Literal(_) -> Float
        | SChar_Literal(_) -> Char
        | SString_Literal(_) -> String
        | SBool_Literal(_) -> Bool
        | SId(_,t) -> t
        | SBinop(_,_,_,t) -> t
        | SUnop(_,_,t) -> t
        | SAssign(_,_,t) -> t
        | SVecAccess(_,_,t) -> t
        | SMatAccess(_,_,_,t) -> t
        | SCall(_,_,t) -> t
        | SNoexpr -> Void
    in

    let get_binop_boolean_sexpr se1 se2 op = 
      let t1 = get_sexpr_type se1 in
      let t2 = get_sexpr_type se2 in
      match (t1, t2) with
        (Int, Int) -> SBinop(se1, op, se2, Bool)
        | (Int, Bool) -> SBinop(se1, op, se2, Bool)
        | (Bool, Int) -> SBinop(se1, op, se2, Bool)
        | (Bool, Bool) -> SBinop(se1, op, se2, Bool)
        | _ -> raise (Failure ("can only perform boolean operators with Int/Bool types"))

    and get_unop_boolean_sexpr se op =
      let t = get_sexpr_type se in
      match t with
        Int -> SUnop(op, se, Bool)
        | Bool -> SUnop(op, se, Bool)
        | _ -> raise (Failure ("can only perform boolean operators with Int/Bool types"))

    and get_binop_arithmetic_sexpr se1 se2 op = 
      let t1 = get_sexpr_type se1 in
      let t2 = get_sexpr_type se2 in
      match (t1, t2) with
        (Int, Int) -> SBinop(se1, op, se2, Int)
        | (Int, Float) -> SBinop(se1, op, se2, Float)
        | (Float, Int) -> SBinop(se1, op, se2, Float)
        | (Float, Float) -> SBinop(se1, op, se2, Float)
        | _ -> raise (Failure ("can only perform binary arithmetic operators with Int/Float variables or matrices"))

    and get_unop_arithmetic_sexpr se op = 
      let t = get_sexpr_type se in
      match t with
        Int  -> SUnop(op, se, Int)
        | Float -> SUnop(op, se, Float)
        | _ -> raise (Failure ("can only perform unary arithmetic operators with Int/Float variables or matrices"))

    and get_binop_bitwise_sexpr se1 se2 op = 
      let t1 = get_sexpr_type se1 in
      let t2 = get_sexpr_type se2 in
      match (t1, t2) with
        (Int, Int) -> SBinop(se1, op, se2, Int)
        | _ -> raise (Failure ("can only perform bitwise operations on integer types"))

    and get_binop_comparison_sexpr se1 se2 op = 
      let t1 = get_sexpr_type se1 in
      let t2 = get_sexpr_type se2 in
      match (t1, t2) with
        (Int, Int) -> SBinop(se1, op, se2, Bool)
        | (Float, Float) -> SBinop(se1, op, se2, Bool)
        | _ -> raise (Failure ("can only compare ints/floats with themselves for inequalities"))

    and get_equality_type se1 se2 op = 
      let t1 = get_sexpr_type se1 in
      let t2 = get_sexpr_type se2 in
      match (t1, t2) with
        (Int, Int) -> SBinop(se1, op, se2, Bool)
        | (Float, Float) -> SBinop(se1, op, se2, Bool)
        | (Char, Char) -> SBinop(se1, op, se2, Bool)
        | _ -> raise (Failure ("can only compare ints/floats/chars with themselves for equality"))
    in

    (* Return an sexpr given an expr *)
    let rec expr_to_sexpr = function
        Int_Literal(i) -> SInt_Literal(i)
      | String_Literal(s) -> SString_Literal(s)
      | Float_Literal(f) -> SFloat_Literal(f)
      | Bool_Literal(b) -> SBool_Literal(b)
      | Char_Literal(c) -> SChar_Literal(c)
      | Vector_Literal(el) -> SVector_Literal(List.map check_vec_el el, get_vec_type el)
      | Matrix_Literal(ell) -> check_matrix_types ell
      | Id s -> SId(s, type_of_identifier s)
      | VecAccess(v, e) -> check_int_expr e; SVecAccess(v, e, access_type (type_of_identifier v))
      | MatAccess(v, e1, e2) ->  check_int_expr e1; check_int_expr e2; SMatAccess(v, e1, e2, access_type (type_of_identifier v))
      | Binop(e1, op, e2) (* as e *) -> get_binop_sexpr e1 e2 op
      | Unop(op, e) (* as ex *) -> get_unop_sexpr op e
      | Noexpr -> SNoexpr
      | Assign(var, e) (* as ex *) -> get_assign_sexpr var e
      | Call(fname, actuals) as call -> let fd = function_decl fname in
         if List.length actuals != List.length fd.formals then
           raise (Failure ("expecting " ^ string_of_int
             (List.length fd.formals) ^ " arguments in " ^ string_of_expr call))
         else
           SCall(fname,List.map2 (fun (ft, _) e -> let se = expr_to_sexpr e in
            let et = get_sexpr_type se in
              if et == ft then se else raise (Failure ("illegal actual argument found " ^
                string_of_typ ft ^ " expected " ^ string_of_typ et ^ " in " ^
                string_of_expr e))) fd.formals actuals
            ,fd.typ)

    and check_vec_el e = match e with
        Int_Literal(i) -> SInt_Literal(i)
        | Float_Literal(i) -> SFloat_Literal(i)
        | _ -> raise (Failure ("vector/matrix literals can only contain float/int literals"))

    and get_vec_type = function
        Int_Literal(_) :: ss -> get_vec_type ss
        | Float_Literal(_) -> Float
        | [] -> Int

    and check_matrix_types ell = SMatrix_Literal(expr_list_to_sexpr_list ell, get_mat_type ell)

    and expr_list_to_sexpr_list ell = 
      let check_list_lengths =
        let length_first_list = List.length (List.hd ell) in
        List.iter (fun l -> if (List.compare_length_with l length_first_list) then 
                               raise (Failure ("matrix row lengths must be equal")) else ()) ell
      in
      check_list_lengths; List.map get_mat_sexpr ell

    and get_mat_sexpr ell = 
      List.map (fun el -> List.map check_vec_el el) ell

    and get_mat_type ell = 
      let mtype = 


    (* and check_matrix_types e = match e with
        Int_Literal(i) -> SInt_Literal(i)
        | Float_Literal(i) -> SFloat_Literal(i)
        | _ -> raise (Failure ("vector/matrix literals can only contain float/int literals")) *)
        

    and get_binop_sexpr e1 e2 op =
      let se1 = expr_to_sexpr e1 in
      let se2 = expr_to_sexpr e2 in
      match op with
        Equal | Neq -> get_equality_type se1 se2 op
        | And | Or  -> get_binop_boolean_sexpr se1 se2 op
        | Less | Leq | Greater | Geq -> get_binop_comparison_sexpr se1 se2 op
        | Shiftleft | Shiftright | Bitand | Bitor | Bitxor -> get_binop_bitwise_sexpr se1 se2 op
        | Add | Sub | Mult | Div | Divint | Mod -> get_binop_arithmetic_sexpr se1 se2 op

    and get_unop_sexpr op e =
      let se = expr_to_sexpr e in
      match op with
        Neg -> get_unop_arithmetic_sexpr se op
        | Not -> get_unop_boolean_sexpr se op

    and get_assign_sexpr var e =
      let lt = type_of_identifier var in
      let se = expr_to_sexpr e in
      let rt = get_sexpr_type se in
      if lt == rt then SAssign(var,se,lt) else raise (Failure ("illegal assignment " ^
         string_of_typ lt ^ " = " ^ string_of_typ rt ^ " in " ^ string_of_expr e))

    and check_bool_expr e = if get_sexpr_type (expr_to_sexpr e) != Bool
      then raise (Failure ("expected boolean expression in " ^ string_of_expr e))
      else ()

    and check_int_expr e = if get_sexpr_type (expr_to_sexpr e) != Int
      then raise (Failure ("expected integer expression in " ^ string_of_expr e))
      else () in

    (* Return a sstmt given a stmt *)
    let rec stmt_to_sstmt = function
	    Block(sl) -> let rec check_block = function (* just check if return statement is end of block *)
           [Return _] -> ()
         | Return _ :: _ -> raise (Failure "nothing may follow a return")
         | Block sl :: ss -> check_block (sl @ ss)
         | _ :: ss -> (* stmt_to_sstmt s; *) check_block ss
         | [] -> ()
        in 
        check_block sl; SBlock(List.map stmt_to_sstmt sl)
      | Expr(e) -> SExpr(expr_to_sexpr e)
      | Return(e) -> let se = expr_to_sexpr e in 
        let t = get_sexpr_type se in
        if t == func.typ then SReturn(se) else
         raise (Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                         string_of_typ func.typ ^ " in " ^ string_of_expr e))
      | If(p, b1, b2) -> check_bool_expr p; SIf(expr_to_sexpr p, stmt_to_sstmt b1, stmt_to_sstmt b2)
      | For(e1, e2, e3, st) -> SFor(expr_to_sexpr e1, expr_to_sexpr e2, expr_to_sexpr e3, stmt_to_sstmt st)
      | While(p, s) -> check_bool_expr p; SWhile(expr_to_sexpr p, stmt_to_sstmt s)
      | Break -> SBreak
      | Continue -> SContinue
    in

    (* check variable declaration type *)
    let check_var_decl (t, id) = match t with
      Int | Bool | Float | Char | String -> (t, id)
      | Void -> raise (Failure ("cannot declare a void type variable"))
      | Vector(_, e) -> check_int_expr e; (t, id)
      | Matrix(_, e1, e2) -> check_int_expr e1; check_int_expr e2; (t, id)
    in

    { 
      styp = func.typ;
      sfname = func.fname;
      sformals = func.formals;
      slocals = List.map check_var_decl func.locals;
      sbody = List.map stmt_to_sstmt func.body;
    }
   
  in 
  let sfdecls = List.map fdecl_to_sfdecl functions in
  (globals, sfdecls)

