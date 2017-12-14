(* Semantic checking for the MicroC compiler *)

open Ast

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

  let protected_functions = ["print"; "perror"; "scan"; "size"; "load"; "write";
                                 "display"; "resize"; "transform"] in
  let rec check_protected function_list = function
    [] -> ()
    | h :: t -> if List.mem h (List.map (fun fd -> fd.fname) functions)
        then raise (Failure ("function" ^ h ^ "may not be defined"))
        else ignore (check_protected t)
  in check_protected protected_functions;

  (*if List.mem "print" (List.map (fun fd -> fd.fname) functions)
  then raise (Failure ("function print may not be defined")) else ();*)

  report_duplicate (fun n -> "duplicate function " ^ n)
    (List.map (fun fd -> fd.fname) functions);

  (* Function declaration for a named function *)
  let built_in_decls =  StringMap.add "print"
     { typ = Void; fname = "print"; formals = [(Int, "x")];
       locals = []; body = [] } (StringMap.add "printb"
     { typ = Void; fname = "printb"; formals = [(Bool, "x")];
       locals = []; body = [] } (StringMap.add "printbig"
     { typ = Void; fname = "printbig"; formals = [(Int, "x")];
       locals = []; body = [] } (StringMap.singleton "print_string"
     { typ = Void; fname = "print_string"; formals = [(String, "x")];
       locals = []; body = [] })))
   in
     
  let function_decls = List.fold_left (fun m fd -> StringMap.add fd.fname fd m)
                         built_in_decls functions
  in

  let function_decl s = try StringMap.find s function_decls
       with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = function_decl "main" in (* Ensure "main" is defined *)

  let check_function func =

    List.iter (check_not_void (fun n -> "illegal void formal " ^ n ^
      " in " ^ func.fname)) func.formals;

    report_duplicate (fun n -> "duplicate formal " ^ n ^ " in " ^ func.fname)
      (List.map snd func.formals);

    List.iter (check_not_void (fun n -> "illegal void local " ^ n ^
      " in " ^ func.fname)) func.locals;

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

    let get_binop_boolean_sexpr se1 se2 op = 
      let t1 = get_sexpr_type se1 in
      let t2 = get_sexpr_type se2 in
      let sexpr = function
        (Int, Int) -> SBinop(se1, op, se2, Bool)
        | (Int, Bool) -> SBinop(se1, op, se2, Bool)
        | (Bool, Int) -> SBinop(se1, op, se2, Bool)
        | (Bool, Bool) -> SBinop(se1, op, se2, Bool)
        | _ -> raise (Failure ("can only perform boolean operators with Int/Bool types"))
      in sexpr (t1, t2)
    in

    let get_unop_boolean_sexpr se op =
      let t = get_sexpr_type se in
      let sexpr = function
        Int -> SUnop(op, se, Bool)
        | Bool -> SUnop(op, se, Bool)
        | _ -> raise (Failure ("can only perform boolean operators with Int/Bool types"))
      in sexpr t
    in

    let get_binop_arithmetic_sexpr se1 se2 op = 
      let t1 = get_sexpr_type se1 in
      let t2 = get_sexpr_type se2 in
      let sexpr = function
        (Int, Int) -> SBinop(se1, op, se2, Int)
        | (Int, Float) -> SBinop(se1, op, se2, Float)
        | (Float, Int) -> SBinop(se1, op, se2, Float)
        | (Float, Float) -> SBinop(se1, op, se2, Float)
        | _ -> raise (Failure ("can only perform binary arithmetic operators with Int/Float variables or matrices"))
      in sexpr (t1, t2)
    in

    let get_unop_arithmetic_sexpr se op = 
      let t = get_sexpr_type se in
      let sexpr = function
        Int  -> SUnop(op, se, Int)
        | Float -> SUnop(op, se, Float)
        | _ -> raise (Failure ("can only perform unary arithmetic operators with Int/Float variables or matrices"))
      in sexpr t
    in

    let get_binop_bitwise_sexpr se1 se2 op = 
      let t1 = get_sexpr_type se1 in
      let t2 = get_sexpr_type se2 in
      let sexpr = function
        (Int, Int) -> SBinop(se1, op, se2, Int)
        | _ -> raise (Failure ("can only perform bitwise operations on integer types"))
      in sexpr (t1 t2)
    in

    let get_binop_comparison_sexpr se1 se2 op = 
      let t1 = get_sexpr_type se1 in
      let t2 = get_sexpr_type se2 in
      let sexpr = function
        (Int, Int) -> SBinop(se1, op, se2, Bool)
        | (Float, Float) -> SBinop(se1, op, se2, Bool)
        | _ -> raise (Failure ("can only compare ints/floats with themselves for inequalities"))
      in sexpr (t1, t2)
    in

    let get_equality_type se1 se2 op = 
      let t1 = get_sexpr_type se1 in
      let t2 = get_sexpr_type se2 in
      let sexpr = function
        (Int, Int) -> SBinop(se1, op, se2, Bool)
        | (Float, Float) -> SBinop(se1, op, se2, Bool)
        | (Char, Char) -> SBinop(se1, op, se2, Bool)
        | _ -> raise (Failure ("can only compare ints/floats/chars with themselves for equality"))
      in sexpr (t1, t2)
    in

    let get_sexpr_type se = match se with
        SInt_Literal(_) -> Int
        | SFloat_Literal(_) -> Float
        | SString_Literal(_) -> String
        | SBool_Literal(_) -> Bool
        | SId(_,t) -> t
        | SBinop(_,_,_,t) -> t
        | SUnop(_,_,t) -> t
        | SAssign(_,_,t) -> t
        (* | SVecAccess(_,_,t) -> t
        | SMatAccess(_,_,_,t) -> t *)
        | SCall(_,_,t) -> t
        | SNoexpr -> Void
    in

    let get_binop_sexpr e1 e2 op =
      let se1 = expr_to_sexpr e1 in
      let se2 = expr_to_sexpr e2 in
      match op with
        Equal | Neq -> get_equality_type t1 t2 op
        | And | Or  -> get_binop_boolean_sexpr t1 t2 op
        | Less | Leq | Greater | Geq -> get_binop_comparison_sexpr t1 t2 op
        | Shiftleft | Shiftright | Bitand | Bitor | Bitxor -> get_binop_bitwise_sexpr t1 t2 op
        | Add | Sub | Mult | Div | Divint | Mod -> get_binop_arithmetic_sexpr t1 t2 op
        | _ -> raise (Failure ("invalid binary operator: " ^ op))

    and get_unop_sexpr op e =
      let se = expr_to_sexpr e in
      let t = get_sexpr_type se in
      match op with
        Neg -> get_unop_arithmetic_sexpr t
        | Not -> get_unop_boolean_sexpr t
        | _ -> raise (Failure ("invalid unary operator: " ^ op))

    and get_assign_sexpr var e =
      let lt = type_of_identifier var
      and let se = expr_to_sexpr e
      and let rt = get_sexpr_type se in
      if lt == rt then SAssign(var,se,lt) else raise (Failure ("illegal assignment " ^
         string_of_typ lt ^ " = " ^ string_of_typ rt ^ " in " ^ string_of_expr e))

    (* Return an sexpr given an expr *)
    and expr_to_sexpr = function
        Int_Literal(i) -> SInt_Literal(i)
      | String_Literal(s) -> SString_Literal(s)
      | Float_Literal(f) -> SFloat_Literal(f)
      | Bool_Literal(b) -> SBool_Literal(b)
      | Char_Literal(c) -> SChar_Literal(c)
(*      | Pixel(r, g, b, x, y) -> Pixel 
      | Image(h, w) -> Image *)
      | Id s -> SId(s, type_of_identifier s)
(*      | VecAccess(v, e) -> access_type (type_of_identifier v)
      | MatAccess(v, e1, e2) ->  access_type (type_of_identifier v) *)
      | Binop(e1, op, e2) (* as e *) -> get_binop_sexpr e1 e2 op
         (* let se1 = expr_to_sexpr e1 and se2 = expr_to_sexpr e2 in (match op with
            Add | Sub | Mult | Div |
            Bitor | Shiftleft 
        | Shiftright | Bitand | Bitxor | Mod | Divint
          when t1 = Int && t2 = Int -> Int
	| Equal | Neq when t1 = t2 -> Bool
	| Less | Leq | Greater | Geq when t1 = Int && t2 = Int || t1 = Float && t2 = Float -> Bool
        | And | Or when t1 = Bool && t2 = Bool -> Bool
        | _ -> raise (Failure ("illegal binary operator " ^
              string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
              string_of_typ t2 ^ " in " ^ string_of_expr e))
        ) *)
      | Unop(op, e) (* as ex *) -> get_unop_sexpr op e
      (* let t = check_expr e in
	 (match op with
	   Neg when t = Int -> Int
	 | Not when t = Bool -> Bool
         | _ -> raise (Failure ("illegal unary operator " ^ string_of_uop op ^
	  		   string_of_typ t ^ " in " ^ string_of_expr ex))) *)
      | Noexpr -> SNoexpr
      | Assign(var, e) (* as ex *) -> get_assign_sexpr var e
      (* let lt = type_of_identifier var
                                and rt = check_expr e in
        check_assign lt rt (Failure ("illegal assignment " ^ string_of_typ lt ^
				     " = " ^ string_of_typ rt ^ " in " ^ 
				     string_of_expr ex)) *)
      | Call(fname, actuals) (* as call *) -> let fd = function_decl fname in
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
    in

    let check_bool_expr e = if get_sexpr_type (expr_to_sexpr e) != Bool
     then raise (Failure ("expected boolean expression in " ^ string_of_expr e))
     else () in

    let check_int_expr e = if get_sexpr_type (expr_to_sexpr e) != Int
     then raise (Failure ("expected integer expression in " ^ string_of_expr e))
     else () in

    (* Return a sstmt given a stmt *)
    let rec stmt_to_sstmt = function
	  Block sl -> let rec check_block = function
           [Return _ as s] -> check_stmt s
         | Return _ :: _ -> raise (Failure "nothing may follow a return")
         | Block sl :: ss -> check_block (sl @ ss)
         | s :: ss -> check_stmt s ; check_block ss
         | [] -> ()
        in check_block sl
      | Expr e -> ignore (check_expr e)
      | Return e -> let t = check_expr e in if t = func.typ then () else
         raise (Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                         string_of_typ func.typ ^ " in " ^ string_of_expr e))
           
      | If(p, b1, b2) -> check_bool_expr p; check_stmt b1; check_stmt b2
      | For(e1, e2, e3, st) -> ignore (expr e1); check_bool_expr e2;
                               ignore (expr e3); check_stmt st
      | While(p, s) -> check_bool_expr p; check_stmt s
    in

    check_stmt (Block func.body)
   
  in 
  ignore(List.iter check_function functions);
  let sfdecls = List.map fdecl_to_sfdecl functions in
  (globals, sfdecls)

