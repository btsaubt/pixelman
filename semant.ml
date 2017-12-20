(* Semantic checking for the pixelman compiler 
 * Teresa Choe
 * Brian Tsau
 * Anthony Chan
 * Gabriel Kramer-Garcia
*)

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

  let protected_functions = ["print_int"; "perror"; "scan"; "size"; "load"; "write";
                                 "display"; "resize"; "transform"; "print_float"; "print_string"] in
  let rec check_protected = function
    [] -> ()
    | h :: t -> if List.mem h (List.map (fun fd -> fd.fname) functions)
        then raise (Failure ("function" ^ h ^ "may not be defined"))
        else ignore (check_protected t)
  in check_protected protected_functions;

  report_duplicate (fun n -> "duplicate function " ^ n)
    (List.map (fun fd -> fd.fname) functions);

  (* Function declaration for a named function *)
  let built_in_decls =  StringMap.add "print_int"
     { typ = Void; fname = "print_int"; formals = [(Int, "x")];
       locals = []; body = [] } (StringMap.add "printb"
     { typ = Void; fname = "printb"; formals = [(Bool, "x")];
       locals = []; body = [] } (StringMap.add "makePic"
     { typ = Void; fname = "makePic"; formals = [];
       locals = []; body = [] } (StringMap.add "printbig"
     { typ = Void; fname = "printbig"; formals = [(Int, "x")];
       locals = []; body = [] } (StringMap.add "print_string"
     { typ = Void; fname = "print_string"; formals = [(String, "x")];
       locals = []; body = [] } (StringMap.singleton "print_float" 
     { typ = Void; fname = "print_float"; formals = [(Float, "x")];
       locals = []; body = [] } )))))
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

    let is_vec_matrix t = match t with
        Vector(_,_) -> ()
      | Matrix(_,_,_) -> ()
      | _ -> raise (Failure ("cannot get size of non-vector/non-matrix type"))
    in

    let get_sexpr_type se = match se with
        SInt_Literal(_) -> Int
        | SFloat_Literal(_) -> Float
        | SChar_Literal(_) -> Char
        | SString_Literal(_) -> String
        | SBool_Literal(_) -> Bool
        | SVector_Literal(_, t) -> t
        | SMatrix_Literal(_, t) -> t
        | SId(_,t) -> t
        | SBinop(_,_,_,t) -> t
        | SUnop(_,_,t) -> t
        | SAssign(_,_,t) -> t
        | SVecAccess(_,_,t) -> t
        | SMatAccess(_,_,_,t) -> t
	| SImAccess(_,_,t) -> t
        | SCall(_,_,t) -> t
        | SSizeOf(_,t) -> t
        | SNoexpr -> Void
    in

    let get_binop_boolean_sexpr se1 se2 op = 
      let t1 = get_sexpr_type se1 in
      let t2 = get_sexpr_type se2 in
      match (t1, t2) with
        (Bool, Bool) -> SBinop(se1, op, se2, Bool)
        | _ -> raise (Failure ("can only perform boolean operators with Int/Bool types"))

    and get_unop_boolean_sexpr se op =
      let t = get_sexpr_type se in
      match t with
        Bool -> SUnop(op, se, Bool)
        | _ -> raise (Failure ("can only perform boolean operators with Int/Bool types"))

    and get_binop_arithmetic_sexpr se1 se2 op = 
      let t1 = get_sexpr_type se1 in
      let t2 = get_sexpr_type se2 in
      match (t1, t2) with
        (Int, Int) -> SBinop(se1, op, se2, Int)
        | (Int, Float) -> SBinop(SUnop(FloatCast, se1, Float), op, se2, Float)
        | (Float, Int) -> SBinop(se1, op, SUnop(FloatCast, se2, Float), Float)
        | (Float, Float) -> SBinop(se1, op, se2, Float)
        | (Vector(tm1, Int_Literal(i)), ta2) -> (match op with
              Mult -> (match ta2 with 
                  Float -> SCall("scalar_mult_vecf", [se2; se1], Vector(Float, Int_Literal(i)))
                  | Int -> if tm1 == Float 
                    then SCall("scalar_mult_vecf", [se2; se1], Vector(Float, Int_Literal(i)))
                    else SCall("scalar_mult_veci", [se2; se1], Vector(Int, Int_Literal(i)))
                  | Vector(tm2, _) -> if tm2 == Float || tm1 == Float 
                      then SCall("vec_dot_productf", [se2; se1], Float)
                      else SCall("vec_dot_producti", [se2; se1], Int)
                  | Matrix(tm2, _, Int_Literal(j2)) -> if tm2 == Float || tm1 == Float 
                          then SCall("vec_mat_multf", [se1; se2], Matrix(Float, Int_Literal(i), Int_Literal(j2)))
                          else SCall("vec_mat_multi", [se1; se2], Matrix(Int, Int_Literal(i), Int_Literal(j2)))
                  | _ -> raise (Failure ("can only perform binary arithmetic operators with Int/Float variables or matrices"))
              )
              | Sub -> (match ta2 with 
                    Vector(tm2, Int_Literal(i)) -> if tm2 == Float || tm1 == Float 
                      then SCall("vec_subf", [se1; se2], Vector(Float, Int_Literal(i)))
                      else SCall("vec_subi", [se1; se2], Vector(Int, Int_Literal(i)))
                      | _ -> raise (Failure ("oh no! can only perform this operation on vector of same length.")))
              | Add -> (match ta2 with 
                    Vector(tm2, Int_Literal(i)) -> if tm2 == Float || tm1 == Float 
                      then SCall("vec_addf", [se1; se2], Vector(Float, Int_Literal(i)))
                      else SCall("vec_addi", [se1; se2], Vector(Int, Int_Literal(i)))
                    | _ -> raise (Failure ("oh no! can only perform this operation on vector of same length.")))
              | _ -> raise (Failure ("oh no! cannot perform this operation on matrix.")))
        | (Matrix(tm1, Int_Literal(i), Int_Literal(j)), ta2) -> (match op with
              Mult -> (match ta2 with (* matrix x ta2 check *)
                      Float -> SCall("scalar_mult_matf", [se2; se1], Matrix(Float, Int_Literal(i), Int_Literal(j)))
                      | Int -> if tm1 == Float 
                        then SCall("scalar_mult_matf", [se2; se1], Matrix(Float, Int_Literal(i), Int_Literal(j)))
                        else SCall("scalar_mult_mati", [se2; se1], Matrix(Int, Int_Literal(i), Int_Literal(j)))
                      | Vector(tm2, _)  -> if tm2 == Float || tm1 == Float  
                          then SCall("mat_vec_multf", [se1; se2], Matrix(Float, Int_Literal(i), Int_Literal(1)))
                          else SCall("mat_vec_multi", [se1; se2], Matrix(Int, Int_Literal(i), Int_Literal(1)))
                      | Matrix(tm2, _, Int_Literal(j2)) -> if tm2 == Float || tm1 == Float 
                          then SCall("mat_mat_multf", [se1; se2], Matrix(Float, Int_Literal(i), Int_Literal(j2)))
                          else SCall("mat_mat_multi", [se1; se2], Matrix(Int, Int_Literal(i), Int_Literal(j2)))
                      | _ -> raise (Failure ("can only perform binary arithmetic operators with Int/Float variables or matrices")))
              | Sub -> (match ta2 with 
                  Matrix(tm2, Int_Literal(i), Int_Literal(j)) -> if tm2 == Float || tm1 == Float 
                    then SCall("mat_subf", [se1; se2], Matrix(Float, Int_Literal(i), Int_Literal(j)))
                    else SCall("mat_subi", [se1; se2], Matrix(Int, Int_Literal(i), Int_Literal(j)))
                  | _ -> raise (Failure ("oh no! can only perform this operation on matrix of same size.")))
              | Add -> (match ta2 with 
                  Matrix(tm2, Int_Literal(i), Int_Literal(j)) -> if tm2 == Float || tm1 == Float 
                    then SCall("mat_addf", [se1; se2], Matrix(Float, Int_Literal(i), Int_Literal(j)))
                    else SCall("mat_addi", [se1; se2], Matrix(Int, Int_Literal(i), Int_Literal(j)))
                    | _ -> raise (Failure ("oh no! can only perform this operation on matrix of same size.")))
              | _ -> raise (Failure ("oh no! cannot perform this operation on matrix.") ))
        | (Int, Vector(Int,Int_Literal(i))) -> SCall("scalar_mult_veci", [se1; se2], Vector(Int, Int_Literal(i)))
        | (Float, Vector(Int,Int_Literal(i))) | (Int, Vector(Float,Int_Literal(i))) | (Float, Vector(Float,Int_Literal(i))) -> SCall("scalar_mult_vecf", [se1; se2], Vector(Float, Int_Literal(i)))
        | (Int, Matrix(Int,Int_Literal(i),Int_Literal(j))) -> SCall("scalar_mult_mati", [se1; se2], Matrix(Int, Int_Literal(i), Int_Literal(j)))
        | (Float, Matrix(Int,Int_Literal(i),Int_Literal(j))) | (Int, Matrix(Float,Int_Literal(i),Int_Literal(j))) | (Float, Matrix(Float,Int_Literal(i),Int_Literal(j))) -> SCall("scalar_mult_matf", [se1; se2], Matrix(Float, Int_Literal(i), Int_Literal(j)))
        | _ -> raise (Failure ("can only perform binary arithmetic operators with Int/Float variables or matrices"))

    and get_unop_arithmetic_sexpr se op = 
      let t = get_sexpr_type se in
      match t with
        Int  -> SUnop(op, se, Int)
        | Float -> SUnop(op, se, Float)
        | Vector(Int,_) -> SCall("negVectori",[se],t)
        | Vector(Float,_) -> SCall("negVectorf",[se],t)
        | Matrix(Int,_,_) -> SCall("negMatrixi",[se],t)
        | Matrix(Float,_,_) -> SCall("negMatrixf",[se],t)
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

    and get_unop_cast_sexpr se op = 
      let t1 = get_sexpr_type se in
      let t = match t1 with
        Int | Float -> t1
        | Vector(t2,_) -> t2
        | Matrix(t2,_,_) -> t2
        | _ -> raise (Failure ("can only cast int/float expressions to int/float expressions"))
      in
      let op_t = match op with
        IntCast -> Int
        | FloatCast -> Float
        | _ -> raise (Failure ("this is impossible to reach :~)"))
      in
      if t == op_t then se else
        match t1 with
          Int  -> SUnop(op, se, op_t)
          | Float -> SUnop(op, se, op_t)
          | Vector(Int,se1) -> SCall("vec_int_to_float", [se], Vector(op_t,se1))
          | Vector(Float,se1) -> SCall("vec_float_to_int", [se], Vector(op_t,se1))
          | Matrix(Int,se1,se2) -> SCall("mat_int_to_float", [se], Matrix(op_t,se1,se2))
          | Matrix(Float,se1,se2) -> SCall("mat_float_to_int", [se], Matrix(op_t,se1,se2))
          | _ -> raise (Failure ("this is impossible to reach :~)"))

    and get_equality_type se1 se2 op = 
      let t1 = get_sexpr_type se1 in
      let t2 = get_sexpr_type se2 in
      match (t1, t2) with
        (Int, Int) -> SBinop(se1, op, se2, Bool)
        | (Float, Float) -> SBinop(se1, op, se2, Bool)
        | (Char, Char) -> SBinop(se1, op, se2, Bool)
        | _ -> raise (Failure ("can only compare ints/floats/chars with themselves for equality"))

    and check_image_accessor ia = 
	match ia with
	0 -> ()
	| 1 -> ()
	| 2 -> ()
	| _ -> raise (Failure ("can only access 0, 1, or 2"))
    
    and check_is_image id = 
	let idd = type_of_identifier id in
	match idd with
	Image(_,_) -> ()
	| _ -> raise (Failure ("can only access images"))
	
    in

    (* Return an sexpr given an expr *)
    let rec expr_to_sexpr = function
        Int_Literal(i) -> SInt_Literal(i)
      | String_Literal(s) -> SString_Literal(s)
      | Float_Literal(f) -> SFloat_Literal(f)
      | Bool_Literal(b) -> SBool_Literal(b)
      | Char_Literal(c) -> SChar_Literal(c)
      | Vector_Literal(el) -> check_vector_types el
      | Matrix_Literal(ell) -> check_matrix_types ell
      | Id s -> SId(s, type_of_identifier s)
      | SizeOf(s) -> is_vec_matrix (type_of_identifier s); SSizeOf(s, Int)
      | VecAccess(v, e) -> check_int_expr e; check_vec_access_type v; SVecAccess(v, expr_to_sexpr e, access_type (type_of_identifier v))
      | MatAccess(v, e1, e2) ->  check_int_expr e1; check_int_expr e2; check_mat_access_type v; SMatAccess(v, expr_to_sexpr e1, expr_to_sexpr e2, access_type (type_of_identifier v))
      | MatRow(s,e) -> check_int_expr e; check_mat_access_type s; get_mat_row_sexpr s e
      | MatCol(s,e) -> check_int_expr e; check_mat_access_type s; get_mat_col_sexpr s e
      | ImAccess(idd, color) -> ignore(check_image_accessor color); ignore(check_is_image idd); SImAccess(idd,color,(type_of_identifier idd))
      | Binop(e1, op, e2) (* as e *) -> get_binop_sexpr e1 e2 op
      | Unop(op, e) (* as ex *) -> get_unop_sexpr op e
      | Noexpr -> SNoexpr
      | Assign(var, e) (* as ex *) -> get_assign_sexpr var e
      (* | Call() *) (* let's put the std library functions in here *)
      | Call(fname, actuals) as call -> let fd = function_decl fname in
         if List.length actuals != List.length fd.formals then
           raise (Failure ("expecting " ^ string_of_int
             (List.length fd.formals) ^ " arguments in " ^ string_of_expr call))
         else
            SCall(fname,List.map2 (fun (ft, _) e -> let se = 
              let e' = expr_to_sexpr e 
              in let et2 = get_sexpr_type e'
              in match ft with
                Float -> if get_sexpr_type e' == Int then SUnop(FloatCast, e', Float) else e'
                | Vector(Float,_) -> (match et2 with
                    Vector(Float,_) -> e'
                  | Vector(Int,Int_Literal(i))   -> SUnop(FloatCast, e', Vector(Float,Int_Literal(i)))
                  | _                            -> raise (Failure("can only have vector of int/float")))
                | Matrix(Float,_,_) -> (match et2 with
                    Matrix(Float,_,_) -> e'
                  | Matrix(Int,Int_Literal(i),Int_Literal(j)) -> SUnop(FloatCast, e', Matrix(Float,Int_Literal(i),Int_Literal(j)))
                  | _                            -> raise (Failure("can only have vector of int/float")))
                | _ -> e'
            in
              let et = get_sexpr_type se in
                if check_formal_actual_call ft et then se else raise (Failure ("illegal actual argument found " ^
                  string_of_typ et ^ " expected " ^ string_of_typ ft ^ " in " ^
                  string_of_expr e))) fd.formals actuals
              , fd.typ)

    and check_formal_actual_call ft et = match ft with
      Vector(t1,_) -> (match et with
          Vector(t2,_) -> check_formal_actual_call t1 t2
        | _                       -> false)
      | Matrix(t1,_,_) -> (match et with
          Matrix(t2,_,_) -> check_formal_actual_call t1 t2
        | _            -> false)
      | Float          -> et == Int || et == Float
      | _              -> ft == et

    and check_vec_access_type v = 
      let t = type_of_identifier v
      in match t with
        Vector(_,_) -> ()
        | _ -> raise(Failure("cannot perform vector access on variable " ^ v))

    and check_mat_access_type v = 
      let t = type_of_identifier v
      in match t with
        Matrix(_,_,_) -> ()
        | _ -> raise(Failure("cannot perform matrix access on variable " ^ v))

    and get_mat_row_sexpr v e =
      let se = expr_to_sexpr e
      in let t = type_of_identifier v 
      in match t with
        Matrix(Int,_,Int_Literal(j)) -> SCall("get_mat_rowi", [SId(v,t); se], Vector(Int,Int_Literal(j)))
        | Matrix(Float,_,Int_Literal(j)) -> SCall("get_mat_rowf", [SId(v,t); se], Vector(Float,Int_Literal(j)))
        | _ -> raise(Failure("cannot get row of non-matrix type"))


    and get_mat_col_sexpr v e =
      let se = expr_to_sexpr e
      in let t = type_of_identifier v
      in match t with
        Matrix(Int,Int_Literal(i),_) -> SCall("get_mat_rowi", [SId(v,t); se], Vector(Int,Int_Literal(i)))
        | Matrix(Float,Int_Literal(i),_) -> SCall("get_mat_rowf", [SId(v,t); se], Vector(Float,Int_Literal(i)))
        | _ -> raise(Failure("cannot get row of non-matrix type"))

    (* only gets type of vector; does not go through whole vector all the time *)
    and get_vec_type el = match el with
        Int_Literal(_) :: ss -> get_vec_type ss
        | Float_Literal(_) :: _ -> Float
        | [] -> Int
        | _ -> raise (Failure ("vector/matrix literals can only contain float/int literals"))

    and check_vector_types el =
      let t = get_vec_type el in
      let check_vec_el e = match e with (* this time check the whole vector and convert *)
        Int_Literal(i) -> if t == Int then SInt_Literal(i) else SFloat_Literal(float i)
        | Float_Literal(i) -> SFloat_Literal(i)
        | _ -> raise (Failure ("vector/matrix literals can only contain float/int literals"))
      in
      SVector_Literal(List.map check_vec_el el, Vector(t, Int_Literal(List.length el)))

    and check_matrix_types ell =
      let check_row_lengths ell = (* check row lengths *)
        let length_first_list = List.length (List.hd ell) in
        List.iter (fun l -> if ((List.length l) != length_first_list) then 
                               raise (Failure ("matrix row lengths must be equal")) else ()) ell
      in
      let get_mat_type ell = 
        let rec check_row_types el = match el with
          Int_Literal(_) :: ss -> check_row_types ss
          | Float_Literal(_) :: _ -> Float_Literal(0.0)
          | [] -> Int_Literal(0)
          | _ -> raise (Failure ("vector/matrix literals can only contain float/int literals"))
        in
        get_vec_type (List.map check_row_types ell)
      in
      let t = get_mat_type ell in
      let mat_row_to_smat_row el =
        let mat_el_to_smat_el e = match e with
          Int_Literal(i) -> if t == Int then SInt_Literal(i) else SFloat_Literal(float i)
          | Float_Literal(f) -> SFloat_Literal(f)
          | _ -> raise (Failure ("vector/matrix literals can only contain float/int literals"))
        in
        List.map mat_el_to_smat_el el
      in
      check_row_lengths ell; SMatrix_Literal(List.map mat_row_to_smat_row ell, Matrix(t, Int_Literal(List.length ell), Int_Literal(List.length (List.hd ell))))
        
    and compare_vector_matrix_type v1 v2 = 
      match v1 with
        | Vector(ty1, _) -> 
                ( match v2 with
                  Vector(ty2, _) -> ty1 == ty2 || ((ty1 == Float) && (ty2 == Int))
                  | _ -> raise (Failure ("cannot compare vectors and matrices")) )
        | Matrix(ty1, _, _) -> 
                ( match v2 with
                  Matrix(ty2, _, _) -> 
                          ty1 == ty2 || ((ty1 == Float) && (ty2 == Int))
                  | _ -> raise (Failure ("cannot compare vectors and matrices")) )
        | _ -> raise (Failure ("matrix and vector dimensions must be int literals"))

    and get_binop_sexpr e1 e2 op =
      let se1 = expr_to_sexpr e1 in
      let se2 = expr_to_sexpr e2 in
      match op with
        Equal | Neq -> get_equality_type se1 se2 op
        | And | Or  -> get_binop_boolean_sexpr se1 se2 op
        | Less | Leq | Greater | Geq -> get_binop_comparison_sexpr se1 se2 op
        | Shiftleft | Shiftright | Bitand | Bitor | Bitxor -> get_binop_bitwise_sexpr se1 se2 op
        | Add | Sub | Mult | Div | Mod -> get_binop_arithmetic_sexpr se1 se2 op

    and get_unop_sexpr op e =
      let se = expr_to_sexpr e in
      match op with
        Neg -> get_unop_arithmetic_sexpr se op
        | Not -> get_unop_boolean_sexpr se op
        | IntCast | FloatCast -> get_unop_cast_sexpr se op

    and get_assign_sexpr e1 e2 =
      let se1 = match e1 with
        Id(_) | VecAccess(_,_) | MatAccess(_,_,_) -> expr_to_sexpr e1
        | _ -> raise (Failure ("can only assign to variable or vector/matrix element"))
      in 
      let se2 = expr_to_sexpr e2 in
      let lt = get_sexpr_type se1 in
      let rt = get_sexpr_type se2 in
      match lt with 
      Vector(_,_) -> if compare_vector_matrix_type lt rt then SAssign(se1,se2,lt) else raise (Failure ("illegal assignment "))
        | Matrix(_,_,_) -> if compare_vector_matrix_type lt rt then SAssign(se1,se2,lt) else raise (Failure ("illegal assignment "))
        | _ -> if lt == Float && rt == Int
               then SAssign(se1, SUnop(FloatCast, se2, Float), Float)
               else (if lt == rt then SAssign(se1,se2,lt) 
                       else raise (Failure ("illegal assignment " ^
                       string_of_typ lt ^ " = " ^ string_of_typ rt ^ 
                       " in: " ^ string_of_expr e1 ^ " = " ^ string_of_expr e2)))

    and check_bool_expr e = if get_sexpr_type (expr_to_sexpr e) != Bool
      then raise (Failure ("expected boolean expression in " ^ string_of_expr e))
      else ()

    and check_int_expr e = if get_sexpr_type (expr_to_sexpr e) != Int
      then raise (Failure ("expected integer expression in " ^ string_of_expr e))
      else ()

    and check_int_literal_expr e = match e with
      Int_Literal(_) -> ()
      | _ -> raise(Failure("can only declare vectors/matrices/images with int literals"))
    in

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
        if check_formal_actual_call t func.typ then SReturn(se) else
         raise (Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                         string_of_typ func.typ ^ " in " ^ string_of_expr e))
      | If(p, b1, b2) -> check_bool_expr p; SIf(expr_to_sexpr p, stmt_to_sstmt b1, stmt_to_sstmt b2)
      | For(e1, e2, e3, st) -> SFor(expr_to_sexpr e1, expr_to_sexpr e2, expr_to_sexpr e3, stmt_to_sstmt st)
      | While(p, s) -> check_bool_expr p; SWhile(expr_to_sexpr p, stmt_to_sstmt s)
      (* | Break -> SBreak
      | Continue -> SContinue *)
    in

    (* check variable declaration type *)
    let check_var_decl (t, id) = match t with
      Int | Bool | Float | Char | String -> (t, id)
      | Void -> raise (Failure ("cannot declare a void type variable"))
      | Vector(t1, e) -> check_int_literal_expr e;
        if (t1 != Float) && (t1 != Int)
        then raise(Failure("can only have vectors/matrices of ints/floats"))
        else (); (t, id)
      | Matrix(t1, e1, e2) -> check_int_literal_expr e1; check_int_literal_expr e2;
        if (t1 != Float) && (t1 != Int)
        then raise(Failure("can only have vectors/matrices of ints/floats"))
        else (); (t, id)
      | Image(h,w) -> check_int_literal_expr h; check_int_literal_expr w; (t, id)
    in

    let check_formal_bind (t, id) = match t with
      Vector(_,e) -> if e != Noexpr then check_int_literal_expr e else (); (t, id)
      | Matrix(_,e1,e2) -> if e1 != Noexpr || e2 != Noexpr then (check_int_literal_expr e1; check_int_literal_expr e2) else (); (t, id)
      | _ -> (t,id)
    in
    { 
      styp = func.typ;
      sfname = func.fname;
      sformals = List.map check_formal_bind func.formals;
      slocals = List.map check_var_decl func.locals;
      sbody = List.map stmt_to_sstmt func.body;
    }
   
  in 
  let sfdecls = List.map fdecl_to_sfdecl functions in
  (globals, sfdecls)

