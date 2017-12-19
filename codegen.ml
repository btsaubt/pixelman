(* pixelman's Code generation: translate takes a semantically checked AST and produces LLVM IR
 * http://llvm.org/docs/tutorial/index.html
 * http://llvm.moe/
 * http://llvm.moe/ocaml/
 * Teresa Choe
 * Brian Tsau
 * Anthony Chan
 * Gabriel Kramer-Garcia
*)

module L = Llvm
module A = Ast
module S = Sast

module StringMap = Map.Make(String)

let translate (globals, functions) =
  let context = L.global_context () in
  let the_module = L.create_module context "Pixelman"
  and i32_t    = L.i32_type   context
  and i8_t     = L.i8_type    context
  and i1_t     = L.i1_type    context
  and f_t      = L.double_type context
  and array_t  = L.array_type
  and void_t   = L.void_type  context in

  let int_lit_to_int = function
    A.Int_Literal(i) -> i | _ -> raise(Failure("Can only make vector/matrix of dimension int literal"))
  in
  let rec ltype_of_typ = function
      A.Int -> i32_t
    | A.Float -> f_t
    | A.Bool -> i1_t
    | A.Char -> i8_t
    | A.String -> i32_t
    | A.Void -> void_t 
    | A.Vector(typ, size) -> (match typ with 
                             A.Int -> array_t i32_t (int_lit_to_int size)
                            | A.Float -> array_t f_t (int_lit_to_int size)
                            | _ -> raise(Failure("Can only make vector of type int/float")))
    | A.Matrix(t, s1, s2) -> (match t with 
                          A.Int -> array_t (array_t i32_t (int_lit_to_int s2)) (int_lit_to_int s1)
                            | A.Float -> array_t (array_t f_t (int_lit_to_int s2)) (int_lit_to_int s1)
                            | _ -> raise(Failure("Cannot only make vector of type int/float")))
    | A.Image(h,w) -> let mat_t = ltype_of_typ (A.Matrix(A.Int, h, w))
                      in array_t mat_t 3 (* make an array of 3 h x w matrices *)
  in
  (* Declare each global variable; remember its value in a map *)
  let global_vars =
    let global_var m (t, n) =
            let init = if ltype_of_typ t != f_t then
                           L.const_int (ltype_of_typ t) 0
                       else
                           L.const_float (ltype_of_typ t) 0.0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in

  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  (* Declare the built-in printbig() function *)
  let printbig_t = L.function_type i32_t [| i32_t |] in
  let printbig_func = L.declare_function "printbig" printbig_t the_module in

  (* Define each function (arguments and return type) so we can call it *)
  let function_decls =
    let function_decl m fdecl =
      let name = fdecl.S.sfname
      and formal_types =
	      Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.S.sformals)
      in let ftype = L.function_type (ltype_of_typ fdecl.S.styp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in
  
  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.S.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
    let string_format_str = L.build_global_stringptr "%s\n" "fmt" builder in
    let float_format_str = L.build_global_stringptr "%f\n" "fmt" builder in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (t, n) p = L.set_value_name n p;
        let local = L.build_alloca (ltype_of_typ t) n builder in
      	ignore (L.build_store p local builder);
      	StringMap.add n local m
      in

      let add_local m (t, n) =
  	    let local_var = L.build_alloca (ltype_of_typ t) n builder
  	    in StringMap.add n local_var m 
      in
      let formals = List.fold_left2 add_formal StringMap.empty fdecl.S.sformals
            (Array.to_list (L.params the_function)) 
      in
      List.fold_left add_local formals fdecl.S.slocals 
    in

    (* Return the value for a variable or formal argument *)
    let lookup n = try StringMap.find n local_vars
                   with Not_found -> StringMap.find n global_vars
    in

    let rec get_vector_acc_addr s e1 builder = L.build_gep (lookup s) 
        [| (L.const_int i32_t 0); (expr builder e1) |] s builder

    and get_matrix_acc_addr s e1 e2 builder = L.build_gep (lookup s) 
        [| L.const_int i32_t 0; expr builder e1; expr builder e2 |] s builder

    (* Construct code for an expression; return its value *)
    and expr builder = function
	    S.SInt_Literal i -> L.const_int i32_t i
      | S.SFloat_Literal fl -> L.const_float f_t fl
      | S.SChar_Literal c -> L.const_int i8_t (Char.code c)
      | S.SString_Literal s -> L.build_global_stringptr s "s" builder
      | S.SBool_Literal b -> L.const_int i1_t (if b then 1 else 0)
      | S.SVector_Literal (l, t) -> L.const_array (ltype_of_typ t) (Array.of_list (List.map (expr builder) l))
      | S.SMatrix_Literal (ell, t) -> (match t with 
            A.Matrix(A.Float,_,_) -> 
              let order = List.map List.rev ell in 
              let f_lists = List.map (List.map (expr builder)) order in
              let array_list = List.map Array.of_list f_lists in 
              let f_list_list = List.map (L.const_array f_t) array_list in
              let array_of_arrays = Array.of_list f_list_list in 
                L.const_array(array_t f_t (List.length (List.hd ell))) array_of_arrays 
          | A.Matrix(A.Int,_,_) -> 
            let order = List.map List.rev ell in 
            let i_lists = List.map (List.map (expr builder)) order in 
            let array_list = List.map Array.of_list i_lists in
            let i_list_array = List.map (L.const_array i32_t) array_list in 
            let array_of_arrays = Array.of_list i_list_array in 
              L.const_array(array_t i32_t (List.length (List.hd ell))) array_of_arrays 
          | _ -> raise(Failure(A.string_of_typ t)) 
        )

      | S.SNoexpr -> L.const_int i32_t 0
      | S.SId (s, _) -> L.build_load (lookup s) s builder
      | S.SSizeOf(vm,_) -> L.const_int i32_t (L.array_length (L.element_type (L.type_of (lookup vm))))
      | S.SVecAccess(s, e, _) -> L.build_load (get_vector_acc_addr s e builder) s builder
      | S.SMatAccess(s, e1, e2, _) -> L.build_load (get_matrix_acc_addr s e1 e2 builder) s builder
      | S.SBinop (e1, op, e2, _) -> (* too late to implement using sexpr types to make things easier *)
	  let e1' = expr builder e1
	  and e2' = expr builder e2 in
	  (match op with
	  (*  A.Add     -> L.build_add *)
            A.Add -> (let e1_type_string = L.string_of_lltype (L.type_of e1') in
                      (match e1_type_string with 
                         "double" -> L.build_fadd 
                       | "i32" -> L.build_add
                       | _ -> raise(Failure("Illegal type operation")) )) 
          | A.Sub      -> (let e1_type_string = L.string_of_lltype (L.type_of e1') in 
                          (match e1_type_string with 
                            "double" -> L.build_fsub
                          | "i32"  -> L.build_sub
                          | _ -> raise(Failure("Illegal type operation")) ))
          | A.Mod     -> L.build_urem
          | A.Mult    -> (let e1_type_string = L.string_of_lltype (L.type_of e1') in 
                          (match e1_type_string with 
                          "double" -> L.build_fmul
                           | "i32"  -> L.build_mul
                           | _ -> raise(Failure("illegal type operation")) ))
          | A.Div     -> (let e1_type_string = L.string_of_lltype (L.type_of e1') in 
                          (match e1_type_string with 
                             "double" -> L.build_fdiv
                           | "i32"  -> L.build_sdiv
                           | _ -> raise(Failure("illegal type operation")) ))
          | A.And     -> L.build_and
	        | A.Or      -> L.build_or
          | A.Bitxor  -> L.build_xor
          | A.Bitand  -> L.build_and
          | A.Bitor   -> L.build_or
          | A.Shiftright -> L.build_lshr
          | A.Shiftleft -> L.build_shl
          | A.Equal   -> (let e1_type_string = L.string_of_lltype (L.type_of e1') in 
                          (match e1_type_string with 
                          "double" -> L.build_fcmp L.Fcmp.Oeq
                           | "i32" -> L.build_icmp L.Icmp.Eq
                           | _ -> raise(Failure("Illegal type operation")) )) 
          | A.Neq     -> (let e1_type_string = L.string_of_lltype (L.type_of e1') in 
                          (match e1_type_string with
                          "double" -> L.build_fcmp L.Fcmp.One
                           | "i32" -> L.build_icmp L.Icmp.Ne
                           | _ -> raise(Failure("Illegal type operation")) ))
          | A.Less    -> (let e1_type_string = L.string_of_lltype (L.type_of e1') in 
                          (match e1_type_string with 
                          "double" -> L.build_fcmp L.Fcmp.Olt
                           | "i32" -> L.build_icmp L.Icmp.Slt
                           | _ -> raise(Failure("Illegal type operation")) ))
          | A.Leq     -> (let e1_type_string = L.string_of_lltype (L.type_of e1') in
                          (match e1_type_string with 
                          "double" -> L.build_fcmp L.Fcmp.Ole
                           | "i32"   -> L.build_icmp L.Icmp.Sle
                           | _ -> raise(Failure("Illegal type operation")) )) 
          | A.Greater -> (let e1_type_string = L.string_of_lltype (L.type_of e1') in
                          (match e1_type_string with 
                          "double" -> L.build_fcmp L.Fcmp.Ogt
                           | "i32"   -> L.build_icmp L.Icmp.Sgt
                           | _ -> raise(Failure("Illegal type operation" )) ))
          | A.Geq     -> (let e1_type_string = L.string_of_lltype (L.type_of e1') in 
                          (match e1_type_string with 
                          "double" -> L.build_fcmp L.Fcmp.Oge
                           | "i32"   -> L.build_icmp L.Icmp.Sge
                           | _ -> raise(Failure("Illegal type operation" )) )) 
	  ) e1' e2' "tmp" builder
      | S.SUnop(op, e, t) -> let e' = expr builder e in
	    (match op with
	        A.Neg         -> (if t == A.Float then L.build_fneg else L.build_neg) e' "tmp" builder
          | A.Not       -> L.build_not e' "tmp" builder
          | A.IntCast   -> L.build_fptosi e' i32_t "float_to_int" builder
          | A.FloatCast -> L.build_sitofp e' f_t   "int_to_float" builder )
      | S.SAssign (v, e, _) -> let lsb = (match v with
                                 S.SId(n,_) -> lookup n
                                 | S.SVecAccess(s,e,_) -> get_vector_acc_addr s e builder
                                 | S.SMatAccess(s,e1,e2,_) -> get_matrix_acc_addr s e1 e2 builder
                                 | _ -> raise(Failure("Illegal assignment lvalue")))
                               in
                               let rsb = expr builder e in
                               ignore (L.build_store rsb lsb builder); rsb
      | S.SCall ("print_int", [e], _) ->
	        L.build_call printf_func [| int_format_str ; (expr builder e) |]
	        "printf" builder
      | S.SCall ("print_string", [e], _) ->
                L.build_call printf_func [| string_format_str ; (expr builder e) |]
                "print_string" builder
      | S.SCall ("print_float", [e], _) ->
                L.build_call printf_func [| float_format_str ; (expr builder e) |]
                "print_float" builder
      | S.SCall ("printbig", [e], _) ->
	        L.build_call printbig_func [| (expr builder e) |] "printbig" builder
      | S.SCall (f, act, _) ->
        let (fdef, fdecl) = StringMap.find f function_decls in
           let actuals = List.rev (List.map (expr builder) (List.rev act)) in
           let result = (match fdecl.S.styp with A.Void -> ""
                                               | _      -> f ^ "_result" ) in
           L.build_call fdef (Array.of_list actuals) result builder
        in
    (* Invoke "f builder" if the current block doesn't already
       have a terminal (e.g., a branch). *)
    let add_terminal builder f =
      match L.block_terminator (L.insertion_block builder) with
	Some _ -> ()
      | None -> ignore (f builder) in
	
    (* Build the code for the given statement; return the builder for
       the statement's successor *)
    let rec stmt builder = function
	S.SBlock sl -> List.fold_left stmt builder sl
      | S.SExpr e -> ignore (expr builder e); builder
      | S.SReturn e -> ignore (match fdecl.S.styp with
	  A.Void -> L.build_ret_void builder
	| _ -> L.build_ret (expr builder e) builder); builder
      | S.SIf (predicate, then_stmt, else_stmt) ->
         let bool_val = expr builder predicate in
	 let merge_bb = L.append_block context "merge" the_function in

	 let then_bb = L.append_block context "then" the_function in
	 add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
	   (L.build_br merge_bb);

	 let else_bb = L.append_block context "else" the_function in
	 add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
	   (L.build_br merge_bb);

	 ignore (L.build_cond_br bool_val then_bb else_bb builder);
	 L.builder_at_end context merge_bb

      | S.SWhile (predicate, body) ->
	  let pred_bb = L.append_block context "while" the_function in
	  ignore (L.build_br pred_bb builder);

	  let body_bb = L.append_block context "while_body" the_function in
	  add_terminal (stmt (L.builder_at_end context body_bb) body)
	    (L.build_br pred_bb);

	  let pred_builder = L.builder_at_end context pred_bb in
	  let bool_val = expr pred_builder predicate in

	  let merge_bb = L.append_block context "merge" the_function in
	  ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
	  L.builder_at_end context merge_bb

      | S.SFor (e1, e2, e3, body) -> stmt builder
	    ( S.SBlock [S.SExpr e1 ; S.SWhile (e2, S.SBlock [body ; S.SExpr e3]) ] )
    in

    (* Build the code for each statement in the function *)
    let builder = stmt builder (S.SBlock fdecl.S.sbody) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.S.styp with
        A.Void -> L.build_ret_void
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

  List.iter build_function_body functions;
  the_module
