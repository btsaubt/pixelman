   (*check the resulting AST, generate LLVM IR, and dump the module *)

type action = Ast | Sast | LLVM_IR | Compile

let _ =
  let action = if Array.length Sys.argv > 1 then
    List.assoc Sys.argv.(1) [ ("-a", Ast);	(* Print the AST only *)
                              ("-s", Sast); 
			      ("-l", LLVM_IR);  (* Generate LLVM, don't check *)
			      ("-c", Compile) ] (* Generate, check LLVM IR *)
  else Compile in

let file_to_string file = 
    let array_string = ref [] in
      let ic = file in
          try
             while true do 
                 array_string := List.append !array_string [input_line ic]
              done;
              String.concat "\n" !array_string
          with End_of_file -> close_in ic; String.concat "\n" !array_string 

  in 
  let in_file = open_in "stdlib.px" in 
  let string_in = file_to_string in_file in
  let other_file = file_to_string stdin in
  let str = String.concat "\n" [other_file; string_in] in


  let lexbuf = Lexing.from_string str in
  let ast = Parser.program Scanner.token lexbuf in
  let sast = Semant.check ast in
  match action with
    Ast -> print_string (Ast.string_of_program ast)
  | LLVM_IR -> print_string (Llvm.string_of_llmodule (Codegen.translate sast))
  | Compile -> let m = Codegen.translate sast in
    Llvm_analysis.assert_valid_module m;
    print_string (Llvm.string_of_llmodule m)
