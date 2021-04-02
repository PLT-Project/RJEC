(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)

module L = Llvm
module A = Ast
open Sast 

module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
let translate (globals, functions, structs) =
  let context    = L.global_context () in
  
  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "RJEC" in

  (* Get types from the context *)
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context
  and void_t     = L.void_type   context in

  (* Return the LLVM type for a RJEC type *)
  let ltype_of_typ : A.typ -> L.lltype = function
      A.Int   -> i32_t
    | A.Bool  -> i1_t
    | A.Char ->  i8_t
  in

  (* Create a map of global variables after creating each *)
  let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n) = 
      let init = match t with
          _ -> L.const_int (ltype_of_typ t) 0
          (* TODO: add other cases *)
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in

  let printf_t : L.lltype = 
      L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue = 
      L.declare_function "printf" printf_t the_module in

  let printbool_t : L.lltype =
      L.function_type i32_t [| i1_t |] in
  let printbool_func : L.llvalue =
      L.declare_function "printbool" printbool_t the_module in

  (* Define each function (arguments and return type) so we can 
     call it even before we've created its body *)
  let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types = 
	      Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
      in let ftype = L.function_type i32_t formal_types in
    (* TODO: actually handle the multiple return types. was formerly:
      in let ftype = L.function_type (ltype_of_typ fdecl.styp) formal_types in
    *)
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in
  
  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    and char_format_str = L.build_global_stringptr "%c\n" "fmt" builder
    and str_format_str = L.build_global_stringptr "%s\n" "fmt" builder in
    (* TODO: reevaluate string formatting *)

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (t, n) p = 
        L.set_value_name n p;
        let local = L.build_alloca (ltype_of_typ t) n builder in
              ignore (L.build_store p local builder);
        StringMap.add n local m

      (* Allocate space for any locally declared variables and add the
       * resulting registers to our map *)
      and add_local m (t, n) =
        let local_var = L.build_alloca (ltype_of_typ t) n builder
        in StringMap.add n local_var m 
      in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.sformals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals (*fdecl.slocals*) []
    in

    (* Return the value for a variable or formal argument.
       Check local names first, then global names *)
    let rec lookup n ml = try StringMap.find n (List.hd ml)
                   with Not_found -> match List.tl ml with
                      [] -> raise (Failure("undeclared reference " ^ n))
                    | tail -> lookup n tail

(*     let rec type_of_identifier (v_name : string) (scope : typ StringMap.t list) =
      try
        StringMap.find v_name (List.hd scope)
      with Not_found -> match List.tl scope with
          [] -> raise (Failure("undeclared reference " ^ v_name))
        | tail -> type_of_identifier v_name tail*)


    in

    let vdecl_typ_to_typ : A.vdecl_typ -> A.typ = function
        Int -> Int
      | Bool -> Bool
      | Char -> Char
      | Chan(t) -> Chan(t)
      | ArrayInit(e, t) -> Array(t)
      | Struct(s) -> Struct(s)

     in 
    (* Construct code for an expression; return its value *)
    let rec expr m builder ((_, e) : sexpr) = match e with
    SIntLit i  -> L.const_int i32_t i
      | SStrLit s   -> L.build_global_stringptr s "strlit" builder
      | SCharLit c  -> L.const_int i8_t (Char.code (String.get c 0))
      | SBoolLit b  -> L.const_int i1_t (if b then 1 else 0)
      | SNoexpr     -> L.const_int i32_t 0
      | SId s       -> L.build_load (lookup s m) s builder
      | SBinop (e1, op, e2) ->
        let e1' = expr m builder e1
        and e2' = expr m builder e2 in
        (match op with
          A.Add     -> L.build_add
        | A.Sub     -> L.build_sub
        | A.Mult    -> L.build_mul
        | A.Div     -> L.build_sdiv
        | A.Mod     -> L.build_srem
        | A.And     -> L.build_and
        | A.Or      -> L.build_or
        | A.Equal   -> L.build_icmp L.Icmp.Eq
        | A.Less    -> L.build_icmp L.Icmp.Slt
        | A.Leq     -> L.build_icmp L.Icmp.Sle
        ) e1' e2' "tmp" builder
      | SUnop(op, ((t, _) as e)) ->
        let e' = expr m builder e in
        (match op with
          A.Neg                  -> L.build_neg
        | A.Not                  -> L.build_not) e' "tmp" builder

      | SCall ("printi", [e])  ->
        L.build_call printf_func [| int_format_str ; (expr m builder e) |]
          "printf" builder

      | SCall ("printc", [e]) -> 
        L.build_call printf_func [| char_format_str ; (expr m builder e) |]
          "printf" builder

      | SCall ("prints", [e]) -> 
        L.build_call printf_func [| str_format_str ; (expr m builder e) |]
        "printf" builder

      | SCall ("printb", [e]) ->
        L.build_call printbool_func [| (expr m builder e) |]
        "printbool" builder 

      | SCall (f, args) ->
        let (fdef, fdecl) = StringMap.find f function_decls in
        let llargs = List.rev (List.map (expr m builder) (List.rev args)) in
        (* TODO: fix multiple return types later *)
        let result = "" (*(match fdecl.styp with 
                              A.Void -> ""
                            | _ -> f ^ "_result")*) in
          L.build_call fdef (Array.of_list llargs) result builder
    in
    
    (* LLVM insists each basic block end with exactly one "terminator" 
       instruction that transfers control.  This function runs "instr builder"
       if the current block does not already have a terminator.  Used,
       e.g., to handle the "fall off the end of the function" case. *)
    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
	      Some _ -> ()
      | None -> ignore (instr builder) in
	
    (* Build the code for the given statement; return the builder for
       the statement's successor (i.e., the next instruction will be built
       after the one generated by this call) *)

    let rec stmt m builder = function
	      SBlock sl -> 
          let stmt_list builder ml sl =
            let helper (bldr, map) = stmt map bldr in
            let (b, mm) = List.fold_left helper (builder, ml) sl 
            in (b, List.tl mm)
          in
          stmt_list builder (StringMap.empty::m) sl 


      | SVdeclStmt vdl -> 
          let declare_var (builder, mm) (n, t) =   
            let local = L.build_alloca (ltype_of_typ (vdecl_typ_to_typ t)) n builder in
            let default_value = match t with
                Int | Bool | Char -> L.const_int (ltype_of_typ (vdecl_typ_to_typ t)) 0
              | _ -> raise(Failure("Not implemented"))
            in L.build_store default_value local builder;
            (* TODO: add to the symbol table?/manage scope? *)
            let new_m = StringMap.add n local (List.hd mm)
    
            in (builder, new_m::(List.tl mm)) 

          in List.fold_left declare_var (builder, m) vdl

          (* TODO: MOVE TYP FUNCTION OUT OF HERE *)
          (* PLEASE *)

      | SExpr e -> ignore(expr m builder e); (builder, m) 
      | SReturn e -> ignore(L.build_ret_void builder); (builder, m)
                    (* TODO: fix multiple return types later 
                    ignore(match fdecl.styp with
                              (* Special "return nothing" instr *)
                              A.Void -> L.build_ret_void builder 
                              (* Build return statement *)
                            | _ -> L.build_ret (expr builder e) builder);
                     builder*)
      | SAssignStmt s -> let assign_stmt builder = function
            SAssign sl -> ((List.fold_left (fun builder (s, e) -> let e' = expr m builder e in
            ignore(L.build_store e' (lookup s m) builder) ; builder) builder sl), m)
          | _         -> raise (Failure "not yet implemented")
        in assign_stmt builder s 


      | SIf (predicate, then_stmt, else_stmt) ->
        let bool_val = expr m builder predicate in
	      let merge_bb = L.append_block context "merge" the_function in
        let build_br_merge = L.build_br merge_bb in (* partial function *)

        let then_bb = L.append_block context "then" the_function in
        add_terminal (fst (stmt m (L.builder_at_end context then_bb) then_stmt))
          build_br_merge;

        let else_bb = L.append_block context "else" the_function in
        add_terminal (fst (stmt m (L.builder_at_end context else_bb) else_stmt))
          build_br_merge;

        ignore(L.build_cond_br bool_val then_bb else_bb builder);
        ((L.builder_at_end context merge_bb), m)


    (*  | SWhile (predicate, body) ->
	  let pred_bb = L.append_block context "while" the_function in
	  ignore(L.build_br pred_bb builder);

	  let body_bb = L.append_block context "while_body" the_function in
	  add_terminal (stmt (L.builder_at_end context body_bb) body)
	    (L.build_br pred_bb);

	  let pred_builder = L.builder_at_end context pred_bb in
	  let bool_val = expr pred_builder predicate in

	  let merge_bb = L.append_block context "merge" the_function in
	  ignore(L.build_cond_br bool_val body_bb merge_bb pred_builder);
	  L.builder_at_end context merge_bb

      (* Implement for loops as while loops *)
      | SFor (e1, e2, e3, body) -> stmt builder
	    ( SBlock [SExpr e1 ; SWhile (e2, SBlock [body ; SExpr e3]) ] ) *)
    in

    (* Build the code for each statement in the function *)
    
    let (builder, _) = stmt [local_vars ; global_vars] builder (SBlock fdecl.sbody) in


    (* Add a return if the last block falls off the end *)
    (* TODO: fix later *)
    add_terminal builder (L.build_ret (L.const_int i32_t 0))
      (*(match fdecl.styp with
        A.Void -> L.build_ret_void
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))*)
  in

  List.iter build_function_body functions;
  the_module
