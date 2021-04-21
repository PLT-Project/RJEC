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
  let i64_t      = L.i64_type    context
  and i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context
  and void_t     = L.void_type   context 
  and void_ptr_t = L.pointer_type (L.i8_type context) in
  let func_t     = L.pointer_type (L.function_type i32_t [| void_ptr_t |]) in

  let rec generate_seq n = if n >= 0 then (n :: (generate_seq (n-1))) else [] in
  
  let struct_decls : ((A.typ StringMap.t) * L.lltype) StringMap.t = 
    let add_struct m (n, ml) = 
      let ltype_of_basic_typ ((t, n) : (A.typ * string)) : L.lltype = match t with
          A.Int   -> i32_t
        | A.Bool  -> i1_t
        | A.Char ->  i8_t
        | _ -> raise(Failure("Struct member typ not basic -- should've been checked in parser!\n"))
      in
      let compare_by (_, n1) (_, n2) = compare n1 n2 in
      let member_typs = Array.of_list (List.map ltype_of_basic_typ (List.sort compare_by ml)) in
      let struct_t = L.struct_type context member_typs in
      let member_names = List.fold_left (fun m (t, n) -> StringMap.add n t m) StringMap.empty ml in
      StringMap.add n (member_names, struct_t) m
    in List.fold_left add_struct StringMap.empty structs
  in

  (* Return the LLVM type for a RJEC type *)
  let ltype_of_typ : A.typ -> L.lltype = function
      A.Int   -> i32_t
    | A.Bool  -> i1_t
    | A.Char ->  i8_t
    | A.Struct(n) -> snd (StringMap.find n struct_decls)
    | A.Chan(_) -> void_ptr_t
  in

  let typ_to_typ_char (t : A.typ) = match t with
      A.Int -> L.const_int i8_t (Char.code 'i')
    | A.Bool -> L.const_int i8_t (Char.code 'b')
    | A.Char -> L.const_int i8_t (Char.code 'c')
    | _ -> raise(Failure("non-basic type for chan; should've checked in parser"))
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

  let memset_t : L.lltype = 
    L.function_type void_ptr_t [| void_ptr_t ; i32_t ; i32_t |] in
  let memset_func : L.llvalue = 
    L.declare_function "memset" memset_t the_module in

  let printbool_t : L.lltype =
      L.function_type i32_t [| i1_t |] in
  let printbool_func : L.llvalue =
      L.declare_function "printbool" printbool_t the_module in

  let yeet_t : L.lltype =
      L.function_type void_t [| func_t ; void_ptr_t |] in
  let yeet_func : L.llvalue =
      L.declare_function "yeet" yeet_t the_module in

  let makechan_t : L.lltype = 
      L.function_type void_ptr_t [| i8_t ; i32_t |] in
  let makechan_func : L.llvalue =
    L.declare_function "makechan" makechan_t the_module in

  let send_t : L.lltype = 
    L.function_type void_t [| void_ptr_t ; i8_t ; i32_t |] in
  let send_func : L.llvalue =
    L.declare_function "send" send_t the_module in

  let recv_int_t : L.lltype = 
    L.function_type i32_t [| void_ptr_t |] in
  let recv_bool_t : L.lltype = 
    L.function_type i1_t [| void_ptr_t |] in
  let recv_char_t : L.lltype = 
    L.function_type i8_t [| void_ptr_t |] in
  let recv_int_func : L.llvalue =
    L.declare_function "recv_int" recv_int_t the_module in
  let recv_bool_func : L.llvalue =
    L.declare_function "recv_bool" recv_bool_t the_module in
  let recv_char_func : L.llvalue =
    L.declare_function "recv_char" recv_char_t the_module in

  let closechan_t : L.lltype = 
    L.function_type void_t [| void_ptr_t ; i8_t |] in
  let closechan_func : L.llvalue = 
    L.declare_function "closechan" closechan_t the_module in
  
  let clause_member_typs = [| i8_t ; void_ptr_t ; void_ptr_t ; i64_t |] in
  let clause_t = L.struct_type context clause_member_typs in
  let select_t : L.lltype =
    L.function_type i32_t [| L.pointer_type clause_t ; i32_t |] in
  let select_func : L.llvalue =
    L.declare_function "selectchan" select_t the_module in
    
  let function_arg_structs = 
    let add_function_arg_struct m fdecl =
      let member_typs = Array.of_list (List.map (fun (t, n) -> ltype_of_typ t) fdecl.sformals) in
      let struct_t = L.struct_type context member_typs in
      StringMap.add fdecl.sfname struct_t m
    in
    List.fold_left add_function_arg_struct StringMap.empty functions
  in

  (* Define each function (arguments and return type) so we can 
     call it even before we've created its body *)
  let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname in
    (* TODO: actually handle the multiple return types. *)
      let rt_type = match fdecl.stypes with
          [] -> i32_t
        | t :: [] -> ltype_of_typ t 
        | _ -> raise(Failure("Multiple return types not implemented yet")) in
      let ftype = L.function_type rt_type [| void_ptr_t |] in
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

    let formal_vals = 
      let args_type = StringMap.find fdecl.sfname function_arg_structs in
      let void_ptr = List.hd (Array.to_list (L.params the_function)) in
      let ptr = L.build_bitcast void_ptr (L.pointer_type args_type) (fdecl.sfname ^ "_args_ptr") builder in
      let args = L.build_load ptr (fdecl.sfname ^ "_args") builder in
      let idxs = List.rev (generate_seq ((List.length fdecl.sformals) - 1)) in
      let args_list = List.fold_left (fun l idx -> 
        let arg = L.build_extractvalue args idx (
          fdecl.sfname ^ "_arg_" ^ (string_of_int idx)
        ) builder in arg::l
      ) [] idxs in
      List.rev args_list
    in

    let local_vars =
      let add_formal m (t, n) p = 
        L.set_value_name n p;
        let local = 
          let create_local (t: A.typ) = match t with
              A.Struct(s) as t -> L.build_malloc (ltype_of_typ t) n builder
            | _ -> L.build_alloca (ltype_of_typ t) n builder in
          create_local t in
        ignore (L.build_store p local builder);
        StringMap.add n local m

      (* Allocate space for any locally declared variables and add the
       * resulting registers to our map *)
      and add_local m (t, n) =
        let local_var = L.build_alloca (ltype_of_typ t) n builder
        in StringMap.add n local_var m 
      in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.sformals
          formal_vals in
      List.fold_left add_local formals []
    in

    (* Return the value for a variable or formal argument.
       Check local names first, then global names *)
    let rec lookup n ml = try StringMap.find n (List.hd ml)
                   with Not_found -> match List.tl ml with
                      [] -> raise (Failure("codegen lookup: undeclared reference " ^ n))
                    | tail -> lookup n tail
    in

    let defered_el = [] in 

    let vdecl_typ_to_typ : A.vdecl_typ -> A.typ = function
        Int -> Int
      | Bool -> Bool
      | Char -> Char
      | Chan(t) -> Chan(t)
      | ArrayInit(e, t) -> Array(t)
      | Struct(s) -> Struct(s)
    in 

    let rec svdecl_typ_to_typ : svdecl_typ -> A.typ = function
        SInt -> Int
      | SBool -> Bool
      | SChar -> Char
      | SChan(t) -> Chan(svdecl_typ_to_typ t)
      | SArrayInit(t, e) -> Array(svdecl_typ_to_typ t)
      | SStruct(s) -> Struct(s)
    in 

    let default_value_of_typ (t: A.typ) builder = match t with 
        A.Int | A.Bool | A.Char -> L.const_int (ltype_of_typ t) 0
      | A.Chan(_) -> L.const_pointer_null void_ptr_t
      | A.Struct(n) -> 
        let (member_names, struct_t) = StringMap.find n struct_decls in 
        let compare_by (n1, _) (n2, _) = compare n1 n2 in
        let members = List.sort compare_by 
          (StringMap.bindings member_names) in
        let idxs = List.rev (generate_seq ((List.length members) - 1)) in
        let v = List.fold_left2 (fun agg i member -> 
          let (n, t) = member in
          (L.build_insertvalue agg (L.const_int (ltype_of_typ t) 0) i n builder))
        (L.const_null struct_t) idxs members in v
      | A.Array(t') -> L.const_pointer_null (L.pointer_type (ltype_of_typ t'))
      | _ -> raise(Failure("default values are arbitrary for structs and arrays!"))
    in

    (* Construct code for an expression; return its value *)
    let rec expr m builder ((_, e) : sexpr) = match e with
        SIntLit i  -> L.const_int i32_t i
      | SStrLit s   -> L.build_global_stringptr s "strlit" builder
      | SCharLit c  -> L.const_int i8_t (Char.code (String.get c 0))
      | SBoolLit b  -> L.const_int i1_t (if b then 1 else 0)
      | SStructLit(sn, ml) -> 
        let (_, struct_t) = StringMap.find sn struct_decls in
        let compare_by (n1, _) (n2, _) = compare n1 n2 in
        let sorted_vals = List.map (fun (n, sexpr) -> expr m builder sexpr) (List.sort compare_by ml) in
        let idxs = List.rev (generate_seq ((List.length sorted_vals) - 1)) in
        let v = List.fold_left2 (fun agg i v -> 
          L.build_insertvalue agg v i "tmp" builder) 
        (L.const_null struct_t) idxs sorted_vals in
        let local = L.build_malloc struct_t sn builder in 
        ignore(L.build_store v local builder); local
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
      | SUnop (op, ((t, _) as e)) ->
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
        let (fdef, local, result) = construct_func_call f args m builder in
        L.build_call fdef [| local |] result builder
      | SMake (t, buf) -> 
        let t_char = typ_to_typ_char t in
        let ll_buf = expr m builder buf in
        L.build_call makechan_func [| t_char ; ll_buf |] "makechan" builder
      | SSend (n, e) -> 
        let chan = expr m builder (Int, SId(n)) in
        let value = expr m builder e in
        L.build_call send_func [| chan ; typ_to_typ_char (fst e) ;
            L.build_intcast value i32_t "tmp" builder |] "" builder
      | SRecv (n, t) ->
        let chan = expr m builder (Int, SId(n)) in
        let recv_by_typ (t : A.typ) = match t with 
            A.Int -> L.build_call recv_int_func [| chan |] ("recv_int_from_" ^ n) builder
          | A.Bool -> L.build_call recv_bool_func [| chan |] ("recv_bool_from_" ^ n) builder
          | A.Char -> L.build_call recv_char_func [| chan |] ("recv_int_from_" ^ n) builder 
          | _ -> raise(Failure("trying to receive non-basic type from channel; should've checked in parser")) in 
        recv_by_typ t
      | SClose (n, t) -> 
        let chan = expr m builder (Int, SId(n)) in
        L.build_call closechan_func [| chan ; typ_to_typ_char t |] "" builder
      | SAccess (sx, sn, mn) -> 
        let struct_val = expr m builder (Struct(sn), sx) in
        let (member_names, struct_t) = StringMap.find sn struct_decls in
        let compare_by (n1, _) (n2, _) = compare n1 n2 in
        let sorted_names = List.map (fun (n, _) -> n) (List.sort compare_by (StringMap.bindings member_names)) in
        let name_idx_pairs = List.mapi (fun i n -> (n, i)) sorted_names in
        let idx = snd (List.hd (List.filter (fun (n, _) -> n = mn) name_idx_pairs)) in
        L.build_extractvalue struct_val idx mn builder
      | SSubscript (n, e) -> 
        let arr = expr m builder (Int, SId(n)) in
        let idx = expr m builder e in 
        L.build_load (L.build_gep arr [| idx |] "" builder) "" builder
      | _ -> raise(Failure("unknown sx: should've checked in semant!"))
    and 
    construct_func_call f args m builder = 
      let (fdef, fdecl) = StringMap.find f function_decls in
      let args_t = StringMap.find f function_arg_structs in
      let llargs = List.rev (List.map (
        fun arg -> match arg with
            (_, SStructLit(s, _)) -> let p = expr m builder arg in 
                                     L.build_load p (s ^ "_lit") builder
          | _ -> expr m builder arg
      ) (List.rev args)) in

      let local = L.build_malloc args_t (f ^ "_args") builder in
      let idxs = List.rev (generate_seq ((List.length fdecl.sformals) - 1)) in
      let args = List.fold_left2 (fun agg i llarg -> 
        (L.build_insertvalue agg llarg i ("arg_" ^ string_of_int i) builder))
      (L.const_null args_t) idxs llargs in
      L.build_store args local builder;
      (* TODO: fix multiple return types later *)
      let result = (match fdecl.stypes with 
                            [] -> ""
                          | _ :: [] -> f ^ "_result"
                          | _ -> raise(Failure("Multiple return types not implemented yet"))) in

      let void_ptr_arg = L.build_bitcast local void_ptr_t (f ^ "_arg_pointer") builder in
      (fdef, void_ptr_arg, result)
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

    let rec stmt m dl builder = function
	      SBlock sl -> 
            let stmt_list builder ml dl sl =
              let helper (bldr, map, dl) = stmt map dl bldr in
              let (b, mm, ndl) = List.fold_left helper (builder, ml, dl) sl 
              in (b, List.tl mm, ndl)
            in
          stmt_list builder (StringMap.empty::m) dl sl 

      | SVdeclStmt vdl -> 
          let declare_var (builder, mm, dl) ((n, t) : (string * svdecl_typ)) =   
            let store_default_val = function
                SInt | SBool | SChar | SChan(_) ->
                  let local = L.build_alloca (ltype_of_typ (svdecl_typ_to_typ t)) n builder in 
                  let default_value = (default_value_of_typ (svdecl_typ_to_typ t) builder) in
                  L.build_store default_value local builder; local
              | SStruct(_) -> 
                  let local = L.build_malloc (ltype_of_typ (svdecl_typ_to_typ t)) n builder in
                  let v = default_value_of_typ (svdecl_typ_to_typ t) builder in
                  L.build_store v local builder; local
              | SArrayInit(t, e) -> 
                  let array_t = ltype_of_typ (svdecl_typ_to_typ t) in
                  let array_len = expr m builder e in
                  let ptr = L.build_array_malloc array_t array_len "" builder in
                  let local = L.build_alloca (L.pointer_type array_t) n builder in
                  L.build_store ptr local builder; local
                  (* let t_size_64 = L.size_of array_t in
                  let t_size = L.build_intcast t_size_64 i32_t "size" builder in
                  let total_size = L.build_mul t_size array_len "tmp" builder in
                  let void_ptr = L.build_pointercast ptr void_ptr_t "void_ptr_tmp" builder in
                  L.build_call memset_func [| void_ptr ; L.const_int i32_t 0 ; total_size |] "" builder; ptr *)
              | _ -> raise(Failure("Not implemented")) in
            let local = store_default_val t in
            (* TODO: add to the symbol table?/manage scope? *)
            let new_m = StringMap.add n local (List.hd mm)
            in (builder, new_m::(List.tl mm), dl) 
          in List.fold_left declare_var (builder, m, dl) vdl
      | SExpr e -> ignore(expr m builder e); (builder, m, dl) 
      | SReturn e -> (* TODO: fix multiple return types later *)
                    List.map (fun (fdef, llargs, result) -> 
                      L.build_call fdef llargs result builder) dl ;
                    ignore(match fdecl.stypes with
                          (* Special "return nothing" instr *)
                          [] -> L.build_ret (L.const_int i32_t 0) builder 
                          (* Build return statement *)
                        | _ :: [] -> L.build_ret (expr m builder (List.hd e)) builder
                        | _ -> raise(Failure("Multiple return types not implemented yet")));
                     (builder, m, [])
      | SYeet(SCall(f, args)) -> 
        let (fdef, local, result) = construct_func_call f args m builder in
        (* let local_void_ptr = L.build_bitcast local void_ptr_t (f ^ "_arg_ptr") builder in *)
        L.build_call yeet_func [| fdef ; local |] "" builder ; (builder, m, dl)
      | SAssignStmt s -> let assign_stmt builder = function
            SAssign sl -> 
              List.map (fun (ee, e) -> 
                (function 
                    (_, SId(s)) -> 
                      let e' = expr m builder e in
                      let handle_assign = function
                          (_, SStructLit(_, _)) -> 
                            let v = L.build_load e' "tmp" builder in
                            ignore(L.build_store v (lookup s m) builder); builder
                        | _ -> ignore(L.build_store e' (lookup s m) builder); builder in
                      handle_assign(e)
                  | (_, SSubscript(an, index)) -> 
                    let value = expr m builder e in
                    let arr = L.build_load (lookup an m) "tmp" builder in
                    let idx = expr m builder index in 
                    let ptr = L.build_gep arr [| idx |] "" builder in 
                    ignore(L.build_store value ptr builder); builder
                  | (_, SAccess(sx, sn, mn)) -> 
                    let struct_val = expr m builder (Struct(sn), sx) in
                    let (member_names, struct_t) = StringMap.find sn struct_decls in
                    let compare_by (n1, _) (n2, _) = compare n1 n2 in
                    let sorted_names = List.map (fun (n, _) -> n) (List.sort compare_by (StringMap.bindings member_names)) in
                    let name_idx_pairs = List.mapi (fun i n -> (n, i)) sorted_names in
                    let idx = snd (List.hd (List.filter (fun (n, _) -> n = mn) name_idx_pairs)) in
                    let e' = expr m builder e in
                    let v = L.build_insertvalue struct_val e' idx mn builder in 

                    let insert_value sx = match sx with 
                        SId(n) -> L.build_store v (lookup n m) builder
                      | SSubscript(an, index) -> 
                        let arr = L.build_load (lookup an m) "tmp" builder in
                        let idx = expr m builder index in
                        L.build_store v (L.build_gep arr [| idx |] "" builder) builder
                      | _ -> raise(Failure("invalid access; should've checked in semant!"))
                    in
                    ignore(insert_value sx); builder 
                ) ee) sl; (builder, m, dl)

          | SDeclAssign (vdl, assl) -> let (_, mm, dl) = stmt m dl builder (SVdeclStmt vdl) in 
                let new_assl = List.map (fun (s, e) -> ((svdecl_typ_to_typ (snd (List.hd vdl)), SId(s)), e)) assl in
                stmt mm dl builder (SAssignStmt(SAssign(new_assl)))
          | SInit dal -> List.fold_left (fun (builder, m, dl) da -> stmt m dl builder(SAssignStmt da)) (builder, m, dl) dal 
          | _         -> raise (Failure "not yet implemented")
        in assign_stmt builder s 

      | SIf (predicate, then_stmt, else_stmt) ->
        let bool_val = expr m builder predicate in
	      let merge_bb = L.append_block context "merge" the_function in
        let build_br_merge = L.build_br merge_bb in (* partial function *)

        let then_bb = L.append_block context "then" the_function in
        let (builder1, _, dl) = stmt m dl (L.builder_at_end context then_bb) then_stmt in
        add_terminal builder1 build_br_merge;

        let else_bb = L.append_block context "else" the_function in
        let (builder2, _, dl) = stmt m dl (L.builder_at_end context else_bb) else_stmt in
        add_terminal builder2 build_br_merge;

        ignore(L.build_cond_br bool_val then_bb else_bb builder);
        ((L.builder_at_end context merge_bb), m, dl)


     | SWhile (predicate, body) ->
        let pred_bb = L.append_block context "while" the_function in
        ignore(L.build_br pred_bb builder);

        let body_bb = L.append_block context "while_body" the_function in
        let (builder, mm, dl) = stmt m dl (L.builder_at_end context body_bb) body in
        add_terminal builder (L.build_br pred_bb);

        let pred_builder = L.builder_at_end context pred_bb in
        let bool_val = expr mm pred_builder predicate in

        let merge_bb = L.append_block context "merge" the_function in
        ignore(L.build_cond_br bool_val body_bb merge_bb pred_builder);
        (L.builder_at_end context merge_bb, mm, dl)

    | SFor (e1, e2, e3, body) -> stmt m dl builder
	    ( SBlock [e1 ; SWhile (e2, SBlock [body ; e3]) ] ) 
    
    | SDefer(_, SCall(f, args)) -> 

      let construct_call = function
          "printb" -> (printbool_func, [| (expr m builder (List.hd args)) |], "printbool")
        | "prints" -> (printf_func, [| str_format_str ; (expr m builder (List.hd args)) |], "printf")
        | "printc" -> (printf_func, [| char_format_str ; (expr m builder (List.hd args)) |], "printf")
        | "printi" -> (printf_func, [| int_format_str ; (expr m builder (List.hd args)) |], "printf")
        | _ -> 
          let (fdef, local, result) = construct_func_call f args m builder in
          (fdef, [| local |], result) in

      (builder, m, (construct_call f)::dl)
    | SSelect cl ->
        (* create array of clause structs *)
        let num_elems = List.length cl in
        let ptr = L.build_array_malloc clause_t
            (L.const_int i32_t num_elems) "" builder 
        in
        let val_ptrs = List.mapi (fun i clause ->
          let op = (function
              (SSend(_), _) -> L.const_int i8_t (Char.code 's')
            | (SRecv(_), _) -> L.const_int i8_t (Char.code 'r')
            | _ -> raise(Failure("invalid select clause in codegen"))
          ) clause in
          let chid = (function
              (SSend(id, _), _) -> id
            | (SRecv(id, _), _) -> id
            | _ -> raise(Failure("invalid select clause in codegen"))
          ) clause in
          let chan = expr m builder (Int, SId(chid)) in
          let chtyp = (function
              (SSend(_, (t, _)), _) -> t
            | (SRecv(_, t), _) -> t
            | _ -> raise(Failure("invalid select clause in codegen"))
          ) clause in
          let local = L.build_alloca (ltype_of_typ chtyp) "" builder in
          let value = (function
              (SSend(id, e), _) -> expr m builder e
            | (SRecv(_), _) -> L.const_int (ltype_of_typ chtyp) 0
            | _ -> raise(Failure("invalid select clause in codegen"))
          ) clause in
          L.build_store value local builder;
          let void_ptr_val = L.build_bitcast local void_ptr_t "" builder in
          let len = L.size_of (ltype_of_typ chtyp) in
          let idxs = List.rev (generate_seq (4 - 1)) in
          let elem = List.fold_left2 (fun agg i v ->
            L.build_insertvalue agg v i "clause" builder)
          (L.const_null clause_t) idxs [op ; chan ; void_ptr_val ; len] in
          let idx = L.const_int i32_t i in
          let eptr = L.build_gep ptr [|idx|] "" builder in
          let cptr = L.build_pointercast eptr 
              (L.pointer_type (L.type_of elem)) "" builder in
          let _ = (L.build_store elem cptr builder) 
          in local) cl
        in
        (* call C function *)
        let selected_clause = L.build_call select_func
          [| ptr ; L.const_int i32_t num_elems |] "select" builder
        in
        (* branch to selected case *)
        let merge_bb = L.append_block context "merge" the_function in
        let build_br_merge = L.build_br merge_bb in
        let (_, case_bbs) = List.fold_left2 (fun (i, cases) clause val_ptr ->
          let case_bb = L.append_block context "case" the_function in
          let case_stmt, mm = (function
              SBlock(SAssignStmt(sass) :: case_block) ->
                (* assign received value *)
                let e' = L.build_load val_ptr "recv_val" builder in 
                let rec handle_case_pred = function
                  SAssign([((_, SId(id)), _)]) ->
                    ignore(L.build_store e' (lookup id m) builder); m
                | SDeclAssign([(id, vdt)], _) ->
                    let (_, mm, dl) = stmt m dl builder (SVdeclStmt [(id, vdt)])
                    in
                    ignore(L.build_store e' (lookup id mm) builder); mm
                | SInit([sa]) -> handle_case_pred(sa)
                | _ -> raise(Failure("invalid case predicate detected in codegen"))
              in
              let mm = handle_case_pred sass in
              SBlock(case_block), mm
            | SBlock(_ :: case_block) -> SBlock(case_block), m
            | _ -> raise(Failure("internal error: invalid case block in codegen"))
          ) (snd clause) in
          let (builder1, _, dl) = stmt mm dl (L.builder_at_end context case_bb)
            case_stmt in
          add_terminal builder1 build_br_merge;
          (i+1, case_bb :: cases))
          (0, []) cl val_ptrs in
        let sw = L.build_switch selected_clause merge_bb num_elems builder in
        List.iteri (fun i case_bb ->
          L.add_case sw (L.const_int i32_t i) case_bb;
        ) (List.rev case_bbs);
        ((L.builder_at_end context merge_bb), m, dl)
    in

    (* Build the code for each statement in the function *)
    
    let (builder, _, ndl) = stmt [local_vars ; global_vars] defered_el builder (SBlock fdecl.sbody) in
    
    match L.block_terminator (L.insertion_block builder) with
	      Some _ -> ()
      | None -> List.map (fun (fdef, llargs, result) -> 
        L.build_call fdef llargs result builder) ndl ;

    (* Add a return if the last block falls off the end *)
    (* TODO: fix later *)
    add_terminal builder (L.build_ret (L.const_int i32_t 0))
      (*(match fdecl.styp with
        A.Void -> L.build_ret_void
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))*)
  in

  List.iter build_function_body functions;
  the_module
