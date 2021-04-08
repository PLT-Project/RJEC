(* Semantic checking for the RJEC compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check (globals, (functions, structs)) =

  let vdecl_typ_to_typ : vdecl_typ -> typ = function
      Int -> Int
    | Bool -> Bool
    | Char -> Char
    | Chan(t) -> Chan(t)
    | ArrayInit(e, t) -> Array(t)
    | Struct(s) -> Struct(s)
  in

  let default_vals_in_sexpr : typ -> typ * sx = function
      Int -> (Int, SIntLit 0)
    | Bool -> (Bool, SBoolLit false)
    | Char -> (Char, SCharLit "\x00")
  in
  let flatten_global global = List.map
    (fun name -> (vdecl_typ_to_typ (fst global), name)) (snd global)
  in

  (* Add global names to symbol table *)
  let add_global map vd = 
    let dup_err = "duplicate global " ^ snd vd
    and make_err er = raise (Failure er)
    and n = snd vd
    and typ = fst vd
    in match n with (* No duplicate globals *)
       | _ when StringMap.mem n map -> make_err dup_err  
       | _ ->  StringMap.add n typ map 
  in
  let check_global map global =
    List.fold_left add_global map (flatten_global global)
  in
  let global_decls = List.fold_left check_global StringMap.empty globals
  in

  (* Verify a list of bindings has no duplicate names *)
  let check_binds (kind : string) (binds : bind list) =
    let rec dups = function
        [] -> ()
      |	((_,n1) :: (_,n2) :: _) when n1 = n2 ->
    raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in dups (List.sort (fun (_,a) (_,b) -> compare a b) binds)
  in

  (* Add structs to symbol table *)
  let add_struct smap sd = 
    let dup_err = "duplicate struct " ^ (fst sd)
    and make_err er = raise (Failure er)
    and n = fst sd
    in match n with (* No duplicate globals *)
      | _ when StringMap.mem n global_decls -> make_err ("global variable with same name already declared: " ^ (fst sd))
      | _ when StringMap.mem n smap -> make_err dup_err 
      | _ -> check_binds "struct member in definition" (snd sd);
          let members = List.fold_left (fun m (t, n) -> StringMap.add n t m) StringMap.empty (snd sd) in
          StringMap.add n members smap
    in
  let struct_decls = List.fold_left add_struct StringMap.empty structs
  in

  (**** Check functions ****)

  (* Collect function declarations for built-in functions: no bodies *)
  let built_in_decls = 
    let add_bind map (name, ty) = StringMap.add name {
      types = [];
      fname = name; 
      formals = [(ty, "x")];
      body = [] } map
    in List.fold_left add_bind StringMap.empty [ ("printi", Int);
			                         ("printb", Bool);
			                         ("printc", Char);
			                         ("prints", Array(Char)) ]
  in

  (* Add function name to symbol table *)
  let add_func map fd = 
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *)
    in match fd with (* No duplicate functions or redefinitions of built-ins *)
         _ when StringMap.mem n built_in_decls -> make_err built_in_err
       | _ when StringMap.mem n map -> make_err dup_err  
       | _ ->  StringMap.add n fd map 
  in

  (* Collect all function names into one symbol table *)
  let function_decls = List.fold_left add_func built_in_decls functions
  in
  
  (* Return a function from our symbol table *)
  let find_func s = 
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let find_struct s = 
    try StringMap.find s struct_decls
    with Not_found -> raise (Failure ("unrecognized struct " ^ s))
  in

  let _ = find_func "main" in (* Ensure "main" is defined *)

  let check_function func =
    (* Make sure no formals or locals are duplicates *)
    check_binds "formal" func.formals;

    (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
    let check_assign lvaluet rvaluet err =
       if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in   
      
    let add_to_scope (v_type : typ) (v_name : string) 
        (scope : typ StringMap.t list) =
      let map = List.hd scope in
      try
        match (StringMap.find v_name map) with
          _ -> raise (Failure (v_name ^ " already declared"))
      with Not_found ->
        let newMap = StringMap.add v_name v_type map in
        newMap::List.tl scope
    in 

    let create_scope list =
      let rec helper m = function
          [] -> m
        | (t, n)::tl -> 
          let new_m = StringMap.add n t m in 
          helper new_m tl
      in helper StringMap.empty list
    in 

    let scope = [ create_scope func.formals; global_decls ] 
    in 

    (* Return a variable from our local symbol table *)
    let rec type_of_identifier (v_name : string) (scope : typ StringMap.t list) =
      try
        StringMap.find v_name (List.hd scope)
      with Not_found -> match List.tl scope with
          [] -> raise (Failure("undeclared reference " ^ v_name))
        | tail -> type_of_identifier v_name tail
    in 

    (* Return a semantically-checked expression, i.e., with a type *)
    let rec expr (scope : typ StringMap.t list) (e : expr) : sexpr = match e with
        IntLit  l -> (Int, SIntLit l)
      | StrLit l  -> (Array(Char), SStrLit l)
      | CharLit l  -> (Char, SCharLit l)
      | BoolLit l  -> (Bool, SBoolLit l)
      | StructLit(sn, ml) -> 
        let smembers = find_struct sn in
        let check_member sname members vm (n, e) = 
          let (t, e') = expr scope e in
          match n with 
            _ when ((StringMap.mem n members) && (t = StringMap.find n members)) -> StringMap.add n (t, e') vm
          | _ -> raise(Failure("unknown member " ^ n ^ " of type " ^ (string_of_typ t)
                               ^ " in definition of struct " ^ sname)) in
        let member_vals = List.fold_left (check_member sn smembers) StringMap.empty ml in
        let full_member_vals = List.fold_left (fun m (n, t) -> match n with
            _ when StringMap.mem n m -> m
          | _ -> StringMap.add n (default_vals_in_sexpr t) m
        ) member_vals (StringMap.bindings smembers) in
        (Struct(sn), SStructLit(sn, StringMap.bindings full_member_vals))
      (* TODO: composite literals *)
      (*| Noexpr     -> (Void, SNoexpr)*)
      | Id s       -> (type_of_identifier s scope, SId s)
      | Unop(op, e) as ex -> 
          let (t, e') = expr scope e in
          let ty = match op with
            Neg when t = Int -> t
          | Not when t = Bool -> Bool
          | _ -> raise (Failure ("illegal unary operator " ^ 
                                 string_of_uop op ^ string_of_typ t ^
                                 " in " ^ string_of_expr ex))
          in (ty, SUnop(op, (t, e')))
      | Binop(e1, op, e2) as e -> 
          let (t1, e1') = expr scope e1 
          and (t2, e2') = expr scope e2 in
          (* All binary operators require operands of the same type *)
          let same = t1 = t2 in
          (* Determine expression type based on operator and operand types *)
          let ty : typ = match op with
            Add | Sub | Mult | Div | Mod when same && t1 = Int   -> Int
          | Equal            when same               -> Bool
          | Less | Leq
                     when same && (t1 = Int || t1 = Char) -> Bool
          | And | Or when same && t1 = Bool -> Bool
          | _ -> raise (
	      Failure ("illegal binary operator " ^
                       string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                       string_of_typ t2 ^ " in " ^ string_of_expr e))
          in (ty, SBinop((t1, e1'), op, (t2, e2')))
      | Access(n, mn) -> 
        let check_struct (t : typ) = match t with
            Struct(n) -> n
          | _ -> raise(Failure("Invalid syntax: access member field of non-struct object\n")) in
        let sn = check_struct (type_of_identifier n scope) in
        let smembers = find_struct sn in
        let t = match sn with 
          _ when StringMap.mem mn smembers -> StringMap.find mn smembers
        | _ -> raise(Failure("unknown field " ^ mn ^ " in struct " ^ sn)) in
        (t, SAccess(n, sn, mn))
      | Call(fname, args) as call -> 
          let fd = find_func fname in
          let param_length = List.length fd.formals in
          if List.length args != param_length then
            raise (Failure ("expecting " ^ string_of_int param_length ^ 
                            " arguments in " ^ string_of_expr call))
          else let check_call (ft, _) e = 
            let (et, e') = expr scope e in 
            let err = "illegal argument found " ^ string_of_typ et ^
              " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
            in (check_assign ft et err, e')
          in 
          let args' = List.map2 check_call fd.formals args
          in (Func(List.map (fun (t, s) -> t) fd.formals, fd.types),
                SCall(fname, args'))
        | _ -> raise (Failure ("not yet implemented"))
    in
    
    let check_assign_var scope var e =
      let lt = type_of_identifier var scope and (rt, e') = expr scope e in
       let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^ 
      string_of_typ rt (* ^ " in " ^ string_of_stmt (AssignStmt(Assign(var, e))) *)
      in check_assign lt rt err ; (var, (rt, e'))
    in 

    let check_bool_expr scope e = 
      let (t', e') = expr scope e
      and err = "expected Boolean expression in " ^ string_of_expr e
      in if t' != Bool then raise (Failure err) else (t', e') 
    in

    (* Return a semantically-checked statement i.e. containing sexprs *)
    let rec check_stmt scope = function
        Expr e -> (SExpr(expr scope e), scope)
      | If(p, b1, b2) -> 
        let (sstmt1, _) = check_stmt scope b1 
        and (sstmt2, _) = check_stmt scope b2 in
        (SIf(check_bool_expr scope p, sstmt1, sstmt2), scope)
      (*| For(e1, e2, e3, st) ->
	  SFor(expr scope e1, check_bool_expr scope e2, expr scope e3, check_stmt st)*)
      | Return el -> 
        let check_return_typ e rt = 
          let (t, e') = expr scope e in
          if t = rt then (t, e')
          else raise (Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                      string_of_typ rt ^ " in " ^ string_of_expr e))
        in
        (SReturn(List.map2 check_return_typ el func.types), scope)
	    (* A block is correct if each statement is correct and nothing
	       follows any Return statement.  Nested blocks are flattened. *)
      | AssignStmt s -> 
          let check_assign_stmt = function
              Assign(vl, el)  -> SAssign (List.map2 (check_assign_var scope) vl el)
            | _               -> raise (Failure ("not yet implemented"))
          in (SAssignStmt(check_assign_stmt s), scope)
      | VdeclStmt s -> 
          let (t, nl) = s in
          let (nscope, vdecls) = List.fold_left (fun (scope, vdecls) n -> 
            (add_to_scope (vdecl_typ_to_typ t) n scope, (n, t)::vdecls)
          ) (scope, []) nl
          in (SVdeclStmt(vdecls), nscope)
      | Block sl -> 
          let bscope = (create_scope []) :: scope in
          let rec check_stmt_list scope = function
              [Return _ as s] -> 
                let (ret, _) = check_stmt scope s in ([ret], scope)
            | Return _ :: _   -> raise (Failure "nothing may follow a return")
            | s :: ss         -> 
              let (ret, nscope) = check_stmt scope s in
              let (ret2, nscope2) = check_stmt_list nscope ss in
              (ret :: ret2, nscope2)
            | []              -> ([], scope)
          in let (bret, _) = check_stmt_list bscope sl
          in (SBlock(bret), scope)
      | _ -> raise (Failure ("not yet implemented"))

    in (* body of check_function *)
    { stypes = func.types;
      sfname = func.fname;
      sformals = func.formals;
      sbody = match check_stmt scope (Block func.body) with
	(SBlock(sl), _) -> sl
      | _ -> raise (Failure ("internal error: block didn't become a block?"))
    }
  in (List.flatten (List.map flatten_global globals),
        List.map check_function functions, structs)
