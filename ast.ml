(* Abstract Syntax Tree and functions for printing it 
 * Initially based on MicroC, with inspiration from Shoo
 * Written by Justin Chen, Elaine Wang, Riya Chakraborty, and Caroline Hoang
 *)

type op = Add | Sub | Mult | Div | Mod | Equal | Less | Leq | And | Or

type uop = Neg | Not

type typ = Int | Bool | Char
  | Chan of typ
  | Array of typ
  | Struct of string

type bind = typ * string

type expr =
    IntLit of int
  | StrLit of string
  | CharLit of int
  | BoolLit of bool
  | ArrLit of typ * expr list
  | StructLit of string * (string * expr) list
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Call of string * expr list
  | Access of expr * string
  | Subscript of string * expr
  | Send of expr * expr
  | Recv of expr
  | Make of typ * (expr option)
  | Close of expr
  | Noexpr

type vdecl_typ = Int | Bool | Char
  | Chan of typ
  | ArrayInit of expr * typ
  | Struct of string

type vdecl = vdecl_typ * string list

type assign_stmt =
    DeclAssign of vdecl * expr list
  | Assign of expr list * expr list
  | Init of expr list * expr list

type stmt =
    Block of stmt list
  | Expr of expr
  | VdeclStmt of vdecl
  | AssignStmt of assign_stmt
  | Return of expr list
  | If of expr * stmt * stmt
  | For of (assign_stmt option) * expr * (assign_stmt option) * stmt
  | While of expr * stmt 
  | Select of (stmt * stmt list) list
  | Defer of expr
  | Yeet of expr

type func_decl = {
    types   : typ list;
    fname   : string;
    formals : bind list;
    body    : stmt list;
  }

type sdecl = string * bind list

type program = vdecl list * (func_decl list * sdecl list)

(* Pretty-printing functions *)

let string_of_op = function
    Add   -> "+"
  | Sub   -> "-"
  | Mult  -> "*"
  | Div   -> "/"
  | Mod   -> "%"
  | Equal -> "=="
  | Less  -> "<"
  | Leq   -> "<="
  | And   -> "&&"
  | Or    -> "||"

let string_of_uop = function
    Neg   -> "-"
  | Not   -> "!"

let rec string_of_typ = function
    Array(t)  -> "[]" ^ string_of_typ t
  | Int       -> "int"
  | Bool      -> "bool"
  | Char      -> "char"
  | Struct(t) -> t
  | Chan(t)   -> "chan " ^ string_of_typ t 

let rec string_of_expr = function
    IntLit(l)         -> string_of_int l
  | StrLit(l)         -> l
  | CharLit(l)        -> String.make 1 (Char.chr l)
  | BoolLit(true)     -> "true"
  | BoolLit(false)    -> "false"
  | ArrLit(t, el)     -> "[]" ^ string_of_typ t
      ^ "{" ^ String.concat ", " (List.map string_of_expr el) ^ "}"
  | StructLit(n, m) -> "struct " ^ n ^ "{" ^ String.concat ", "
      (List.map (fun (n, v) -> (n ^ ": " ^ string_of_expr (v))) m) ^ "}"
  | Id(s)             -> s
  | Binop(e1, o, e2)  ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Call(f, el)       ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Access(s, m)      -> (string_of_expr s) ^ "." ^ m
  | Subscript(a, i)   -> a ^ "[" ^ string_of_expr i ^ "]"
  | Send(c, e)        -> string_of_expr c ^ "<-" ^ string_of_expr e
  | Recv(c)           -> "<-" ^ string_of_expr c
  | Make(t, e)        -> "make" ^ "(" ^ "chan " ^ string_of_typ t ^
      (match e with None -> "" | Some(ex) -> "," ^ string_of_expr ex) ^ ")"
  | Close(c)          -> "close" ^ "(" ^ string_of_expr c ^ ")"
  | Noexpr            -> ""

let rec string_of_stmt = function
    Block(stmts)      ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr)        -> string_of_expr expr ^ ";\n";
  | AssignStmt(s)     -> let string_of_assign = function  
      Assign(v, e)    -> (String.concat ", " (List.map string_of_expr v)) 
          ^ " = " ^ (String.concat ", " (List.map string_of_expr e)) ^ ";\n"
    | _               ->  "??????\n"
    in string_of_assign s
  | Return(expr)      -> "return " ^
      String.concat ", " (List.map string_of_expr expr) ^ ";\n"
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | _ -> "??????\n" (* unimplemented pretty-print *)

let string_of_vdecl_typ = function
    ArrayInit(i, t) -> "[" ^ string_of_expr i ^ "]" ^ string_of_typ t
  | Int             -> "int"
  | Bool            -> "bool"
  | Char            -> "char"
  | Struct(t)       -> t
  | Chan(t)         -> "chan " ^ string_of_typ t 

let string_of_vdecl (vd : vdecl) : string = "var " ^ String.concat ", " (snd vd)
    ^ string_of_vdecl_typ (fst vd) ^ " " ^ ";\n"

let string_of_fdecl fdecl =
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ") (" ^ String.concat ", " (List.map string_of_typ fdecl.types) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, (funcs, _)) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
