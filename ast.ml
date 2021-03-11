(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Mod | Equal | Less | Leq | And | Or

type uop = Neg | Not

type typ = Int | Bool | Char
  | Chan of typ
  | Array of typ
  | Struct of string
  | Func of typ list * typ list

type bind = typ * string

type expr =
    IntLit of int
  | StrLit of string
  | CharLit of string
  | BoolLit of bool
  | ArrLit of expr * typ * expr list
  | StructLit of string * (string * expr) list
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Call of string * expr list
  | Access of string * string
  | Subscript of string * expr
  | Send of string * expr
  | Recv of string
  | Make of typ * (expr option)
  | Close of string
  | Noexpr

type vdecl_typ = Int | Bool | Char
  | Chan of typ
  | ArrayInit of expr * typ
  | Struct of string

type vdecl = Vdecl of vdecl_typ * string list

type assign_stmt =
    DeclAssign of vdecl * expr list
  | Assign of string list * expr list
  | Init of string list * expr list

type stmt =
    Block of stmt list
  | Expr of expr
  | VdeclStmt of vdecl
  | AssignStmt of assign_stmt
  | Return of expr list
  | If of expr * stmt * stmt
  | For of (assign_stmt option) * expr * (assign_stmt option) * stmt
  | Select of (stmt * stmt list) list
  | Defer of expr
  | Yeet of expr
  | Break
  | Continue

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    body : stmt list;
  }

type sdecl = Sdecl of string * (typ * string) list

type program = vdecl list * func_decl list * sdecl list

(* Pretty-printing functions *)

(*let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | Fliteral(l) -> l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | Void -> "void"

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)*)
