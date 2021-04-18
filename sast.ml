(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

type sexpr = typ * sx
and sx =
    SIntLit of int
  | SStrLit of string
  | SCharLit of string
  | SBoolLit of bool
  | SStructLit of string * (string * sexpr) list
  | SId of string
  | SBinop of sexpr * op * sexpr
  | SUnop of uop * sexpr
  | SCall of string * sexpr list
  | SAccess of string * string * string 
  | SNoexpr

type svdecl_typ = SInt | SBool | SChar
  | SArrayInit of svdecl_typ * sexpr
  | SChan of svdecl_typ
  | SStruct of string

type sassign_stmt = 
  | SAssign of (sexpr * sexpr) list 
  | SDeclAssign of ((string * vdecl_typ) list) * ((string * sexpr) list)
  | SInit of sassign_stmt list

type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SAssignStmt of sassign_stmt 
  | SReturn of sexpr list
  | SIf of sexpr * sstmt * sstmt
  | SFor of sstmt * sexpr * sstmt * sstmt
(*For of (sassign_stmt option) * sexpr * (sassign_stmt option) * sstmt*)
  | SWhile of sexpr * sstmt
  | SVdeclStmt of (string * vdecl_typ) list
  | SDefer of sexpr
  | SBreak
  | SContinue


type sfunc_decl = {
  stypes : typ list;
  sfname : string;
  sformals : bind list;
  sbody : sstmt list;
}

type sprogram = bind list * sfunc_decl list

(* Pretty-printing functions *)

let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : " ^ (match e with
    SIntLit(l) -> string_of_int l
  | SStrLit(l) -> l
  | SCharLit(l) -> l
  | SBoolLit(true) -> "true"
  | SBoolLit(false) -> "false"
  | SId(s) -> s
  | SBinop(e1, o, e2) ->
      string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
  | SUnop(o, e) -> string_of_uop o ^ string_of_sexpr e
  (*| SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e*)
  | SCall(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
  | SNoexpr -> ""
				  ) ^ ")"				     

let rec string_of_sstmt = function
    SBlock(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n";
  | SReturn(exprs) -> "return " ^ (String.concat ", " (List.map string_of_sexpr exprs)) ^ ";\n";
  | SIf(e, s, SBlock([])) ->
      "if (" ^ string_of_sexpr e ^ ")\n" ^ string_of_sstmt s
  | SIf(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^
      string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SFor(e1, e2, e3, s) ->
      "for (" ^ string_of_sstmt e1  ^ " ; " ^ string_of_sexpr e2 ^ " ; " ^
      string_of_sstmt e3  ^ ") " ^ string_of_sstmt s
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s

let string_of_sfdecl fdecl =
  fdecl.sfname ^ "(" ^ String.concat ", " (List.map snd fdecl.sformals) ^
  ") (" ^ String.concat ", " (List.map string_of_typ fdecl.stypes) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_sstmt fdecl.sbody) ^
  "}\n"

let string_of_globals (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_sprogram (vars, funcs, structs) =
  String.concat "" (List.map string_of_globals vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_sfdecl funcs)
