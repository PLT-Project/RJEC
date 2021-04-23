(* Ocamllex scanner for RJEC
 * Initially based on MicroC, with inspiration from Shoo
 * Written largely by Justin Chen in collaboration with Riya, Elaine, Caroline
 *)

{ open Rjecparse }

let digit = ['0' - '9']
let digits = digit+

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LSQUARE }
| ']'      { RSQUARE }
| ':'      { COLON }
| '.'      { DOT }
| ';'      { SEMI }
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '%'      { MOD }
| '='      { ASSIGN }
| ":="     { INIT }
| "=="     { EQ }
| '<'      { LT }
| "<="     { LEQ }
| "&&"     { AND }
| "||"     { OR }
| "!"      { NOT }
| "<-"     { ARROW }
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "defer"  { DEFER }
| "select" { SELECT }
| "case"   { CASE }
| "return" { RETURN }
| "int"    { INT }
| "bool"   { BOOL }
| "char"   { CHAR }
| "chan"   { CHAN }
| "struct" { STRUCT }
| "var"    { VAR }
| "func"   { FUNC }
| "yeet"   { YEET }
| "make"   { MAKE }
| "close"  { CLOSE }
| "true"   { BLIT(true)  }
| "false"  { BLIT(false) }
| digits as lxm { ILIT(int_of_string lxm) }
| '\"'     { str (Buffer.create 16) lexbuf }
| "\'\\0\'"    { CLIT(0) }
| '\'' _ '\'' as lxm { CLIT(Char.code (String.get lxm 1)) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*     as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }

and str buf = parse
  '\"' { SLIT(Buffer.contents buf) }
| [^ '\"'] { Buffer.add_string buf (Lexing.lexeme lexbuf); str buf lexbuf }
