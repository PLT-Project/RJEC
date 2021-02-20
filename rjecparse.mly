/* Ocamlyacc parser for RJEC */

%{
open Ast
let trd (_, _, t) = t 
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE LSQUARE RSQUARE DOT COMMA COLON
%token PLUS MINUS TIMES DIVIDE MOD ASSIGN INIT
%token NOT EQ LT LEQ AND OR
%token RETURN IF ELSE FOR WHILE INT BOOL CHAR CHAN STRUCT
%token VAR TYPE FUNC YEET MAKE CLOSE
%token ARROW BREAK CONTINUE DEFER SELECT CASE
%token <int> ILIT
%token <bool> BLIT
%token <string> SLIT CLIT ID
%token EOF

%start program
%type <Ast.program> program

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN INIT
%left OR
%left AND
%left EQ
%left LT LEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD
%right ARROW
%right NOT
%left DOT

%%

program:
  decls EOF { $1 }

decls:
   /* nothing */ { ([], [], [])               }
 | decls vdecl SEMI { (($2 :: fst $1), snd $1, trd $1) }
 | decls fdecl { (fst $1, ($2 :: snd $1), trd $1) }
 | decls sdecl { (fst $1, snd $1, ($2 :: trd $1)) }

fdecl:
   FUNC ID LPAREN formals_opt RPAREN typ_opt LBRACE stmt_list RBRACE
     { { fname = $2;
	 formals = List.rev $4;
   types = List.rev $6;
	 body = List.rev $8 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { $1 }

formal_list:
    ID typ                   { [($2,$1)]     }
  | formal_list COMMA ID typ { ($4,$3) :: $1 }

typ_opt:
  /* nothing */ { [] }
| typ_list   { $1 }

typ_list:
    typ                   { [$1]     }
  | typ_list COMMA typ { $3 :: $1 }

typ:
    INT   { Int   }
  | BOOL  { Bool  }
  | CHAR  { Char }
  | CHAN  { Chan  }
  | LSQUARE RSQUARE typ { Array($3) }
  | ID    { Struct($1) }

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl_typ:
    INT   { Int   }
  | BOOL  { Bool  }
  | CHAR  { Char }
  | CHAN  { Chan  }
  | LSQUARE ILIT RSQUARE typ { Array($4) }
  | ID    { Struct($1) }

vdecl:
    VAR ID vdecl_typ { Vdecl($3, $2) }

sdecl:
    TYPE ID STRUCT LBRACE member_list RBRACE { Sdecl($2, $5) }

struct_typ:
    INT   { Int   }
  | BOOL  { Bool  }
  | CHAR  { Char }
  | CHAN  { Chan  }

member_list:
    ID struct_typ SEMI             { [($2,$1)]     }
  | member_list ID struct_typ SEMI { ($3,$2) :: $1 }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI                               { Expr $1               }
  | RETURN expr_opt SEMI                    { Return $2             }
  | LBRACE stmt_list RBRACE                 { Block(List.rev $2)    }
  | IF expr LBRACE stmt_list RBRACE %prec NOELSE
                                      { If($2, Block(List.rev $4), Block([])) }
  | IF expr LBRACE stmt_list RBRACE ELSE LBRACE stmt_list RBRACE
                                      { If($2, Block(List.rev $4), $8)        }
  | FOR expr_opt SEMI expr SEMI expr_opt LBRACE stmt_list RBRACE
                                      { For($2, Block(List.rev $4), $6, $8)   }
  | FOR expr LBRACE stmt_list RBRACE  { While($2, Block(List.rev $4))         }
  | FOR LBRACE stmt_list RBRACE    { While(BoolLit(true), Block(List.rev $3)) }
  | SELECT LBRACE case_list RBRACE          { Select(List.rev $3)   }
  | DEFER expr SEMI                         { Defer($2)             }
  | YEET expr SEMI                          { Yeet($2)              }
  | BREAK SEMI                              { Break                 }
  | CONTINUE SEMI                           { Continue              }

case_list:
    CASE expr COLON stmt_list           { [($2, $4)] }
  | case_list CASE expr COLON stmt_list { ($3, $5) :: $1 }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    ILIT             { IntLit($1)             }
  | SLIT	           { StrLit($1)             }
  | CLIT	           { CharLit($1)            }
  | BLIT             { BoolLit($1)            }
  | ID               { Id($1)                 }
  | expr PLUS   expr { Binop($1, Add,   $3)   }
  | expr MINUS  expr { Binop($1, Sub,   $3)   }
  | expr TIMES  expr { Binop($1, Mult,  $3)   }
  | expr DIVIDE expr { Binop($1, Div,   $3)   }
  | expr MOD    expr { Binop($1, Mod,   $3)   }
  | expr EQ     expr { Binop($1, Equal, $3)   }
  | expr LT     expr { Binop($1, Less,  $3)   }
  | expr LEQ    expr { Binop($1, Leq,   $3)   }
  | expr AND    expr { Binop($1, And,   $3)   }
  | expr OR     expr { Binop($1, Or,    $3)   }
  | NOT expr         { Unop(Not, $2)          }
  | vdecl ASSIGN expr { DeclAssign($1, $3)    }
  | ID ASSIGN expr   { Assign($1, $3)         }
  | ID INIT    expr  { Init($1, $3)           }
  | ID LPAREN args_opt RPAREN { Call($1, $3)  }
  | LPAREN expr RPAREN { $2                   }
  | ID DOT ID        { Access($1, $3)         }
  | ID LSQUARE expr RSQUARE   { Subscript($1, $3) }
  | ID ARROW expr    { Send($1, $3)           }
  | ARROW ID         { Recv($2)           }
  | MAKE LPAREN CHAN chan_typ RPAREN  { Make($4)   }
  | MAKE LPAREN CHAN chan_typ COMMA expr RPAREN  { MakeBuffer($4, $6)   }
  | CLOSE LPAREN ID RPAREN { Close($3)  }

args_opt:
    /* nothing */ { [] }
  | args_list  { List.rev $1 }

args_list:
    expr                    { [$1] }
  | args_list COMMA expr { $3 :: $1 }

chan_typ:
    INT   { Int   }
  | BOOL  { Bool  }
  | CHAR  { Char }
