/* Ocamlyacc parser for RJEC */

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE LSQUARE RSQUARE DOT COMMA COLON
%token PLUS MINUS TIMES DIVIDE MOD ASSIGN INIT
%token NOT EQ LT LEQ AND OR
%token RETURN IF ELSE FOR WHILE INT BOOL CHAR CHAN STRUCT
%token VAR FUNC YEET MAKE CLOSE
%token ARROW BREAK CONTINUE DEFER SELECT CASE
%token <int> ILIT CLIT
%token <bool> BLIT
%token <string> SLIT ID
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
   /* nothing */ { ([], ([], []))               }
 | decls vdecl SEMI { (($2 :: fst $1), (fst (snd $1), snd (snd $1))) }
 | decls fdecl { (fst $1, (($2 :: fst (snd $1)), snd (snd $1))) }
 | decls sdecl { (fst $1, (fst (snd $1), ($2 :: snd (snd $1)))) }

fdecl:
   FUNC ID LPAREN formals_opt RPAREN return_types LBRACE stmt_list RBRACE
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

return_types:
    /* nothing */          {   [] }
  | typ                    { [$1] }
/*  | LPAREN typ_list RPAREN {   $2 } */

// typ_list:
//     typ                   { [$1]     }
//   | typ_list COMMA typ { $3 :: $1 }

typ:
    INT   { Int   }
  | BOOL  { Bool  }
  | CHAR  { Char }
  | CHAN basic_typ           { Chan($2)  }
  | LSQUARE RSQUARE non_arr_typ { Array($3) }
  | STRUCT ID    { Struct($2) }
  
basic_typ:
    INT   { Int   }
  | BOOL  { Bool  }
  | CHAR  { Char }

non_arr_typ:
    INT   { Int   }
  | BOOL  { Bool  }
  | CHAR  { Char }
  | CHAN basic_typ           { Chan($2)  }
  | STRUCT ID    { Struct($2) }

// typ_opt:
//     /* nothing */ { [] }
//   | typ_list      { $1 }

vdecl_typ:
    INT   { Int   }
  | BOOL  { Bool  }
  | CHAR  { Char }
  | CHAN basic_typ                       { Chan($2)  }
  | LSQUARE expr RSQUARE non_arr_typ { ArrayInit($2, $4) }
  | STRUCT ID    { Struct($2) }

vdecl:
    VAR id_list vdecl_typ { ($3, List.rev $2) }

id_list:
    ID { [$1] }
  | id_list COMMA ID { $3 :: $1 }

sdecl:
    STRUCT ID LBRACE member_list RBRACE { ($2, $4) }

/*struct_typ:
    INT            { Int   }
  | BOOL           { Bool  }
  | CHAR           { Char  }
  | CHAN basic_typ { Chan  }*/

member_list:
    ID basic_typ SEMI             { [($2,$1)]     }
  | member_list ID basic_typ SEMI { ($3,$2) :: $1 }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI                               { Expr $1               }
  | vdecl SEMI                              { VdeclStmt $1          }
  | assign_stmt SEMI                        { AssignStmt $1         }
  | RETURN args_opt SEMI                    { Return $2             }
  | LBRACE stmt_list RBRACE                 { Block(List.rev $2)    }
  | IF expr LBRACE stmt_list RBRACE else_opt
                                      { If($2, Block(List.rev $4), $6) }
  | FOR assign_stmt_opt SEMI expr SEMI assign_stmt_opt LBRACE stmt_list RBRACE
                                      { For($2, $4, $6, Block(List.rev $8))   }
  | FOR expr LBRACE stmt_list RBRACE  
                                    { While($2, Block(List.rev $4)) }
  | FOR LBRACE stmt_list RBRACE    
                         { While(BoolLit(true), Block(List.rev $3)) }
  | SELECT LBRACE case_list RBRACE          { Select(List.rev $3)   }
  | DEFER expr SEMI                         { Defer($2)             }
  | YEET expr SEMI                          { Yeet($2)              }
  | BREAK SEMI                              { Break                 }
  | CONTINUE SEMI                           { Continue              }

else_opt:
    %prec NOELSE                                        { Block([]) }
  | ELSE IF expr LBRACE stmt_list RBRACE else_opt
                                   { If($3, Block(List.rev $5), $7) }
  | ELSE LBRACE stmt_list RBRACE               { Block(List.rev $3) }

case_list:
    CASE case_stmt COLON stmt_list           { [($2, List.rev $4)]     }
  | case_list CASE case_stmt COLON stmt_list { ($3, List.rev $5) :: $1 }

case_stmt:
    id_subscript ARROW expr    { Expr (Send ($1, $3))     }
  | ARROW id_subscript         { Expr (Recv $2)           }
  | assign_stmt                { AssignStmt $1            }

assign_stmt:
  | vdecl ASSIGN args_list            { DeclAssign($1, List.rev $3)      }
  | args_list ASSIGN args_list        { Assign(List.rev $1, List.rev $3) }
  | args_list INIT   args_list        { Init(List.rev $1, List.rev $3)   }


assign_stmt_opt:
     /* nothing */ { None }
  |  assign_stmt   { Some($1) }

expr:
    ILIT             { IntLit($1)             }
  | SLIT	           { StrLit($1)             }
  | CLIT	           { CharLit($1)            }
  | BLIT             { BoolLit($1)            }
  | LSQUARE RSQUARE typ LBRACE args_list RBRACE
                     { ArrLit($3, List.rev $5)     }
  | STRUCT ID LBRACE element_list_opt RBRACE
                     { StructLit($2, $4)      }
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
  | MINUS expr %prec NOT { Unop(Neg, $2)      }
  | NOT expr         { Unop(Not, $2)          }
  | ID LPAREN args_opt RPAREN { Call($1, $3)  }
  | LPAREN expr RPAREN { $2                   }
  | expr DOT ID      { Access($1, $3)         }
  | ID LSQUARE expr RSQUARE   { Subscript($1, $3) }
  | id_subscript ARROW expr   { Send($1, $3)      }
  | ARROW id_subscript   { Recv($2)           }
  | MAKE LPAREN CHAN basic_typ RPAREN            { Make($4, None)       }
  | MAKE LPAREN CHAN basic_typ COMMA expr RPAREN { Make($4, Some($6))   }
  | CLOSE LPAREN expr RPAREN { Close($3)        }

id_subscript:
    ID { ID($1) }
  | ID LSQUARE expr RSQUARE   { Subscript($1, $3) }

args_opt:
    /* nothing */ { [] }
  | args_list  { List.rev $1 }

args_list:
    expr                    { [$1] }
  | args_list COMMA expr { $3 :: $1 }

element_list_opt:
    /* nothing */ { [] }
  | element_list  { List.rev $1 }

element_list:
    ID COLON expr                      { [($1,$3)]     }
  | element_list COMMA ID COLON expr   { ($3,$5) :: $1 }
