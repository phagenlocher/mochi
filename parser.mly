%{
  open Util
%}

%token TRUE FALSE
%token NOT
%token IMP BIIMP XOR
%token AND OR
%token FINALLY GLOBALLY NEXT
%token UNTIL WUNTIL RELEASE SRELEASE
%token LPAREN RPAREN
%token <string> VARIABLE
%token EOF

%start main
%type <Util.ltl> main

%%

main:
        | expression EOF                {$1}
        ;
expression:
        | bin_exp                       {$1}
        | un_exp                        {$1}
        ;
bin_exp:
        | expression binop expression   {BinOp ($2,$1,$3)} 
        ;
un_exp:
        | unop expression               {UnOp ($1,$2)}
        | atom                          {$1}
        ;
atom:
        | bool                          {Bool ($1)}
        | VARIABLE                      {Var ($1)}
        | LPAREN expression RPAREN      {$2}
        ;
unop:
        | NOT                           {Not}
        | FINALLY                       {Finally} 
        | GLOBALLY                      {Globally} 
        | NEXT                          {Next}
        ;
binop:
        | AND                           {And}
        | OR                            {Or}
        | BIIMP                         {Biimp}
        | IMP                           {Imp}
        | XOR                           {Xor}
        | UNTIL                         {Until}
        | WUNTIL                        {Wuntil}
        | RELEASE                       {Release}
        | SRELEASE                      {Srelease}
        ;
bool:
        | TRUE                          {true}
        | FALSE                         {false}
        ;
