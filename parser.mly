%{
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
        | or_exp                       {$1}
        ;
or_exp:
        | and_exp OR or_exp             {BinOp (Or,$1,$3)}
        | and_exp                       {$1}
        ;
and_exp:
        | bin_exp AND and_exp           {BinOp (And,$1,$3)}
        | bin_exp                       {$1}
        ;
bin_exp:
        | un_exp binop bin_exp         {BinOp ($2,$1,$3)} 
        | un_exp                        {$1}
        ;
un_exp:
        | unop bin_exp                  {UnOp ($1,$2)}
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
