%{
  let debug x = Util.Debug.print ("WL-Parser: "^x) 
  let counter = ref 0
  let new_id () = incr counter; !counter
%}

%token EQ LEQ
%token NOT TRUE FALSE AND
%token BEGIN END DECL SKIP IF THEN ELSE DO WHILE AWAIT ASSERT PRINT WRITE
%token CLPAREN CRPAREN LPAREN RPAREN LBRACK RBRACK
%token ASSIGN LABELSEP SMCLN COMMA STAR PLUS MINUS
%token <string> ID
%token <string> LABEL
%token <int> INT
%token <string> TEXT
%token EOF EMPTY

%start main
%type <Util.wlang> main

%%

main:
        | decll initl procl aps EOF                 {debug "Done!"; $1,$2,$3,$4}
        ;
decll:                                              {debug "decll EMPTY"; []}
        | DECL declarations SMCLN                   {$2}
        ;
declarations:
        | ID LBRACK INT RBRACK COMMA declarations   {($1,$3)::$6}
        | ID LBRACK INT RBRACK                      {[$1,$3]}
        ;
initl:                                              {debug "initl EMPTY"; []}
        | ID ASSIGN expression SMCLN initl          {($1,$3)::$5}
        | ID ASSIGN expression SMCLN                {[$1,$3]}
        ;
expression:
        | INT                                       {WInt $1}
        | ID                                        {WId $1}
        | expression STAR expression                {WBinOp (WMul, $1, $3)}
        | expression PLUS expression                {WBinOp (WPlus, $1, $3)}
        | expression MINUS expression               {WBinOp (WMinus, $1, $3)}
        | LPAREN expression RPAREN                  {$2}
        ;
procl:                                              {debug "procl EMPTY"; []}
        | process SMCLN procl                       {$1::$3}
        | process procl                             {$1::$2}
        | process                                   {[$1]}
        ;
process:
        | BEGIN seqstatement END                    {new_id (), $2}
        ;
seqstatement:
        | labelstatement seqstatement               {$1::$2}
        | labelstatement                            {[$1]}
        ;
labelstatement:
        | LABEL LABELSEP statement                  {$1, 0, $3}
        | statement                                 {"", 0, $1}
        ;
statement:
        | ID ASSIGN expression SMCLN                {WAssign ($1, $3)}
        | SKIP SMCLN                                {WSkip}
        | IF boolexpression THEN statement ELSE statement
                                                    {WIf ($2,("",0,$4),("",0,$6))}
        | WHILE boolexpression DO statement         {WWhile ($2,("",0,$4))}
        | AWAIT boolexpression SMCLN                {WAwait $2}
        | ASSERT boolexpression SMCLN               {WAssert $2}
        | PRINT TEXT SMCLN                          {WPrint}
        | WRITE expression SMCLN                    {WWrite}
        | CLPAREN seqstatement CRPAREN              {WBlock $2}
        ;
boolexpression:
        | TRUE                                      {WBool true}
        | FALSE                                     {WBool false}
        | expression EQ expression                  {WEq ($1,$3)}
        | expression LEQ expression                 {WLeq ($1,$3)}
        | NOT boolexpression                        {WNot $2}
        | boolexpression AND boolexpression         {WAnd ($1,$3)}
        | LPAREN boolexpression RPAREN              {$2}
        ;
aps:                                                {debug "aps EMPTY"; []}
        | ap aps                                    {$1::$2}
        | ap                                        {[$1]}
        ;
ap:
        | LABEL LABELSEP boolexpression SMCLN       {$1,$3} 
