{
  open Wparser
  let debug x = Util.Debug.print ("WL-Lexer: Read '"^x^"'")
}

rule token = parse
  | [' ' '\t' '\n' '\r']        { token lexbuf }
  | '{'                         { debug "{"; CLPAREN }
  | '}'                         { debug "}"; CRPAREN }
  | '('                         { debug "("; LPAREN }
  | ')'                         { debug ")"; RPAREN }
  | '['                         { debug "["; LBRACK }
  | ']'                         { debug "]"; RBRACK }
  | ';'                         { debug ";"; SMCLN }
  | ','                         { debug ","; COMMA }
  | '*'                         { debug "*"; STAR }
  | '+'                         { debug "+"; PLUS }
  | '-'                         { debug "-"; MINUS }
  | '='                         { debug "="; EQ }
  | "<="                        { debug "<="; LEQ }
  | ':'                         { debug ":"; LABELSEP }
  | "not"                       { debug "NOT"; NOT }
  | "and"                       { debug "AND"; AND }
  | "true"                      { debug "TRUE"; TRUE }
  | "false"                     { debug "FALSE"; FALSE }
  | "beginprocess"              { debug "BEGIN"; BEGIN }
  | "endprocess"                { debug "END"; END }
  | "decl"                      { debug "DECL"; DECL }
  | "skip"                      { debug "SKIP"; SKIP }
  | "if"                        { debug "IF"; IF }
  | "then"                      { debug "THEN"; THEN }
  | "else"                      { debug "ELSE"; ELSE }
  | "do"                        { debug "DO"; DO }
  | "while"                     { debug "WHILE"; WHILE }
  | "await"                     { debug "AWAIT"; AWAIT }
  | "assert"                    { debug "ASSERT"; ASSERT }
  | "print"                     { debug "PRINT"; PRINT }
  | "write"                     { debug "WRITE"; WRITE }
  | ":="                        { debug ":="; ASSIGN }
  | ['0'-'9']+ as d             { debug d; INT (int_of_string d)}
  | (['a'-'z''A'-'Z'])(['a'-'z''A'-'Z''0'-'9']*) as str 
                                { debug str; ID (str) }
  | '_'(['a'-'z''A'-'Z''0'-'9']*) as str 
                                { debug str; LABEL (str) }
  | '"'(_*)'"' as str           { debug str; TEXT (str) }
  | eof                         { EOF }
  
