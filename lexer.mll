{
  open Parser
  open Util
}

rule token = parse
  | [' ' '\t' '\n' '\r']        { token lexbuf }
  | "tt" | "true" | '1'         { debug "1";TRUE }
  | "ff" | "false" | '0'        { debug "0";FALSE }
  | '!' | "NOT"                 { debug "!";NOT }
  | "->" | "-->" | "=>" | "==>" | "IMP" 
                                { debug "->";IMP }
  | "<->" | "<=>" |"BIIMP"      { debug "<->";BIIMP }
  | '^' | "XOR" | "xor"         { debug "xor";XOR }
  | "&&" | '&' | "AND"          { debug "and";AND }
  | "||" | '|' | "OR"           { debug "or";OR }
  | 'F'                         { debug "F";FINALLY }
  | 'G'                         { debug "G";GLOBALLY }
  | 'X'                         { debug "X";NEXT }
  | 'U'                         { debug "U";UNTIL }
  | 'W'                         { debug "W";WUNTIL }
  | 'R'                         { debug "R";RELEASE }
  | 'M'                         { debug "M";SRELEASE }
  | '('                         { debug "(";LPAREN }
  | ')'                         { debug ")";RPAREN }
  | (['a'-'z''_']+)(['a'-'z''A'-'Z''0'-'9']*) as str 
                                { debug str;VARIABLE (str) }
  | eof                         { EOF }
  
