open Util

let parse_wlang fs =
  try
    let lexbuf = Lexing.from_string fs in 
    Some (Wparser.main Wlexer.token lexbuf)
  with 
  | _ -> Debug.print "Failed to parse wlang"; None
