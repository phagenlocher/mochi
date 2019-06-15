open Util

module StringSet = Set.Make (String)

let parse_wlang fs =
  try
    let lexbuf = Lexing.from_string fs in 
    Some (Wparser.main Wlexer.token lexbuf)
  with 
  | _ -> Debug.print "Failed to parse wlang"; None

let check_labels (_, _, procl, apl) = 
  let label_set = ref StringSet.empty
  in
  let reg l = 
    label_set := StringSet.add l !label_set
  in
  let seen l =
    StringSet.mem l !label_set
  in
  let rec check_apl = function
    | (l,_)::xs -> if seen l then Some l else (reg l; check_apl xs)
    | [] -> None
  in
  let rec check_wstmt = function
    | WIf (_,s1,s2) -> check_wlstmt_list [s1;s2]
    | WWhile (_,s) -> check_wlstmt_list [s]
    | WBlock wl -> check_wlstmt_list wl
    | _ -> None
  and check_wlstmt_list = function
    | [] -> None
    | (l,x)::xs -> if seen l then Some l else (
      reg l; 
      let sr = check_wstmt x in
      if sr <> None then sr else check_wlstmt_list xs
    )
  in
  let rec check_procl = function
    | [] -> None
    | (_,x)::xs -> 
      let pr = check_wlstmt_list x in
      if pr <> None then pr else check_procl xs
  in
  let r = check_procl procl in
  if r <> None then r else check_apl apl

(* 
 * The let statements are used in order to force order of execution.
 * Otherwise, labels will not be sequential.
 * *)
let label_statements (decll, initl, procl, apl) = 
  let label_proc (pid,stmts) =
    let counter = ref 0 in
    let new_label () = 
      incr counter;
      Printf.sprintf "_%d:%d" pid !counter
    in
    let rec handle_stmt = function
      | WIf (b,s1,s2) -> 
          let s1 = List.hd (aux [s1])
          in
          let s2 = List.hd (aux [s2])
          in
          WIf (b, s1, s2)
      | WWhile (b,s) -> WWhile (b, List.hd (aux [s]))
      | WBlock wl -> WBlock (aux wl)
      | x -> x
    and aux = function
      | [] -> []
      | (l,s)::xs -> 
          if l <> "" then 
            let s = handle_stmt s in
            let nwls = (l,s) in
            nwls::(aux xs) 
          else
            let l = new_label () in
            let nwls = (l, handle_stmt s) in
            nwls::(aux xs)
    in (pid, aux stmts)
  in
  (decll, initl, List.map label_proc procl, apl)
  
