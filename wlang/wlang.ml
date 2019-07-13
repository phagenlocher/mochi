open Util

module H = Hashtbl
module StringSet = Set.Make (String)

let parse_wlang fs =
  try
    let lexbuf = Lexing.from_string fs in 
    Some (Wparser.main Wlexer.token lexbuf)
  with 
  | _ -> Debug.error "Failed to parse wlang"; None

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
    | (l,i,x)::xs -> if seen l then Some l else (
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

let id_stmt_map = H.create 100

let get_stmt procn i = 
  if i = (-1) then
    ("halt",-1,WHalt)
  else
    H.find id_stmt_map (procn,i)

(* 
 * The let statements are used in order to force order of execution.
 * Otherwise, labels will not be sequential.
 * *)
let label_statements (decll, initl, procl, apl) = 
  let label_proc (pid,stmts) =
    let reg i s = H.add id_stmt_map (pid,i) s in
    let counter = ref (-1) in
    let g_counter = ref (-1) in
    let new_id () = 
      incr g_counter; !g_counter
    in
    let new_label () = 
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
      | (l,i,s)::xs -> 
          incr counter; 
          let sid = new_id () in
          if l <> "" then 
            let s = handle_stmt s in
            let nwls = (l,sid,s) in
            reg sid nwls;
            nwls::(aux xs) 
          else
            let l = new_label () in
            let nwls = (l,sid,handle_stmt s) in
            reg sid nwls;
            nwls::(aux xs)
    in (pid, aux stmts)
  in
  (decll, initl, List.map label_proc procl, apl)

let rec get_stmt_from_wlstmt l p = 
  match l with
  | [] -> None
  | (l,i,wstmt)::xs ->
      if p (l,i,wstmt) then 
        Some (l,i,wstmt)
      else 
        let r = get_stmt_from_wstmt wstmt p in
        if r <> None then r else get_stmt_from_wlstmt xs p
and get_stmt_from_wstmt stmt p =
  match stmt with
  | WIf (_,s1,s2) -> get_stmt_from_wlstmt [s1;s2] p
  | WWhile (_,s) -> get_stmt_from_wlstmt [s] p
  | WBlock sl -> get_stmt_from_wlstmt sl p
  | _ -> None

(* (int*int) -> (wbool * wlstmt) list *)
let next_map = H.create 100

let get_next_stmts procn i = if i=(-1) then [] else H.find next_map (procn, i)

let rec tl = function
  | [] -> failwith "tl failed on []"
  | [x] -> x
  | _::xs -> tl xs

let rec last_stmts (l,i,stmt) = match stmt with
  | WIf (_,s1,s2) -> (last_stmts s1) @ (last_stmts s2)
  | WBlock sl -> last_stmts (tl sl)
  | _ -> [(l,i,stmt)]

let build_next_map ((_,_,procl,_) : wlang) = 
  let rec aux procn ?u:(u=None) l =
    let hadd i x = match H.find_opt next_map (procn,i) with
      | None -> 
          H.add next_map (procn,i) x
      | Some xs -> 
          H.replace next_map (procn,i) (x@xs)
    in
    match l with
    | [] -> ()
    | (l,i,stmt)::xs -> 
      let curs = (l,i,stmt)
      in
      let halt = ("halt", (-1), WHalt)
      in
      let next = (match xs with [] -> (match u with Some x -> x | _ -> halt) | n::_ -> n)
      in
      begin
        match stmt with
        | WIf (b,ts,es) -> 
            hadd i [b, ts; WNot b, es]; aux procn ~u:(Some next) [ts]; aux procn ~u:(Some next) [es]
        | WWhile (b,s) -> 
            hadd i [b, s; WNot b, next]; aux procn ~u:(Some (l,i,stmt)) [s]
        | WBlock sl -> hadd i [WBool true, List.hd sl]; aux procn ~u:u sl
        | WAssert b -> hadd i [b, next; WNot b, halt]
        | WAwait b -> hadd i [b, next; WNot b, curs]
        | _ -> hadd i [WBool true, next]
      end;
      aux procn ~u:u xs
  in 
  List.iter (fun (i,x) -> (aux i x)) procl

type kripke_state = {
  lims : (string, int) H.t;
  vars : (string, int) H.t;
  ppos : (int, int) H.t
}

let state_to_string s =
  List.fold_left (
    fun acc (procn,i) -> acc^(
      Printf.sprintf "%d: %s; " procn
      (match get_stmt procn i with (l,_,_) -> l)
    )
  ) "" (H.to_seq s.ppos |> seq_to_list |> List.sort (fun (x,_) (y,_) -> x-y))

let copy_state {lims; vars; ppos} = 
  {
    lims;
    vars = H.copy vars;
    ppos = H.copy ppos
  }

let rec eval_exp vars = function
  | WInt i -> i
  | WId id -> H.find vars id
  | WBinOp (op, e1, e2) ->
    let r1 = eval_exp vars e1 in
    let r2 = eval_exp vars e2 in
    match op with
      | WMul -> r1 * r2
      | WPlus -> r1 + r2
      | WMinus -> r1 - r2

let rec eval_bool vars = function
  | WBool b -> b
  | WNot b -> not (eval_bool vars b)
  | WAnd (b1,b2) -> (eval_bool vars b1) && (eval_bool vars b2)
  | WEq (e1,e2) -> (eval_exp vars e1) = (eval_exp vars e2)
  | WLeq (e1,e2) -> (eval_exp vars e1) <= (eval_exp vars e2)

let init_state (decll, initl, procl, _) =
  let lims = H.create (List.length decll) in
  let vars = H.create (List.length decll) in
  let ppos = H.create (List.length procl) in
  let eval_exp = eval_exp vars
  in
  List.iter (
    fun (n,i) -> H.add lims n (max 0 (i-1)); H.add vars n 0
  ) decll;
  List.iter (
    fun (n,e) -> H.replace vars n (eval_exp e)
  ) initl;
  List.iter (
    fun (i,_) -> H.add ppos i 0
  ) procl;
  {lims; vars; ppos}

let next_state procn {lims; vars; ppos} =
  let old_state = {lims; vars; ppos} in
  let new_vars = H.copy vars in
  let new_ppos = H.copy ppos in
  let new_state = {lims; vars=new_vars; ppos=new_ppos} in
  let npos = (H.find ppos procn) in
  if npos = (-1) then 
    old_state
  else
  match get_stmt procn npos with (_,i,stmt) -> 
    (if npos <> i then Debug.error "npos is not stmt id!");
    begin
      match stmt with
      | WAssign (varn, e) -> 
          let nv = (eval_exp vars e)
          in
          H.replace new_vars varn (max 0 (min nv (H.find lims varn)))
      | _ -> ()
    end;
    match get_next_stmts procn npos with
    | [] -> old_state
    | sl -> match List.find (fun (b,s) -> eval_bool vars b) sl with
      | (_,(_,ni,_)) ->
          if ni=i then 
            old_state 
          else (
            H.replace new_ppos procn ni; 
            new_state
          )

let next_states state = 
  List.map (
    fun procn -> next_state procn state
  ) (H.to_seq_keys state.ppos |> seq_to_list)

let hashtbl_neq h1 h2 =
  let aux h1 h2 = 
    H.fold (
      fun k v b -> if b then true else (H.find h2 k) <> v
    ) h1 false
  in
  (aux h1 h2) || (aux h2 h1)

let states_neq s1 s2 =
  (hashtbl_neq s1.vars s2.vars) || (hashtbl_neq s1.ppos s2.ppos)

let explore_everything p =
  let map = H.create 12345
  in
  let ns = ref []
  in
  let add x y = match H.find_opt map x with
    | Some e when e=y -> ()
    | _ -> H.add map x y
  in
  let news x = 
    if (H.mem map x) || (List.mem x !ns) then 
      () 
    else 
      ns := x::!ns
  in
  let rec aux s = match next_states s with
    | [] -> ()
    | sl -> List.iter (fun x -> add s x; news x) sl
  in
  ns := [init_state p];
  while !ns <> [] do
    let olds = !ns in
    Debug.print ("Frontier-size: "^(List.length olds |> string_of_int));
    ns := [];
    List.iter (fun x -> aux x) olds;
    ns := List.filter (fun x -> not (H.mem map x)) !ns
  done;
  H.to_seq map |> seq_to_list

let state_map_to_dot m = 
  let dot = 
    List.fold_left (
      fun acc (x,y) -> 
        acc^"\""^(state_to_string x)^"\" -> \""^(state_to_string y)^"\";\n"
    ) "" m
  in
  Printf.sprintf "digraph k { \n %s }" dot

let ap_holds name aps {lims;vars;ppos} =
  match List.assoc_opt name aps with
  | Some bexp -> eval_bool vars bexp
  | None ->
    let pos_labels = List.map (
      fun (p,i) -> (fun (l,_,_) -> l) (get_stmt p i)
    ) (H.to_seq ppos |> seq_to_list)
    in
    List.mem name pos_labels

