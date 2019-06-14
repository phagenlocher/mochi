let remove_duplicates ?f:(f=(=)) xs =
  List.fold_left 
  (
    fun acc x -> 
      if List.exists (fun z -> (f x z)) acc then acc else x::acc
  ) [] xs

(* Set like union on lists *)
let rec list_union ?f:(f=(=)) a = function
  | [] -> a
  | x::xs ->
      if List.exists (f x) a then 
        list_union a xs
      else
        list_union (x::a) xs

(* Set like removal for lists *)
let list_remove p = List.filter (fun x -> not (p x))

(* Set like difference for lists *)
let list_diff ?f:(f=(=)) a b =
  list_remove (fun x -> List.exists (f x) b) a

(* Set like equality *)
let rec list_eq ?f:(f=(=)) a b = match a,b with
  | [],[] -> true
  | x::xs, [] | [], x::xs -> false
  | x::xs, ys ->
      if List.exists (f x) ys then
        list_eq ~f:(f) xs (list_remove (fun y -> f x y) ys)
      else
        false

let list_to_string f l =
    let res = List.fold_left (fun acc x -> acc^(f x)^";") "" l
    in "["^res^"]"

(* List.find_opt for hashtables *)
let hashtbl_find_opt p ht =
  Seq.fold_left (
    fun acc x -> if (acc <> None) then acc else if (p x) then Some x else None
  ) None (Hashtbl.to_seq_values ht)

let numbered_assoc ?start:(start=0) xs =
  let rec aux n = function
    | [] -> []
    | x::xs -> (x,n)::(aux (n+1) xs)
  in aux start xs

let rec all_assoc x = function
  | [] -> []
  | (a,b)::xs -> if x=a then b::(all_assoc x xs) else all_assoc x xs

let seq_to_list s = 
  Seq.fold_left (fun acc x -> x::acc) [] s
  |> List.rev

module Debug = struct 

    let debug = ref false

    let init x = debug := x

    let in_debug_mode () = !debug

    let print x = if !debug then Printf.fprintf stderr "%s\n" x

end

type ltl =
        | Bool of bool
        | UnOp of unop * ltl
        | BinOp of binop * ltl * ltl
        | Var of string
and binop =
        | Or
        | And
        | Biimp
        | Imp
        | Xor
        | Until
        | Wuntil
        | Release
        | Srelease
and unop =
        | Not
        | Finally
        | Globally
        | Next       

let unop_to_string = function
        | Not -> "!"
        | Finally -> "F"
        | Globally -> "G"
        | Next -> "X"

let binop_to_string = function
        | Or -> "|"
        | And -> "&"
        | Biimp -> "<->"
        | Imp -> "->"
        | Xor -> "^"
        | Until -> "U"
        | Wuntil -> "W"
        | Release -> "R"
        | Srelease -> "M"

let rec ltl_to_string = function
        | Bool x -> string_of_bool x
        | UnOp (op,Var p) -> (unop_to_string op)^p
        | UnOp (op,f) -> (unop_to_string op)^("("^(ltl_to_string f)^")")
        | BinOp (op,x1,x2) -> "("^(ltl_to_string x1)^")"^(binop_to_string op)^"("^(ltl_to_string x2)^")"
        | Var x -> x

type wlang = wdecl list * winit list * wproc list * wap list
and wdecl = string * int
and winit = string * wexp
and wproc = wlstmt list
and wap = string * wbool
and wbool = 
        | WBool of bool
        | WNot of wbool
        | WAnd of wbool * wbool
        | WEq of wexp * wexp
        | WLeq of wexp * wexp
and wexp = 
        | WInt of int
        | WId of string
        | WBinOp of wbinop * wexp * wexp
and wbinop =
        | WMul
        | WPlus
        | WMinus
and wlstmt = string * wstmt
and wstmt = 
        | WAssign of string * wexp
        | WSkip
        | WIf of wbool * wstmt * wstmt
        | WWhile of wbool * wstmt
        | WAwait of wbool
        | WAssert of wbool
        | WPrint
        | WWrite
        | WBlock of wlstmt list

let wdecl_to_string (n,i) = n^"["^(string_of_int i)^"]"

let wbinop_to_string = function
  | WMul -> "*"
  | WPlus -> "+"
  | WMinus -> "-"

let rec wexp_to_string = function
  | WInt i -> string_of_int i
  | WId s -> s
  | WBinOp (op, e1, e2) -> (wexp_to_string e1)^(wbinop_to_string op)^(wexp_to_string e2)

let winit_to_string (n,e) = n^" := "^(wexp_to_string e)

let rec wbool_to_string = function
  | WBool x -> string_of_bool x
  | WNot x -> "not "^(wbool_to_string x)
  | WAnd (x1,x2) -> (wbool_to_string x1)^" and "^(wbool_to_string x2)
  | WEq (x1,x2) -> (wexp_to_string x1)^" = "^(wexp_to_string x2)
  | WLeq (x1,x2) -> (wexp_to_string x1)^" <= "^(wexp_to_string x2)

let rec wstmt_to_string = function
  | WAssign (n,e) -> n^" := "^(wexp_to_string e)
  | WIf (b,s1,s2) -> "if "^(wbool_to_string b)^" then\n"^(wstmt_to_string s1)^"\nelse\n"^(wstmt_to_string s2)
  | WWhile (b,s) -> "while "^(wbool_to_string b)^" do\n"^(wstmt_to_string s)
  | WAwait b -> "await "^(wbool_to_string b)
  | WAssert b -> "assert "^(wbool_to_string b)
  | WPrint -> "print"
  | WSkip -> "skip"
  | WWrite -> "write"
  | WBlock sl -> 
      let c = List.map (fun x -> "\t"^(wlstmt_to_string x)) sl
      in "{\n"^(String.concat "\n" c)^"\n}"
and wlstmt_to_string (l,s) = 
  if l = "" then wstmt_to_string s else l^": "^(wstmt_to_string s)

let wproc_to_string sl = 
  let c = List.map (fun x -> (wlstmt_to_string x)) sl
  in "beginprocess\n"^(String.concat "\n" c)^"\nendprocess"

let wap_to_string (n,b) = n^": "^(wbool_to_string b)

let rec wlang_to_string (decll, initl, procl, wapl) = 
  let d = List.map wdecl_to_string decll |> String.concat "\n\t"
  in
  let i = List.map winit_to_string initl |> String.concat "\n\t"
  in
  let p = List.map wproc_to_string procl |> String.concat "\n"
  in
  let a = List.map wap_to_string wapl |> String.concat "\n\t"
  in
  Printf.sprintf
  "Declarations:\n\t%s\nInitialization:\n\t%s\nProcs:\n%s\nAPs:\n\t%s"
  d
  i
  p
  a
