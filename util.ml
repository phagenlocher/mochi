(* Set like union on lists *)
let rec list_union a = function
  | [] -> a
  | x::xs ->
      if List.exists ((=) x) a then 
        list_union a xs
      else
        list_union (x::a) xs

(* Set like removal for lists *)
let list_remove p = List.filter (fun x -> not (p x))

(* Set like difference for lists *)
let list_diff a b =
  list_remove (fun x -> List.exists ((=) x) b) a

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
