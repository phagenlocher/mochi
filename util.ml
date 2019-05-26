let string_list_to_string = String.concat "" 

let rec repeat_string i s = if i<=1 then s else s^(repeat_string (i-1) s)

let debug x = print_endline x

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
        | UnOp (op,x) -> (unop_to_string op)^(ltl_to_string x)
        | BinOp (op,x1,x2) -> (ltl_to_string x1)^(binop_to_string op)^(ltl_to_string x2)
        | Var x -> x
