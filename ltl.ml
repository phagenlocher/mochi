open Util

let remove_dublicates =
  List.fold_left 
  (
    fun acc x -> 
      if List.exists (fun z -> x=z) acc then acc else x::acc
  ) []

let parse_ltl fs =
  let fix_variables fs =
    (* TODO: Fix lexer *)
    let delims = 
      [
        "tt"; "true"; "ff";
        "false"; "!"; "NOT";
        "->"; "-->"; "=>"; "==>";
        "<->"; "<=>"; "BIIMP";
        "^"; "XOR"; "xor";
        "&&"; "&"; "AND";
        "||"; "|"; "OR";
        "F"; "G"; "X"; "U"; "W"; "R"; "M"; "("; ")"
      ]
    in 
    let rgx_s = "\\("^(String.concat "\\|" delims)^"\\)"
    in
    Str.full_split (Str.regexp rgx_s) (" "^fs)
    |> List.map (fun x -> match x with Str.Text s -> s | Str.Delim s -> s)
    |> String.concat " " 
  in
  try
    let fs = (fix_variables fs) in
    Debug.print ("Fixed input formula: "^fs);
    let lexbuf = Lexing.from_string fs in 
    Some (Parser.main Lexer.token lexbuf)
  with 
  | _ -> None

let subformulae fx = 
  let rec aux f = f ::
    begin
      match f with
      | UnOp (_, sf) -> aux sf
      | BinOp (_, sf1, sf2) -> (aux sf1) @ (aux sf2)
      | _ -> []
    end
  in
  remove_dublicates (aux fx)

let rec atomic_propositions_ltl = function
  | Var p -> [Var p]
  | Bool _ -> []
  | UnOp (Not, Var f) -> [UnOp (Not, Var f)]
  | UnOp (_, f) -> atomic_propositions_ltl f
  | BinOp (_, f1, f2) -> list_union (atomic_propositions_ltl f1) (atomic_propositions_ltl f2)

let rec atomic_propositions l = List.map ltl_to_string (atomic_propositions_ltl l)

(* Non contradicting atom. prop. *)
let rec non_con_aps_ltl l = 
  let aps = atomic_propositions_ltl l in
  let aps = List.filter (
    fun x -> match x with
    | Var p -> not (List.exists ((=) (UnOp (Not, Var p))) aps)
    | UnOp (Not, Var p) -> not (List.exists ((=) (Var p)) aps)
  ) aps in
  Debug.print ("Non. con. atom. props.: "^(ltl_to_string l)^" -> "^(list_to_string ltl_to_string aps));
  aps

let is_atomic_proposition = function
  | Var _ -> true
  | UnOp (Not, Var _) -> true
  | _ -> false

let is_negated = function
  | UnOp (Not,_) -> true
  | _ -> false

let negate_formula f = UnOp (Not, f)

let nnf fx = 
  let (!!) = negate_formula
  in
  let rec simp = function
    (* Boolean simplifications *)
    | BinOp (And, Bool false, f) | BinOp (And, f, Bool false) -> Bool false
    | BinOp (Or, Bool true, f) | BinOp (Or, f, Bool true) -> Bool true
    | BinOp (Imp, Bool false, f) -> Bool true
    (* Until *)
    | BinOp (Until, f, Bool true) -> Bool true
    | BinOp (Until, f, Bool false) -> Bool false
    | BinOp (Until, Bool false, f) -> f
    (* Release *)
    | BinOp (Release, f, Bool true) -> Bool true
    | BinOp (Release, f, Bool false) -> Bool false
    (* Implications *)
    | BinOp (Imp, f1, f2) -> simp (BinOp (Or, simp (!!f1), simp f2))
    | BinOp (Biimp, f1, f2) -> simp (BinOp (And, simp (BinOp (Imp, f1, f2)), simp (BinOp (Imp, f2, f1))))
    (* XOR *)
    | BinOp (Xor, f1, f2) -> simp (BinOp (Or, (BinOp (And, simp (!!f1), simp f2)), (BinOp (And, simp f1, simp (!!f2)))))
    (* Weak until *)
    | BinOp (Wuntil, f1, f2) -> simp (BinOp (Until, f1, simp (BinOp (Or, f2, simp (UnOp (Globally, f1))))))
    (* Strong release *)
    | BinOp (Srelease, f1, f2) -> simp (BinOp (And, simp (BinOp (Release, f1, f2)), simp (UnOp (Finally, f1))))
    (* Globally and Finally  *)
    | UnOp (Globally, UnOp (Globally, f)) -> simp (UnOp (Globally, f))
    | UnOp (Finally, UnOp (Finally, f)) -> simp (UnOp (Finally, f))
    | UnOp (Globally, f) -> BinOp (Release, Bool false, simp f)
    | UnOp (Finally, f) -> BinOp (Until, Bool true, simp f)
    (* Nothing to simplify *)
    | UnOp (op, f) -> UnOp (op, simp f)
    | BinOp (op, f1, f2) -> BinOp (op, simp f1, simp f2)
    | x -> x
  in
  let rec aux f =
    match f with
    | UnOp (Not, nf) -> 
      begin
        match nf with
        | UnOp (Not, f) -> aux f
        | UnOp (Finally, f) -> UnOp (Globally, aux (!!f))
        | UnOp (Globally, f) -> UnOp (Finally, aux (!!f))
        | UnOp (Next, f) -> UnOp (Next, aux (!!f))
        | BinOp (Until, f1, f2) -> aux (BinOp (Release, !!f1, !!f2))
        | BinOp (Release, f1, f2) -> aux (BinOp (Until, !!f1, !!f2))
        | BinOp (Or, f1, f2) -> aux (BinOp (And, !!f1, !!f2))
        | BinOp (And, f1, f2) -> aux (BinOp (Or, !!f1, !!f2))
        | Bool b -> Bool (not b)
        | Var p -> !! (Var p)
        | _ -> failwith "simp failed!"
      end
    | UnOp (op, f) -> UnOp (op, aux f)
    | BinOp (op, f1, f2) -> BinOp (op, aux f1, aux f2)
    | x -> x
  in 
  simp (aux (simp fx))

