open Util

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
  remove_duplicates (aux fx)

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

let is_next = function
  | UnOp (Next,_) -> true
  | _ -> false

let negate_formula f = UnOp (Not, f)

let rec is_negated_of f1 f2 = 
  (nnf (negate_formula f1) = nnf f2) || (nnf f1 = nnf (negate_formula f2))
and ltl_eq a b = 
  (Debug.print ((ltl_to_string a)^" EQ? "^(ltl_to_string b)));
  let res = match ((nnf a), (nnf b)) with (* fix this *)
  | UnOp (op1, f), UnOp (op2, g) -> (op1=op2) && (ltl_eq f g)
  | BinOp (And, f1, f2), BinOp (And, g1, g2)
  | BinOp (Or, f1, f2), BinOp (Or, g1, g2) -> (ltl_eq f1 g1 && ltl_eq f2 g2) || (ltl_eq f1 g2 && ltl_eq f2 g1)
  | BinOp (op1, f1, f2), BinOp (op2, g1, g2) -> (op1=op2) && (ltl_eq f1 g1 && ltl_eq f2 g2)
  | x,y -> x=y
  in
  (Debug.print ("EQ = "^(string_of_bool res))); res
and nnf fx = 
  let (!!) = negate_formula
  in
  let (===) a b = ltl_eq a b
  in
  (* Boolean rewriting *)
  let rec simp4 f = match f with
    | BinOp (And, BinOp (Until, f1, f2), BinOp (Until, f3, f4))
    | BinOp (And, BinOp (Until, f1, f2), BinOp (Wuntil, f3, f4))
      when (f2 === f4) -> simp4 (BinOp (Until, BinOp (And, f1, f3), f2))
    | BinOp (And, BinOp (Wuntil, f1, f2), BinOp (Wuntil, f3, f4)) 
      when (f2 === f4) -> simp4 (BinOp (Wuntil, BinOp (And, f1, f3), f2))
    | BinOp (And, BinOp (Release, f1, f2), BinOp (Release, f3, f4))
      when (f1 === f3) -> simp4 (BinOp (Release, f1, BinOp (And, f2, f4))) 
    | BinOp (And, BinOp (Release, f1, f2), BinOp (Srelease, f3, f4))
      when (f1 === f3) -> simp4 (BinOp (Srelease, f1, BinOp (And, f2, f4))) 
    | BinOp (And, BinOp (Srelease, f1, f2), BinOp (Srelease, f3, f4))
      when (f1 === f3) -> simp4 (BinOp (Srelease, f1, BinOp (And, f2, f4))) 
    | BinOp (Or, BinOp (Until, f1, f2), BinOp (Until, f3, f4)) 
      when (f1 === f3) -> simp4 (BinOp (Until, f1, BinOp (Or, f2, f4)))
    | BinOp (Or, BinOp (Until, f1, f2), BinOp (Wuntil, f3, f4)) 
      when (f1 === f3) -> simp4 (BinOp (Wuntil, f1, BinOp (Or, f2, f4)))
    | BinOp (Or, BinOp (Wuntil, f1, f2), BinOp (Wuntil, f3, f4)) 
      when (f1 === f3) -> simp4 (BinOp (Wuntil, f1, BinOp (Or, f1, f4)))
    | BinOp (Or, BinOp (Release, f1, f2), BinOp (Release, f3, f4)) 
      when (f2 === f4) -> simp4 (BinOp (Release, BinOp (Or, f1, f3), f2))
    | BinOp (Or, BinOp (Release, f1, f2), BinOp (Srelease, f3, f4)) 
      when (f2 === f4) -> simp4 (BinOp (Release, BinOp (Or, f1, f3), f3))
    | BinOp (Or, BinOp (Srelease, f1, f2), BinOp (Srelease, f3, f4)) 
      when (f2 === f4) -> simp4 (BinOp (Srelease, BinOp (Or, f1, f3), f3))
    (* Pass to simp3 *)
    | x -> x
  in
  (* Rewriting *)
  let rec simp3 f = match simp4 f with
    | BinOp (Wuntil, f, Bool true) -> simp3 (UnOp (Globally, simp3 f))
    | BinOp (Srelease, f, Bool true) -> simp3 (UnOp (Finally, simp3 f))
    | BinOp (op, (UnOp (Next, f1)), (UnOp (Next, f2))) -> simp3 (UnOp (Next, simp3 (BinOp (op, f1, f2))))
    | BinOp (biop, f1, UnOp (unop, f2)) 
        when  (((biop = Until || biop = Wuntil) && unop = Globally) ||
              ((biop = Release || biop = Srelease) && unop = Finally))
              && (f1 === f2) ->
                simp3 (UnOp (unop, simp3 f1))
    (* Pass to simp2 *)
    | x -> x 
  in
  (* Basic Simps *)
  let rec simp2 f = match simp3 f with 
    (* Next *)
    | UnOp (Next, (UnOp (Finally, (UnOp (Globally, f))))) -> simp2 (UnOp (Finally, (UnOp (Globally, f))))
    | UnOp (Next, (UnOp (Globally, (UnOp (Finally, f))))) -> simp2 (UnOp (Globally, (UnOp (Finally, f))))
    (* Globally & Finally *)
    | UnOp (Globally, (UnOp (Finally, (UnOp (Globally, f))))) -> simp2 (UnOp (Finally, (UnOp (Globally, f))))
    | UnOp (Finally, (UnOp (Globally, (UnOp (Finally, f))))) -> simp2 (UnOp (Globally, (UnOp (Finally, f))))
    | UnOp (Globally, (BinOp (Release, _, f))) -> simp2 (UnOp (Globally, simp2 f))
    | UnOp (Finally, (BinOp (Until, _, f))) -> simp2 (UnOp (Finally, simp2 f))
    | UnOp (Globally, (BinOp (Wuntil, f1, f2))) -> simp2 (UnOp (Globally, (BinOp (Or, simp2 f1, simp2 f2))))
    | UnOp (Finally, (BinOp (Srelease, f1, f2))) -> simp2 (UnOp (Finally, (BinOp (And, simp2 f1, simp2 f2))))
    (* Pass to simp *)
    | x -> x
  in
  let rec simp f = match simp2 f with
    (* Boolean simplifications *)
    | BinOp (And, Bool false, f) | BinOp (And, f, Bool false) -> Bool false
    | BinOp (And, Bool true, f) | BinOp (And, f, Bool true) -> simp f
    | BinOp (Or, Bool false, f) | BinOp (Or, f, Bool false) -> simp f
    | BinOp (Or, Bool true, f) | BinOp (Or, f, Bool true) -> Bool true
    | BinOp (And, f1, f2) when (is_negated_of f1 f2) -> Bool false
    | BinOp (Or, f1, f2) when (is_negated_of f1 f2) -> Bool true
    | BinOp (Or, f1, f2) when (f1 === f2) -> simp f1
    | BinOp (And, f1, f2) when (f1 === f2) -> simp f1
    | BinOp (Imp, Bool false, f) -> Bool true
    (* Until *)
    | BinOp (Until, f, Bool true) -> Bool true
    | BinOp (Until, f, Bool false) -> Bool false
    | BinOp (Until, Bool false, f) -> simp f
    | BinOp (Until, f1, f2) when (f1 === f2) -> simp f1
    (* Release *)
    | BinOp (Release, f, Bool true) -> Bool true
    | BinOp (Release, f, Bool false) -> Bool false
    | BinOp (Release, f1, f2) when (f1 === f2) -> simp f1
    (* Implications *)
    | BinOp (Imp, f1, f2) -> simp (BinOp (Or, simp (!!f1), simp f2))
    | BinOp (Biimp, f1, f2) when (f1 === f2) -> Bool true
    | BinOp (Biimp, f1, f2) -> simp (BinOp (And, simp (BinOp (Imp, f1, f2)), simp (BinOp (Imp, f2, f1))))
    (* XOR *)
    | BinOp (Xor, f1, f2) when (f1 === f2) -> Bool false
    | BinOp (Xor, f1, f2) -> simp (BinOp (Or, (BinOp (And, simp (!!f1), simp f2)), (BinOp (And, simp f1, simp (!!f2)))))
    (* Weak until *)
    | BinOp (Wuntil, f1, f2) when (f1 === f2) -> simp f1
    | BinOp (Wuntil, f1, f2) -> simp (BinOp (Until, simp f1, simp (BinOp (Or, simp f2, simp (UnOp (Globally, simp f1))))))
    (* Strong release *)
    | BinOp (Srelease, f1, f2) when (f1 === f2) -> simp f1
    | BinOp (Srelease, f1, f2) -> simp (BinOp (And, simp (BinOp (Release, simp f1, simp f2)), simp (UnOp (Finally, simp f1))))
    (* Globally and Finally  *)
    | UnOp (Globally, UnOp (Globally, f)) -> simp (UnOp (Globally, simp f))
    | UnOp (Finally, UnOp (Finally, f)) -> simp (UnOp (Finally, simp f))
    | UnOp (Finally, UnOp (Next, f)) -> simp (UnOp (Next, UnOp (Finally, simp f)))
    | UnOp (Globally, UnOp (Next, f)) -> simp (UnOp (Next, UnOp (Globally, simp f)))
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
  simp fx |> simp |> aux |> simp |> simp

