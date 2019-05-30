open Util

module H = Hashtbl

let new_id =
  let cur = ref 0 in
  let aux () =
    incr cur; !cur
  in aux

type node = {
  id : int;
  incoming : int list;
  cur : ltl list;
  old : ltl list;
  next : ltl list;
}

let node_id {id;incoming;cur;old;next} = id

let node_to_string {id;incoming;cur;old;next} =
  Printf.sprintf "Id: %d\nIn: %s\nCur: %s\nOld: %s\nNext: %s\n\n"
  id
  (list_to_string string_of_int incoming)
  (list_to_string ltl_to_string cur)
  (list_to_string ltl_to_string old)
  (list_to_string ltl_to_string next)

type generalized_buchi = {
  states : (int * ltl list) list;
  start : int list;
  final : int list list;
  transitions : (int * int) list;
  formula : ltl;
}

let normalize_buchi_ids {states;start;final;transitions;formula} =
  let ids = List.map fst states in
  let t = 
    let nids = numbered_assoc ids in
    let aux o = List.assoc o nids in
    aux
  in
  let states = List.map (fun (o,x) -> (t o, x)) states in
  let start = List.map t start in
  let final = List.map (fun l -> List.map t l) final in
  let transitions = List.map (fun (i,o) -> (t i, t o)) transitions in
  {states;start;final;transitions;formula} 

(* Gerth et al. *)
let ltl_to_generalized_buchi formula =
  let node_set_to_string node_set =
    Seq.fold_left (
      fun acc n ->
        acc ^ (node_to_string n)
    ) "" (H.to_seq_values node_set)
  in
  let node_set_to_buchi node_set =
    (if Debug.in_debug_mode () then Debug.print (node_set_to_string node_set));
    let nodes = H.to_seq_values node_set |> seq_to_list in
    let states = List.map (
      fun {id;incoming;cur;old;next} -> 
          id, (List.filter Ltl.is_atomic_proposition old |> List.fold_left (fun acc x -> list_union acc [x]) [])
    ) nodes in
    let start = 
      List.filter (
        fun {id;incoming;cur;old;next} -> List.exists ((=) (-1)) incoming
    ) nodes |> List.map node_id
    in
    let final = List.fold_left (fun acc x ->
      match x with
      | BinOp (Until, f1, f2) -> 
          let fin = List.filter (
            fun {id;incoming;cur;old;next} -> 
              (not (List.exists ((=) x) old)) || (List.exists ((=) f2) old)
          ) nodes in (List.map node_id fin)::acc
      | _ -> acc
    ) [] (Ltl.subformulae formula)
    in
    let transitions = List.map (
      fun {id;incoming;cur;old;next} -> 
        List.fold_left (
          fun acc x -> if x=(-1) then acc else (x,id)::acc
        ) [] incoming
    ) nodes |> List.flatten
    in
    {states;start;final;transitions;formula}
  in
  let neg = Ltl.negate_formula 
  in
  let new1 = function
    | BinOp (Until, f1, f2) -> [f1]
    | BinOp (Release, f1, f2) -> [f2]
    | BinOp (Or, f1, f2) -> [f1]
  in
  let new2 = function
    | BinOp (Until, f1, f2) -> [f2]
    | BinOp (Release, f1, f2) -> [f1;f2]
    | BinOp (Or, f1, f2) -> [f2]
  in
  let next1 f = match f with
    | BinOp (Until, _, _) -> [f]
    | BinOp (Release, _, _) -> [f]
    | BinOp (Or, _, _) -> []
  in
  let is_inconsistent node = 
    let res = List.exists (
      fun x -> List.exists (Ltl.is_negated_of x) node.cur
    ) node.cur
    in
    (if res then Debug.print ("Dropping node:\n"^(node_to_string node)));
    res
  in
  let rec expand node node_set = 
    if is_inconsistent node then 
      node_set
    else
    match node.cur with
    | [] -> 
        begin
          match hashtbl_find_opt (fun x -> x.old = node.old && x.next = node.next) node_set with
          | None -> 
              expand {id=new_id (); incoming=[node.id]; cur=node.next; old=[]; next=[]} (H.add node_set node.id node; node_set)
          | Some nd -> 
              H.replace node_set nd.id {nd with incoming=(list_union nd.incoming node.incoming)};
              node_set
        end
    | f::fx ->
        begin
          let node = {node with cur=fx} in
          Debug.print ("Buchi: matching "^(ltl_to_string f));
          match f with
          | Bool _ | Var _ | UnOp (Not, Var _) -> 
              if (f = Bool false) || List.exists ((=) (neg f)) node.old then
                node_set
              else
                expand {node with old=(list_union node.old [f])} node_set
          | UnOp (Next, x) ->
              expand {node with old=list_union node.old [f]; next=list_union node.next [x]} node_set
          | BinOp (Until, f1, f2) | BinOp (Release, f1, f2) | BinOp (Or, f1, f2) ->
              let n1 = {
                id=new_id (); 
                incoming=node.incoming; 
                cur=(list_union node.cur (list_diff (new1 f) node.old));
                old=(list_union node.old [f]);
                next=(list_union node.next (next1 f))
              } 
              in
              let n2 = {
                id=new_id (); 
                incoming=node.incoming; 
                cur=(list_union node.cur (list_diff (new2 f) node.old));
                old=(list_union node.old [f]);
                next=node.next
              } 
              in
              expand n2 (expand n1 node_set)
          | BinOp (And, f1, f2) ->
              expand {node with cur=(list_union node.cur (list_diff [f1;f2] node.old)); old=(list_union node.old [f])} node_set
        end 
  in
  expand {id=new_id (); incoming=[(-1)]; cur=[formula]; old=[]; next=[]} (H.create 101)
  |> node_set_to_buchi
  |> normalize_buchi_ids

let hoa_of_generalized_buchi {states;start;final;transitions;formula} =
  let aps = Ltl.atomic_propositions_ltl formula in
  let apss = List.map (fun x -> match x with Var p | UnOp (Not, Var p) -> "\""^p^"\"") aps |> String.concat " " in
  let aps_assoc = numbered_assoc aps in
  let ap_number x = List.assoc x aps_assoc in
  let apsn = List.length aps in
  let starts = String.concat "\n" (List.map string_of_int start |> List.map (fun x -> "Start: "^x)) in
  let finn = List.length final in
  let fin_assoc = numbered_assoc final in
  let fin_numbers n = List.map (
    fun x -> if List.exists ((=) n) x then List.assoc_opt x fin_assoc else None
  ) final |> List.filter ((<>) None) |> List.map (fun (Some a) -> a) in
  let fins = List.map (fun (_,x) -> "Inf("^(string_of_int x)^")") fin_assoc |> String.concat "&" in
  let body = List.fold_left (
    fun acc (id,x) -> 
      acc^(
        Printf.sprintf "\nState: [%s] %d %s\n%s"
        (
          let ns = (List.map ap_number x |> List.map (fun n -> (if Ltl.is_negated (List.nth aps n) then "!" else "")^(string_of_int n)) |> String.concat "&")
          in (if ns="" then "t" else ns)
        )
        id
        (let fn = fin_numbers id in if fn = [] then "" else "{"^(List.map string_of_int fn |> String.concat " ")^"}") 
        ("    "^(List.map string_of_int (all_assoc id transitions) |> String.concat " "))
    )
  ) "" states in
  Printf.sprintf 
  "HOA: v1\nname: \"%s\"\nStates: %d\n%s\nacc-name: generalized-Buchi\nAcceptance: %d %s\nAP: %d %s\n--BODY--\n%s\n--END--\n"
  (ltl_to_string formula)
  (List.length states)
  starts
  finn
  (if finn=0 then "t" else ("("^fins^")"))
  apsn
  apss
  body
