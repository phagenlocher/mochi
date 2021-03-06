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
          id, (List.filter Ltl.is_atomic_proposition old |> remove_duplicates)
    ) nodes in
    let start = 
      List.filter (
        fun {id;incoming;cur;old;next} -> List.mem (-1) incoming
    ) nodes |> List.map node_id
    in
    let final = List.fold_left (fun acc x ->
      match x with
      | BinOp (Until, _, f) | UnOp (Finally, f) -> 
          let fin = List.filter (
            fun {id;incoming;cur;old;next} -> 
              (not (List.mem x old)) || (List.mem f old)
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
    | _ -> failwith "impossible for new1"
  in
  let new2 = function
    | BinOp (Until, f1, f2) -> [f2]
    | BinOp (Release, f1, f2) -> [f1;f2]
    | BinOp (Or, f1, f2) -> [f2]
    | _ -> failwith "impossible for new2"
  in
  let next1 f = match f with
    | BinOp (Until, _, _) -> [f]
    | BinOp (Release, _, _) -> [f]
    | BinOp (Or, _, _) -> []
    | _ -> failwith "impossible for next1"
  in
  let rec expand node node_set = 
    match node.cur with
    | [] -> 
        begin
          match hashtbl_find_opt (fun x -> x.old = node.old && x.next = node.next) node_set with
          | None -> 
              expand {id=new_id (); incoming=[node.id]; cur=node.next; old=[]; next=[]} (H.replace node_set node.id node; node_set)
          | Some nd -> 
              H.replace node_set nd.id {nd with incoming=(list_union nd.incoming node.incoming)};
              node_set
        end
    | f::fx ->
        begin
          let node = {node with cur=fx} in
          match f with
          | Bool _ | Var _ | UnOp (Not, Var _) -> 
              if (f = Bool false) || List.exists ((=) (neg f)) node.old then
                node_set
              else
                expand {node with old=(list_union node.old [f])} node_set
          | UnOp (Next, x) ->
              expand {node with old=list_union node.old [f]; next=list_union node.next [x]} node_set
          | UnOp (Finally, x) ->
              let n1 = {
                id=new_id (); 
                incoming=node.incoming; 
                cur=(list_union node.cur (list_diff [Bool true] node.old));
                old=(list_union node.old [f]);
                next=(list_union node.next [f])
              } 
              in
              let n2 = {
                id=new_id (); 
                incoming=node.incoming; 
                cur=(list_union node.cur (list_diff [x] node.old));
                old=(list_union node.old [f]);
                next=node.next
              } 
              in
              expand n2 (expand n1 node_set)
          | UnOp (Globally, x) ->
              let n1 = {
                id=new_id (); 
                incoming=node.incoming; 
                cur=(list_union node.cur (list_diff [x] node.old));
                old=(list_union node.old [f]);
                next=(list_union node.next [f])
              } 
              in
              let n2 = {
                id=new_id (); 
                incoming=node.incoming; 
                cur=(list_union node.cur (list_diff [Bool false;x] node.old));
                old=(list_union node.old [f]);
                next=node.next
              } 
              in
              expand n2 (expand n1 node_set)
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
          | _ -> failwith "impossible for expand" 
        end 
  in
  expand {id=new_id (); incoming=[(-1)]; cur=[formula]; old=[]; next=[]} (H.create 12345)
  |> node_set_to_buchi
  |> normalize_buchi_ids

let atom_prop_assoc formula =
  let aps = Ltl.atomic_propositions_ltl formula in
  List.map (fun x -> match x with UnOp (Not, Var p) -> Var p | x -> x) aps 
  |> remove_duplicates

let hoa_of_generalized_buchi {states;start;final;transitions;formula} =
  (Debug.print "Generating HOA...");
  let aps = atom_prop_assoc formula in
  let apss = List.map (fun x -> match x with Var p -> "\""^p^"\"" | _ -> failwith "apps expected Var constructor") aps |> String.concat " " in
  let aps_assoc = numbered_assoc aps in
  let ap_number x = match x with
    | Var p -> List.assoc x aps_assoc 
    | UnOp (Not, Var p) -> List.assoc (Var p) aps_assoc
    | _ -> failwith "ap_number expected atom. prop."
  in
  let apsn = List.length aps in
  let starts = String.concat "\n" (List.map string_of_int start |> List.map (fun x -> "Start: "^x)) in
  let finn = List.length final in
  let fin_assoc = numbered_assoc final in
  let fin_numbers n = List.map (
    fun x -> if List.exists ((=) n) x then List.assoc_opt x fin_assoc else None
  ) final |> List.filter ((<>) None) |> List.map (fun o -> match o with Some a -> a | None -> failwith "fin_numbers didn't expect None") in
  let fins = List.map (fun (_,x) -> "Inf("^(string_of_int x)^")") fin_assoc |> String.concat "&" in
  let body = List.fold_left (
    fun acc (id,x) -> 
      acc^(
        Printf.sprintf "\nState: [%s] %d %s\n%s"
        (
          let ns = ((List.map (fun ap -> (if (Ltl.is_negated ap) then "!" else "")^(string_of_int (ap_number ap))) x) |> String.concat "&")
          in (if ns="" then "t" else ns)
        )
        id
        (let fn = fin_numbers id in if fn = [] then "" else "{"^(List.map string_of_int fn |> remove_duplicates |> String.concat " ")^"}") 
        ("    "^(List.map string_of_int (all_assoc id transitions) |> String.concat " "))
    )
  ) "" states in
  Printf.sprintf 
  "HOA: v1\nname: \"%s\"\nStates: %d\n%s\nacc-name: generalized-Buchi %d\nAcceptance: %d %s\nAP: %d %s\n--BODY--%s\n--END--"
  (ltl_to_string formula)
  (List.length states)
  starts
  finn
  finn
  (if finn=0 then "t" else ("("^fins^")"))
  apsn
  apss
  body

type buchi = {
    states : (int * ltl list) list;
    start : int list;
    final : int list;
    transitions : (int * int) list;
    formula : ltl;
}

module S = Set.Make(struct type t = int let compare = compare end)

let remove_non_reachable_states (b:buchi) =
  let rec reach f =
    let new_f = S.union f (S.of_list ((List.filter (fun (x,_) -> S.mem x f) b.transitions) |> List.map snd))
    in
    if S.equal f new_f then 
      new_f
    else
      reach new_f
  in
  let reachables = reach (S.of_list b.start)
  in
  let new_states = List.filter (fun (x,_) -> S.mem x reachables) b.states
  in
  let new_final = List.filter (fun x -> S.mem x reachables) b.final
  in
  let new_trans = List.filter (fun (x,_) -> S.mem x reachables) b.transitions
  in
  {b with states=new_states;final=new_final;transitions=new_trans}

let normalize_ids {states;start;final;transitions;formula} =
  let ids = List.map fst states in
  let t = 
    let nids = numbered_assoc ids in
    let aux o = List.assoc o nids in
    aux
  in
  let states = List.map (fun (o,x) -> (t o, x)) states in
  let start = List.map t start in
  let final = List.map t final in
  let transitions = List.map (fun (i,o) -> (t i, t o)) transitions in
  {states;start;final;transitions;formula} 

let degeneralize (gba:generalized_buchi) : buchi =
  let flatten (gba:generalized_buchi) =
    let states = gba.states in
    let start = gba.start in
    let transitions = gba.transitions in
    let formula = gba.formula in
    let final = List.flatten gba.final
    in
    {states;start;final;transitions;formula}
  in
  let transform (gba:generalized_buchi) =
    let new_id =
      let c = ref (-1) in
      let aux () =
        incr c; !c
      in aux
    in
    let fin_nums = List.init (List.length gba.final) (fun x -> x)
    in
    let n0 = List.hd fin_nums
    in
    let new_states = H.create 100
    in
    let get_state oid n = H.find new_states (oid,n) |> fst
    in
    let is_fin id n = List.mem id (List.nth gba.final n)
    in
    List.iter (
      fun (i,l) -> 
        List.iter (
          fun x -> H.add new_states (i,x) (new_id (), l)
        ) fin_nums 
    ) gba.states;
    let start = List.map (fun i -> get_state i n0) gba.start
    in
    let final = List.map (fun i -> get_state i n0) (List.nth (gba.final) n0)
    in
    let transitions = 
      List.map (
         fun n -> 
          List.map (
            fun (id1,id2) -> 
              let nid1 = get_state id1 n in
              if is_fin id1 n then
                (nid1, get_state id2 ((n+1) mod (List.length gba.final)))
              else
                (nid1, get_state id2 n)
          ) gba.transitions
      ) fin_nums |> List.flatten
    in
    let states = H.to_seq_values new_states |> seq_to_list
    in
    {states;start;final;transitions;formula=gba.formula}
  in
  if List.length gba.final <= 1 then 
    begin
    Debug.print "Degeneralization: Flatten";
    flatten gba
    end
  else 
    begin
    Debug.print "Degeneralization: Transform";
    transform gba |> remove_non_reachable_states |> normalize_ids
    end

let hoa_of_buchi {states;start;final;transitions;formula} =
  (Debug.print "Generating HOA...");
  let aps = atom_prop_assoc formula in
  let apss = List.map (fun x -> match x with Var p -> "\""^p^"\"" | _ -> failwith "apps expected Var constructor") aps |> String.concat " " in
  let aps_assoc = numbered_assoc aps in
  let ap_number x = match x with
    | Var p -> List.assoc x aps_assoc 
    | UnOp (Not, Var p) -> List.assoc (Var p) aps_assoc
    | _ -> failwith "ap_number expected atom. prop."
  in
  let apsn = List.length aps in
  let starts = String.concat "\n" (List.map string_of_int start |> List.map (fun x -> "Start: "^x)) in
  let finn = if final=[] then 0 else 1 in
  let fin_assoc = numbered_assoc [final] in
  let fin_numbers n = (if List.mem n final then [0] else []) in
  let fins = List.map (fun (_,x) -> "Inf("^(string_of_int x)^")") fin_assoc |> String.concat "&" in
  let body = List.fold_left (
    fun acc (id,x) -> 
      acc^(
        Printf.sprintf "\nState: [%s] %d %s\n%s"
        (
          let ns = ((List.map (fun ap -> (if (Ltl.is_negated ap) then "!" else "")^(string_of_int (ap_number ap))) x) |> String.concat "&")
          in (if ns="" then "t" else ns)
        )
        id
        (let fn = fin_numbers id in if fn = [] then "" else "{"^(List.map string_of_int fn |> remove_duplicates |> String.concat " ")^"}") 
        ("    "^(List.map string_of_int (all_assoc id transitions) |> String.concat " "))
    )
  ) "" states in
  Printf.sprintf 
  "HOA: v1\nname: \"%s\"\nStates: %d\n%s\nacc-name: Buchi\nAcceptance: %d %s\nAP: %d %s\n--BODY--%s\n--END--"
  (ltl_to_string formula)
  (List.length states)
  starts
  finn
  (if finn=0 then "t" else ("("^fins^")"))
  apsn
  apss
  body
