open Util

module H = Hashtbl

type id_t = int

let new_id =
  let cur = ref (0) in
  let aux () =
    incr cur; !cur
  in aux

type node = {
  id : id_t;
  incoming : id_t list;
  cur : ltl list;
  old : ltl list;
  next : ltl list;
}

type gbuchi = {
  states : int list;
  start : int list;
  final : int list list;
  transitions : (int * int) list;
  formula : ltl;
}

let node_id {id;incoming;cur;old;next} = id

(* Gerth et al. *)
let ltl_to_generalized_buchi formula =
  let node_set_to_string node_set =
    Seq.fold_left (
      fun acc {id;incoming;cur;old;next} ->
        acc ^ (
        Printf.sprintf "Id: %d\nIn: %s\nCur: %s\nOld: %s\nNext: %s\n\n"
        id
        (list_to_string string_of_int incoming)
        (list_to_string ltl_to_string cur)
        (list_to_string ltl_to_string old)
        (list_to_string ltl_to_string next)
        )
    ) "" (H.to_seq_values node_set)
  in
  let node_set_to_buchi node_set =
    (if Debug.in_debug_mode () then Debug.print (node_set_to_string node_set));
    let nodes = H.to_seq_values node_set |> seq_to_list in
    let states = List.map node_id nodes in
    let start = 
      List.filter (
        fun {id;incoming;cur;old;next} -> List.exists ((=) 0) incoming
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
          fun acc x -> if x=0 then acc else (x,id)::acc
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
  let rec expand node node_set = match node.cur with
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
          | UnOp (Next, f) ->
              expand {node with old=list_union node.old [f]; next=list_union node.next [f]} node_set
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
  expand {id=new_id (); incoming=[0]; cur=[formula]; old=[]; next=[]} (H.create 101)
  |> node_set_to_buchi
