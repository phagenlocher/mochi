open Util
open Buchi
open Wlang

module H = Hashtbl

let fair = ref false

let print x = print_endline x; flush_all ()

type search_state = {
  bstate : int;
  kstate : kripke_state;
  last_procn : int;
}

let pap_rgx = Str.regexp "_process\(\([1-9]\)+\)"

let is_process_ap x = 
  Str.string_match pap_rgx x 0

let matched_procn x = 
  Str.matched_group 2 x |> int_of_string

let intersect_states (b:buchi) (_,_,_,aps) ks bs =
  List.map (
    fun (p,k) -> 
      List.map (
          fun b -> {bstate=b;kstate=k;last_procn=p}
      ) bs
  ) ks
  |> List.flatten |> List.filter (
    fun {bstate;kstate;last_procn} -> 
      let handle_ap p =
          if is_process_ap p then
            last_procn = (matched_procn p)
          else
            ap_holds p aps kstate
      in
      let formulae = List.assoc bstate b.states
      in
      List.for_all (
        fun f -> match f with
        | Var p -> handle_ap p
        | UnOp (Not, Var p) -> not (handle_ap p)
        | _ -> failwith "intersect states: bstate has non ap!"
      ) formulae
  )

let init_state (b:buchi) p =
  let kstate = init_state p in
  intersect_states b p [-1, kstate] b.start

let next_states (b:buchi) p s =
  let ks = Wlang.next_states s.kstate
  in
  let bs = List.filter (fun (x,_) -> s.bstate=x ) b.transitions |> List.map snd
  in
  intersect_states b p ks bs

let dfs (b:buchi) p =
  let accept_check x =
    let res = List.mem x.bstate b.final || List.length b.final = 0
    in
    if res then 
    (
        print "Counterexample found!"; 
        exit 0
    )
  in
  let map = H.create 12345
  in
  let nr = ref 0 
  in
  let w = ref [] in let d = ref [] 
  in
  let set s b n = H.replace map s (b,n)
  in
  let num x = match H.find map x with (_,n) -> n 
  in
  let push e = w := e::!w
  in
  let istop e = match !w with
    | [] -> false
    | (x,_)::_ -> x=e
  in
  let pop () = match !w with
    | [] -> failwith "w empty"
    | x::xs -> w := xs; x
  in
  let new_id () = incr nr; !nr
  in
  let clear () =
    nr := 0;
    w := [];
    d := [];
    H.clear map
  in
  let rec aux s =
    set s true (new_id ());
    push (s,[s]);
    List.iter (
      fun t ->
        match H.find_opt map t with
        | None -> aux t
        | Some (true,tnum) -> (
            d := [];
            let (u,c) = pop () in
            let u = ref u in
            let unum = ref (num !u) in
            (* Debug.print (Printf.sprintf "u %d - t %d" !unum tnum); *)
            accept_check !u;
            d := list_union c !d;
            while !unum > tnum do
              let (nu,c) = pop () in
              u := nu;
              unum := num !u;
              (* Debug.print (Printf.sprintf "u %d - t %d" !unum tnum); *)
              accept_check !u;
              d := list_union c !d;             
            done;
            push (!u,!d)
        )
        | _ -> ()
    ) (next_states b p s);
    if istop s then (
      let (s,c) = pop () in
      List.iter (fun x -> set x false (num x)) c
    )
  in
  List.iter (fun s -> aux s; clear ()) (init_state b p)

let make_fair_ltl f (_,_,procl,_) =
  let make_proc_ap pid = 
    Var (Printf.sprintf "_process%d" pid)
  in
  let gf x = 
    UnOp (Globally, UnOp (Finally, x))
  in
  let rec and_chain = function
    | [] -> failwith "and_chain got []"
    | [x] -> x
    | x::xs -> BinOp (And, x, and_chain xs)
  in
  let inv = List.map (fun (pid,_) -> gf (make_proc_ap pid)) procl |> and_chain
  in
  BinOp (Imp, inv, f)

let start f p = 
  let f = (
    if !fair then
      begin
        print "Generating fairness constraint...";
        make_fair_ltl f p
      end
    else 
      f
  )
  in
  print "Generating NNF of negated formula...";
  let nf = Ltl.nnf (Ltl.negate_formula f) in
  Debug.print (ltl_to_string nf);
  print "Building GBA from formula...";
  let b = Buchi.ltl_to_generalized_buchi nf in
  print "Degeneralizing GBA...";
  let b = Buchi.degeneralize b in
  Debug.print (Buchi.hoa_of_buchi b);
  print "Checking for emptiness... please wait :)";
  dfs b p;
  print (ltl_to_string f^" holds!")
