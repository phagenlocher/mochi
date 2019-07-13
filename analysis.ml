open Util
open Buchi
open Wlang

module H = Hashtbl

let print x = print_endline x; flush_all ()

type search_state = {
  bstate : int;
  kstate : kripke_state
}

let intersect_states (b:buchi) (_,_,_,aps) ks bs =
  List.map (
    fun k -> 
      List.map (
        fun b -> {bstate=b;kstate=k}
      ) bs
  ) ks
  |> List.flatten |> List.filter (
    fun {bstate;kstate} ->
      let formulae = List.assoc bstate b.states
      in
      List.for_all (
        fun f -> match f with
        | Var p -> ap_holds p aps kstate
        | UnOp (Not, Var p) -> not (ap_holds p aps kstate)
        | _ -> failwith "intersect states: bstate has non ap!"
      ) formulae
  )

let init_state (b:buchi) p =
  let kstate = init_state p in
  intersect_states b p [kstate] b.start

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
            Debug.print (Printf.sprintf "u %d - t %d" !unum tnum);
            accept_check !u;
            d := list_union c !d;
            while !unum > tnum do
              let (nu,c) = pop () in
              u := nu;
              unum := num !u;
              Debug.print (Printf.sprintf "u %d - t %d" !unum tnum);
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

let start f p = 
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
