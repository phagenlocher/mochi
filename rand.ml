open Util

let rand_choice x = List.nth x (Random.int (List.length x))

let rand_ltl vars depth =
  Random.self_init ();
  let aps = List.init vars (fun i -> "p"^(string_of_int i)) in
  let rec aux n =
    if n <= 0 then 
      Var (rand_choice aps)
    else
      match Random.int 4 with
      | 0 -> Var (rand_choice aps)
      | 1 -> UnOp (rand_choice [Not; Finally; Globally; Next], aux (n-1))
      | _ -> 
          BinOp (
            rand_choice [And;Or;Until;Wuntil;Release;Srelease],
            aux (n-1),
            aux (n-1)
          )
    in
    aux depth
