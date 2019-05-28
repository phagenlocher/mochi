open Ltl
open Util

let parse_args () =
  let file = ref "" in
  Arg.parse [
    "-i", String (fun x -> file := x), "specifies the input file";
    "-d", Unit (fun () -> Util.Debug.init true), "activate debug mode"
  ] (fun x -> ()) "";
  !file

let unravel = function Some x -> x | None -> failwith "impossible"

let read_file filename =
  let chan = open_in filename in
  let rec aux lines =
    try
      let line = input_line chan in
      aux (line::lines)
    with
    | End_of_file -> close_in chan; List.rev lines
    | _ -> close_in_noerr chan; []
  in
  String.concat " " (aux [])

let parse_file filename =
  read_file filename |> parse_ltl

let rec main () =
  print_endline ("Input:");
  print_string ("   ");
  let formula = unravel (parse_ltl (read_line ())) in
  print_endline ("Formula:");
  print_endline ("   " ^ (ltl_to_string formula));
  print_endline ("NNF:");
  let nnff = nnf formula in
  print_endline ("   " ^ (ltl_to_string (nnff)));
  print_endline ("");
  let _ = Buchi.ltl_to_generalized_buchi nnff in
  main ()

let _ =
  let _ = parse_args () in
  main ()
