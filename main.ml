open Util

let parse_args () =
  let file = ref "" in
  Arg.parse ["-i", String (fun x -> file := x), "specifies the input file"] (fun x -> ()) "";
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
  read_file filename |> Ltl.parse_ltl

let rec main () = 
  print_endline ("Input:");
  print_string ("   ");
  let formula = unravel (Ltl.parse_ltl (read_line ())) in
  print_endline ("Formula:");
  print_endline ("   " ^ (ltl_to_string formula));
  print_endline ("NNF:");
  print_endline ("   " ^ (ltl_to_string (Ltl.nnf formula)));
  print_endline ("");
  main ()

let _ =
  main ()
