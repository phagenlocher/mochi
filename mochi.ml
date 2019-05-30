open Ltl
open Util

let interactive_mode = ref false
let ltl_mode = ref false
let formula = ref ""

let parse_args () =
  Arg.parse [
    "-i", Unit (fun () -> interactive_mode := true), "activate interactive mode";
    "-f", String (fun x -> formula := x), "specify the LTL formula";
    "-d", Unit (fun () -> Util.Debug.init true), "activate debug output";
    "-l", Unit (fun () -> ltl_mode := true), "activate LTL mode (no Büchi translation)";
    "-r", Int (fun i -> 
      print_endline (ltl_to_string (Rand.rand_ltl 10 i)); exit 0
    ), "print a random ltl formula with maximal depth of the given argument"
  ] (fun x -> ()) ""

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

let rec interactive_loop () =
  print_endline ("Input:");
  print_string ("   ");
  let formula = unravel (parse_ltl (read_line ())) in
  print_endline ("Formula:");
  print_endline ("   " ^ (ltl_to_string formula));
  print_endline ("NNF:");
  let nnff = nnf formula in
  print_endline ("   " ^ (ltl_to_string (nnff)));
  print_endline ("");
  let b = Buchi.ltl_to_generalized_buchi nnff in
  print_endline (Buchi.hoa_of_generalized_buchi b);
  interactive_loop ()

let main () =
  if !formula = "" then
    print_endline "No formula specified! Please do so with -f!"
  else if !ltl_mode then
    print_endline (ltl_to_string (nnf (unravel (parse_ltl !formula))))
  else
    let b = Buchi.ltl_to_generalized_buchi (nnf (unravel (parse_ltl !formula)))
    in print_endline (Buchi.hoa_of_generalized_buchi b)

let _ =
  parse_args ();
  begin
    if !interactive_mode then
      interactive_loop ()
    else
      main ()
  end
