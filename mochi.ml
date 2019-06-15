open Ltl
open Util

let ltl_mode = ref false
let buchi_mode = ref false
let formula = ref ""
let filename = ref ""

let parse_args () =
  Arg.parse [
    "-f", String (fun x -> formula := x), "specify the LTL formula";
    "-p", String (fun x -> filename := x), "specify i7w-file";
    "-d", Unit (fun () -> Util.Debug.init true), "activate debug output";
    "-l", Unit (fun () -> ltl_mode := true), "activate LTL mode (no Büchi translation)";
    "-b", Unit (fun () -> buchi_mode := true), "activate Büchi mode (output HOA)";
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
  String.concat "\n" (aux [])

let parse_file filename =
  let fc = read_file filename 
  in Debug.print ("Read file:\n"^fc);
  Wlang.parse_wlang fc |> unravel |> Wlang.label_statements

let warn_duplicate_labels wl = match Wlang.check_labels wl with
  | None -> ()
  | Some l -> Debug.error ("Duplicate label found: "^l)

let main () =
  let wl = parse_file !filename
  in
  warn_duplicate_labels wl;
  print_endline (wlang_to_string wl)

let pre_main () =
  if !formula = "" then
    print_endline "No formula specified! Please do so with -f!"
  else if !ltl_mode then
    print_endline (ltl_to_string (nnf (unravel (parse_ltl !formula))))
  else if !buchi_mode then
    let b = Buchi.ltl_to_generalized_buchi (nnf (unravel (parse_ltl !formula)))
    in print_endline (Buchi.hoa_of_generalized_buchi b)
  else 
    main ()

let _ =
  parse_args ();
  pre_main ()
