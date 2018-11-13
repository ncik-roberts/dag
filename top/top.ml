let run_on_ast (ast : Ast.t) : unit =
  Tc.check ast;
  print_endline "Typechecking succeeded.";
  ignore (Dag.of_ast ast);
  print_endline "Creation of dag succeeded."

let run_on_file (file : string) : unit =
  if not (Sys.file_exists file)
    then failwith (Printf.sprintf "File %s does not exist." file)
    else run_on_ast (Parse.parse_file file)

let run () : unit =
  if Array.length Sys.argv != 2
    then failwith (Printf.sprintf "Usage: %s <file>" Sys.argv.(0))
    else run_on_file Sys.argv.(1)

let () =
  try run ()
  with Failure str ->
    prerr_endline str;
    exit 1
