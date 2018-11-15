open Core

let run_on_ast (ast : Ast.t) : unit =
  Tc.check ast;
  print_endline "Typechecking succeeded.";
  let dag = Dag.of_ast ast in
  List.iter dag ~f:(fun dag_fun ->
    print_endline (Sexp.to_string_hum (Dag.sexp_of_dag_fun dag_fun));
    let traversal = Dag_traversal.any_traversal Dag.(dag_fun.dag_graph) in
    print_endline (Sexp.to_string_hum (Dag_traversal.sexp_of_traversal traversal)))

let run_on_file (file : string) : unit =
  match Sys.file_exists file with
  | `No | `Unknown -> failwith (Printf.sprintf "File %s does not exist." file)
  | `Yes -> run_on_ast (Parse.parse_file file)

let run () : unit =
  if Array.length Sys.argv <> 2
    then failwith (Printf.sprintf "Usage: %s <file>" Sys.argv.(0))
    else run_on_file Sys.argv.(1)
