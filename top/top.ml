open Core

let verbosity = ref false

let say (msgs : unit -> string list) : unit =
  if !verbosity then List.iter ~f:print_endline (msgs ())

let run_on_ast (ast : Ast.t) : unit =
  Tc.check ast;
  say (fun () -> ["Typechecking succeeded."]);
  let dag = Dag.of_ast ast in
  List.iter dag ~f:(fun dag_fun ->
    let inline = Dag.inline Dag.(dag_fun.dag_graph) dag in
    say (fun () -> [
      "Inlined:";
      Sexp.to_string_hum (Dag.sexp_of_dag inline);
    ]);
    let traversal = Dag_traversal.any_traversal inline in
    say (fun () -> [
      "Traversal:";
      Sexp.to_string_hum (Dag_traversal.sexp_of_traversal traversal);
    ]);
    let ir = Dag_to_ir.run inline traversal in
    say (fun () -> [
      "IR:";
      Sexp.to_string_hum (Ir.sexp_of_t ir);
    ]))

let run_on_file ?(verbose : bool = false) (file : string) : unit =
  verbosity := verbose;
  match Sys.file_exists file with
  | `No | `Unknown -> failwith (Printf.sprintf "File %s does not exist." file)
  | `Yes -> run_on_ast (Parse.parse_file file)
