open Core

let verbosity = ref false

let say (msgs : unit -> string list) : unit =
  if !verbosity then List.iter ~f:print_endline (msgs ())

let run_on_ast (ast : unit Ast.t) (to_compile : string option) : Cuda_ir.t =
  let (ctx, ast) = Tc.check ast in
  let mem =
    let last_fn = List.last_exn ast in
    Option.value_map to_compile
      ~default:((=) Ast.(last_fn.fun_name))
      ~f:((=))
  in
  say (fun () -> ["Typechecking succeeded."]);
  let dag = Dag.of_ast ast in
  List.concat_map dag ~f:(fun dag_fun ->
    if mem Dag.(dag_fun.dag_name) then begin
      say (fun() -> [
        "Original dag:";
        Sexp.to_string_hum (Dag.sexp_of_dag_fun dag_fun);
      ]);
      let inline = Dag.inline dag_fun dag in
      say (fun () -> [
        "Inlined:";
        Sexp.to_string_hum (Dag.sexp_of_dag_fun inline);
      ]);
      let traversals = Dag_traversal.all_traversals inline.Dag.dag_graph in
      List.iter traversals ~f:(fun traversal -> say (fun () -> [
        "Traversal:";
        Sexp.to_string_hum (Dag_traversal.sexp_of_traversal traversal);
      ]));
      let airs = List.concat_map traversals ~f:(fun traversal ->
        let (ir, temp_dag) = Dag_to_ir.run inline traversal in
        say (fun () -> [
          "IR:";
          Sexp.to_string_hum (Ir.sexp_of_t ir);
        ]);
        Ir_to_air.all ir temp_dag)
      in
      let cudas = List.mapi airs ~f:(fun i air ->
        say (fun () -> [
          Printf.sprintf "AIR #%d" i;
          Air.Pretty_print.pp_t air;
        ]);
        let ann = Annotate.annotate air in (* Annotations *)
        say (fun () -> [
          (*Sexp.to_string_hum (Annotated_air.sexp_of_result ann);*)
        ]);

        let cuda = Cuda_trans.trans air Tc.(ctx.struct_ctx) ann in
        say (fun () -> [
          Printf.sprintf "Cuda:";
          Cuda_ir.fmt_gstmts cuda;
        ]);
        cuda
      ) in cudas
    end else []) |> List.hd_exn

let run_on_file
 ?(verbose : bool = false)
  (file : string)
 ~(out : string option) : string option -> unit =
  verbosity := verbose;
  match Sys.file_exists file with
  | `No | `Unknown -> failwith (Printf.sprintf "File %s does not exist." file)
  | `Yes ->
      fun to_compile ->
        let ir = run_on_ast (Parse.parse_file file) to_compile in
        let out =
          Option.value out
            ~default:(let (f, _) = Filename.split_extension file in f ^ ".cu")
        in
        Out_channel.write_all out ~data:(Cuda_ir.fmt_gstmts ir)
