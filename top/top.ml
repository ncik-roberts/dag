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
  let timing = ref [] in
  let prev = ref (Time.now ()) in
  let note msg =
    let now = Time.now () in
    let span = Time.diff now !prev in
    prev := now;
    timing := !timing @ [(msg, span)]
  in
  try
    say (fun () -> ["Typechecking succeeded."]);
    let dag = Dag.of_ast ast in
    List.concat_map dag ~f:(fun dag_fun ->
      if mem Dag.(dag_fun.dag_name) then begin
        note "start";
        (*say (fun() -> [
          "Original dag:";
          Sexp.to_string_hum (Dag.sexp_of_dag_fun dag_fun);
        ]);*)
        let (inline, fn_ptrs) = Dag.inline dag_fun dag in
        note "inline";
        (*say (fun () -> [
          "Inlined:";
          Sexp.to_string_hum (Dag.sexp_of_dag_fun inline);
        ]);*)
        let traversal = Dag_traversal.any_traversal inline.Dag.dag_graph ~seed:101
        in
        let fn_ptr_traversals = List.map fn_ptrs
          ~f:(fun f -> Dag_traversal.any_traversal f.Dag.dag_graph ~seed:102)
        in
        note "traversal";
        (*say (fun () -> [
          "Traversal:";
          Sexp.to_string_hum (Dag_traversal.sexp_of_traversal traversal);
        ]);*)
        let airs =
          let (ir, temp_dag) = Dag_to_ir.run inline traversal in
          (*say (fun () -> [
            "IR:";
            Sexp.to_string_hum (Ir.sexp_of_t ir);
          ]);*)
          Ir_to_air.all ir temp_dag ~n:(Some 4)
        in
        let fn_ptr_airs = List.map2_exn fn_ptrs fn_ptr_traversals ~f:(fun inline t ->
          let (ir, temp_dag) = Dag_to_ir.run inline t in
          Ir_to_air.any ir temp_dag)
        in
        note "ir_to_air";
        let cudas = List.filter_mapi airs ~f:(fun i air ->
          say (fun () -> [
            Printf.sprintf "AIR #%d" i;
            Air.Pretty_print.pp_t air;
          ]);
          let ann = Annotate.annotate air in (* Annotations *)
          say (fun () -> [
            (*Sexp.to_string_hum (Annotated_air.sexp_of_result ann);*)
          ]);

          let fn_ptr_with_anns = List.map fn_ptr_airs ~f:(fun x -> (x, Annotate.annotate x)) in

          let cuda = Cuda_trans.trans fn_ptr_with_anns air Tc.(ctx.struct_ctx) ann in
          say (fun () -> [
            Printf.sprintf "Cuda:";
            begin
              match cuda with
              | None -> "None"
              | Some cuda -> Cuda_ir.fmt_gstmts cuda
            end;
          ]);
          cuda
        ) in

        note "annotation + trans";
        cudas
      end else []) |>
    function
      | [] -> failwith "Could not find function to compile."
      | x :: _ ->
          List.iter !timing ~f:(fun (msg, ts) ->
            Printf.printf "%s: %s\n" msg (Time.Span.to_string_hum ts));
          x

  with e ->
    List.iter !timing ~f:(fun (msg, ts) ->
      Printf.printf "%s: %s\n" msg (Time.Span.to_string_hum ts));
    raise e

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
