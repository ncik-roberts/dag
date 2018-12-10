(** Run toplevel. *)
open Core

(* Command-line arguments. *)
let command =
  let open Command.Let_syntax in
  let open Command.Param in
  Command.basic
    ~summary:"Compile selected functions from a dag input file to cuda."
    [%map_open
      let filename = anon ("filename" %: string)
      and function_name = flag ~aliases:["f"] "function-name" (optional string)
        ~doc:"The function name to compile (the last one if empty)"
      and out_file = flag "out" (optional string)
        ~doc:"output-file the file to write the output to (by default FILENAME.cu)"
      and verbose = flag "verbose" no_arg
        ~doc:"print extra debug information on exception"
      in
      fun () ->
        try Top.run_on_file ~verbose filename function_name ~out:out_file
        with Failure str as e ->
          if not verbose
            then Printf.fprintf stderr "Error: %s\n" str
            else raise e;
          exit 1;
    ]

let () =
  try Command.run command ~version:"0.1"
  with Failure str ->
    prerr_endline str;
    exit 1
