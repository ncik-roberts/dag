let run_on_file (file : string) : unit =
  if not (Sys.file_exists file)
    then raise (Failure (Printf.sprintf "File %s does not exist." file))
    else begin
      Parse.parse_file file |> ignore;
      print_endline "Parse successful.";
    end

let run () : unit =
  if Array.length Sys.argv != 2
    then raise (Failure (Printf.sprintf "Usage: %s <file>" Sys.argv.(0)))
    else run_on_file Sys.argv.(1)

let () =
  try run ()
  with Failure str ->
    prerr_endline str;
    exit 1
