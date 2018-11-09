let fun_ctx : Tc.fun_type Tc.IdentMap.t =
  let open Tc in
  IdentMap.of_alist_exn
    [ ( "zipWith"
      , { return_type = Array Int
        ; param_types =
            [ Fun
                { return_type = Int
                ; param_types = [ Int; Int; ]
                }
            ; Array Int
            ; Array Int
            ]
        }
      )
    ; ( "reduce"
      , { return_type = Int
        ; param_types =
            [ Fun
                { return_type = Int
                ; param_types = [ Int; Int; ]
                }
            ; Int
            ; Array Int
            ]
        }
      )
    ; ( "transpose"
      , { return_type = Array (Array Int)
        ; param_types = [ Array (Array Int); ]
        }
      )
    ]

let run_on_ast (ast : Ast.t) : unit =
  Tc.check_with Tc.{ empty with fun_ctx } ast;
  print_endline "Typechecking succeeded."

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
