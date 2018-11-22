(* Top level for dag-to-cuda compiler.
 * `run` should be run directly as a program with the DAG program
 * to compile as the argument.
 *)
val run_on_ast : Ast.t -> unit
val run_on_file : ?verbose:bool -> string -> unit
