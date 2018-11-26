(* Top level for dag-to-cuda compiler.
 * string list denotes function names to compile to an external interface.
 *)
val run_on_ast : Ast.t -> string list -> unit
val run_on_file : ?verbose:bool -> string -> string list -> unit
