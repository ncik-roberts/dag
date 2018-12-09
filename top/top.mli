(* Top level for dag-to-cuda compiler.
 * string list denotes function names to compile to an external interface.
 *)
val run_on_ast : unit Ast.t -> string option -> Cuda_ir.t
val run_on_file : ?verbose:bool -> string -> out:string option -> string option -> unit
