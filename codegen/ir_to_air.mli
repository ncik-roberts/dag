(* Convert IR (with nested parallelism) to AIR (with only one level
 * of parallelism).
 *)

val any : Ir.t -> Temp_dag.dag -> Air.t
val all : Ir.t -> ?n : int option -> Temp_dag.dag -> Air.t list
