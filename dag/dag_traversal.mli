(** Topological sort of dag. *)
type traversal = Dag.Vertex.t list
val any_traversal : Dag.dag -> traversal
val all_traversals : Dag.dag -> traversal list
