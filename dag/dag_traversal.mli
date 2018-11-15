(** Topological sort of dag. *)
type traversal_tree =
  | Block of Dag.Vertex.t * traversal
  | Just of Dag.Vertex.t
and traversal = traversal_tree list [@@deriving sexp]

val any_traversal : Dag.dag -> traversal
val all_traversals : Dag.dag -> traversal list
