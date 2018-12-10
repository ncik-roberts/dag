(** Topological sort of dag. *)
type traversal_tree =
  | Block of Dag.Vertex.t * traversal
  | Just of Dag.Vertex.t
and traversal = traversal_tree list [@@deriving sexp]

val any_traversal : Dag.dag -> seed:int -> traversal
(*val all_traversals :
  ?n : [ `Take_all_of_'em
       | `Actually_I_don't_want_all_of_them of
          [ `Please_stop_at of int]
       ] -> Dag.dag -> traversal list*)
