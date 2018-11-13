open Core

(** Directed acyclic graph *)
type t
module Vertex : Comparable.S
module Vertex_view : sig
  type literal =
    | Int32 of int32
    | Bare_binop of Ast.binop

  type t =
    | Parallel_block of Vertex.t
    | Function of Ast.call_name
    | Binop of Ast.binop
    | Unop of Ast.unop
    | Literal of literal
    | Return
end

val return_vertex : t -> Vertex.t
val predecessors : t -> Vertex.t -> Vertex.t list
val successors : t -> Vertex.t -> Vertex.Set.t
val inputs : t -> Vertex.t list

(** Function name + graph. *)
type dag_fun = {
  dag_name : string;
  dag_graph : t;
}

val of_ast : Ast.t -> dag_fun list
