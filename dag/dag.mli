open Core

(** Directed acyclic graph *)
type dag [@@deriving sexp]
module Vertex : Comparable.S
module Vertex_view : sig
  type literal =
    | Int32 of int32
    | Bare_binop of Ast.binop
    [@@deriving sexp]

  type t =
    | Parallel_block of Vertex.t
    | Function of Ast.call_name
    | Binop of Ast.binop
    | Unop of Ast.unop
    | Literal of literal
    | Return
    [@@deriving sexp]
end

val return_vertex : dag -> Vertex.t
val predecessors : dag -> Vertex.t -> Vertex.t list
val successors : dag -> Vertex.t -> Vertex.Set.t
val inputs : dag -> Vertex.t list

(** Function name + graph. *)
type dag_fun = {
  dag_name : string;
  dag_graph : dag;
} [@@deriving sexp]

type t = dag_fun list [@@deriving sexp]
val of_ast : Ast.t -> t
