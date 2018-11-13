open Core

(** Directed acyclic graph *)
type dag [@@deriving sexp]
module Vertex : Utils.Comparable_sexpable
module Vertex_view : sig
  type literal =
    | Int32 of int32
    | Bare_binop of Ast.binop
    [@@deriving sexp]

  type t =
    | Parallel_block of Vertex.t (* Return statement *)
    | Function of Ast.call_name
    | Binop of Ast.binop
    | Unop of Ast.unop
    | Literal of literal
    | Parallel_binding of Ast.ident
    | Input of Ast.ident
    | Return
    [@@deriving sexp]
end

val return_vertex : dag -> Vertex.t
val predecessors : dag -> Vertex.t -> Vertex.t list
val successors : dag -> Vertex.t -> Vertex.Set.t
val inputs : dag -> Vertex.t list

(* Does the vertex contain a nested graph? (E.g. a parallel block vertex
 * contains a nested graph.) If so, return the return vertex of that
 * graph. The name `unroll` is customary for recursive types.
 *)
val unroll : dag -> Vertex.t -> Vertex.t option

(** Function name + graph. *)
type dag_fun = {
  dag_name : string;
  dag_graph : dag;
} [@@deriving sexp]

type t = dag_fun list [@@deriving sexp]
val of_ast : Ast.t -> t

(** TODO: inline; i.e. write a function inline : dag -> t -> dag. *)
