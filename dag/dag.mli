open Core

(** Directed acyclic graph *)
type dag [@@deriving sexp]
module Vertex : Utils.Comparable_sexpable
module Vertex_view : sig
  type literal =
    | Int32 of int32
    | Float of float
    | Bare_binop of Ast.binop
    | Bare_unop of Ast.unop
    [@@deriving sexp]

  type t =
    | Parallel_block of Vertex.t * Vertex.t (* (Parallel binding, Return statement) *)
    | Function of Ast.call_name
    | Binop of Ast.binop
    | Unop of Ast.unop
    | Access of Ast.ident
    | Index 
    | Struct_Init of Tc.typ * Ast.ident list
    | Literal of literal
    | Input of Ast.ident
    [@@deriving sexp]
end

module type Daglike = sig
  module Vertex : Utils.Comparable_sexpable
  type dag
  val return_vertex : dag -> Vertex.t
  val predecessors : dag -> Vertex.t -> Vertex.t list
  val successors : dag -> Vertex.t -> Vertex.Set.t
  val inputs : dag -> Vertex.t list
  val view : dag -> Vertex.t -> Vertex_view.t
  val vertices : dag -> Vertex.Set.t

  (** Returned in smallest-to-largest order. *)
  val enclosing_parallel_blocks : dag -> Vertex.t -> Vertex.t list

  (** Returns empty set if the vertex is not a parallel block vertex. *)
  val vertices_in_block : dag -> parallel_block_vertex:Vertex.t -> Vertex.Set.t

  (* Does the vertex contain a nested graph? (E.g. a parallel block vertex
   * contains a nested graph.) If so, return the return vertex of that
   * graph. The name `unroll` is customary for recursive types.
   *)
  val unroll : dag -> Vertex.t -> Vertex.t option

  val type_of : dag -> Vertex.t -> Tc.typ
end

include Daglike with module Vertex := Vertex and type dag := dag

(** Function name + graph. *)
type dag_fun = {
  dag_name : string;
  dag_graph : dag;
} [@@deriving sexp]

type t = dag_fun list [@@deriving sexp]
val of_ast : Tc.typ Ast.fun_defn list -> t

(** Inline all function calls. *)
val inline : dag_fun -> t -> dag_fun
