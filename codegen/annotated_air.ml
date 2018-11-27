open Core

module Param = struct
  type t =
    | Array of Temp.t * Temp.t list (* Second component is dimensions. First elem of dimensions is length of outermost array. *)
    | Not_array of Temp.t
    [@@deriving sexp]
end

type expr =
  | Expr_temp of Temp.t
  | Expr_mult of expr * expr
  [@@deriving sexp]

type buffer_info = {
  length : expr; (* Is only allowed to mention variables bound in params. *)
  index : expr -> expr; (* `b.index i` is the expression denoting the ith element of b *)
  typ : Ast.typ;
} [@@deriving sexp]

type kernel_info = {
  (* Variables we're closing over. *)
  free_variables : Temp.Set.t;

  (* This is a submap of buffer_infos. *)
  additional_buffers : buffer_info Temp.Map.t;
} [@@deriving sexp]

type result = {
  params : Param.t list;

  (* TODO: May need to eventually know whether a buffer is inside a kernel launch? *)
  buffer_infos : buffer_info Temp.Map.t;

  (* You can determine current lvalue during translation. *)

  (* e.g. x <- parallel(y <- ys) { ... } with id I; the key of this map is I.
   * Remember that the air type includes a unique ID for each parallel block.
   * That's the key.
   *)
  kernel_infos : kernel_info Id.Map.t;
} [@@deriving sexp]
