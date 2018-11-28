open Core

module Param = struct
  type t =
    | Array of Temp.t * Temp.t list (* Second component is dimensions. First elem of dimensions is length of outermost array. *)
    | Not_array of Temp.t
    [@@deriving sexp]
end

module Expr = struct
  type t =
    | Temp of Temp.t
    | Call of Ir.Operator.t * t list
    | Index of t * t
    | Const of Int32.t
    [@@deriving sexp]
end

module Length_expr = struct
  type t =
    | Temp of Temp.t
    | Mult of t * t
    [@@deriving sexp]

  let rec to_expr : t -> Expr.t = function
    | Temp t -> Expr.Temp t
    | Mult (t1, t2) -> Expr.Call (Ir.Operator.Binop Ast.Times, [ to_expr t1; to_expr t2; ])

  let equals : t -> t -> bool =
    let rec collect_temps (acc : Temp.t list) : t -> Temp.t list =
      function
        | Temp t -> t :: acc
        | Mult (t1, t2) -> collect_temps (collect_temps acc t1) t2
    in
    fun t1 t2 ->
      let canonicalize t = List.sort (collect_temps [] t) ~compare:Temp.compare in
      List.equal (canonicalize t1) (canonicalize t2) ~equal:Temp.equal
end

type buffer_info_sum =

  (* Take for example:
   *
   *   parallel (t <- ts) { ... }
   *
   * The temp mapped to this buffer_info is the temp `t`.
   * The details of the buffer_info is for the array view `ts`.
   *)
  | Bound_parallel

  (* Take for example:
   *
   *  t <- run(ts)
   *
   * The temp mapped to this buffer_info is the temp `t`.
   * The details of the buffer_info is for the array view `ts`.
   *)
  | Run_array_view
  [@@deriving sexp]

type buffer_info = {
  dim : int; (* First argument to length ranges from [0, dim) *)
  (* Int is the dimension we're looking up. *)
  length : int -> Length_expr.t; (* Is only allowed to mention variables bound in params. *)
  index : Expr.t list -> Expr.t; (* `b.index [i; j; k;]` is the expression denoting the b[i][j][k] *)
  typ : Tc.typ;
  variety : buffer_info_sum;
} [@@deriving sexp]

type kernel_info = {
  (* Variables we're closing over. *)
  free_variables : Temp.Set.t;

  (* This is a subset of the keys of buffer_infos. *)
  additional_buffers : Temp.Set.t;
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
