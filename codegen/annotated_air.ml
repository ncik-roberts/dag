open Core

module Param = struct
  type t =
    | Array of Temp.t * Temp.t list (* Second component is dimensions. First elem of dimensions is length of outermost array. *)
    | Not_array of Temp.t
    [@@deriving sexp, compare]
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
    | Div of t * t
    | Minus of t * t
    [@@deriving sexp]

  let rec to_expr : t -> Expr.t = function
    | Temp t -> Expr.Temp t
    | Mult (t1, t2) -> Expr.Call (Ir.Operator.Binop Ast.Times, [ to_expr t1; to_expr t2; ])
    | Minus (t1, t2) -> Expr.Call (Ir.Operator.Binop Ast.Minus, [ to_expr t1; to_expr t2; ])
    | Div (t1, t2) -> Expr.Call (Ir.Operator.Binop Ast.Div, [ to_expr t1; to_expr t2; ])

  let equals : t -> t -> bool =
    let rec collect_temps (acc : Temp.t list) : t -> Temp.t list =
      function
        | Temp t -> t :: acc
        | Mult (t1, t2) | Div (t1, t2) | Minus (t1, t2) ->
              collect_temps (collect_temps acc t1) t2
    in
    fun t1 t2 ->
      let canonicalize t = List.sort (collect_temps [] t) ~compare:Temp.compare in
      List.equal (canonicalize t1) (canonicalize t2) ~equal:Temp.equal
end

type buffer_info = {
  dim : int; (* First argument to length ranges from [0, dim) *)

  (* Is only allowed to mention variables bound in params. *)
  length : Length_expr.t list; (* The dimensions of the buffer_info. *)

  (* Only present if the buffer_info is derived from a filtered list. *)
  filtered_lengths : Temp.t option list;

  index : (Expr.t, Expr.t) Utils.Many_fn.t;
  (* `b.index [i; j; k;]` is the expression denoting the b[i][j][k] *)
  typ : Tc.typ;
} [@@deriving sexp]

type kernel_info = {
  (* Variables we're closing over. *)
  free_variables : Temp.Set.t;

  (* This is a subset of the keys of buffer_infos. *)
  additional_buffers : Temp.Set.t;
} [@@deriving sexp]

type result = {
  params : Param.t list;

  (* In the case that the return type of the function is an array, we also include
   * an out_param field in the result. This field, when present, indicates the
   * buffer where the return value is to be stored. If the field is present,
   * the following also holds:
   *   - The field is a member of the param list as an "Array" param.
   *   - The field is a key into the buffer_infos map.
   *)
  out_param : Temp.t option;

  (* For a temp used as an array index av, what temps are it backed by? *)
  backing_temps : Temp.Set.t Temp.Map.t;

  (* TODO: May need to eventually know whether a buffer is inside a kernel launch? *)
  buffer_infos : buffer_info Temp.Map.t;

  returned_buffer_infos : buffer_info Temp.Map.t;

  (* You can determine current lvalue during translation. *)

  (* e.g. x <- parallel(y <- ys) { ... } with id I; the key of this map is I.
   * Remember that the air type includes a unique ID for each parallel block.
   * That's the key.
   *)
  kernel_infos : kernel_info Id.Map.t;
} [@@deriving sexp]
