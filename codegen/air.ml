open Core

(** Abstract IR *)

type array_view =
  | Array of Temp.t
  | Bound_row_of of array_view
  | Zip_with of Ir.Operator.t * array_view list
  | Reverse of array_view
  | Transpose of array_view
  [@@deriving sexp]

type operand =
  | Const of Int32.t
  | Temp of Temp.t
  | Dim of int * array_view (* Nth dimension of an array *)
  [@@deriving sexp]

(* Statement with either parallel or sequential semantics. *)
type 'a stmt = (* Type param stands for either par_stmt or seq_stmt *)
  | For of Temp.t * (Temp.t * array_view) * 'a
  | Run of Temp.t * array_view
  | Block of 'a list
  | Reduce of Temp.t * Ir.Operator.t * operand * array_view
  [@@deriving sexp]

(** Parallel statement *)
type par_stmt =
  (* All temps here are destinations. *)
  | Parallel of Temp.t * (Temp.t * array_view) list * seq_stmt
  | Par_stmt of par_stmt stmt
  | Seq of seq_stmt
  [@@deriving sexp]

and seq_stmt =
  | Seq_stmt of seq_stmt stmt
  | Binop of Temp.t * Ast.binop * operand * operand
  | Unop of Temp.t * Ast.unop * operand
  | Assign of Temp.t * operand
  [@@deriving sexp]

type t = {
  params : Temp.t list;
  body : par_stmt;
} [@@deriving sexp]
