open Core

(** Abstract IR *)

type array_view =
  | Array of Temp.t
  | Zip_with of Ir.Operator.t * array_view list
  | Reverse of array_view
  | Transpose of array_view

type operand =
  | Const of Int32.t
  | Temp of Temp.t
  | Dim of int * Temp.t (* Nth dimension of an array *)

(** Parallel statement *)
type par_stmt =
  (* All temps here are destinations. *)
  | Par_for of Temp.t * (Temp.t * array_view) list * seq_stmt list
  | Seq_for of Temp.t * (Temp.t * array_view) * par_stmt list
  | Run of Temp.t * array_view
  | Seq of seq_stmt

and seq_stmt =
  | Binop of Temp.t * Ast.binop * operand * operand
  | Unop of Temp.t * Ast.unop * operand
  | Assign of Temp.t * operand

type t = {
  params : Temp.t list;
  body : par_stmt list;
}
