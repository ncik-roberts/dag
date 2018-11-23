open Core

module Operator = struct
  type t =
    | Binop of Ast.binop
    | Unop of Ast.unop
    [@@deriving sexp]
end

type fun_call =
  | Reduce of Operator.t
  | Map of Operator.t
  | Zip_with of Operator.t
  | Transpose
  | Dim of int
  [@@deriving sexp]

type operand =
  | Const of Int32.t
  | Temp of Temp.t (* This temp is the only source temp *)
  [@@deriving sexp]

(** All sorts of statements *)
type stmt =
  (* All temps here are destinations. *)
  | Parallel of Temp.t * operand * stmt list
  | Binop of Temp.t * Ast.binop * operand * operand
  | Unop of Temp.t * Ast.unop * operand
  | Fun_call of Temp.t * fun_call * operand list
  | Assign of Temp.t * operand
  | Nop
  | Return of operand
  [@@deriving sexp]

type t = {
  params : Temp.t list;
  body : stmt list;
} [@@deriving sexp]
