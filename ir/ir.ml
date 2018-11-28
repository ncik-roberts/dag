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

type dest =
  | Return of Temp.t
  | Dest of Temp.t
  [@@deriving sexp]

let type_of_dest : dest -> Tc.typ = function
  | Return t -> Temp.to_type t
  | Dest t -> Temp.to_type t

(** All sorts of statements *)
type stmt =
  | Parallel of dest * operand * Temp.t * stmt list
  | Binop of dest * Ast.binop * operand * operand
  | Unop of dest * Ast.unop * operand
  | Fun_call of dest * fun_call * operand list
  | Assign of dest * operand
  | Nop
  [@@deriving sexp]

type t = {
  params : Temp.t list;
  body : stmt list;
} [@@deriving sexp]
