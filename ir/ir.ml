open Core

module Operator = struct
  type t =
    | Binop of Ast.binop
    | Unop of Ast.unop
    [@@deriving sexp]
end

type fun_call =
  | Scan of Operator.t
  | Reduce of Operator.t
  | Map of Operator.t
  | Zip_with of Operator.t
  | Tabulate
  | Filter_with
  | Float_of_int
  | Int_of_float
  | Min
  | Max
  | Log2
  | Transpose
  | Dim of int
  [@@deriving sexp]

type operand =
  | Const of Int32.t
  | Float of float
  | Bool of bool
  | Temp of Temp.t (* This temp is the only source temp *)
  [@@deriving sexp]

type dest =
  | Return of Temp.t
  | Dest of Temp.t
  [@@deriving sexp]

let type_of_dest : dest -> Tc.typ = function
  | Return t -> Temp.to_type t
  | Dest t -> Temp.to_type t

let temp_of_dest : dest -> Temp.t = function
  | Return t -> t
  | Dest t -> t

(** All sorts of statements *)
type stmt =
  | Parallel of dest * operand * Temp.t * stmt list
  | Binop of dest * Ast.binop * operand * operand
  | Index of dest * operand * operand
  | Access of dest * operand * Ast.ident
  | Unop of dest * Ast.unop * operand
  | Struct_Init of Tc.typ * dest * (Ast.ident * operand) list
  | Fun_call of dest * fun_call * operand list
  | Assign of dest * operand
  | Nop
  [@@deriving sexp]

type t = {
  params : Temp.t list;
  body : stmt list;
  return_type : Tc.typ;
  fn_name : string;
} [@@deriving sexp]
