open Core

type ident = string [@@deriving sexp]

type typ =
  | Ident of ident
  | Array of typ
  [@@deriving sexp]

type param = {
  param_type : typ;
  param_ident : ident;
} [@@deriving sexp]

type unop =
  | Negate
  | Logical_not
  [@@deriving sexp]

type binop =
  | Plus
  | Minus
  | Times
  | Div
  | Mod
  [@@deriving sexp]

type stmt =
  | Let of {
      let_type : typ;
      let_ident : ident;
      let_expr : expr;
    }
  | Return of expr
  [@@deriving sexp]

and arg =
  | Expr of expr
  | Bare_binop of binop
  [@@deriving sexp]

and call_name =
  | Reduce
  | Map
  | Transpose
  | Zip_with
  | Fun_ident of ident
  [@@deriving sexp]

and expr =
  | Parallel of {
      parallel_arg : expr;
      parallel_type : typ;
      parallel_ident : ident;
      parallel_body : stmt list;
    }
  | Fun_call of {
      call_name : call_name;
      call_args : arg list;
    }
  | Unop of {
      unary_operator : unop;
      unary_operand : expr;
    }
  | Binop of {
      binary_operand1 : expr;
      binary_operator : binop;
      binary_operand2 : expr;
    }
  | Const of int32
  | Variable of ident
  [@@deriving sexp]

type fun_defn = {
  fun_ret_type : typ;
  fun_name : ident;
  fun_params : param list;
  fun_body : stmt list;
} [@@deriving sexp]

type t = fun_defn list
