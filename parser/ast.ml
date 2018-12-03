open Core

type ident = string [@@deriving sexp]

type typ =
  | Ident of ident
  | Array of typ
  [@@deriving sexp]

type 'a param = {
  param_type : 'a * typ;
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
  | Lshift
  | Rshift
  | And
  | Or
  | BitAnd
  | BitOr
  | BitXor
  | Less
  | Greater
  | LessEq
  | GreaterEq
  [@@deriving sexp]

type 'a stmt =
  | Let of {
      let_type : 'a * typ;
      let_ident : ident;
      let_expr : 'a expr;
    }
  | Return of 'a expr
  [@@deriving sexp]

and 'a arg =
  | Expr of 'a expr
  | Bare_binop of 'a * binop
  | Bare_unop of 'a * unop
  [@@deriving sexp]

and call_name =
  | Reduce
  | Map
  | Transpose
  | Zip_with
  | Tabulate
  | Float_of_int
  | Int_of_float
  | Min
  | Max
  | Dim of int
  | Fun_ident of ident
  [@@deriving sexp]

and 'a expr = 'a * 'a expr'
  [@@deriving sexp]

and 'a expr' =
  | Parallel of {
      parallel_arg : 'a expr;
      parallel_type : 'a * typ;
      parallel_ident : ident;
      parallel_body : 'a stmt list;
    }
  | Fun_call of {
      call_name : call_name;
      call_args : 'a arg list;
    }
  | Unop of {
      unary_operator : unop;
      unary_operand : 'a expr;
    }
  | Binop of {
      binary_operand1 : 'a expr;
      binary_operator : binop;
      binary_operand2 : 'a expr;
    }
  | Index of {
     index_source : 'a expr;
     index_expr : 'a expr;
  }
  | Struct_Init of { 
    struct_type : 'a * typ;
    struct_fields : 'a field list;
  }
  | Access of 'a expr * ident
  | Const of int32
  | Bool of bool
  | Float of float
  | Variable of ident
  [@@deriving sexp]

and 'a field = {
  field_name : ident;
  field_expr : 'a expr;
}

type 'a fun_defn = {
  fun_ret_type : 'a * typ;
  fun_name : ident;
  fun_params : 'a param list;
  fun_body : 'a stmt list;
} [@@deriving sexp]

type 'a struct_decl = {
  struct_name : ident;
  struct_fields : 'a param list;
}

type 'a global_stmt = Fun of 'a fun_defn | Struct of 'a struct_decl

type 'a t = 'a global_stmt list

let x = 
  Float.of_string
