type ident = string

type typ =
  | Ident of ident
  | Array of typ

type param = {
  param_type : typ;
  param_ident : ident;
}

type unop =
  | Negate
  | Logical_not

type binop =
  | Plus
  | Minus
  | Times
  | Div
  | Mod

type stmt =
  | Let of {
      let_type : typ;
      let_ident : ident;
      let_expr : expr;
    }
  | Bind of {
      bind_type : typ;
      bind_ident : ident;
      bind_expr : expr;
    }
  | Return of expr

and expr =
  | Parallel of stmt list
  | Fun_call of {
      call_name : ident;
      call_args : expr list;
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
  | Variable of string

type fun_defn = {
  fun_ret_type : typ;
  fun_name : ident;
  fun_params : param list;
  fun_body : stmt list;
}

type t = fun_defn list
