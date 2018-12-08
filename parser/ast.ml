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
  | Filter_with
  | Scan
  | Tabulate
  | Float_of_int
  | Int_of_float
  | Min
  | Max
  | Log2
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
    struct_name : ident;
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

(* plz derive this for me *)
let rec map_t xs ~f = List.map xs ~f:(map_global_stmt ~f)
and map_global_stmt ~f = function
  | Fun fn -> Fun (map_fun_defn fn ~f)
  | Struct s -> Struct (map_struct_decl s ~f)
and map_struct_decl sd ~f = { sd with struct_fields = List.map ~f:(map_param ~f) sd.struct_fields }
and map_param p ~f = { p with param_type = map_type ~f p.param_type }
and map_type ~f = Tuple2.map_fst ~f
and map_fun_defn fd ~f = { fd with
  fun_ret_type = map_type ~f fd.fun_ret_type;
  fun_params = List.map ~f:(map_param ~f) fd.fun_params;
  fun_body = List.map ~f:(map_stmt ~f) fd.fun_body;
}
and map_stmt ~f = function
  | Let l -> Let { l with
      let_type = map_type ~f l.let_type;
      let_expr = map_expr ~f l.let_expr;
    }
  | Return expr -> Return (map_expr ~f expr)
and map_arg ~f = function
  | Expr e -> Expr (map_expr ~f e)
  | Bare_binop (x, b) -> Bare_binop (f x, b)
  | Bare_unop (x, u) -> Bare_unop (f x, u)
and map_field ~f s = { s with field_expr = map_expr ~f s.field_expr }
and map_expr ~f (x, e) = (f x, map_expr' ~f e)
and map_expr' ~f = function
  | Parallel p -> Parallel { p with
      parallel_arg = map_expr ~f p.parallel_arg;
      parallel_type = map_type ~f p.parallel_type;
      parallel_body = List.map ~f:(map_stmt ~f) p.parallel_body;
    }
  | Fun_call fc -> Fun_call { fc with call_args = List.map ~f:(map_arg ~f) fc.call_args }
  | Unop u -> Unop { u with unary_operand = map_expr ~f u.unary_operand }
  | Binop b -> Binop { b with binary_operand1 = map_expr ~f b.binary_operand1;
                              binary_operand2 = map_expr ~f b.binary_operand2; }
  | Index i -> Index {
      index_source = map_expr ~f i.index_source;
      index_expr = map_expr ~f i.index_expr;
    }
  | Struct_Init si -> Struct_Init { si with struct_fields = List.map ~f:(map_field ~f) si.struct_fields; }
  | Access (e, i) -> Access (map_expr ~f e, i)
  | Const i -> Const i
  | Bool b -> Bool b
  | Float f -> Float f
  | Variable i -> Variable i
