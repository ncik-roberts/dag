type ident = string [@@deriving sexp]
module IdentMap = Core.String.Map

type typ =
  | Int
  | Float
  | Struct of ident
  | Array of typ
  | Pointer of typ
  | Fun of fun_type
  [@@deriving sexp]

and fun_type = {
  return_type : typ;
  param_types : typ list;
}

type struct_field_type = {
  field_name : ident;
  field_type : typ;
}

type struct_type = struct_field_type list

type t = {
  local_var_ctx : typ IdentMap.t;
  fun_ctx : fun_type IdentMap.t;
  struct_ctx : struct_type IdentMap.t;
  return_type : typ option;
}

val empty : t

val check : unit Ast.t -> typ Ast.t
val check_with : t -> unit Ast.t -> typ Ast.t
