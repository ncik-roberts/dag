type ident = string [@@deriving sexp]
module IdentMap = Core.String.Map

type typ =
  | Int
  | Bool
  | Float
  | Struct of ident
  | Array of typ
  | Pointer of typ
  | Fun of fun_type
  [@@deriving sexp]

and fun_type = {
  allowed_types : typ list;
  return_type : typ option;
  param_types : typ option list;
}

type struct_field_type = {
  field_name : ident;
  field_type : typ;
}

type struct_type = struct_field_type list

type tctxt = {
  local_var_ctx : typ IdentMap.t;
  fun_ctx : fun_type IdentMap.t;
  struct_ctx : struct_type IdentMap.t;
  return_type : typ option;
}

val empty : tctxt

val check : unit Ast.t -> typ Ast.fun_defn list
val check_with : tctxt -> unit Ast.t -> typ Ast.fun_defn list
