open Core

type ident = string
module IdentMap = String.Map

type typ =
  | Int
  | Struct of ident
  | Array of typ
  | Pointer of typ
  | Fun of fun_type

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

let empty = {
  local_var_ctx = IdentMap.empty;
  fun_ctx = IdentMap.empty;
  struct_ctx = IdentMap.empty;
  return_type = None;
}

type ('input, 'output) failfmt =
  ('input -> unit -> 'output, unit, string, unit -> 'output) format4

let add_with_failure
  (m : 'a IdentMap.t)
  ~(key : IdentMap.Key.t)
  ~(data : 'a)
  ~(on_duplicate : (string, 'a IdentMap.t) failfmt) : 'a IdentMap.t =
  match IdentMap.add m ~key ~data with
  | `Ok result -> result
  | `Duplicate -> failwithf on_duplicate key ()

let infer_binop (binop : Ast.binop) : fun_type = {
  param_types = [ Int; Int; ];
  return_type = Int;
}

let infer_unop (unop : Ast.unop) : fun_type = {
  param_types = [ Int ];
  return_type = Int;
}

let check_binop (typ1 : typ) (typ2 : typ) (binop : Ast.binop) : typ =
  let fun_type = infer_binop binop in
  if fun_type.param_types <> [ typ1; typ2; ]
  then failwith "Invalid binop types."
  else fun_type.return_type

let check_unop (typ : typ) (unop : Ast.unop) : typ =
  let fun_type = infer_unop unop in
  if fun_type.param_types <> [ typ ]
  then failwith "Invalid unop types."
  else fun_type.return_type

let check_fun (ctx : t) (fun_name : Ast.call_name) (arg_types : typ list) : typ =
  match fun_name with
  | Ast.Map ->
      begin
        match arg_types with
        | [ Fun fun_type; Array typ; ] when [ typ ] = fun_type.param_types ->
            Array (fun_type.return_type)
        | _ -> failwith "Invalid argument to map."
      end
  | Ast.Reduce ->
      begin
        match arg_types with
        | [ Fun fun_type; typ1; Array typ2; ] when
            typ1 = typ2
              && [ typ1; typ2; ] = fun_type.param_types
              && typ1 = fun_type.return_type -> typ1
        | _ -> failwith "Invalid argument to reduce."
      end
  | Ast.Zip_with ->
      begin
        match arg_types with
        | [ Fun fun_type; Array typ1; Array typ2; ]
            when fun_type.param_types = [ typ1; typ2; ] ->
              Array (fun_type.return_type)
        | _ -> failwith "Invalid argument to zip_with."
      end
  | Ast.Transpose ->
      begin
        match arg_types with
        | [ Array (Array typ); ] -> Array (Array typ)
        | _ -> failwith "Invalid argument to transpose."
      end
  | Ast.Fun_ident fun_name ->
      begin
        match IdentMap.find ctx.fun_ctx fun_name with
        | Some fun_type ->
            if List.equal ~equal:(=) arg_types fun_type.param_types
            then fun_type.return_type
            else failwith "Invalid argument types."
        | None -> failwithf "Unknown function `%s`" fun_name ()
      end

let rec check_type (ctx : t) (ast : Ast.typ) : typ = match ast with
  | Ast.Ident "int" -> Int
  | Ast.Ident ident -> failwithf "Unknown type `%s`" ident ()
  | Ast.Array ast' -> Array (check_type ctx ast')

let rec check_expr (ctx : t) (ast : Ast.expr) : typ = match ast with
  | Ast.Const _ -> Int
  | Ast.Variable ident ->
      Option.value_exn (IdentMap.find ctx.local_var_ctx ident)
        ~error:(Error.of_exn (Failure (Printf.sprintf "Unbound variable `%s`." ident)))
  | Ast.Binop binop ->
      let type1 = check_expr ctx binop.binary_operand1 in
      let type2 = check_expr ctx binop.binary_operand2 in
      check_binop type1 type2 binop.binary_operator
  | Ast.Unop unop ->
      let typ = check_expr ctx unop.unary_operand in
      check_unop typ unop.unary_operator
  | Ast.Parallel parallel ->
      begin
        let typ = check_expr ctx parallel.parallel_arg in
        let given_typ = check_type ctx parallel.parallel_type in
        begin
          match typ, given_typ with
          | Array t1, t2 when t1 = t2 -> ()
          | _ -> failwith "Incompatible types in parallel binding."
        end;
        let ctx' = { ctx with local_var_ctx =
          add_with_failure ctx.local_var_ctx ~key:parallel.parallel_ident ~data:given_typ
            ~on_duplicate:"Duplicate parallel binding defn `%s`.";
          return_type = None;
        } in
        match infer_stmts ctx' parallel.parallel_body with
        | { return_type = None; _; } -> failwith "Parallel stmts no return."
        | { return_type = Some typ; _; } -> Array typ
      end
  | Ast.Fun_call fun_call ->
      let arg_types = List.map fun_call.call_args ~f:(function
        | Ast.Bare_binop binop -> Fun (infer_binop binop)
        | Ast.Expr expr -> check_expr ctx expr)
      in
      check_fun ctx fun_call.call_name arg_types

and check_stmt (ctx : t) (ast : Ast.stmt) : t =
  if Option.is_some ctx.return_type then failwith "Function already returned.";
  match ast with
  | Ast.Let let_exp ->
      let typ = check_type ctx let_exp.let_type in
      let body_type = check_expr ctx let_exp.let_expr in
      begin
        if body_type <> typ
          then failwithf "Wrong type annotation on `%s`" let_exp.let_ident ()
      end;
      { ctx with
          local_var_ctx =
            add_with_failure ctx.local_var_ctx
              ~key:let_exp.let_ident ~data:typ
              ~on_duplicate:"Duplicate local variable definition `%s`"; }
  | Ast.Return exp -> { ctx with return_type = Some (check_expr ctx exp) }

and infer_stmts (ctx : t) (ast : Ast.stmt list) : t =
  List.fold_left ~init:ctx ~f:check_stmt ast

and check_stmts (ctx : t) (ast : Ast.stmt list) (typ : typ) : unit =
  let ctx' = infer_stmts ctx ast in
  (* Check return type *)
  match ctx'.return_type with
  | None -> failwith "Statements do not return"
  | Some inferred_type ->
      if typ <> inferred_type
      then failwith "Given return type incompatible with inferred type"

let check_fun_defn (ctx : t) (ast : Ast.fun_defn) : t =
  let open Ast in
  let return_type = check_type ctx ast.fun_ret_type in
  let param_types = List.map ~f:(fun p -> check_type ctx p.param_type) ast.fun_params in
  let body_ctx =
    let params_with_type =
      List.zip_exn (List.map ~f:(fun p -> p.param_ident) ast.fun_params) param_types
    in
    { ctx with
        local_var_ctx =
          List.fold_left params_with_type ~init:ctx.local_var_ctx ~f:(fun map (key, data) ->
            add_with_failure map ~key ~data ~on_duplicate:"Duplicate parameter `%s`") }
  in
  check_stmts body_ctx ast.fun_body return_type;
  (* Update fun ctx. *)
  { ctx with
      fun_ctx =
        add_with_failure ctx.fun_ctx
          ~key:ast.fun_name ~data:{ return_type; param_types; }
          ~on_duplicate: "Duplicate function definition `%s`"; }

let check_with (ctx : t) (ast : Ast.t) : unit =
  ignore (List.fold_left ~f:check_fun_defn ~init:ctx ast : t)

let check (ast : Ast.t) : unit = check_with empty ast
