open Core

type ident = string [@@deriving sexp]
module IdentMap = String.Map

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

type tctxt = {
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
  ~(on_duplicate : (string, 'a IdentMap.t) failfmt) =
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

let check_index (typ1 : typ) (typ2 : typ) : typ = 
  match typ1,typ2 with 
  | Array t,Int -> t
  | _,_ -> failwith "Invalid index types."

let rec is_at_least_n_dimensional ~n typ = match n, typ with
  | _, _ when n <= 0 -> false
  | 1, Array _ -> true
  | _, Array typ' -> is_at_least_n_dimensional ~n:(n-1) typ'
  | _ -> false

let check_fun (ctx : tctxt) (fun_name : Ast.call_name) (arg_types : typ list) : typ =
  match fun_name with
  | Ast.Map ->
      begin
        match arg_types with
        | [ Fun fun_type; Array typ; ] when [ typ ] = fun_type.param_types ->
            Array (fun_type.return_type)
        | _ -> failwith "Invalid argument to map."
      end
  | Ast.Dim n ->
      begin
        match arg_types with
        | [ typ ] when is_at_least_n_dimensional typ ~n -> Int
        | _ -> failwith "Invalid argument to length."
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
        | Fun fun_type :: array_typs
            when array_typs = List.map fun_type.param_types ~f:(fun t -> Array t)
            -> Array fun_type.return_type
        | _ -> failwith "Invalid argument to zip_with."
      end
  | Ast.Transpose ->
      begin
        match arg_types with
        | [ Array (Array typ); ] -> Array (Array typ)
        | _ -> failwith "Invalid argument to transpose."
      end
  | Ast.Tabulate -> 
      begin
        match arg_types with 
        | [ typ1; typ2; typ3 ] when 
          typ1 = typ2 && typ2 = typ3
          && typ1 = Int -> typ1
        | _ -> failwith "Invalid arguments to Tabulate."
      end
  | (Ast.Min|Ast.Max) ->
      begin
      match arg_types with
      | [typ1; typ2 ] when typ1 = typ2 -> typ1
      | _ -> failwith "Invalid arguments to min/max"
      end
  | Ast.Float_of_int ->
      begin
      match arg_types with
      | [Int] -> Float
      | _ -> failwith "Invalid arguments to I -> F"
      end
  | Ast.Int_of_float ->
      begin
      match arg_types with 
      | [Float] -> Int
      | _ -> failwith "Invalid arguments to F -> I"
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
  

let rec check_type (ctx : tctxt) (ast : Ast.typ) : typ = match ast with
  | Ast.Ident "int" -> Int
  | Ast.Ident "float" -> Float
  | Ast.Ident ident ->
    (match Map.find ctx.struct_ctx ident with
      | Some _ -> Struct ident
      | None -> failwithf "Unknown type `%s`" ident ())
  | Ast.Array ast' -> Array (check_type ctx ast')

let rec check_expr (ctx : tctxt) (ast : unit Ast.expr) : typ Ast.expr = match snd ast with
  | Ast.Const c -> (Int, Ast.Const c)
  | Ast.Float f -> (Float, Ast.Float f)
  | Ast.Variable ident ->
      let typ = Option.value_exn (IdentMap.find ctx.local_var_ctx ident)
        ~error:(Error.of_exn (Failure (Printf.sprintf "Unbound variable `%s`." ident)))
      in (typ, Ast.Variable ident)
  | Ast.Binop binop ->
      let (type1, _) as res1 = check_expr ctx binop.binary_operand1 in
      let (type2, _) as res2 = check_expr ctx binop.binary_operand2 in
      let typ = check_binop type1 type2 binop.binary_operator in
      (typ, Ast.Binop { binop with binary_operand1 = res1; binary_operand2 = res2; })
  | Ast.Unop unop ->
      let (type1, _) as res = check_expr ctx unop.unary_operand in
      let typ = check_unop type1 unop.unary_operator in
      (typ, Ast.Unop { unop with unary_operand = res })
  | Ast.Index index ->
      let (typ1,_) as res1 = check_expr ctx index.index_source in
      let (typ2,_) as res2 = check_expr ctx index.index_expr in
      let typ = check_index typ1 typ2 in
      (typ, Ast.Index {index_source = res1; index_expr = res2})
  | Ast.Parallel parallel ->
      begin
        let (typ, _) as res = check_expr ctx parallel.parallel_arg in
        let (given_typ, _) as parallel_type = match parallel.parallel_type with
          | (), typ -> (check_type ctx typ, typ)
        in
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
        | ({ return_type = None; _; }, _) -> failwith "Parallel stmts no return."
        | ({ return_type = Some typ; _; }, res_stmts) ->
            (Array typ, Ast.Parallel { parallel with parallel_arg = res; parallel_body = res_stmts; parallel_type; })
      end
  | Ast.Fun_call fun_call ->
      let (arg_types, args) = List.map fun_call.call_args ~f:(function
        | Ast.Bare_binop ((), binop) -> let typ = Fun (infer_binop binop) in (typ, Ast.Bare_binop (typ, binop))
        | Ast.Bare_unop ((), unop) -> let typ = Fun (infer_unop unop) in (typ, Ast.Bare_unop (typ, unop))
        | Ast.Expr expr -> let (t, e) = check_expr ctx expr in (t, Ast.Expr (t, e)))
        |> List.unzip
      in
      let ret_ty = check_fun ctx fun_call.call_name arg_types in
      (ret_ty, Ast.Fun_call { fun_call with call_args = args })

and check_stmt (ctx : tctxt) (ast : unit Ast.stmt) : tctxt * typ Ast.stmt =
  if Option.is_some ctx.return_type then failwith "Function already returned.";
  match ast with
  | Ast.Let let_exp ->
      let (typ, _) as let_type = match let_exp.let_type with
        | (), typ -> (check_type ctx typ, typ)
      in
      let (body_type, _) as result = check_expr ctx let_exp.let_expr in
      begin
        if body_type <> typ
          then failwithf "Wrong type annotation on `%s`" let_exp.let_ident ()
      end;
      let ctx' = { ctx with
          local_var_ctx =
            add_with_failure ctx.local_var_ctx
              ~key:let_exp.let_ident ~data:typ
              ~on_duplicate:"Duplicate local variable definition `%s`"; }
      in (ctx', Ast.Let { let_exp with let_expr = result; let_type; })
  | Ast.Return expr ->
      let (typ, _) as result = check_expr ctx expr in
      ({ ctx with return_type = Some typ }, Ast.Return result)

and infer_stmts (ctx : tctxt) (ast : unit Ast.stmt list) : tctxt * typ Ast.stmt list =
  List.fold_map ~init:ctx ~f:check_stmt ast

and check_stmts (ctx : tctxt) (ast : unit Ast.stmt list) (typ : typ) : typ Ast.stmt list =
  let (ctx', result) = infer_stmts ctx ast in
  (* Check return type *)
  match ctx'.return_type with
  | None -> failwith "Statements do not return"
  | Some inferred_type ->
      if typ <> inferred_type
      then failwith "Given return type incompatible with inferred type"
      else result

let check_global_stm (ctx : tctxt) (ast : unit Ast.global_stmt) : tctxt * (typ Ast.fun_defn option) =
  let open Ast in
  match ast with 
  | Fun ast ->
    let (return_type, _) as fun_ret_type = match ast.fun_ret_type with
      | (), return_type -> (check_type ctx return_type, return_type)
    in
    let fun_params = List.map ast.fun_params ~f:(fun ({ param_type = ((), param_type) } as r) ->
      { r with param_type = (check_type ctx param_type, param_type) })
    in
    let param_types = List.map fun_params ~f:(fun { param_type } -> fst param_type) in
    let body_ctx =
      let params_with_type =
        List.zip_exn (List.map ~f:(fun p -> p.param_ident) ast.fun_params) param_types
      in
      { ctx with
          local_var_ctx =
            List.fold_left params_with_type ~init:ctx.local_var_ctx ~f:(fun map (key, data) ->
              add_with_failure map ~key ~data ~on_duplicate:"Duplicate parameter `%s`") }
    in
    let result = check_stmts body_ctx ast.fun_body return_type in
    (* Update fun ctx. *)
    let ctx' = { ctx with
        fun_ctx =
          add_with_failure ctx.fun_ctx
            ~key:ast.fun_name ~data:{ return_type; param_types; }
            ~on_duplicate: "Duplicate function definition `%s`"; }
    in (ctx',Some { ast with fun_body = result; fun_ret_type; fun_params; })
  | Struct ast -> 
    let new_struct_fields = List.map ast.struct_fields 
                            ~f:(fun {param_type; param_ident} -> 
                            let (_,t) = param_type in
                            {field_name = param_ident; field_type = (check_type ctx t)})
    in
    let ctx' = { ctx with 
      struct_ctx = add_with_failure ctx.struct_ctx
        ~key:ast.struct_name ~data:new_struct_fields 
        ~on_duplicate: "Duplicate struct defn `%s`";
      }
    in
    (ctx', None)
 
let check_with (ctx : tctxt) (ast : unit Ast.t) : typ Ast.fun_defn list =
  let typc = List.folding_map ~f:check_global_stm ~init:ctx ast in
  List.filter_opt typc

let check (ast : unit Ast.t) : typ Ast.fun_defn list = check_with empty ast
