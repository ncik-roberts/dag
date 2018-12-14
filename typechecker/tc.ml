open Core

type ident = string [@@deriving sexp, compare]
module IdentMap = String.Map

type typ =
  | Int
  | Bool
  | Float
  | Struct of ident
  | Array of typ
  | Pointer of typ
  | Fun of fun_type
  [@@deriving sexp, compare]

(* when they are options, we pretend that it's polymorphic. *)
(* all `None`s corefer. *)
and fun_type = {
  allowed_types : typ list;
  return_type : typ option;
  param_types : typ option list;
}

module Type = Comparable.Make (struct type t = typ [@@deriving sexp, compare] end)

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

let instantiate_return_type typs fun_type =
  let types_for_none =
    List.fold2 typs fun_type.param_types
      ~init:Type.Set.empty
      ~f:(fun acc_set t p -> match p with
        | None ->
            if List.mem fun_type.allowed_types t ~equal:Type.equal
            then Type.Set.add acc_set t
            else failwith "Disallowed type for polymorphic fn."
        | Some t2 -> if t = t2 then acc_set else failwith "Types don't unify")
    |> function List.Or_unequal_lengths.Ok x -> x
              | List.Or_unequal_lengths.Unequal_lengths -> failwith "Wrong # of args."
  in
    if Set.length types_for_none > 1 then failwith "Incompatible instantiations of polymorphic fn.";
    match (fun_type.return_type, Set.choose types_for_none) with
    | None, None -> failwith "Polymorphic fn uninstantiated."
    | None, Some t -> t
    | Some t, _ -> t

(* *)
let infer_binop (binop : Ast.binop) : fun_type =
  let allowed_types = [ Int; Float;] in
  match binop with
  | Ast.(Plus | Minus | Times | Div | Mod | Lshift | Rshift | BitAnd | BitOr | BitXor) ->
      { param_types = [ None; None; ]; return_type = None; allowed_types; }
  | Ast.(And | Or ) ->
      { param_types = [ Some Bool; Some Bool;]; return_type = Some Bool; allowed_types = []; }
  | Ast.(Less | Greater | LessEq | GreaterEq ) ->
      { param_types = [ None; None;]; return_type = Some Bool; allowed_types; }

(* Negate works on ints and floats, logical not works on ints and bools. *)
let infer_unop (unop : Ast.unop) : fun_type =
  let allowed_types = match unop with
  | Ast.Negate -> [ Int; Float; ]
  | Ast.Logical_not -> [ Int; Bool; ]
  | Ast.Bitwise_not -> [ Int; ]
  in
  {
    param_types = [ None ];
    return_type = None;
    allowed_types;
  }

let check_binop (typ1 : typ) (typ2 : typ) (binop : Ast.binop) : typ =
  let fun_type = infer_binop binop in
  instantiate_return_type [ typ1; typ2; ] fun_type

let check_unop (typ : typ) (unop : Ast.unop) : typ =
  let fun_type = infer_unop unop in
  instantiate_return_type [typ] fun_type

let check_index (typ1 : typ) (typ2 : typ) : typ =
  match typ1,typ2 with
  | Array t,Int -> t
  | _,_ -> failwith "Invalid index types."

let check_access ctx typ field =
  match typ with
  | Struct s ->
    (match Map.find (ctx.struct_ctx) s with
    | Some t ->
      (match List.find t ~f:(fun f -> f.field_name = field) with
        | Some f -> f.field_type
        | None -> failwith "Struct does not have field.")
    | None -> failwith "Struct does not exist?" )
  | _ -> failwith "Cannot access non-struct."

let rec is_at_least_n_dimensional ~n typ = match n, typ with
  | _, _ when n < 0 -> false
  | 0, Array _ -> true
  | _, Array typ' -> is_at_least_n_dimensional ~n:(n-1) typ'
  | _ -> false

let check_fun (ctx : tctxt) (fun_name : Ast.call_name) (arg_types : typ list) : typ =
  match fun_name with
  | Ast.Filter_with ->
      begin
        match arg_types with
        | [ Array typ; Array Bool; ] -> Array typ
        | _ -> failwith "Invalid argument to filter_with."
      end
  | Ast.Map ->
      begin
        match arg_types with
        | [ Fun fun_type; Array typ; ] -> Array (instantiate_return_type [typ] fun_type)
        | _ -> failwith "Invalid argument to map."
      end
  | Ast.Dim n ->
      begin
        match arg_types with
        | [ typ ] when is_at_least_n_dimensional typ ~n -> Int
        | _ -> failwith "Invalid argument to length."
      end
  | Ast.Reduce | Ast.Scan ->
      begin
        match arg_types with
        | [ Fun fun_type; typ1; Array typ2; ] when
            typ1 = typ2
              && typ1 = instantiate_return_type [typ1; typ2;] fun_type ->
                (* Scan creates an array of these things;
                 * reduce just gives you the final result. *)
                if fun_name = Ast.Scan then Array typ1
                else typ1
        | _ -> failwith "Invalid argument to reduce."
      end
  | Ast.Zip_with ->
      begin
        match arg_types with
        | Fun fun_type :: array_typs ->
            let unarray_types = List.map array_typs ~f:(function
              |  Array t -> t
              | _ -> failwith "Not array zipwith")
            in
            Array (instantiate_return_type unarray_types fun_type)
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
        | [ Int; ] -> Array Int
        | _ -> failwith "Invalid arguments to Tabulate."
      end
  | Ast.Log2 ->
      begin
        match arg_types with
         | [ Int; ] -> Int
         | _ -> failwith "Invalid argument to log2."
      end
  | (Ast.Min | Ast.Max) ->
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
            let concrete_param_types =
              List.map fun_type.param_types ~f:(function
                | None -> failwith "Not concrete??"
                | Some ty -> ty)
            in
            let concrete_ret_ty = match fun_type.return_type with
              | Some ty -> ty
              | None -> failwith "???"
            in
            if List.equal ~equal:(=) arg_types concrete_param_types
            then concrete_ret_ty
            else failwith "Invalid argument types."
        | None -> failwithf "Unknown function `%s`" fun_name ()
      end


let rec check_type (ctx : tctxt) (ast : Ast.typ) : typ = match ast with
  | Ast.Ident "int" -> Int
  | Ast.Ident "float" -> Float
  | Ast.Ident "bool" -> Bool
  | Ast.Ident ident ->
    (match Map.find ctx.struct_ctx ident with
      | Some _ -> Struct ident
      | None -> failwithf "Unknown type `%s`" ident ())
  | Ast.Array ast' -> Array (check_type ctx ast')

let rec check_struct_fields (ctx : tctxt) (fields : struct_type) (ls : 'a Ast.field list) : unit =
  let sorted1 = List.sort fields ~compare:(fun f1 f2 -> String.compare f1.field_name f2.field_name) in
  let sorted2 = List.sort ls ~compare:(fun f1 f2 -> String.compare Ast.(f1.field_name) Ast.(f2.field_name)) in
  match
    List.iter2 sorted1 sorted2 ~f:(fun tc_field ast_field ->
      let (typ, _) = Ast.(ast_field.field_expr) in
      if not (Type.equal tc_field.field_type typ)
        then failwith "Wrong struct fields.")
  with
  | List.Or_unequal_lengths.Unequal_lengths -> failwith "Wrong number of struct fields."
  | List.Or_unequal_lengths.Ok () -> ()

let rec check_expr (ctx : tctxt) (ast : unit Ast.expr) : typ Ast.expr = match snd ast with
  | Ast.Const c -> (Int, Ast.Const c)
  | Ast.Float f -> (Float, Ast.Float f)
  | Ast.Bool b -> (Bool, Ast.Bool b)
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
  | Ast.Access (e,f) ->
      let (typ1,_) as res1 = check_expr ctx e in
      let typ = check_access ctx typ1 f in
      (typ, Ast.Access (res1,f))

  | Ast.Struct_Init struc ->
      let results = List.map ~f:(check_expr ctx)
                    (List.map ~f:(fun f -> Ast.(f.field_expr)) struc.struct_fields) in
      let new_fields = List.map2_exn struc.struct_fields results ~f:(fun f exp -> Ast.{f with field_expr = exp }) in
      let fields = match Map.find ctx.struct_ctx struc.struct_name with
        | None -> failwith "struct not found"
        | Some t -> t
      in
      check_struct_fields ctx fields new_fields;
      (Struct struc.struct_name, Ast.Struct_Init { struc with struct_fields = new_fields })

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
      let (arg_types, args) = List.map fun_call.call_args ~f:(fun arg ->
        match arg with
        | Ast.Bare_binop ((), b) -> let ty = Fun (infer_binop b) in (ty, Ast.Bare_binop (ty, b))
        | Ast.Bare_unop ((), u) -> let ty = Fun (infer_unop u) in (ty, Ast.Bare_unop (ty, u))
        | Ast.Fn_ptr _ -> failwith "This isn't identified by the user."
        | Ast.Expr e ->
            try
              let (ty, _) as e' = check_expr ctx e in (ty, Ast.Expr e')
            with _ -> begin
              let open Option.Monad_infix in begin
                match e with
                | ((), Ast.Variable id) -> Option.return id
                | _ -> None
              end >>= fun id ->
              Map.find ctx.fun_ctx id >>= fun fun_type ->
              Option.return (Fun fun_type, id)
            end |> function
              | None -> failwith "Failed to typecheck expression used as argument."
              | Some (fun_type, id) -> (fun_type, Ast.Fn_ptr (fun_type, id))
          )
        |> List.unzip
      in
      (* An ast with partially-applied functions at each node. *)
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
            ~key:ast.fun_name ~data:{
              return_type = Some return_type;
              param_types = List.map ~f:Option.some param_types;
              allowed_types = [];
            }
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

let check_with (ctx : tctxt) (ast : unit Ast.t) : tctxt * typ Ast.fun_defn list =
  let (tctx, typc) = List.fold_map ~f:check_global_stm ~init:ctx ast in
  (tctx, List.filter_opt typc)

let check (ast : unit Ast.t) : tctxt * typ Ast.fun_defn list = check_with empty ast
