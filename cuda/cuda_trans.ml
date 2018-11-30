open Core

module Ano = Annotated_air
module Length_expr = Annotated_air.Length_expr
module Expr = Annotated_air.Expr
module Op = Ir.Operator
module CU = Cuda_ir
module Many_fn = Utils.Many_fn

let failwith n = failwith ("AIR -> CUDA : " ^ n)

type context = {
  result : Ano.result;
  lvalue : CU.cuda_expr; (* this is an lvalue *)

  (* Temps indicating the dimensions of parameters. *)
  dims_of_params : Temp.t list;

  (* Map from temps to kind of allocation necessary *)
  (* If it's not in here, it's just on the host. *)
  allocation_method :
    [ `Just_device (* additional_buffers *)
    | `Host_and_device of Temp.t (* temp is the device temp *)
    ] Temp.Table.t;
}

(* Unique identifier for functions. s*)
let fn_next : unit -> string =
  let fn_name_ctr = ref 0 in
  fun () ->
    incr fn_name_ctr;
    sprintf "fn_%d" !fn_name_ctr

(* Translate a temp into a variable.*)
let temp_name (t : Temp.t) : string =
  sprintf "t_%d" (Temp.to_int t)

(* Put that variable into a CUDA one. *)
let temp_to_var (t : Temp.t) : CU.cuda_expr = CU.Var (temp_name t)

let next_var () : CU.cuda_expr = temp_to_var (Temp.next Tc.Int ())

(* on_return is the lvalue to use upon encountering a return stmt. *)
let dest_to_lvalue (ctx : context) (dest : Ir.dest) : CU.cuda_expr =
  match dest with
  | Ir.Return _ -> ctx.lvalue
  | Ir.Dest t -> temp_to_var t

(* These flatten out any nesting. We're presuming the nd_array struct takes care of it. *)
let rec typ_name (typ : Tc.typ) : string =
  match typ with
  | Tc.Array t -> typ_name t ^ "[]"
  | Tc.Int -> "int"
  | _ -> failwith "Not supported."

let rec trans_type (typ : Tc.typ) : CU.cuda_type =
  match typ with
  | Tc.Int -> CU.Integer
  | Tc.Float -> CU.Float
  | Tc.Struct i -> CU.Struct i
  | Tc.Array t -> CU.Pointer (trans_type t)
  | _ -> failwith "Don't currently support other types"

(* Translate an AIR parameter list into a list of CUDA parameters.
 *
 * Notice that `temp_name t` allows us to uniquely determine the name of a temp at
 * any point. Therefore, we don't have to maintain a separate context mapping temps
 * to cuda variable names. What a relief!
 *
 * Recall, graciously, that the parameter for the output buffer has already been
 * added to the parameter list in the context, so it too is translated by the
 * concat_map.
 *)
let trans_params (params : Ano.Param.t list) : (CU.cuda_type * CU.cuda_ident) list =
   List.concat_map params ~f:(fun t ->
     match t with
     (* An array parameter is accompanied by `n` dimension parameters, where n is the
      * dimensionality of the array. *)
     | Ano.Param.Array (t, dims) ->
         let array_param = (trans_type (Temp.to_type t), temp_name t) in
         let dim_params = List.map dims ~f:(fun t -> (CU.Integer, temp_name t)) in
         array_param :: dim_params

     (* A non-array parameter, sadly, is not. *)
     | Ano.Param.Not_array t -> List.return (CU.Integer, temp_name t))

(* Translate AIR operands into their cuda equivalents. s*)
let trans_op = function
  | Air.IndexOp (t1, t2) -> CU.Index (temp_to_var t1, temp_to_var t2)
  | Air.Const c -> CU.IConst (Int64.of_int32_exn c)
  | Air.Float f -> CU.FConst (f)
  | Air.Bool b -> CU.BConst (b)
  | Air.Temp t -> CU.Var (temp_name t)
  | Air.Dim (n, view) -> CU.IConst (Int64.of_int_exn n)

let trans_prim =
  let trans_cmp op a b =
    let a,b = trans_op a, trans_op b in
    CU.Ternary(CU.Binop(op,a,b),a,b)
  in
  function
  | Air.Min (a,b) -> trans_cmp CU.LT a b
  | Air.Max (a,b) -> trans_cmp CU.GT a b
  | Air.I2F a -> CU.Cast(CU.Float,trans_op a)
  | Air.F2I a -> CU.Cast(CU.Integer,trans_op a)

let trans_binop = function
  | Ast.Plus -> CU.ADD
  | Ast.Minus -> CU.SUB
  | Ast.Times -> CU.MUL
  | Ast.Div -> CU.DIV
  | Ast.Mod -> CU.MOD
  | Ast.Lshift -> CU.SHL
  | Ast.Rshift -> CU.SHR
  | Ast.And -> CU.AND
  | Ast.Or -> CU.OR
  | Ast.BitAnd -> CU.BITAND
  | Ast.BitOr -> CU.BITOR
  | Ast.BitXor -> CU.BITXOR
  | Ast.Less -> CU.LT
  | Ast.LessEq -> CU.LTE
  | Ast.Greater -> CU.GT
  | Ast.GreaterEq -> CU.GTE

let trans_unop = function
  | Ast.Negate -> CU.NEG
  | Ast.Logical_not -> CU.NOT

(* Creates the loop header (initial value, max value, and increment stride)
 * -> for (var = init;var < limit; var += stride) *)
let trans_incr_loop_hdr ~loop_var ~lo ~hi ~stride =
 (CU.DeclareAssign (CU.Integer, loop_var, lo),
   CU.Cmpop (CU.LT, CU.Var loop_var, hi), CU.AssignOp (CU.ADD, CU.Var loop_var, stride))

(* Actually perform the reduction! *)
let make_reduce op args =
  match (op, args) with
    | Op.Binop bin, [a;b] -> CU.AssignOp (trans_binop bin, a, b)
    | Op.Binop _, _ -> failwith "Binary operators have two arguments."
    | Op.Unop _, _ -> failwith "Cannot reduce with a unary operator."

let rec trans_len_expr : Length_expr.t -> CU.cuda_expr = function
  | Length_expr.Temp t -> temp_to_var t
  | Length_expr.Mult (e1, e2) -> CU.Binop (CU.MUL, trans_len_expr e1, trans_len_expr e2)

let rec trans_expr : Expr.t -> CU.cuda_expr = function
  | Expr.Temp t -> temp_to_var t
  | Expr.Const i -> CU.IConst (Int64.of_int32 i)
  | Expr.Index (arr, i) -> CU.Index (trans_expr arr, trans_expr i)
  | Expr.Call (op, xs) as e ->
  let open Ir.Operator in
  match op, xs with
  | Binop b, [ e1; e2; ] -> CU.Binop (trans_binop b, trans_expr e1, trans_expr e2)
  | Unop u, [ e1; ] -> CU.Unop (trans_unop u, trans_expr e1)
  | _ -> failwithf "Invalid: %s." (Sexp.to_string_hum (Expr.sexp_of_t e)) ()

let rec app_expr f e = Many_fn.compose trans_expr (Many_fn.app_exn ~msg:"app_expr" f e)

(* Find a buffer info's length. *)
let get_lengths (ctx : context) (t : Temp.t) : CU.cuda_expr list =
  match Map.find Ano.(ctx.result.buffer_infos) t with
  | Some inf -> List.map ~f:trans_len_expr Ano.(inf.length)
  | None -> failwithf "Couldn't find buffer size for `%d`" (Temp.to_int t) ()

let get_length ctx t = List.hd_exn (get_lengths ctx t)

let get_index (ctx : context) (t : Temp.t) : Temp.t -> (Expr.t, CU.cuda_expr) Many_fn.t =
  match Map.find Ano.(ctx.result.buffer_infos) t with
  | Some inf ->
      (* Lookup function *)
      fun t -> app_expr Ano.(inf.index) (Expr.Temp t)
  | None -> failwithf "Couldn't find buffer index for `%d`" (Temp.to_int t) ()

(* Create a loop that iterates over the buffer t. *)
let create_loop (ctx : context) ~counter:loop_temp (t : Temp.t) =
  trans_incr_loop_hdr
    ~loop_var:(temp_name loop_temp)
    ~lo:(CU.IConst 0L)
    ~hi:(get_length ctx t)
    ~stride:(CU.IConst 1L)

(* Used to perform operations with length expressions. *)
let build_cached_reduce_expr (op : CU.binop) (exprs : CU.cuda_expr list) =
  let rec reduce_expr exprs =
    match exprs with
    | [] -> failwith "Cannot bootstrap empty expression."
    | [expr] -> expr
    | ex :: xs -> (CU.Binop (op, ex, reduce_expr xs))
  in
  reduce_expr exprs

(* Translate a statement irrespective of parallel/sequential semantics *)
let rec trans_a_stmt : type a. (context -> a -> CU.cuda_stmt list) -> context -> a Air.stmt -> CU.cuda_stmt list =
  fun continuation ctx -> function
    | Air.For (dest, (bound_temp, t_idx, view), stmt) ->
        (* Get the destination buffer which the for loop is being assigned into. *)
        let dest_array = dest_to_lvalue ctx dest in

        (* Iterate over the bound temp (which Annotate stores as having all the
         * information of the view array). *)
        let hdr = create_loop ~counter:t_idx ctx bound_temp in

        let body =
          (* The lvalue for the for body is an index into the destination array. *)
          (* Translate the body under the new lvalue. *)
          let ctx' = { ctx with lvalue = CU.(Index (dest_array, temp_to_var t_idx)) } in
          continuation ctx' stmt in
        [ CU.Loop (hdr, body); ]

    | Air.Block s -> List.concat_map ~f:(continuation ctx) s
    | Air.Nop -> []
    | Air.Reduce _ | Air.Run _ ->
        failwith
          "Unable to perform reduce/run without specifically associated parallel or sequential semantics."

(* Translate a sequential statement *)
and trans_seq_stmt (ctx : context) (stmt : Air.seq_stmt) : CU.cuda_stmt list =
  let (<--) d src = CU.Assign (dest_to_lvalue ctx d, src) in

  match stmt with
  | Air.Binop (d, op, s1, s2) ->
      [ d <-- CU.Binop (trans_binop op, trans_op s1, trans_op s2) ]
  | Air.Index (d, src, i) ->
      [ d <-- CU.Index (trans_op src, trans_op i)]
  | Air.Access (d,src,f) ->
      [ d <-- CU.Field (trans_op src, f)]
  | Air.Unop (d, op, s) ->
      [ d <-- CU.Unop (trans_unop op, trans_op s) ]
  | Air.Assign (d, s) ->
      [ d <-- trans_op s ]
  | Air.Primitive (d, s) ->
      [ d <-- trans_prim s]
  | Air.Struct_Init (d,t,flx) -> 
      let dest = dest_to_lvalue ctx d in 
      [ CU.InitStruct(trans_type t,dest,List.map flx ~f:(fun (n,o) -> (n,trans_op o)))]
  | Air.Seq_stmt seq_stmt ->

  (* Run/reduce given sequential semantics. *)
  match seq_stmt with
  (* Don't need to bind view: we already figured out how to index into
   * it in the annotation phase. Based on the value of view, the
   * call to get_index will return the right indexing function. *)
  | Air.Run (dest, _) ->
      let dest_temp = Ir.temp_of_dest dest in
      let index_fn = get_index ctx dest_temp in
      let loop_temp = Temp.next Tc.Int () in
      let hdr = create_loop ~counter:loop_temp ctx dest_temp in
      let body = trans_array_view ctx (dest, loop_temp, index_fn) in
      [ CU.Loop (hdr, [ body ]) ]

  (* For reduce, we associate the buffer_info for the view with t. *)
  | Air.Reduce (dest, op, init, (t, _)) ->
      let lvalue = Temp.next Tc.Int () in
      let index_fn = get_index ctx t in
      let loop_temp = Temp.next Tc.Int () in
      let hdr = create_loop ~counter:loop_temp ctx t in
      let assign_stmt = make_reduce op
        [ temp_to_var lvalue;
          Many_fn.result_exn ~msg:"Reduce result_exn." (index_fn loop_temp);
        ]
      in
      [ CU.DeclareAssign (CU.Integer, temp_name lvalue, trans_op init);
        CU.Loop (hdr, [assign_stmt]);
        CU.Assign (dest_to_lvalue ctx dest, temp_to_var lvalue);
      ]
  | _ -> trans_seq_stmt_stmt ctx seq_stmt

and trans_par_stmt (ctx : context) (stmt : Air.par_stmt) : CU.cuda_stmt list =
  match stmt with
  | Air.Seq seq_stm -> trans_seq_stmt ctx seq_stm

  (* TODO: Make these actually run in parallel. *)
  | Air.Par_stmt (Air.Run (a, b)) -> trans_seq_stmt ctx (Air.Seq_stmt (Air.Run (a, b)))
  | Air.Par_stmt (Air.Reduce (a, b, c, d)) -> trans_seq_stmt ctx (Air.Seq_stmt (Air.Reduce (a, b, c, d)))
  | Air.Par_stmt par_stmt -> trans_par_stmt_stmt ctx par_stmt

  | Air.Parallel (dest, id, bound_array_views, body) ->
      let kernel_info =
        match Map.find Ano.(ctx.result.kernel_infos) id with
        | Some k_inf -> k_inf
        | None -> failwith "Failed to find kernel info. (trans_stmt_par)"
      in

      (* Get the temps of the array views to pass to the kernel launch. *)
      let rec temps_in_array_view (av : Air.array_view) : Temp.Set.t =
        match snd av with
        | Air.Zip_with (_, avs) -> Temp.Set.union_list (List.map avs ~f:temps_in_array_view)
        | Air.Reverse av -> temps_in_array_view av
        | Air.Array t -> Temp.Set.singleton t
        | Air.Transpose av -> temps_in_array_view av
        | Air.Tabulate (b,e,s) -> Temp.Set.of_list [b;e;s]
        | Air.Array_index (t1, t2) -> Temp.Set.of_list [t1; t2;]
      in

      let array_view_host_args =
        List.map bound_array_views ~f:(fun (_, _, av) -> temps_in_array_view av)
          |> Temp.Set.union_list
          |> Set.to_list
      in

      let host_args = List.concat [
        array_view_host_args;
        Set.to_list Ano.(kernel_info.free_variables);
      ] in

      let device_args =
        List.map host_args ~f:(fun host_t ->
          match Temp.Table.find ctx.allocation_method host_t with
          | Some `Just_device -> failwith "I don't think this is possible."
          | Some (`Host_and_device t) -> t
          | None ->
              let new_temp = Temp.next (Temp.to_type host_t) () in
              Temp.Table.add_exn ctx.allocation_method ~key:host_t ~data:(`Host_and_device new_temp);
              new_temp)
      in

      (* Mark additional buffers as just-device. *)
      let additional_buffers = Set.to_list Ano.(kernel_info.additional_buffers) in
      List.iter additional_buffers ~f:(fun key -> Temp.Table.add_exn ctx.allocation_method ~key ~data:`Just_device);

      (* In addition to the array view args, also get the args for the
       * additional buffers and free variables to pass to the kernel launch.
       *)
      let args = List.concat [
        device_args;
        additional_buffers;

        (* We also need to pass in the dimensions of the original parameters! :) *)
        ctx.dims_of_params;
      ] in

      (* Process:
       - Allocate what needs to be allocated of them on the device.
       - Translate the kernel body.
       - Place the args / body into the kernel.
       - Launch.
      *)

      (* We can re-use argument names as parameter names since all the arguments
       * are unique temps (and therefore will have unique parameter names).
       *)
      let output_buffer_param = temp_name (Temp.next (Ir.type_of_dest dest) ()) in
      let (kernel_params, kernel_args) =
        (* List of tuples of types, params, and args *)
        let tpas = List.concat [
          (* Don't forget to include as a parameter the dest. *)
          List.return ((trans_type (Ir.type_of_dest dest), output_buffer_param), dest_to_lvalue ctx dest);
          List.map args ~f:(fun t -> ((trans_type (Temp.to_type t), temp_name t), temp_to_var t));
        ]
        in List.unzip tpas
      in

      (* Total length of all the things we're parallel over. *)
      let bound_temps = List.map ~f:Tuple3.get1 bound_array_views in
      let lengths = List.map ~f:(get_length ctx) bound_temps in
      let total_length = build_cached_reduce_expr CU.MUL lengths in

      let gdim = (CU.IConst 256L, CU.IConst 1L, CU.IConst 1L) in
      let bdim = (CU.Binop (CU.DIV, total_length, CU.IConst 256L), CU.IConst 1L, CU.IConst 1L) in (* Todo: make this blocks/thrd  *)
      let index_expr =
        (* blockDim.x * blockIdx.x + threadIdx.x *)
        (* TODO: change this? *)
        CU.(Binop (ADD, Binop (MUL, KVar (BlockDim X), KVar (BlockIdx X)),
                        KVar (ThreadIdx X)))
      in

      (* Ok, so we must split the index_expr into the corresponding indices for each of the
       * bound_temps. *)
      let indices = List.map bound_array_views ~f:Tuple3.get2 in

      let kernel = CU.({
        typ = Device;
        ret = Void;
        name = "K_" ^ fn_next ();
        params = kernel_params;
        body =
          begin
            (* In the body, the user assigns to this guy. *)
            let i_temp = Temp.next Tc.Int () in
            let i = temp_to_var i_temp in
            let acc_temp = Temp.next Tc.Int () in
            let acc = temp_to_var acc_temp in
            let lvalue = CU.Index (CU.Var output_buffer_param, i) in
            List.concat [
              [ CU.DeclareAssign (CU.Integer, temp_name i_temp, index_expr);
                CU.DeclareAssign (CU.Integer, temp_name acc_temp, i);
              ];
              List.concat_map (List.zip_exn indices lengths) ~f:(fun (idx, len) ->
                [ CU.DeclareAssign (CU.Integer, temp_name idx, CU.Binop (CU.MOD, acc, len));
                  CU.Assign (acc, CU.Binop (CU.DIV, acc, len));
                ]
              );
              trans_seq_stmt { ctx with lvalue; } body;
            ]
          end;
      }) in

      let memcpy tfr = List.map2_exn ~f:(fun dest src ->
        let typ = Temp.to_type dest in
        let lengths =
          try get_lengths ctx dest
          with _ -> get_lengths ctx src
        in
        let len = CU.Binop (CU.MUL, build_cached_reduce_expr CU.MUL lengths, CU.Size_of (trans_type typ)) in
        CU.Transfer (temp_to_var dest, temp_to_var src, len, tfr)
      ) in

      List.concat [
        (* Memcpy the host args into the device args. *)
        memcpy CU.(Device, Host) device_args host_args;
        [CU.Launch (gdim, bdim, kernel, kernel_args)];
        (* Memcpy the device args into the host args. *)
        memcpy CU.(Host, Device) host_args device_args;
      ]

and trans_seq_stmt_stmt ctx stmt = trans_a_stmt trans_seq_stmt ctx stmt
and trans_par_stmt_stmt ctx stmt = trans_a_stmt trans_par_stmt ctx stmt

(* Store array_view in dest using loop_var as the counter variable. *)
and trans_array_view (ctx : context) (dest, loop_temp, index_fn) : CU.cuda_stmt =
  let dest_arr = dest_to_lvalue ctx dest in
  let lvalue = CU.Index (dest_arr, temp_to_var loop_temp) in

  (* Ok, we have to case on the type of the destination in order to figure out whether we
   * need to memcpy or if a simple assignment is sufficient. *)
  let rec loop lvalue = function
    | Many_fn.Result src -> CU.Assign (lvalue, src)
    | Many_fn.Fun f ->
        let t_idx = Temp.next Tc.Int () in
        (* TODO: do more than one level *)
        let t = Ir.temp_of_dest dest in
        let hdr = create_loop ~counter:t_idx ctx t in
        let i = temp_to_var t_idx in
        CU.Loop (hdr, [ loop (CU.Index (lvalue, i)) (f (Expr.Temp t_idx)) ])
  in
  loop lvalue (index_fn loop_temp)

let rec extract_kernel_launches : CU.cuda_stmt list -> CU.cuda_func list =
  List.fold_left ~init:[] ~f:(fun kernel_launches -> function
    | CU.Sync | CU.Nop | CU.DeclareArray _ | CU.DeclareAssign _ | CU.Assign _
    | CU.AssignOp _ | CU.Cuda_malloc _ | CU.Malloc _ | CU.Free _ | CU.Transfer _
    | CU.Expression _ | CU.Memcpy _ -> kernel_launches
    | CU.Launch (_, _, cuda_func, _) -> cuda_func :: kernel_launches
    | CU.Loop (_, stmts) -> extract_kernel_launches stmts @ kernel_launches
    | CU.Condition (_, stmts1, stmts2) ->
        let gstmts1 = extract_kernel_launches stmts1 in
        let gstmts2 = extract_kernel_launches stmts2 in
        gstmts1 @ gstmts2 @ kernel_launches)

(* The translated gstmt contains within it kernel launches.
 * These kernel launches actually include the kernel DEFINITION.
 * It's up to a later phase to float all these kernel definitions to the top level.
 *)
let trans (program : Air.t) (result : Ano.result) : CU.cuda_gstmt list =
  let params = trans_params Ano.(result.params) in
  let lvalue = match Ano.(result.out_param) with
    (* When there is an output buffer, this is the lvalue that an Air `return`
     * stmt should write into. *)
    | Some t -> t

    (* Just make up a new temp in the case that there is no buffer
     * to write into. *)
    | None -> Temp.next Tc.Int ()
  in
  let allocation_method = Temp.Table.create () in
  let body = trans_par_stmt {
    result;
    allocation_method;
    lvalue = temp_to_var lvalue;
    dims_of_params = List.concat_map Ano.(result.params) ~f:(function
      | Ano.Param.Array (_, dims) -> dims
      | Ano.Param.Not_array _ -> []);
  } Air.(program.body) in

  (* Malloc on the device and on the host. *)
  let malloc'ing =

    (* Remove outermost arrays. *)
    let rec remove_arrays =
      function Tc.Array typ -> remove_arrays typ | typ -> trans_type typ in

    List.concat_map (Map.to_alist Ano.(result.buffer_infos)) ~f:(fun (t, bi) ->
      let f t k =
        let typ = Temp.to_type t in k CU.(
          trans_type typ,
          temp_name t,
          Binop (MUL,
            build_cached_reduce_expr MUL (List.map ~f:trans_len_expr Ano.(bi.length)),
            Size_of (remove_arrays typ)))
      in
      match Temp.Table.find allocation_method t with
      | None -> f t (fun (a, b, c) -> [ CU.Malloc (a, b, c) ])
      | Some `Just_device -> f t (fun (a, b, c) -> [ CU.Cuda_malloc (a, b, c) ])
      | Some `Host_and_device t' -> f t (fun (a, b, c) -> f t' (fun (a', b', c') -> [ CU.Malloc (a, b, c); CU.Cuda_malloc (a', b', c'); ])))
  in

  let gdecls = extract_kernel_launches body in
  List.concat [
    gdecls;
    List.return
      CU.({ typ = Host;
            ret = Void;
            name = "dag_" ^ Air.(program.fn_name);
            params;
            body = malloc'ing @ body; });
  ] |> List.map ~f:(fun x -> CU.Function x)
