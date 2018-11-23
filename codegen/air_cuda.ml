open Core

module IR = Air
module CU = CudaIR

(* Unique identifier for functions. s*)
let fn_name_ctr = ref 0
let fn_next () = 
  incr fn_name_ctr;
  sprintf ("fn_%d") !fn_name_ctr

(* Translate a temp into a variable.*)
let temp_name t = 
  sprintf ("t_%d") (Temp.to_int t)

(* Put that variable into a CUDA one. *)
let temp_to_var t = 
  CU.Var (temp_name t)

(* Get the device version of a variable *)
let var_to_dev var =
  match var with 
  | CU.Var v -> CU.Var ("d_"^v)
  | _ -> failwith "AIR -> CUDA : Cannot device a non-variable."

(* Convert to cuda constant *)
let con i = CU.Const (Int64.of_int_exn i)

(* These flatten out any nesting. We're presuming the nd_array struct takes care of it. *)
let rec typ_name (typ : Ast.typ) = 
  match typ with 
  | Ast.Array t -> typ_name t
  | Ast.Ident t -> (t : string)

let rec trans_typ (typ : Ast.typ) = 
  match typ with
  | Ast.Ident t -> 
    (match t with
    | "int" -> CU.Integer
    | "bool" -> CU.Boolean
    | "float" -> CU.Float
    | "double" -> CU.Double
    | _ -> failwith ("AIR -> CUDA : Don't currently support complex type trans."))
  | Ast.Array t -> trans_typ t

(* Pointer, Lengths of dimensions *)
type nd_array = Temp.t * (Temp.t list)
let nd_array_id = "dag_nd_array_t"
let nd_array_dims = "dim"
let nd_array_lens = "lens"
let nd_array_data = "data"

(* Global struct representing a multidimensional array. 
   Polymorphic - feed it the type to get the decl you want.
   Should probably be declared in every DAG program. s*)
let dag_array_struct typ : CU.cuda_gstmt = 
  CU.StructDecl(nd_array_id^"_"^typ_name typ,
    [
     (CU.Integer,nd_array_dims);
     (CU.Pointer CU.Integer,nd_array_lens);
     (CU.Pointer (trans_typ typ),nd_array_data);
    ]
  )

(* Extract the length information from the recursive AIR param. *)
let flatten_array p l : nd_array=
  let rec flatten par len acc =
    match par with 
    | IR.Temp_param (t,_) -> (t,List.rev (len::acc))
    | IR.Array_param {param = subp; length = subl} -> 
      flatten subp subl (len::acc)
  in
  flatten p l []

(* Translate an AIR parameter list into a list of CUDA fnargs. 
   This requires the actual struct objects to be initialized elsewhere. *)
let trans_params (params : IR.param list) : (CU.cuda_type * CU.cuda_ident) list = 
   let trans_param = function
   | IR.Array_param {param = p; length = l} -> 
      let (ptr,_) = flatten_array p l in
      (CU.Pointer (CU.Struct nd_array_id), temp_name ptr)
   | IR.Temp_param (t,typ) -> (trans_typ typ, temp_name t)
   in
   List.map params ~f:(trans_param)

(* Translate AIR operands into their cuda equivalents. s*)
let trans_op = 
  function
  | IR.Const c -> CU.Const (Int64.of_int32_exn c)
  | IR.Temp t -> CU.Var (temp_name t)

let trans_binop = 
  function
  | Ast.Plus -> CU.ADD
  | Ast.Minus -> CU.SUB
  | Ast.Times -> CU.MUL
  | Ast.Div -> CU.DIV
  | Ast.Mod -> CU.MOD

let trans_unop = 
  function
  | Ast.Negate -> CU.NEG 
  | Ast.Logical_not -> CU.NOT

let trans_seq_stmt : IR.seq_stmt -> CU.cuda_stmt = 
  function 
  | IR.Binop (d,op,s1,s2) -> 
      CU.Assign(temp_to_var d,
      CU.Binop(trans_binop op, trans_op s1, trans_op s2))
  | IR.Unop (d,op,s) -> 
      (* This will depend on the nature of the unop. *)
      CU.Assign(temp_to_var d,
      CU.Unop(trans_unop op,trans_op s))
  | IR.Assign (d,s) -> 
      CU.Assign(temp_to_var d,trans_op s)

let trans_incr_loop_hdr var init limit stride = 
 (CU.Assign(var,init),CU.Cmpop(CU.LT,var,limit),CU.AssignOp(CU.ADD,var,stride))

let rec trans_par_stmt = function
  | IR.Par_for (seqs,seq_stms) -> 
      (* This implies either a kernel launch (if we're not in a par block) 
         or an additional level of indexing (if we are) *) []
  | IR.Seq_for ((dest,aview),par_stms) -> 
      (* This is just a for loop. I think. *) 
      begin
      match aview with 
      | IR.Array (arr,len) -> []
      | IR.Zip_with (op,subviews) -> []
      | IR.Reverse (subview) -> []
      | IR.Transpose (subview) -> []
        (* This really only makes sense in 2D. We'll assume this was validated... *)
      end
  | IR.Run (t,v) -> 
      (* Call the queued sequence computation *) []
  | IR.Seq stm -> [trans_seq_stmt stm]

(* Returns:  ([list of processing instrs],array temp, array length) *)
(* Right now this naively performs the operation. We should squash it. *)
and trans_array_view arr_view (dest : Temp.t)  = 
  let vDest = temp_to_var dest in
  match arr_view with 
  | IR.Array (arr,len) -> ([],arr,len)
  | IR.Zip_with(op,subviews) -> 
    let var = CU.Var "z" in (* Loop Variable *)
    let vIndex = CU.Index(vDest,var) in
    (* Inner loop operation function *)
    let make_op subarrs =
      begin
      match op,subarrs with 
        | IR.Op.Unop u,[s]-> 
            CU.Assign(vIndex,CU.Unop(trans_unop u, CU.Index(s,var)))
        | IR.Op.Binop b,[s1;s2] ->
            let idx1,idx2 = CU.Index(s1,var),CU.Index(s2,var) in
            CU.Assign(vIndex,CU.Binop(trans_binop b,idx1,idx2))
        | IR.Op.Fun_ptr f,zips -> 
            let zinds = List.map ~f:(fun s -> CU.Index(s,var)) zips in
            CU.Assign(vIndex,CU.FnCall (f,zinds))
        | _ -> failwith "AIR -> CUDA : Incorrect arg lengths to ZipWith!"
      end
    in
      (* Pipe this into the zip function. *)
      let trans_list = subviews
        |> List.map ~f:(fun _ -> Temp.next ())
        |> List.map2_exn subviews ~f:trans_array_view in
      let subarrs = List.map trans_list ~f:(fun (_,a,_) -> temp_to_var a) in
      let previns = trans_list |> List.map ~f:(fun (a,_,_) -> a) |> List.concat in
      let (_,_,lng) = List.nth_exn trans_list 0 in
      let hdr = trans_incr_loop_hdr var (con 0) (temp_to_var lng) (con 1) in
      let instrs = [CU.Loop(hdr,[make_op subarrs])] in 
      (previns @ instrs,dest,lng)

  | IR.Reverse (subview) -> 
    let reverse_loop array len = 
      let (cArr,cLen) = temp_to_var array, temp_to_var len in
      let var = CU.Var ("r"^(temp_name (Temp.next ()))) in
      let hdr = trans_incr_loop_hdr (var) (con 0) (cLen) (con 1) in
      let (fIndex,bIndex) = CU.Index(cArr,var),CU.Index(vDest,CU.Binop(CU.SUB,cLen,var)) in
      ([CU.Loop(hdr,[CU.Assign(bIndex,fIndex)])],dest,len)
    in 
      let (ins,tmp,lng) = trans_array_view subview (Temp.next ())in
      let (rev_ins,_,lng) = reverse_loop tmp lng in 
      (ins @ rev_ins,dest,lng)

  (* Due to the implementation, this is slightly different.. *)
  | IR.Transpose _ ->  failwith "AIR -> CUDA : No Sequential Transpose! Use primitive. "


let trans_body (body : IR.par_stmt list) : (CU.cuda_stmt) list = 
  body |> List.map ~f:(trans_par_stmt) |> List.concat

let translate (program : IR.t) : CU.cuda_gstmt = 
  let args = trans_params IR.(program.params) in
  let body = trans_body IR.(program.body) in 
  CU.(Function {
    typ = Host; (* Might make other calls. This isn't those. *)
    ret = Void;
    name = "dag_main";
    args = args;
    body = body;
  })