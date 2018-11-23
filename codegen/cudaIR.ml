
open Core

type cuda_ident = string

type cuda_type = Integer | Float | Double | Boolean | Void | Pointer of cuda_type | ConstType of cuda_type | Struct of cuda_ident

type cuda_mem_type = Host | Device | Shared

type cuda_struct = cuda_ident * (cuda_type * cuda_ident) list

(* Basic arithmetic. *)
type binop = ADD | SUB | MUL | DIV | MOD
type unop = INCR | DECR | NEG | NOT
type cmpop = EQ  | NEQ | GTE | LTE | GT | LT

type dim = X | Y | Z

type cuda_variable = 
  | BlockIdx  of dim
  | BlockDim  of dim
  | GridDim   of dim
  | ThreadIdx of dim

(* Simple nested expressions. *)
type cuda_expr =
  | Const of Int64.t
  | Var of cuda_ident
  | KVar of cuda_variable
  | Unop of unop * cuda_expr
  | Binop of binop * cuda_expr * cuda_expr
  | Cmpop of cmpop * cuda_expr * cuda_expr
  | Address of cuda_expr
  | Deref of cuda_expr
  | Index of cuda_expr * cuda_expr
  | Field of cuda_expr * cuda_ident

type grid_dim = cuda_expr * cuda_expr * cuda_expr

(* Represents a funciton definition *)
type cuda_func = {
  typ : cuda_mem_type;
  ret : cuda_type;
  name : cuda_ident;
  args : (cuda_type * cuda_ident) list;
  body : cuda_stmt list
}

and cuda_stmt = 
  | DeclareArray of cuda_mem_type * cuda_type * cuda_ident * (cuda_expr list)
              (* type ident = expr *)
  | DeclareAssign of cuda_type * cuda_ident * cuda_expr

   (* Pretending exprs are lvalues. *)
  | Assign of cuda_expr * cuda_expr 
  | AssignOp of binop * cuda_expr * cuda_expr
  | Expression of cuda_expr 
              (* for(_,_,_), {_} *)
  | Loop of (cuda_stmt * cuda_expr * cuda_stmt) * cuda_stmt list
              (* if (_) then {_} else {_} *)
  | Condition of cuda_expr * (cuda_stmt list) * (cuda_stmt list)
              (* type, source, size *)
  | Allocate of cuda_type * cuda_ident * cuda_expr
              (* dest, source, size, transfer type *)
  | Transfer of cuda_ident * cuda_expr * cuda_expr * cuda_mem_type
    (* Launch dimension, blocks/thread, kernel, arguments *)
  | Launch of grid_dim * int * cuda_func * cuda_ident list

  | Free of cuda_ident
  | Sync

type cuda_gstmt = 
  | Function of cuda_func
  | Decl of cuda_stmt
  | StructDecl of cuda_struct

type cuda_program = cuda_gstmt list

let fmt_args args =
  (List.fold args ~init:"(" ~f:(fun str a -> str^", "^a))^")"

let str_depth n =
  String.concat (List.map (List.range 0 n) ~f:(fun _ ->"  "))

let rec fmt_typ = function
  | Integer -> "int"
  | Float -> "float"
  | Double -> "double"
  | Boolean -> "bool"
  | Void -> "void"
  | ConstType t -> "const "^(fmt_typ t)
  | Pointer t -> (fmt_typ t)^"*"
  | Struct s -> s

let fmt_mem_hdr = function
  | Host -> ""
  | Device -> "__global__"
  | Shared -> "__shared__"

let fmt_mem_tfr = function
  | Host   -> "cudaMemcpyDeviceToHost"
  | Device -> "cudaMemcpyHostToDevice"
  | Shared -> ""

let fmt_unop = function
  | INCR -> "++"
  | DECR -> "--"
  | NEG -> "-"
  | NOT -> "!"

let fmt_binop = function
  | ADD -> "+"
  | SUB -> "-"
  | MUL -> "*"
  | DIV -> "/"
  | MOD -> "%"

let fmt_cmpop = function
  | EQ  -> "=="
  | NEQ -> "!="
  | LT  -> "<"
  | LTE -> "<="
  | GT  -> ">"
  | GTE -> ">="

let fmt_dim = function
  | X -> "x"
  | Y -> "y"
  | Z -> "z"

let fmt_kvar = function
  | BlockIdx  idx ->  "blockIdx."^fmt_dim idx
  | BlockDim  idx ->  "blockDim."^fmt_dim idx
  | GridDim   idx ->   "gridDim."^fmt_dim idx
  | ThreadIdx idx -> "threadIdx."^fmt_dim idx

let rec fmt_expr = function
  | Const c -> Int64.to_string c
  | Var v -> v
  | KVar v -> fmt_kvar v
  | Unop (u,e) -> 
    (match u with 
    | (INCR|DECR) -> sprintf "(%s)%s" (fmt_expr e) (fmt_unop u)
    | (NEG | NOT) -> sprintf "%s(%s)" (fmt_unop u) (fmt_expr e))
  | Binop (b,e1,e2) -> 
    sprintf "(%s %s %s)" (fmt_expr e1) (fmt_binop b) (fmt_expr e2)
  | Cmpop (c,e1,e2) ->
    sprintf "(%s %s %s)" (fmt_expr e1) (fmt_cmpop c) (fmt_expr e2)
  | Address e -> sprintf "&(%s)" (fmt_expr e)
  | Index (e,i) -> sprintf "%s[%s]" (fmt_expr e) (fmt_expr i) 
  | Deref e -> sprintf "*(%s)" (fmt_expr e)
  | Field (s,f) -> sprintf "(%s)->%s" (fmt_expr s) f

let rec fmt_block n block = 
   let sp = str_depth n in
   let body = List.map block ~f:(fun s -> fmt_stmt (n+1) s^"\n") in 
   let body' = String.concat body in
   "{\n"^body'^sp^"}\n"

and fmt_stmt n stm = 
 let sp = str_depth n in
 match stm with 
 | DeclareAssign (typ,id,exp) ->
   sprintf "%s %s %s = %s;" sp (fmt_typ typ) (id) (fmt_expr exp)

 | DeclareArray (mem,typ,id,sizes) ->
   let arr_exps = String.concat(List.map sizes ~f:(fun e -> "["^fmt_expr e^"]")) in
   sprintf "%s %s %s %s %s;" sp (fmt_mem_hdr mem) (fmt_typ typ) id arr_exps
 
 | Assign (l,exp) ->
   sprintf "%s %s = %s;" sp (fmt_expr l) (fmt_expr exp)
 
 | AssignOp (op,l,exp) ->
   sprintf "%s %s %s=  %s;" sp (fmt_expr l) (fmt_binop op) (fmt_expr exp)

 | Expression e -> sp^fmt_expr e^";"

 | Loop ((f,e,a),b) -> 
   let guard = sprintf "%s for(%s %s %s)\n" sp (fmt_stmt 0 f) (fmt_expr e) (fmt_stmt 0 a) in
   guard^sp^(fmt_block n b)

 | Condition (c,b1,b2) -> 
   let guard = sprintf "%s if(%s)\n" sp (fmt_expr c) in
   guard^(fmt_block n b1)^"else"^(fmt_block n b2)
  
 | Allocate (typ,src,size) ->
   let decl = sprintf "%s %s d_%s;\n" sp (fmt_typ typ) (src) in
   let malloc = sprintf "%scudaMalloc(&%s,%s);" sp (src) (fmt_expr size) in
   decl ^ malloc

 | Transfer (dest,src,size,ttyp) -> 
   sprintf "%scudaMemcpy(%s,%s,%s,%s);" sp dest 
   (fmt_expr src) (fmt_expr src) (fmt_mem_tfr ttyp)

 | Launch ((x,y,z),bt,func,args) -> 
   let block = sprintf "%sdim3 dimBlock(%s,1,1);\n" sp (string_of_int bt) in
   let grid = sprintf "%sdim3 dimGrid(%s,%s,%s);\n" sp (fmt_expr x) (fmt_expr y) (fmt_expr z) in
   let launch = sprintf "%s%s<<<dimGrid,dimBlock>>>%s;" sp (func.name) (fmt_args args)
   in block^grid^launch

 | Sync -> sprintf "%s %s;\n" sp "__syncthreads()"
 | Free id -> sprintf "%s cudaFree(%s);\n" sp id

let fmt_func f =
  let arglist = List.map f.args ~f:(fun (t,id) -> (fmt_typ t)^" "^id) in
  let header = sprintf ("%s %s %s%s") 
  (fmt_mem_hdr f.typ) (fmt_typ f.ret) (f.name) (fmt_args arglist) in
  let body = fmt_block 0 (f.body) in
  "\n"^header^body^"\n"

let fmt_struct (id,fields) = 
  let sp = str_depth 1 in
  let header = sprintf "struct %s{\n" id in
  let block = List.map fields ~f:(fun (t,id) -> sp^(fmt_typ t)^" "^id^";\n") in
  let block' = String.concat block^"}\n" in
  header^block'

let fmt_gstm = function
  | Function f -> fmt_func f
  | Decl d -> fmt_stmt 0 d
  | StructDecl (id,fields) -> fmt_struct (id,fields)

let print_program (program : cuda_program) = 
  List.map program ~f:(fun f -> prerr_endline (fmt_gstm f))

(* IR Representation of the Transpose function from the CUDA documentation. *)
let primitive_transpose : cuda_program = 
  [
    Decl(DeclareAssign(ConstType(Integer),"tp_TILE_DIM",Const 32L));
    Decl(DeclareAssign(ConstType(Integer),"tp_BLOCK_ROWS",Const 8L)); 
    
    Function {
      typ = Device;
      ret = Void;
      name = "transposeCoalesced";
      args = [(Pointer(Integer),"result");(ConstType(Pointer(Integer)),"in")];
      body = 
      [
        DeclareAssign(ConstType(Integer),"TILE_DIM",Var "tp_TILE_DIM");
        DeclareAssign(ConstType(Integer),"BLOCK_ROWS",Var "tp_BLOCK_ROWS");
        DeclareArray(Shared,Integer,"tile",[Var "TILE_DIM";Var "TILE_DIM"]);

        DeclareAssign(Integer,"x",Binop(ADD,Binop(MUL,KVar (BlockIdx X),Var "TILE_DIM"),KVar (ThreadIdx X)));
        DeclareAssign(Integer,"y",Binop(ADD,Binop(MUL,KVar (BlockIdx Y),Var "TILE_DIM"),KVar (ThreadIdx Y)));

        DeclareAssign(Integer,"width",Binop(MUL,KVar (GridDim X),Var "TILE_DIM"));

        Loop(
          (DeclareAssign(Integer,"j",Const 0L),
           Cmpop(LT,Var "j",Var "TILE_DIM"),
           AssignOp(ADD,Var "j",Var "BLOCK_ROWS")),
          [
            Assign(
              Index(Index(Var "tile",Binop(ADD,KVar (ThreadIdx Y),Var "j")),KVar (ThreadIdx X)),
              Index(Var "in",Binop(ADD,Binop(MUL,Binop(ADD,Var "y",Var "j"),Var "width"),Var "x"))
            );
          ]);

        Sync;
        Assign(Var "x",Binop(ADD,Binop(MUL,KVar (BlockIdx Y),Var "TILE_DIM"),KVar (ThreadIdx X)));
        Assign(Var "y",Binop(ADD,Binop(MUL,KVar (BlockIdx X),Var "TILE_DIM"),KVar (ThreadIdx Y)));

        Loop(
          (DeclareAssign(Integer,"j",Const 0L),
           Cmpop(LT,Var "j",Var "TILE_DIM"),
           AssignOp(ADD,Var "j",Var "BLOCK_ROWS")),
          [
            Assign(
              Index(Var "result",Binop(ADD,Binop(MUL,Binop(ADD,Var "y",Var "j"),Var "width"),Var "x")),
              Index(Index(Var "tile",KVar (ThreadIdx X)),Binop(ADD,KVar (ThreadIdx Y),Var "j"))
            ); 
          ]);
      ]
    };

    
  ]