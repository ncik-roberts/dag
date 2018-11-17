
open Core

type cuda_type = Integer | Float | Double | Void | Pointer of cuda_type | Const of cuda_type | Struct of string

type cuda_mem_type = Host | Device | Shared

type cuda_ident = string

(* Basic arithmetic. *)
type binop = ADD | SUB | MUL | DIV | MOD
type unop = INCR | DECR
type cmpop = EQ  | NEQ | GTE | LTE | GT | LT

(* Simple nested expressions. *)
type cuda_expr =
  | Const of Int64.t
  | Var of cuda_ident
  | Unop of unop * cuda_expr
  | Binop of binop * cuda_expr * cuda_expr
  | Cmpop of cmpop * cuda_expr * cuda_expr
  | Address of cuda_expr
  | Deref of cuda_expr
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
              (* type ident = expr *)
  | Assign of cuda_type * cuda_ident * cuda_expr
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

type cuda_program = cuda_func list

let fmt_args args =
  (List.fold args ~init:"(" ~f:(fun str a -> str^", "^a))^")"

let str_depth n =
  String.concat (List.map (List.range 0 n) ~f:(fun _ ->"  "))

let rec fmt_typ = function
  | Integer -> "int"
  | Float -> "float"
  | Double -> "double"
  | Void -> "void"
  | Const t -> "const "^(fmt_typ t)
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

let rec fmt_expr = function
  | Const c -> Int64.to_string c
  | Var v -> v
  | Unop (u,e) -> 
    sprintf "(%s)%s" (fmt_expr e) (fmt_unop u)
  | Binop (b,e1,e2) -> 
    sprintf "(%s %s %s)" (fmt_expr e1) (fmt_binop b) (fmt_expr e2)
  | Cmpop (c,e1,e2) ->
    sprintf "(%s %s %s)" (fmt_expr e1) (fmt_cmpop c) (fmt_expr e2)
  | Address e -> sprintf "&(%s)" (fmt_expr e)
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
 | Assign (typ,id,exp) ->
   sprintf "%s %s %s = %s;" sp (fmt_typ typ) (id) (fmt_expr exp)

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

let print_program funcs = 
  List.map funcs ~f:(fun f -> prerr_endline (fmt_func f))
