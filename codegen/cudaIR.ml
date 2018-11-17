
type cuda_type = Integer | Float | Double | Pointer of cuda_type

type cuda_mem_type = Host | Device

type cuda_ident = string

(* Basic arithmetic. *)
type binop = ADD | SUB | MUL | DIV
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

type grid_dim = int * int * int

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
              (* source, size *)
  | Allocate of cuda_ident * cuda_ident
              (* dest, source, size, transfer type *)
  | Transfer of cuda_ident * cuda_ident * cuda_ident * cuda_mem_type
    (* Launch dimension, blocks/thread, kernel, arguments *)
  | Launch of grid_dim * int * cuda_func * cuda_ident list

  | Free of cuda_ident
  | Sync

