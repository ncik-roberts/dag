
type temp = int
type op = ADD | SUB | MUL | DIV | MOD 

type array_view = 
  | Array of temp
  | ZipWith of op * temp list

type par_stm =
  | Parallel of (temp * temp) list * seq_stm list 
  | For of temp * (par_stm list)
  | Reduce of temp * op * array_view
  | Run of array_view
  | Seq of seq_stm 

and seq_stm = 
  | Binop 
  | Unop 
  | Etc