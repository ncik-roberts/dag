/*
 * DAG parser
 *
 * Dan Cascaval
 * Nick Roberts
 * Carnegie Mellon University
 */

%token <string> IDENT
%token <int32> CONST
%token <float> FLOATCONST

// Operators
// %token DOT ARROW
%token TIMES DIV MOD
%token PLUS MINUS
%token LSHIFT RSHIFT
%token LT GT LTE GTE
// %token EQUALS NOT_EQUALS
%token BITWISE_AND
%token BITWISE_XOR
%token BITWISE_OR
%token LOGICAL_AND
%token LOGICAL_OR

%token ASSIGN BIND
%token SEMICOLON COMMA

%token PARALLEL
%token RETURN

%token LBRACE RBRACE /* {} */
%token LBRACKET RBRACKET /* [] */
%token LPAREN RPAREN /* () */

%token EOF

// Precedences
%left LOGICAL_OR
%left LOGICAL_AND
%left BITWISE_OR
%left BITWISE_XOR
%left BITWISE_AND
%left LT LTE GT GTE
%left LSHIFT RSHIFT
%left PLUS MINUS
%left TIMES DIV MOD
%nonassoc UNARY_MINUS

%type <unit Ast.fun_defn list> fun_defns
%start fun_defns

%%

/**********************************
 * FUN DEFNS
 **********************************/

fun_defns :
  | EOF;
      { [] }
  | f = fun_defn;
    fs = fun_defns;
      { f :: fs }
  ;

fun_defn :
  | fun_ret_type = typ;
    fun_name = IDENT;
    LPAREN;
    fun_params = params;
    RPAREN;
    LBRACE;
    fun_body = stmts;
    RBRACE;
      { Ast.{ fun_ret_type; fun_name; fun_params; fun_body; } }
  ;

/**********************************
 * PARAMS
 **********************************/

param :
  | param_type = typ;
    param_ident = IDENT;
      { Ast.{ param_type; param_ident; } }
  ;

param_tail :
  | /* empty */
      { [] }
  | COMMA;
    p = param;
    ps = param_tail;
      { p :: ps }
  ;

params :
  | /* empty */
      { [] }
  | first_param = param;
    other_params = param_tail;
      { first_param :: other_params }
  ;

/**********************************
 * ARGUMENTS
 **********************************/

arg :
  | b = binop;
      { Ast.Bare_binop ((), b) }
  | e = expr;
      { Ast.Expr e }

arg_tail :
  | /* empty */
      { [] }
  | COMMA;
    argument = arg;
    arguments = arg_tail;
      { argument :: arguments }
  ;

args :
  | /* empty */
      { [] }
  | first_argument = arg;
    other_arguments = arg_tail;
      { first_argument :: other_arguments }
  ;

/**********************************
 * STATEMENTS
 **********************************/

stmts :
  | /* empty */
      { [] }
  | s = stmt;
    SEMICOLON;
    ss = stmts;
      { s :: ss }
  ;

stmt :
  | let_type = typ;
    let_ident = IDENT;
    ASSIGN;
    let_expr = expr;
      { Ast.Let { let_type; let_ident; let_expr; } }
  | RETURN;
    ret_expr = expr;
      { Ast.Return ret_expr }
  ;

/**********************************
 * EXPRESSIONS
 **********************************/

expr :
  | e = _expr;
      { ((), e) }
  ;

_expr :
  | i = CONST;
      { Ast.Const i }
  | i = FLOATCONST;
      { Ast.Float i }
  | ident = IDENT;
      { Ast.Variable ident }
  | call_ident = IDENT;
    LPAREN;
    call_args = args;
    RPAREN;
      { let call_name = match call_ident with
          | "reduce" -> Ast.Reduce
          | "transpose" -> Ast.Transpose
          | "map" -> Ast.Map
          | "max" -> Ast.Max
          | "min" -> Ast.Min
          | "zipWith" -> Ast.Zip_with
          | "tabulate" -> Ast.Tabulate
          | "float_of_int" -> Ast.Float_of_int
          | "int_of_float" -> Ast.Int_of_float
          | s ->
            let open Core in
            let dim =
              if String.is_prefix s ~prefix:"dim"
                then int_of_string_opt (String.suffix s (String.length "dim"))
                else None
            in Option.value_map dim ~f:(fun n -> Ast.Dim n)
                  ~default:(Ast.Fun_ident call_ident)
        in Ast.Fun_call { call_name; call_args; } }
  | unary_operator = unop;
    unary_operand = expr;
      %prec UNARY_MINUS
      { Ast.Unop { unary_operator; unary_operand; } }
  | binary_operand1 = expr;
    binary_operator = binop;
    binary_operand2 = expr;
      { Ast.Binop { binary_operand1; binary_operator; binary_operand2; } }
  | PARALLEL;
    LPAREN;
    parallel_type = typ;
    parallel_ident = IDENT;
    BIND;
    parallel_arg = expr;
    RPAREN;
    LBRACE;
    parallel_body = stmts;
    RBRACE;
      { Ast.Parallel { parallel_arg; parallel_type; parallel_ident; parallel_body; } }
  ;

unop :
  | MINUS;
      { Ast.Negate }
  ;

%inline
binop :
  | PLUS;
      { Ast.Plus }
  | MINUS;
      { Ast.Minus }
  | TIMES;
      { Ast.Times }
  | DIV;
      { Ast.Div }
  | MOD;
      { Ast.Mod }
  | LSHIFT; 
      { Ast.Lshift }
  | RSHIFT;
      { Ast.Rshift }
  | LOGICAL_AND;
      { Ast.And }
  | LOGICAL_OR;
      { Ast.Or }
  | BITWISE_OR;
      { Ast.BitOr }
  | BITWISE_AND;
      { Ast.BitAnd }
  | BITWISE_XOR;
      { Ast.BitXor }
  | LT;
      { Ast.Less }
  | LTE;
      { Ast.LessEq }
  | GT;
      { Ast.Greater }
  | GTE;
      { Ast.GreaterEq }
  ;

/**********************************
 * TYPES
 **********************************/

typ :
  | t = _typ; { ((), t) }
  ;

_typ :
  | ident = IDENT;
      { Ast.Ident ident }
  | t = _typ;
    LBRACKET;
    RBRACKET;
      { Ast.Array t }
  ;
