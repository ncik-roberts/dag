{
module P = Parser
module L = Lexing

let commentLevel = ref 0

let error str = raise (Failure str)

let errorf lexbuf str =
  raise (Failure (Printf.sprintf str (L.lexeme lexbuf)))

let enterComment lexbuf = commentLevel := !commentLevel + 1

let exitComment () =
  commentLevel := !commentLevel - 1;
  !commentLevel = 0

let decnumber s lexbuf = match Int32.of_string_opt s with
  | Some i -> P.CONST i
  | None -> errorf lexbuf "cannot parse integral constant `%s`"

let hexnumber s lexbuf = match Int32.of_string_opt s with
  | Some i -> P.CONST i
  | None -> errorf lexbuf "cannot parse hexadecimal constant `%s`"

let floatnumber s lexbuf = match float_of_string_opt s with
  | Some i -> P.FLOATCONST i
  | None -> errorf lexbuf "cannot parse float constant `%s`"

let eof () =
  if !commentLevel > 0 then error "unterminated comment";
  P.EOF

}

let id = ['A'-'Z' 'a'-'z' '_']['A'-'Z' 'a'-'z' '0'-'9' '_']*
let decnum = ("0" | ['1'-'9'](['0'-'9']*))
let hexnum = "0"['x' 'X']['0'-'9' 'a'-'f' 'A'-'F']+
let fnum = decnum '.' (['0'-'9']*)
let ws = [' ' '\t' '\r' '\011' '\012']

rule initial =
  parse
  | ws+  { initial lexbuf }
  | '\n' { initial lexbuf }

  | '{' { P.LBRACE }
  | '}' { P.RBRACE }
  | '(' { P.LPAREN }
  | ')' { P.RPAREN }
  | '[' { P.LBRACKET }
  | ']' { P.RBRACKET }

  | '.' { P.DOT }
  | ',' { P.COMMA }
  | ';' { P.SEMICOLON }

  | '='  { P.ASSIGN }
  | "<-" { P.BIND }

  | '+'  { P.PLUS }
  | '-'  { P.MINUS }
  | '*'  { P.TIMES }
  | '/'  { P.DIV }
  | '%'  { P.MOD }
  | "<<" { P.LSHIFT }
  | ">>" { P.RSHIFT }
  | '<'  { P.LT }
  | "<=" { P.LTE }
  | '>'  { P.GT }
  | ">=" { P.GTE }
  | "&&" { P.LOGICAL_AND }
  | "||" { P.LOGICAL_OR }
  | '&'  { P.BITWISE_AND }
  | '|'  { P.BITWISE_OR }
  | '^'  { P.BITWISE_XOR }

  | "return"   { P.RETURN }
  | "parallel" { P.PARALLEL }
  | "struct"   { P.STRUCT }

  | "true"      { P.BOOLCONST true }
  | "false"     { P.BOOLCONST false }
  | decnum as n { decnumber n lexbuf }
  | fnum as n   { floatnumber n lexbuf }
  | hexnum as n { hexnumber n lexbuf }

  | id as name { P.IDENT name }

  | "/*" { enterComment lexbuf; comment lexbuf }
  | "*/" { error "unbalanced comments" }

  | "//" { comment_line lexbuf }
  | eof  { eof () }
  | _    { errorf lexbuf "illegal character: \"%s\"" }

and comment =
  parse
  | "/*" { enterComment lexbuf; comment lexbuf }
  | "*/" { (if exitComment () then initial else comment) lexbuf }
  | eof  { eof () }
  | _    { comment lexbuf }

and comment_line =
  parse
  | '\n' { initial lexbuf }
  | eof  { eof () }
  | _    { comment_line lexbuf }

{}
