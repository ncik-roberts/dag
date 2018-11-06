open Core

let parse_lexbuf (lexbuf : Lexing.lexbuf) : Ast.t =
  try
    Parser.fun_defns Lexer.initial lexbuf
  with
  | Parsing.Parse_error -> raise (Failure "Parse error.")
  | Sys_error s -> raise (Failure ("System error: " ^ s))

let parse_file (file_name : string) : Ast.t =
  In_channel.with_file file_name ~f:(Fn.compose parse_lexbuf Lexing.from_channel)

let parse (input : string) : Ast.t =
  parse_lexbuf (Lexing.from_string input)
