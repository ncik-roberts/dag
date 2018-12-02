open Core

let parse_lexbuf (lexbuf : Lexing.lexbuf) : unit Ast.t =
  try
    Parser.global_stms Lexer.initial lexbuf
  with
  | Parsing.Parse_error -> failwith "Parse error."
  | Sys_error s -> failwith ("System error: " ^ s)

let parse_file (file_name : string) : unit Ast.t =
  In_channel.with_file file_name ~f:(Fn.compose parse_lexbuf Lexing.from_channel)

let parse (input : string) : unit Ast.t =
  parse_lexbuf (Lexing.from_string input)
