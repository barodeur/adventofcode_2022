let parse_packet str =
  let lexbuf = Lexing.from_string str in
  try Some (Parser.packet Lexer.read lexbuf) with
  | Lexer.SyntaxError _ -> None
  | Parser.Error -> None
