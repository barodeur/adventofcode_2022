{
open Parser

exception SyntaxError of string
}

let digit = ['0'-'9']
let white = [' ' '\t']+

rule read =
  parse
  | white    { read lexbuf }
  | digit+   { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | '['      { LEFT_BRACKET }
  | ']'      { RIGHT_BRACKET }
  | ','      { COMMA }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof      { EOF }
