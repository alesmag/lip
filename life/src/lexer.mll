{
  open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let num = digit+

rule read_token =
  parse
  | white { read_token lexbuf }  
  | "S" { S }
  | "B" { B }
  | "/" { SLASH }
  | num { CONST (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }
