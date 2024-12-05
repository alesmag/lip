{
  open Parser
}

let white = [' ' '\t']+
let num = ['0'-'9']

rule read_token =
  parse
  | white { read_token lexbuf }  
  | "S" { S }
  | "B" { B }
  | "/" { SLASH }
  | num { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }
