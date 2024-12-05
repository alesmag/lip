{
  open Parser
}

let white = [' ' '\t']+
let num = ['0'-'9']
let dots = ".."
let slash = "/"
let comma = ","

rule read_token =
  parse
  | white { WHITE }  
  | "S" { S }
  | "B" { B }
  | "E" { E }
  | comma { COMMA }
  | slash { SLASH }
  | dots { DOTS }
  | num { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }
