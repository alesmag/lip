{
  open Token
}

let white = [' ' '\t']+
let letter = ['a'-'z' 'A'-'Z']
let chr = ['a'-'z' 'A'-'Z' '0'-'9']
let num = ['0'-'9']|['1'-'9']['0'-'9']*
let to_upper = ['A'-'Z']
let lower_vowel = ['a' 'e' 'i' 'o' 'u']
let vowel = ['a' 'e' 'i' 'o' 'u' 'A' 'E' 'I' 'O' 'U']
let consonant = ['b'-'d' 'f'-'h' 'j'-'n' 'p'-'t' 'v'-'z'
                 'B'-'D' 'F'-'H' 'J'-'N' 'P'-'T' 'V'-'Z']
let minus = ['-']
let number = ['0'-'9']
let point = ['.']
let hex_start = "0x" | "0X"
let hex_number = ['0'-'9' 'A'-'F' 'a'-'f']+

let atok = to_upper chr*
let btok = lower_vowel+
let ctok = consonant* vowel? consonant*
let dtok = minus? number* (point number*)?
let etok = hex_start hex_number+
let id = letter chr*


rule read_token =
  parse
  | white { read_token lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "=" { ASSIGN }
  | "+" { PLUS }
  | ";" { SEQ }  
  | atok { ATOK (Lexing.lexeme lexbuf) }
  | btok { BTOK (Lexing.lexeme lexbuf) }
  | ctok { CTOK (Lexing.lexeme lexbuf) }
  | dtok { DTOK (Lexing.lexeme lexbuf) }
  | etok { ETOK (Lexing.lexeme lexbuf) }
  | id { ID (Lexing.lexeme lexbuf) }
  | num { CONST (Lexing.lexeme lexbuf) }    
  | eof { EOF }
