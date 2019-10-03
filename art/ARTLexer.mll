{

  open Lexing
  open ARTParser

}

let letter = ['a'-'z' 'A'-'Z' '_']
let id = letter(letter|['0'-'9'])*
let integer = '-'?['0'-'9']+
let boolean = "true"|"false"
let blank = [' ' '\t' '\r'] (* Il devrait y avoir un caractère spécial *)
let comment = '#'[^'\n']*'\n'
let unop = '-'|'!'

  
rule token = parse
  | ['\n']
      { new_line lexbuf; token lexbuf }
  | blank+
      { token lexbuf }
  | integer as n
      { INT (int_of_string n) }
  | ".text"
      { TEXT }
  | ".data"
      { DATA }
  | "print"
      { PRINT }
  | "exit"
      { EXIT }

  | "jump"
      { JUMP }

  | "when"
      { WHEN }

  | "nop"
      { NOP }

  | ":="
      { AFFECT }

  | ";"
      { SEMI }
  | "("
      { LP }
  | ")"
      { RP }

  | id as s
      { ID (s) }

  | boolean as b
      { BOOL (bool_of_string b) }

  | '+'
      { ADD }

  | '*'
      { MUL }

  | '/'
      { DIV }

  | '-'
      { SUB }

  | "=="
      { EQUAL }

  | "!="
      { NOT_EQUAL }

  | '>'
      { SUP }

  | '<'
      { INF }

  | "<="
      { INF_EQUAL }

  | ">="
      { SUP_EQUAL }

  | '%'
      { MOD }

  | "&&"
      { AND }

  | "||"
      { OR }

  | unop as s
      { UNOP (s) }

  | ':'
      { TWO_POINT }

  | '*'
      { LEFT_EXPR_STAR }

  | _
      { failwith ("Unknown character : " ^ (lexeme lexbuf)) }
  | eof
      { EOF }