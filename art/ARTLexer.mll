{

  open Lexing
  open ARTParser

}

let letter = ['a'-'z' 'A'-'Z' '_']
let id = letter(letter|['0'-'9'])*
let integer = ['0'-'9']+
let boolean = "true"|"false"
let blank = [' ' '\t' '\r'] (* Il devrait y avoir un caractère spécial *)
let comment = '#' [^ '\n']* ('\n' | eof)

  
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

  | comment
    { token lexbuf }

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

  | boolean as b
    { BOOL (bool_of_string b) }

  | id as s
      { ID (s) }

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

  | '!'
      { NOT }

(* Opérateur permettant d'effectuer un not binaire ( complémentaire ) *)
  | '~'
      { CPL }

(* Ajout de l'opérateur permettant d'aller chercher l'adresse d'une variable et de l'affecter eventuellement
a un pointeur.*)
  | '&'
      { ADRESS }

  | ':'
      { TWO_POINT }
      
  | _
      { failwith ("Unknown character : " ^ (lexeme lexbuf)) }
  | eof
      { EOF }
