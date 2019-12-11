{
  open TYPParser
}

let digit = ['0'-'9']
let number = digit+
let label = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let comment = '#' [^ '\n']* ('\n' | eof)
  
rule token = parse
  | "/*"
      { comment lexbuf; token lexbuf }
  | "extends"
    { EXTENDS }
  | "var"
      { VAR }
  | "int"
      { TINT }
  | "bool"
      { TINT }
  | "char"
      { TINT }
  | "string"
      { TSTRING }
  | "fun"
      { TFUN }
  | "type"
      { TYPE }
  | "true"
      { BOOL true }
  | "false"
      { BOOL false }
  | "nop"
      { NOP }
  | "print"
      { PRINT }
  | "exit"
      { EXIT }
  | "return"
      { RETURN }
  | "if"
      { IF }
  | "else"
      { ELSE }
  | "while"
      { WHILE }
  | "for"
      { FOR }
  | "continue"
      { CONTINUE }
  | "break"
      { BREAK }
  | "="
      { SEQ }
  | "<>"
      { NSEQ }
  | "->"
      { ARROW }
  | ":="
      { ASSIGN }
  | "++"
      { INCR }
  | "--"
      { DECR }
  | "+="
      { ADDASSIGN }
  | "-="
      { SUBASSIGN }
  | "*="
      { MULTASSIGN }
  | "/="
      { DIVASSIGN }
  | "=="
      { EQ }
  | "!="
      { NEQ }
  | "<"
      { LT }
  | "<="
      { LE }
  | ">"
      { GT }
  | ">="
      { GE }
  | "+"
      { ADD }
  | "-"
      { SUB }
  | "*"
      { MULT }
  | "/"
      { DIV }
  | "%"
      { REM }
  | "&&"
      { AND }
  | "||"
      { OR }

  | "!"
      { NOT }
  | "~"
      { CPL }
  | "&"
      { ADDRESS }

  | ";"
      { SEMI }
  | ":"
      { COLON }
  | ","
      { COMMA }
  | "|"
      { PIPE }
  | "("
      { LP }
  | ")"
      { RP }
  | "{"
      { LB }
  | "}"
      { RB }
  | "["
      { LS }
  | "]"
      { RS }
  | "'" (_ as c) "'"
      { INT (int_of_char c) }
  | "."
      { DOT }

  | label as lab
      { LABEL(lab) }
  | number as n
      { INT(int_of_string n) }
  | ['\n']
      { Lexing.new_line lexbuf; token lexbuf }
  | comment
      { Lexing.new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']+
      { token lexbuf }
  | eof
      { EOF }
  | _
      { failwith ("Unknown character : " ^ (Lexing.lexeme lexbuf)) }

(* Commentaires imbriqués *)
and comment = parse
  | "*/"  {()}
  | "/*"  {comment lexbuf; comment lexbuf}
  | '\n'  {Lexing.new_line lexbuf; comment lexbuf}
  | _     {comment lexbuf}
  | eof   {failwith ("Reached end of file while still parsing comment")}
