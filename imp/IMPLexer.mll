{
  open IMPParser
  
}

let digit = ['0'-'9']
let number = digit+
let label = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let comment = '#' [^ '\n']* ('\n' | eof)
  
rule token = parse
  | ".text"
      { TEXT }
  | ".data"
      { DATA }
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
  | "goto"
      { GOTO }
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
  | "stack_pointer"
      { STACKPOINTER }

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

  | "="
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

  | ";"
      { SEMI }
  | ":"
      { COLON }
  | "("
      { LP }
  | ")"
      { RP }
  | "{"
      { LB }
  | "}"
      { RB }

  | label as lab
      { LABEL(lab) }
  | number as n
      { INT(int_of_string n) }

  | ['\n']
      { new_line lexbuf; token lexbuf }
  | comment
      { new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']+
      { token lexbuf }
  | _
      { failwith ("Unknown character : " ^ (lexeme lexbuf)) }
  | eof
      { EOF }
