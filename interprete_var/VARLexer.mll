{

  open Lexing
  open VARParser

  let keyword_or_label =
    let h = Hashtbl.create 17 in
    List.iter (fun (s, k) -> Hashtbl.add h s k)
      [ "nop",    NOP;
        "print",  PRINT;
        "exit",   EXIT;
        "if",     IF;
        "else",   ELSE;
        "while",  WHILE;
        "var",    VAR;
        "return", RETURN;
        "true",   BOOL true;
        "false",  BOOL false;
      ] ;
    fun s ->
      try  Hashtbl.find h s
      with Not_found -> LABEL(s)
        
}

let digit = ['0'-'9']
let number = digit+
let alpha = ['a'-'z' 'A'-'Z']
let label = ['a'-'z' '_'] (alpha | '_' | digit)*
  
rule token = parse
  | ['\n']
      { new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']+
      { token lexbuf }
  | "#" [^ '\n']* "\n"
      { new_line lexbuf; token lexbuf }
      
  | number as n
      { INT (int_of_string n) }
  | label as lab
      { keyword_or_label lab }
      
  | ";"
      { SEMI }
  | ","
      { COMMA }
  | ":="
      { SET }
  | "+"
      { PLUS }
  | "-"
      { MINUS }
  | "*"
      { STAR }
  | "/"
      { SLASH }
  | "%"
      { PRCT }
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
  | "!"
      { NOT }
  | "&&"
      { AND }
  | "||"
      { OR }
  | "("
      { LP }
  | ")"
      { RP }
  | "{"
      { BEGIN }
  | "}"
      { END }
  | _
      { failwith ("Unknown character : " ^ (lexeme lexbuf)) }
  | eof
      { EOF }
