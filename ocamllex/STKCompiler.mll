{
  open Printf

  let nbarg = Array.length Sys.argv

  let src = Sys.argv.(1)
  let lexbuf = Lexing.from_channel (open_in src)

  let output_file =
    if nbarg >= 3 then
      Sys.argv.(2)
    else
      "a.asm"
  let output = open_out output_file


  type position = {
    lnum: int;
    line: string
  }

  exception Compilation_Error of position * string

  let first_position = {lnum = 1; line = ""}

  let next_line position = {
    lnum = position.lnum + 1;
    line = ""
  }

  let next_lexeme position =
    {position with line = position.line ^ (Lexing.lexeme lexbuf)}


  let fmt = sprintf
  (*let put = fprintf output*)

  let register_str num_reg =
    if num_reg < 0 || num_reg >= 16 then
      raise (Failure (fmt "Invalid register number: %d" num_reg))
    else
      "$r" ^ (string_of_int num_reg)


  (* 
    On a le contrôle complet des registres puisque STK n'en utilise pas.
    Pour éviter de s'emmêler, une petite liste des registres
    auxquels on assigne un rôle spécial. 
  *)
  (* Registre dans lequel on mettra l'adresse de là où on stocke 'stack_pointer' *)
  let spa = register_str 15

  (* Registre contenant la valeur de stack_pointer, autrement dit le sommet de la pile *)
  let sp = register_str 14

  (* Registre accumulateur *)
  let acc = register_str 13


  type pushable = 
    | Tag of string 
    | Int of string 
    | Reg of int

  (* Faudra peut-être modifier la signature *)
  let push pushable =
    let put_specific () =
      match pushable with
      | Tag s ->
        fprintf output "ADDRESS $r0 %s\n" s; 0
      | Int s ->
        fprintf output "CONST $r0 %s\n" s; 0
      | Reg x -> x
    in
    (*put "READ %s %s\n" sp spa;*)
    fprintf output "DECR %s 1\n" sp;
    let x = put_specific () in
    fprintf output "WRITE %s %s\n" sp (register_str x);
    fprintf output "WRITE %s %s\n" spa sp

  let pop dest =
    let rec pop_rec l =
      match l with
      | [] -> 
        fprintf output "WRITE %s %s\n" spa sp
      | x::s ->
        fprintf output "READ %s %s\n" (register_str x) sp;
        fprintf output "INCR %s 1\n" sp;
        pop_rec s
    in
    (*put "READ %s %s\n" sp spa;*)
    pop_rec dest

  let pop_n n =
    let rec make_list i acc =
      if i >= n then
        acc
      else
        make_list (i+1) (i::acc)
    in
    pop (make_list 0 [])
}

let letter = ['a'-'z' '_'] (* comprenait initialement A-Z, 
    mais les commandes avec fautes de frappes seraient alors interprétées comme des tags *)
let tag = letter(letter|['0'-'9'])*
let integer = '-'?['0'-'9']+
let blank = [' ' '\t'] (* Il devrait y avoir un caractère spécial *)
let comment = '#'_*'\n'

rule dater_tag pos =
  parse
  | comment     { dater_tag (next_lexeme pos) lexbuf }
  | blank       { dater_tag (next_lexeme pos) lexbuf }
  | ':'         {
    error pos "Syntax error: unexpected ':'" lexbuf
  }
  | tag         {
    let tag = Lexing.lexeme lexbuf in
    fprintf output "%s:\n" tag;
    dater_points (next_lexeme pos) tag lexbuf
  }
  | integer     {
    error pos (fmt "Syntax error: found `%s` without a tag" (Lexing.lexeme lexbuf)) lexbuf
  }
  | '\n'        { dater_tag (next_line pos) lexbuf }
  | eof         {}
  | _           { 
    error (next_lexeme pos) "Syntax error: only tag definitions are allowed after '.data'" lexbuf
  }

and dater_points pos previous =
  parse
  | comment     { dater_points (next_lexeme pos) previous lexbuf }
  | blank       { dater_points (next_lexeme pos) previous lexbuf }
  | ':'         { dater_data (next_lexeme pos) previous lexbuf }
  | tag         {
   error (next_lexeme pos) (fmt "Syntax error: tag '%s' was not given a value" previous) lexbuf
  }
  | integer     {
    error (next_lexeme pos) (fmt "Syntax error: found data '%s' directly after tag '%s'. You may have forgotten ':'." (Lexing.lexeme lexbuf) previous) lexbuf
  }
  | '\n'        { dater_points (next_line pos) previous lexbuf }
  | eof         {
    raise (Compilation_Error(pos, fmt "Syntax error: tag '%s' was not given a value" previous))
  }
  | _           { 
    error (next_lexeme pos) (fmt "Syntax error while looking for ':' after declaration of tag '%s'" previous) lexbuf
  }

and dater_data pos previous =
  parse
  | comment     { dater_data (next_lexeme pos) previous lexbuf }
  | blank       { dater_data (next_lexeme pos) previous lexbuf }
  | ':'         { 
    error (next_lexeme pos) "Syntax error: duplicate ':'" lexbuf
  }
  | tag         {
   error (next_lexeme pos) (fmt "Syntax error: found tag '%s', expected a value" (Lexing.lexeme lexbuf)) lexbuf
  }
  | integer     {
    fprintf output "%s\n" (Lexing.lexeme lexbuf);
    dater_tag (next_lexeme pos) lexbuf
  }
  | '\n'        { dater_data (next_line pos) previous lexbuf }
  | eof         {
    raise (Compilation_Error(pos, fmt "Syntax error: tag '%s' was not given a value" previous))
  }
  | _           { 
    error (next_lexeme pos) (fmt "Syntax error while looking for the value of tag '%s'" previous) lexbuf
  }

and texter pos = 
  parse
  | ".data"     { dater_tag (next_lexeme pos) lexbuf }

  (* detection des espaces blanc *)
  | comment     { texter (next_lexeme pos) lexbuf }
  | blank       { texter (next_lexeme pos) lexbuf }
  | '\n'        { texter (next_line pos) lexbuf }

  (*  instruction qui n'affecte pas la pile *)
  | "NOP"       { 
    fprintf output "NOP\n";
    texter (next_lexeme pos) lexbuf
  }
  | "EXIT"      { 
    fprintf output "EXIT\n";
    texter (next_lexeme pos) lexbuf
  }

  (* affiche le caractère au sommet de la pile *)
  | "PRINT"     { 
    pop [0];
    fprintf output "PRINT $r0\n";
    texter (next_lexeme pos) lexbuf
  }

  (* instructions mémoire *)
  | "READ"      {
    pop_n 1;
    fprintf output "READ $r1 $r0\n";
    push (Reg 1);
    texter (next_lexeme pos) lexbuf
  }

  | "WRITE"     {
    fprintf output "# - THE write - begin ------------------------------------\n";
    pop_n 2;
    fprintf output "WRITE $r0 $r1\n";
    fprintf output "# - THE write - end --------------------------------------\n";
    texter (next_lexeme pos) lexbuf
  }

  (* instruction jump *)
  | "JUMP"      {
    pop_n 1;
    fprintf output "JUMP $r0\n";
    texter (next_lexeme pos) lexbuf
  }
  | "JUMPWHEN"  {
    pop_n 2;
    fprintf output "JUMP $r0 WHEN $r1\n";
    texter (next_lexeme pos) lexbuf
  }

  (* instructions unaires *)
  | "MINUS"     {  
    pop_n 1;
    fprintf output "MINUS $r0\n";
    push (Reg 0);
    texter (next_lexeme pos) lexbuf
  }
  | "NOT"       {
    pop_n 1;
    fprintf output "NEG $r0\n";
    push (Reg 0);
    texter (next_lexeme pos) lexbuf
  }

  (* instructions arithmétiques *)
  | "ADD" | "SUB" | "MULT" | "DIV"
  | "REM" | "EQ"  | "NEQ"  | "LT"
  | "LE"  | "GT"  | "GE"   | "AND"
  | "OR"        {
    pop_n 2;
    fprintf output "%s $r0 $r0 $r1\n" (Lexing.lexeme lexbuf);
    push (Reg 0);
    texter (next_lexeme pos) lexbuf
  }

  (* rajouter des espaces blancs éventuels entre tag et : ? \b* *)
  | tag blank* ':'   { 
    fprintf output "%s\n" (Lexing.lexeme lexbuf);
    texter (next_lexeme pos) lexbuf 
  } 
  | tag         {
    push (Tag (Lexing.lexeme lexbuf));
    texter (next_lexeme pos) lexbuf
  }
  | integer     { 
    push (Int (Lexing.lexeme lexbuf));
    texter (next_lexeme pos) lexbuf 
  }

  | eof         {
    (* Ce n'est pas forcément une erreur de compilation *)
  }

  | _           { 
    error (next_lexeme pos) "Syntax error" lexbuf
  }

and lexer pos =
  parse
  | ".text"   { texter (next_lexeme pos) lexbuf }
  | comment   { lexer (next_lexeme pos) lexbuf }
  | blank     { lexer (next_lexeme pos) lexbuf }
  | '\n'      { lexer (next_line pos) lexbuf }
  | eof       {
    raise (Compilation_Error (pos, "Reached unexpected end of file"))
  }
  | _         { 
    error (next_lexeme pos) (fmt "Syntax error while looking for '.text'") lexbuf
  }

and error pos msg =
  parse
  | '\n'      
  | eof       { raise (Compilation_Error(pos, msg)) }
  | _         { 
    error (next_lexeme pos) msg lexbuf
  }


{
  let _ =
    let beginning = Sys.time () in
    printf "Compiling %s...\n" src; 
    fprintf output "ADDRESS %s stack_pointer\n" spa;
    fprintf output "READ %s %s\n" sp spa;
    try
      lexer first_position lexbuf;
      (* si succès -> *)
      fprintf output "stack_pointer:\n65536";
      close_out output;
      printf "Compilation STK -> ASM successful (%fs)\n" (Sys.time () -. beginning)
    with
    | Compilation_Error (pos, msg) ->
      printf "[ERROR] Line %d:\n%s\n%s\n" pos.lnum pos.line msg
    | Failure msg ->
      printf "[ERROR] The compilation failed. Error : %s\n" msg
}