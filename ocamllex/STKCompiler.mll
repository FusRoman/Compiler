{
  let nbarg = Array.length Sys.argv

  let src = Sys.argv.(1)
  let lexbuf = Lexing.from_channel (open_in src)

  let output_file =
    if nbarg >= 2 then
      Sys.argv.(1)
    else
      "a.asmr"
  let output = open_out output_file

  type pushable = 
    | Tag of string 
    | Int of string 
    | Reg of int

  exception Compilation_Error of int * string

  let fmt = Printf.sprintf
  (*let put = Printf.fprintf output*)

  let register_str num_reg =
    if num_reg < 0 || num_reg >= 32 then
      raise (Compilation_Error (0, fmt "Invalid register number: %d" num_reg))
    else
      "$r" ^ (string_of_int num_reg)

  (* 
    On a le contrôle complet des registres puisque STK n'en utilise pas.
    Pour éviter de s'emmêler, une petite liste des registres
    auxquels on assigne un rôle spécial. 
  *)
  (* Registre dans lequel on mettra l'adresse de là où on stocke 'stack_pointer' *)
  let spa = register_str 31

  (* Registre contenant la valeur de stack_pointer, autrement dit le sommet de la pile *)
  let sp = register_str 30

  (* Faudra peut-être modifier la signature *)
  let push pushable =
    let put_specific () =
      match pushable with
      | Tag s ->
        Printf.fprintf output "ADDRESS $r0 %s\n" s; 0
      | Int s ->
        Printf.fprintf output "CONST $r0 %s\n" s; 0
      | Reg x -> x
    in
    (*put "READ %s %s\n" sp spa;*)
    Printf.fprintf output "DECR %s\n" sp;
    Printf.fprintf output "WRITE %s %s\n" spa sp;
    let x = put_specific () in
    Printf.fprintf output "WRITE %s %s\n" sp (register_str x)

  let pop dest =
    let rec pop_rec l =
      match l with
      | [] -> 
        Printf.fprintf output "WRITE %s %s\n" spa sp
      | x::s ->
        Printf.fprintf output "READ %s %s\n" (register_str x) sp;
        Printf.fprintf output "INCR %s\n" sp;
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

let letter = ['a'-'z' 'A'-'Z' '_']
let tag = letter(letter|['0'-'9'])*
let integer = '-'?['0'-'9']+
let blank = [' ' '\t' '\n'] (* Il devrait y avoir un caractère spécial *)
let comment = '#'_*'\n'

rule dater = 
  parse
  | comment     { dater lexbuf }
  | blank       { dater lexbuf }
  | tag blank* ':' blank* integer {
    Printf.fprintf output "%s\n" (Lexing.lexeme lexbuf);
    dater lexbuf
  }
  | _           {
    raise (Compilation_Error (0, "Syntax error : only tag definitions are allowed after '.data'"))
  }

and texter = 
  parse
  | ".data"     { dater lexbuf }

  (* detection des espaces blanc *)
  | comment     { texter lexbuf }
  | blank       { texter lexbuf }

  (* rajouter des espaces blancs éventuels entre tag et : ? \b* *)
  | tag blank* ':'   { 
    Printf.fprintf output "%s\n" (Lexing.lexeme lexbuf);
    texter lexbuf 
  } 
  | tag         { 
    push (Tag (Lexing.lexeme lexbuf));
    texter lexbuf
  }
  | integer     { 
    push (Int (Lexing.lexeme lexbuf));
    texter lexbuf 
  }

  (*  instruction qui n'affecte pas la pile *)
  | "NOP"       { 
    Printf.fprintf output "NOP\n";
    texter lexbuf
  }
  | "EXIT"      { 
    Printf.fprintf output "EXIT\n";
    texter lexbuf
  }

  (* affiche le caractère au sommet de la pile *)
  | "PRINT"     { 
    pop [0];
    Printf.fprintf output "PRINT $r0\n";
    texter lexbuf
  }

  (* instructions mémoire *)
  | "READ"      {
    pop_n 2;
    Printf.fprintf output "READ $r0 $r1\n";
    push (Reg 0);
    texter lexbuf
  }

  | "WRITE"     {
    pop_n 2;
    Printf.fprintf output "WRITE $r0 $r1\n";
    texter lexbuf
  }

  (* instruction jump *)
  | "JUMP"      {
    pop_n 1;
    Printf.fprintf output "JUMP $r0\n";
    texter lexbuf
  }
  | "JUMPWHEN"  {
    pop_n 2;
    Printf.fprintf output "JUMPWHEN $r0 $r1\n";
    texter lexbuf
  }

  (* instructions unaires *)
  | "MINUS"     {  
    pop_n 1;
    Printf.fprintf output "MINUS $r0\n";
    push (Reg 0);
    texter lexbuf
  }
  | "NOT"       {
    pop_n 1;
    Printf.fprintf output "NEG $r0\n";
    push (Reg 0);
    texter lexbuf
  }

  (* instructions arithmétiques *)
  | "ADD" | "SUB" | "MULT" | "DIV"
  | "REM" | "EQ"  | "NEQ"  | "LT"
  | "LE"  | "GT"  | "GE"   | "AND"
  | "OR"        {
    pop_n 2;
    Printf.fprintf output "%s $r0 $r0 $r1" (Lexing.lexeme lexbuf);
    push (Reg 0);
    texter lexbuf
  }

  | _           {
    raise (Compilation_Error (0, "Syntax error lol"))
  }

and lexer =
  parse
  | ".text"   { texter lexbuf }
  | comment   { lexer lexbuf }
  | blank     { lexer lexbuf }
  | _         {raise (Compilation_Error (0, (fmt "Syntax error while looking for '.text'")))}

{
  let _ =
    Printf.fprintf output "ADDRESS %s stack_pointer\n" spa;
    try
      lexer lexbuf
    with
    | End_of_file ->
      (* si succès -> *)
      (* rajouter la ligne stack_pointer à 2^16 dans data *)
      Printf.fprintf output "stack_pointer: 65536\n";
      close_out output;
      Printf.printf "Compilation STK -> ASM successful (00:00:00)\n"
    | Compilation_Error (l, msg) ->
      Printf.printf "[ERROR] Line %d: %s\n%s\n" l (Lexing.lexeme lexbuf) msg
}