{
  let nbarg = Array.length Sys.argv

  let src = Sys.argv.(1)
  let lexbuf = Lexing.lexing_from_channel (open_in src)



  let output_file =
    if nbarg >= 2 then
      Sys.argv.(2)
    else
      "a.out"
  in
  let output = open_out output_file

  exception Compilation_Error of int * string

  type lol = int * string
  let xd = (6, "coucou")

  let fmt = Printf.sprintf

  let register_str num_reg =
    if num_reg < 0 || num_reg >= 32 then
      raise (Compilation_Error (0 (*num_line*), fmt "Invalid register number: %d" num_reg))
    else
      "$r" ^ (string_of_int num_reg)

  (* 
    On a le contrôle complet des registres puisque STK n'en utilise pas.
    Pour éviter de s'emmêler, une petite liste des registres
    auxquels on assigne un rôle spécial. 
  *)
  (* Registre dans lequel on mettra la valeur de la zone mémoire 'stack_pointer' *)
  let sp_reg = register_str 31

  (* Faudra peut-être modifier la signature *)
  let push output =
    ()

  let pop output dest = ()
}

let letter = ['a'-'z' 'A'-'Z' '_']

let id = letter(letter|['0'-'9'])*

let tag = id:


rule lexer output =
  parse
  | ".text"   { }
  | ".data"
  | tag     {  (* empiler *)  }
  | number  { (* empiler *) }


  (*  instruction qui n'affecte pas la pile *)
  | "NOP"     {  }
  | "EXIT"    {  }

  (* affiche le caractère au sommet de la pile *)
  | "PRINT" {  }


  (* instructions mémoire *)
  | "READ" {  }
  | "WRITE" {  }

  (* instruction jump *)
  | "JUMP" {  }
  | "JUMPWHEN" {  }

  (* instructions unaires *)
  | "MINUS" {  }
  | "NOT" {  }

  (* instructions arithmétiques *)
  | "ADD" {  }
  | "SUB" {  }
  | "MULT"  {  }
  | "DIV" {  }
  | "REM" {  }

  (* instructions logiques *)
  | "EQ"  {  }
  | "NEQ" {  }
  | "LT"  {  }
  | "LE"  {  }
  | "GT"  {  }
  | "GE"  {  }
  | "AND" {  }
  | "OR"  {  }

  (* règles diverses *)
  | ^'#''.*$ {}
  | ^\b*'\n'$ {}

{
  let _ =
    Printf.printf "CompilatiPrintf.sprintfrors do not display the actual line number. Please ignore it for now.\n";
    try
      while true do
        lexer output
      done
    with
    | End_of_file ->
      Printf.printf "CompilaPrintf.sprintfsuccessful in 00:00:00\n"
    | Compilation_Error (l, Printf.sprintf->
      Printf.printf "[ERROR]Printf.sprintf %d: %s\n" l msg
}