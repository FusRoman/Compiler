{
  open Printf

  let nbarg = Array.length Sys.argv

  let src = Sys.argv.(1)
  let lexbuf = Lexing.from_channel (open_in src)
  let _ =
    if not (Filename.check_suffix src ".stk") then
      failwith "expected .stk extension"

  let output_file =
    if nbarg >= 3 then
      Sys.argv.(2)
    else
      (Filename.chop_suffix src ".stk") ^ ".asm"
  let output = open_out output_file


  type position = {
    lnum: int;
    line: string;
    acc_empty: bool
  }

  exception Compilation_Error of position * string

  let first_position = {lnum = 1; line = ""; acc_empty = true}

  let next_line position = {
    lnum = position.lnum + 1;
    line = "";
    acc_empty = position.acc_empty
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
    Liste des registres spéciaux
  *)
  (* Registre dans lequel on mettra l'adresse de là où on stocke 'stack_pointer' *)
  let spa = register_str 15
  (* Registre contenant la valeur de stack_pointer, autrement dit le sommet de la pile *)
  let sp = register_str 14
  (* 
    Comme spa n'est jamais modifié, on peut n'y mettre l'adresse de stack_pointer qu'une seule fois, au début du programme. 
    De même, sp n'est jamais modifié en dehors du code pour la gestion de la pile. 
    On n'aura donc jamais besoin de le mettre à jour avec la valeur pointée par spa, sauf une fois au début.
    Il est par contre nécessaire de mettre à jour stack_pointer afin que les langages plus haut niveau puissent l'utiliser.
    On peut quand même faire économiser les 'WRITE spa sp' en remarquant que le seul moyen qu'a le STK d'accéder à sa valeur
    est d'empiler d'abord stack_pointer, puis READ. Il peut y avoir des étapes intermédiaires (comme tag; READ; PRINT),
    donc on ne peut pas juste tester la séquence 'stack_pointer; READ'. Un 'WRITE spa sp' par READ du STK, 
    bien qu'étant non nécessaire dans certains cas, représente déjà une amélioration satisfaisante.

    Si la pile déborde et finit par inclure stack_pointer, alors stack_pointer ne sera certes plus à jour,
    mais le programme aurait été dysfonctionnel quand bien même on mette des 'WRITE spa sp' à la fin de chaque push et pop.
    Donc peu importe dans ce cas.
  *)

  (* Registre accumulateur *)
  let acc = register_str 13


  type pushable = 
    | Tag of string 
    | Int of string 
    | Reg of int

  let push pushable pos =
    let fill_acc () =
        match pushable with
        | Tag s ->
          fprintf output "ADDRESS %s %s\n" acc s
        | Int s ->
          fprintf output "CONST %s %s\n" acc s
        | Reg x ->
          fprintf output "MOVE %s %s\n" acc (register_str x)
    in
    let _ =
      if pos.acc_empty then
        fill_acc ()  
      else
      begin
        fprintf output "WRITE %s %s\n" sp acc;
        fill_acc ()
      end
    in
    fprintf output "DECR %s 1\n" sp;
    {pos with acc_empty = false}

  (*
    Utilisé par les instructions qui prennent un élément sur la pile et n'en remettent pas (PRINT et JUMP).
    Diminue la pile de 1 et met le nouveau sommet dans acc.
    A appeler après avoir utilisé acc comme l'ancien sommet de la pile, donc à la fin en général.
    Pour les instructions à 1 argument et retournant un résultat, il suffit de stocker le résultat dans acc ;
    aucun appel à une fonction pop n'est nécessaire dans ce cas.
  *)
  (* Est-ce que j'aurais fait l'INCR et le READ dans le mauvais sens ? *)
  let pop1 () =
    fprintf output "INCR %s 1\n" sp;
    fprintf output "READ %s %s\n" acc sp

  (* 
    Utilisé par les instructions qui prennent deux éléments sur la pile et n'en remettent pas (WRITE et JUMPWHEN).
    Procède en plusieurs étapes :
    - récupère dans le registre dest1 l'avant-dernier élément de la pile, le dernier étant acc.
      dest1 correspondra au premier argument, acc au deuxième.
    - appelle instr de type string -> unit : la chaîne donnée est la représentation du registre dest1.
      Le but de cette fonction est d'écrire sur le fichier les instructions ASM correspondant à l'instruction STK
      qu'on considère. Au moment de l'appel, sp pointera vers le nouveau sommet de la pile.
    - On met à jour acc avec la nouvelle valeur du sommet de la pile.
  *)
  let pop2_no_return dest1 instr =
    let dest1 = register_str dest1 in
    fprintf output "INCR %s 1\n" sp;
    fprintf output "READ %s %s\n" dest1 sp;
    fprintf output "INCR %s 1\n" sp;
    instr dest1;
    fprintf output "READ %s %s\n" acc sp

  (*
    Utilisé par les instructions qui prennent deux éléments sur la pile et en remettent un.
    Récupère dans le registre dest1 l'avant-dernier élément de la pile.
    Après l'appel, sp pointera non pas vers le sommet de la pile, mais un élément plus bas ;
    de cette façon, après que les instructions à rajouter aient stocké leur résultat dans acc,
    sp pointera de nouveau le sommet de la pile sans avoir été modifié.
    Retourne la représentation du registre dest1.
  *)
  let pop2_return dest1 =
    let dest1 = register_str dest1 in
    fprintf output "INCR %s 1\n" sp;
    fprintf output "READ %s %s\n" dest1 sp;
    dest1
}

let letter = ['a'-'z' '_'] (* comprenait initialement A-Z, 
    mais les commandes avec fautes de frappes seraient alors interprétées comme des tags *)
let tag = letter(letter|['0'-'9'])*
let integer = '-'?['0'-'9']+
let blank = [' ' '\t'] (* Il devrait y avoir un caractère spécial *)
let comment = '#'[^'\n']*'\n'


(* Appelé en entrant dans la section .data. Appelle data_points dès qu'un tag est trouvé. *)
rule dater_tag pos =
  parse
  | comment     { dater_tag (next_line pos) lexbuf }
  | blank       { dater_tag (next_lexeme pos) lexbuf }
  | '\n'        { dater_tag (next_line pos) lexbuf }

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

  | eof         {}

  | _           { 
    error (next_lexeme pos) "Syntax error: only tag definitions are allowed after '.data'" lexbuf
  }


(* Appelé par dater_tag dès qu'un tag est trouvé. Appelle dater_data dès que ':' est trouvé. *)
and dater_points pos previous =
  parse
  | comment     { dater_points (next_line pos) previous lexbuf }
  | blank       { dater_points (next_lexeme pos) previous lexbuf }
  | '\n'        { dater_points (next_line pos) previous lexbuf }
  | ':'         { dater_data (next_lexeme pos) previous lexbuf }

  | tag         {
   error (next_lexeme pos) (fmt "Syntax error: tag '%s' was not given a value" previous) lexbuf
  }

  | integer     {
    error (next_lexeme pos) (fmt "Syntax error: found data '%s' directly after tag '%s'. You may have forgotten ':'." (Lexing.lexeme lexbuf) previous) lexbuf
  }

  | eof         {
    raise (Compilation_Error(pos, fmt "Syntax error: tag '%s' was not given a value" previous))
  }

  | _           { 
    error (next_lexeme pos) (fmt "Syntax error while looking for ':' after declaration of tag '%s'" previous) lexbuf
  }


(* Appelé par dater_points quand un tag suivi de deux points a été trouvé. Cherche la donnée correspondante, puis rappelle dater_tag. *)
and dater_data pos previous =
  parse
  | comment     { dater_data (next_line pos) previous lexbuf }
  | blank       { dater_data (next_lexeme pos) previous lexbuf }
  | '\n'        { dater_data (next_line pos) previous lexbuf }

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

  | eof         {
    raise (Compilation_Error(pos, fmt "Syntax error: tag '%s' was not given a value" previous))
  }

  | _           { 
    error (next_lexeme pos) (fmt "Syntax error while looking for the value of tag '%s'" previous) lexbuf
  }


(* Analyseur de la section .text. Appelle dater_tag dès que '.data' a été trouvé. *)
and texter pos = 
  parse
  | ".data"     { dater_tag (next_lexeme pos) lexbuf }

  (* Détection des espaces blanc *)
  | comment     { texter (next_line pos) lexbuf }
  | blank       { texter (next_lexeme pos) lexbuf }
  | '\n'        { texter (next_line pos) lexbuf }

  | "NOP" | "EXIT"      { 
    fprintf output "%s\n" (Lexing.lexeme lexbuf);
    texter (next_lexeme pos) lexbuf
  }

  | "PRINT" | "JUMP"    {
    fprintf output "%s %s\n" (Lexing.lexeme lexbuf) acc;
    pop1 ();
    texter (next_lexeme pos) lexbuf
  }

  | "READ"      {
    (*pop_n 1;
    (* on doit mettre à jour la valeur de stack_pointer, au cas où c'est ce qu'on veut lire *)
    fprintf output "WRITE %s %s\n" spa sp;
    fprintf output "READ $r1 $r0\n";
    push (Reg 1);*)
    fprintf output "WRITE %s %s\n" spa sp;
    fprintf output "READ %s %s\n" acc acc; 
    texter (next_lexeme pos) lexbuf
  }

  | "WRITE"     {
    (*pop_n 2;
    fprintf output "WRITE $r0 $r1\n";*)
    pop2_no_return 0 (fun dest1 -> fprintf output "WRITE %s %s\n" dest1 acc);
    texter (next_lexeme pos) lexbuf
  }

  | "JUMPWHEN"  {
    (*pop_n 2;
    fprintf output "JUMP $r0 WHEN $r1\n";*)
    pop2_no_return 0 (fun dest1 -> fprintf output "JUMP %s WHEN %s\n" dest1 acc);
    texter (next_lexeme pos) lexbuf
  }

  | "MINUS" | "NOT"       {
    (*pop_n 1;
    fprintf output "NEG $r0 $r0\n";
    push (Reg 0);*)
    fprintf output "%s %s %s\n" (Lexing.lexeme lexbuf) acc acc;
    texter (next_lexeme pos) lexbuf
  }

  | "ADD" | "SUB" | "MULT" | "DIV"
  | "REM" | "EQ"  | "NEQ"  | "LT"
  | "LE"  | "GT"  | "GE"   | "AND"
  | "OR"        {
    (*pop_n 2;
    fprintf output "%s $r0 $r0 $r1\n" (Lexing.lexeme lexbuf);
    push (Reg 0);*)
    let dest1 = pop2_return 0 in
    fprintf output "%s %s %s %s\n" (Lexing.lexeme lexbuf) acc dest1 acc;
    texter (next_lexeme pos) lexbuf
  }

  (* Définition de tag *)
  | tag blank* ':'   {
    fprintf output "%s\n" (Lexing.lexeme lexbuf);
    texter (next_lexeme pos) lexbuf 
  } 

  (* Empilement de tag *)
  | tag         {
    let pos' = push (Tag (Lexing.lexeme lexbuf)) pos in
    texter (next_lexeme pos') lexbuf
  }

  (* Empilement d'entier *)
  | integer     { 
    let pos' = push (Int (Lexing.lexeme lexbuf)) pos in
    texter (next_lexeme pos') lexbuf 
  }

  | eof         {}

  | _           { 
    error (next_lexeme pos) "Syntax error" lexbuf
  }


(* Première règle à être appelée. Accepte les commentaires et lignes et appelle texter quand '.text' est rencontré. N'accepte rien d'autre. *)
and lexer pos =
  parse
  | ".text"   { texter (next_lexeme pos) lexbuf }
  | comment   { lexer (next_line pos) lexbuf }
  | blank     { lexer (next_lexeme pos) lexbuf }
  | '\n'      { lexer (next_line pos) lexbuf }
  | eof       {
    raise (Compilation_Error (pos, "Reached unexpected end of file"))
  }
  | _         { 
    error (next_lexeme pos) (fmt "Syntax error while looking for '.text'") lexbuf
  }


(* Règle d'erreur dont le seul rôle est de parcourir le reste de la ligne pour avoir un message d'erreur plus complet. *)
and error pos msg =
  parse
  | '\n'      
  | eof       { raise (Compilation_Error(pos, msg)) }
  | _         { error (next_lexeme pos) msg lexbuf }


{
  let _ =
    let beginning = Sys.time () in
    fprintf output "ADDRESS %s stack_pointer\n" spa;
    fprintf output "READ %s %s\n" sp spa;
    try
      lexer first_position lexbuf;
      fprintf output "stack_pointer:\n65536";
      close_out output;
      printf "Compilation STK -> ASM successful (%fs)\n" (Sys.time () -. beginning);
      exit 0
    with
    | Compilation_Error (pos, msg) ->
      printf "[ERROR] Line %d, token '%s':\n%s\n%s\n" pos.lnum (Lexing.lexeme lexbuf) pos.line msg;
      exit 1
    | Failure msg ->
      printf "[ERROR] The compilation failed. Error : %s\n" msg;
      exit 1
}