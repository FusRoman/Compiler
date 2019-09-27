{
  (*
    Encore à faire avant de passer à STKCompilerAlloc :
    - au lieu de printer dans le fichier ligne par ligne, construire une suite d'instructions,
      ce qui permettra d'optimiser les pop suivis de push (push -> pop l'étant déjà)
    - du coup on pourra checker si tous les tags qu'on empile existent bien
    - stack_pointer est-il réellement nécessaire ? J'aimerais bien le virer. Ca ferait quelques optimisations possibles en plus.
  *)
  open Printf

  module TagSet = Set.Make(String)

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
    acc_empty: bool;
    tags: TagSet.t
  }

  exception Compilation_Error of position * string * string

  let first_position = {
    lnum = 1; 
    line = ""; 
    acc_empty = true;
    tags = TagSet.empty
  }

  let next_line position = 
    {position with lnum = position.lnum + 1; line = ""}

  let next_lexeme position =
    {position with line = position.line ^ (Lexing.lexeme lexbuf)}

  let add_tag position tag =
    {position with tags = TagSet.add tag position.tags}


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
  (* 
    Registre dans lequel on met l'adresse du sommet de la pile
    Auparavant, stack_pointer était une donnée automatiquement rajoutée à la fin des fichiers ASM,
    de façon à ce que les programmes plus haut niveau puissent y accéder.
    Mais ceci est en fait réalisable en traitant le tag 'stack_pointer' comme un cas particulier dans ce fichier.
    On peut ainsi stocker stack_pointer dans un registre, ce qui est plus pratique.
  *)
  let sp = register_str 15

  (* 
    Registre accumulateur
    Stocke ce qui devrait être le sommet de la pile pour limiter les interactions avec cette dernière.

    Lorsqu'on retire un élément de la pile, les programmes mettront automatiquement l'élément précédent dans acc. 
    Cela pose un problème lorsqu'il n'y en a pas, i.e. qu'on a vidé la pile. La machine virtuelle lance une exception
    out of bounds. Faire un test en runtime à chaque fois pour vérifier si la pile est vide annule tous les bénéfices 
    du registre accumulateur, et vérifier à la compilation si ce test en runtime est nécessaire ou pas est impossible.
    En guise de solution, nous avons simplement laissé un mot inutilisé à la fin, de façon à ne jamais sortir 
    en-dehors de la mémoire. C'est pourquoi la ligne rajoutée à la fin du fichier compilé inclut 65535 et pas 65536.
  *)
  let acc = register_str 14


  type pushable = 
    | Tag of string 
    | Int of string 
    | Reg of string

  (*
    Un programme doit dans les faits démarrer par un push ; avant, il ne peut faire que des NOP ou des EXIT.
    A partir du premier élément dans la pile, il devient possible de faire un JUMP et vérifier que la pile est 
    nécessairement vide à un certain endroit du programme est trop compliqué (voire impossible).
    En revanche, pour le premier push, on sait que la pile est nécessairement vide donc on peut agir en conséquence.
  *)
  let push pushable pos =
    let fill_acc () =
        match pushable with
        | Tag s ->
          fprintf output "ADDRESS %s %s\n" acc s
        | Int s ->
          fprintf output "CONST %s %s\n" acc s
        | Reg s ->
          fprintf output "MOVE %s %s\n" acc s
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

  (* Si on tombe sur un pop avant le premier push, on sait qu'il y a erreur. *)
  let no_push pos error =
    if pos.acc_empty then
      error (next_lexeme pos) (Lexing.lexeme lexbuf)
        "No push instruction before first popping" lexbuf

  (*
    Utilisé par les instructions qui prennent un élément sur la pile et n'en remettent pas (PRINT et JUMP).
    Diminue la pile de 1 et met le nouveau sommet dans acc.
    A appeler après avoir utilisé acc comme l'ancien sommet de la pile, donc à la fin en général.
    Pour les instructions à 1 argument et retournant un résultat, il suffit de stocker le résultat dans acc ;
    aucun appel à une fonction pop n'est nécessaire dans ce cas.
  *)
  let pop1 pos error =
    no_push pos error;
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
  let pop2_no_return dest1 instr pos  error=
    no_push pos error;
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
  let pop2_return dest1 pos error =
    no_push pos error;
    let dest1 = register_str dest1 in
    fprintf output "INCR %s 1\n" sp;
    fprintf output "READ %s %s\n" dest1 sp;
    dest1

  (* Buggé ! *)
  let extract_tag s =
    let length = String.length s in
    let i = ref 0 in
    let nonwhite = ref true in
    while !i < length && !nonwhite do
      let c = String.get s !i in
      if c <> ' ' && c <> '\t' 
        && c <> '\n' && c <> ':' then
        incr i
      else
        nonwhite := false
    done;
    if !i = 0 then
      (* Ne devrait jamais arriver *)
      raise (Failure (fmt "Failed while extracting the tag name in '%s'" s))
    else
      String.sub s 0 !i
}

let letter = ['a'-'z' 'A'-'Z' '_']
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
    error pos (Lexing.lexeme lexbuf) "Syntax error: unexpected ':'" lexbuf
  }

  | tag         {
    let tag = Lexing.lexeme lexbuf in
    fprintf output "%s:\n" tag;
    dater_points (next_lexeme pos) tag lexbuf
  }

  | integer     {
    let token = Lexing.lexeme lexbuf in
    error pos token (fmt "Syntax error: found `%s` without a tag" token) lexbuf
  }

  | eof         {}

  | _           { 
    error (next_lexeme pos) (Lexing.lexeme lexbuf)
      "Syntax error: only tag definitions are allowed after '.data'" lexbuf
  }


(* Appelé par dater_tag dès qu'un tag est trouvé. Appelle dater_data dès que ':' est trouvé. *)
and dater_points pos previous =
  parse
  | comment     { dater_points (next_line pos) previous lexbuf }
  | blank       { dater_points (next_lexeme pos) previous lexbuf }
  | '\n'        { dater_points (next_line pos) previous lexbuf }
  | ':'         { dater_data (next_lexeme pos) previous lexbuf }

  | tag         {
    error (next_lexeme pos) (Lexing.lexeme lexbuf)
      (fmt "Syntax error: tag '%s' was not given a value" previous) lexbuf
  }

  | integer     {
    let token = Lexing.lexeme lexbuf in
    error (next_lexeme pos) token (fmt 
        "Syntax error: found data '%s' directly after tag '%s'. You may have forgotten ':'." 
        token previous) 
      lexbuf
  }

  | eof         {
    raise (Compilation_Error(pos, (Lexing.lexeme lexbuf), 
      fmt "Syntax error: tag '%s' was not given a value" previous))
  }

  | _           { 
    error (next_lexeme pos) (Lexing.lexeme lexbuf)
      (fmt "Syntax error while looking for ':' after declaration of tag '%s'" previous) lexbuf
  }


(* Appelé par dater_points quand un tag suivi de deux points a été trouvé. Cherche la donnée correspondante, puis rappelle dater_tag. *)
and dater_data pos previous =
  parse
  | comment     { dater_data (next_line pos) previous lexbuf }
  | blank       { dater_data (next_lexeme pos) previous lexbuf }
  | '\n'        { dater_data (next_line pos) previous lexbuf }

  | ':'         { 
    error (next_lexeme pos) (Lexing.lexeme lexbuf) "Syntax error: duplicate ':'" lexbuf
  }

  | tag         {
    let token = Lexing.lexeme lexbuf in
    error (next_lexeme pos) token
      (fmt "Syntax error: found tag '%s', expected a value" token) lexbuf
  }

  | integer     {
    fprintf output "%s\n" (Lexing.lexeme lexbuf);
    dater_tag (next_lexeme pos) lexbuf
  }

  | eof         {
    raise (Compilation_Error(pos, (Lexing.lexeme lexbuf),
      fmt "Syntax error: tag '%s' was not given a value" previous))
  }

  | _           { 
    error (next_lexeme pos) (Lexing.lexeme lexbuf)
      (fmt "Syntax error while looking for the value of tag '%s'" previous) lexbuf
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

  | "PRINT"     {
    fprintf output "PRINT %s\n" acc;
    pop1 pos error;
    texter (next_lexeme pos) lexbuf
  }

  | "JUMP"      {
    let r0 = register_str 0 in
    fprintf output "MOVE %s %s\n" r0 acc;
    pop1 pos error;
    fprintf output "JUMP %s\n" r0;
    texter (next_lexeme pos) lexbuf
  }

  | "WRITE"     {
    pop2_no_return 0 (fun dest1 -> 
        fprintf output "WRITE %s %s\n" dest1 acc)
       pos error;
    texter (next_lexeme pos) lexbuf
  }

  | "JUMPWHEN"  {
    (* pop2_no_return 0 (fun dest1 -> ()) pos error; *)
    no_push pos error;
    let r0 = register_str 0 in
    let r1 = register_str 1 in
    fprintf output "MOVE %s %s\n" r1 acc;
    fprintf output "INCR %s 1\n" sp;
    fprintf output "READ %s %s\n" r0 sp;
    fprintf output "INCR %s 1\n" sp;
    fprintf output "READ %s %s\n" acc sp;
    fprintf output "JUMP %s WHEN %s\n" r0 r1;
    texter (next_lexeme pos) lexbuf
  }

  | "READ" | "MINUS" | "NOT"       {
    no_push pos error;
    fprintf output "%s %s %s\n" (Lexing.lexeme lexbuf) acc acc;
    texter (next_lexeme pos) lexbuf
  }

  | "ADD" | "SUB" | "MULT" | "DIV"
  | "REM" | "EQ"  | "NEQ"  | "LT"
  | "LE"  | "GT"  | "GE"   | "AND"
  | "OR"        {
    let dest1 = pop2_return 0 pos error in
    fprintf output "%s %s %s %s\n" (Lexing.lexeme lexbuf) acc dest1 acc;
    texter (next_lexeme pos) lexbuf
  }

  (* Définition de tag *)
  | tag blank* ':'   {
    let tag = extract_tag (Lexing.lexeme lexbuf) in
    if tag = "stack_pointer" then
      error pos tag 
        "'stack_pointer' is a reserved tag and can not be used to name new tags" lexbuf
    else
      if TagSet.mem tag pos.tags then
        error pos tag
          (fmt "Duplicate tag '%s'" tag) lexbuf
      else
      begin
        let pos' = add_tag pos tag in
        fprintf output "%s:\n" tag;
        texter (next_lexeme pos') lexbuf
      end
  } 

  (* Empilement de tag *)
  | tag         {
    (* Traiter le cas particulier stack_pointer ?
    => empiler la valeur du registre 
       ou calculer la vraie taille de la pile en incluant les registres intermédiaires (info plus utile haut niveau) ? *)
    let tag = Lexing.lexeme lexbuf in
    let pos' = 
      if tag = "stack_pointer" then
        (* pour l'instant, juste la valeur du registre *)
        push (Reg sp) pos
      else
        push (Tag tag) pos
    in
    texter (next_lexeme pos') lexbuf
  }

  (* Empilement d'entier *)
  | integer     { 
    let pos' = push (Int (Lexing.lexeme lexbuf)) pos in
    texter (next_lexeme pos') lexbuf 
  }

  | eof         {}

  | _           { 
    error (next_lexeme pos) (Lexing.lexeme lexbuf) "Syntax error" lexbuf
  }


(* Première règle à être appelée. Accepte les commentaires et lignes et appelle texter quand '.text' est rencontré. N'accepte rien d'autre. *)
and lexer pos =
  parse
  | ".text"   { texter (next_lexeme pos) lexbuf }
  | comment   { lexer (next_line pos) lexbuf }
  | blank     { lexer (next_lexeme pos) lexbuf }
  | '\n'      { lexer (next_line pos) lexbuf }
  | eof       {
    raise (Compilation_Error (pos, "end of file", "Reached unexpected end of file while looking for '.text'"))
  }
  | _         { 
    error (next_lexeme pos) (Lexing.lexeme lexbuf)
      (fmt "Syntax error while looking for '.text'") lexbuf
  }


(* Règle d'erreur dont le seul rôle est de parcourir le reste de la ligne pour avoir un message d'erreur plus complet. *)
and error pos token msg =
  parse
  | '\n'      
  | eof       { raise (Compilation_Error(pos, token, msg)) }
  | _         { error (next_lexeme pos) token msg lexbuf }


{
  let _ =
    let beginning = Sys.time () in
    fprintf output "CONST %s 65535\n" sp;
    try
      lexer first_position lexbuf;
      close_out output;
      printf "Compilation STK -> ASM successful (%fs)\n" (Sys.time () -. beginning);
      exit 0
    with
    | Compilation_Error (pos, token, msg) ->
      printf "[ERROR] Line %d, token '%s':\n%s\n%s\n" pos.lnum token pos.line msg;
      exit 1
    | Failure msg ->
      printf "[ERROR] The compilation failed due to an internal error. Error : %s\n" msg;
      exit 1
}