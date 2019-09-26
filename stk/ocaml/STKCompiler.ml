# 1 "stk/STKCompiler.mll"
 
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

  exception Compilation_Error of position * string * string

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

# 182 "stk/STKCompiler.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\248\255\249\255\011\000\021\000\031\000\252\255\253\255\
    \254\255\001\000\255\255\002\000\164\000\248\255\249\255\106\000\
    \127\000\192\000\252\255\253\255\254\255\004\000\255\255\032\000\
    \068\001\248\255\249\255\137\000\175\000\032\001\252\255\253\255\
    \254\255\079\000\255\255\114\000\182\001\240\255\241\255\011\001\
    \042\001\040\002\008\000\126\000\166\000\018\000\007\000\129\000\
    \166\000\011\000\025\000\011\000\041\000\083\000\129\000\252\255\
    \253\255\115\000\027\000\068\000\050\000\070\000\255\255\254\255\
    \158\000\088\000\120\000\246\255\251\255\245\255\097\000\088\000\
    \128\000\124\000\119\000\250\255\128\000\126\000\124\000\161\000\
    \165\000\158\000\247\255\128\001\169\000\249\255\165\000\168\000\
    \184\000\248\255\178\000\177\000\240\000\243\000\243\000\004\001\
    \005\001\008\001\245\000\244\255\186\001\194\001\250\255\251\255\
    \252\255\253\255\066\001\241\000\001\001\240\000\245\000\255\255\
    \254\255\099\001\100\001\254\255\255\255";
  Lexing.lex_backtrk =
   "\255\255\255\255\255\255\005\000\007\000\004\000\255\255\255\255\
    \255\255\007\000\255\255\255\255\255\255\255\255\255\255\005\000\
    \007\000\004\000\255\255\255\255\255\255\007\000\255\255\255\255\
    \255\255\255\255\255\255\005\000\007\000\004\000\255\255\255\255\
    \255\255\007\000\255\255\255\255\255\255\255\255\255\255\013\000\
    \015\000\012\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\255\255\
    \255\255\015\000\015\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\005\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\005\000\005\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255";
  Lexing.lex_default =
   "\001\000\000\000\000\000\255\255\255\255\255\255\000\000\000\000\
    \000\000\011\000\000\000\011\000\013\000\000\000\000\000\255\255\
    \255\255\255\255\000\000\000\000\000\000\023\000\000\000\023\000\
    \025\000\000\000\000\000\255\255\255\255\255\255\000\000\000\000\
    \000\000\035\000\000\000\035\000\037\000\000\000\000\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
    \000\000\064\000\255\255\255\255\255\255\255\255\000\000\000\000\
    \064\000\255\255\255\255\000\000\000\000\000\000\255\255\255\255\
    \255\255\255\255\255\255\000\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\000\000\255\255\255\255\000\000\255\255\255\255\
    \255\255\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\000\000\255\255\102\000\000\000\000\000\
    \000\000\000\000\113\000\255\255\255\255\255\255\255\255\000\000\
    \000\000\113\000\115\000\000\000\000\000";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\008\000\007\000\010\000\010\000\000\000\022\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \008\000\000\000\000\000\009\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\022\000\000\000\000\000\004\000\000\000\000\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\006\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\034\000\069\000\098\000\097\000\086\000\083\000\005\000\
    \076\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\072\000\034\000\063\000\005\000\059\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\069\000\060\000\061\000\062\000\
    \063\000\069\000\071\000\070\000\068\000\020\000\019\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\069\000\020\000\096\000\065\000\021\000\
    \068\000\073\000\074\000\075\000\067\000\077\000\078\000\095\000\
    \066\000\016\000\069\000\079\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\018\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\080\000\081\000\069\000\082\000\085\000\087\000\091\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\069\000\090\000\088\000\089\000\094\000\092\000\
    \002\000\255\255\255\255\017\000\255\255\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \255\255\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\039\000\039\000\039\000\039\000\039\000\
    \039\000\039\000\039\000\039\000\039\000\093\000\067\000\069\000\
    \069\000\069\000\069\000\069\000\112\000\032\000\031\000\255\255\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\039\000\039\000\039\000\039\000\039\000\039\000\
    \039\000\039\000\039\000\039\000\032\000\108\000\109\000\033\000\
    \110\000\111\000\000\000\000\000\000\000\112\000\116\000\000\000\
    \000\000\028\000\255\255\255\255\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\030\000\029\000\
    \000\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\000\000\000\000\000\000\255\255\000\000\
    \000\000\000\000\000\000\029\000\014\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\056\000\
    \055\000\084\000\000\000\100\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\105\000\104\000\069\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\056\000\000\000\
    \000\000\057\000\100\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\105\000\040\000\058\000\106\000\039\000\039\000\
    \039\000\039\000\039\000\039\000\039\000\039\000\039\000\039\000\
    \107\000\000\000\000\000\000\000\099\000\000\000\000\000\047\000\
    \000\000\000\000\045\000\053\000\000\000\043\000\000\000\000\000\
    \051\000\000\000\044\000\048\000\054\000\042\000\052\000\000\000\
    \050\000\046\000\000\000\000\000\000\000\049\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\041\000\000\000\041\000\
    \041\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
    \041\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
    \041\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
    \041\000\100\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\255\255\000\000\026\000\000\000\000\000\000\000\
    \100\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \041\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
    \041\000\041\000\099\000\255\255\116\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\041\000\
    \000\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
    \041\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
    \041\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
    \041\000\041\000\041\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\038\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\103\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\009\000\011\000\255\255\021\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\000\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\023\000\255\255\255\255\000\000\255\255\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\033\000\042\000\045\000\046\000\049\000\050\000\000\000\
    \051\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\052\000\035\000\057\000\005\000\058\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\053\000\059\000\060\000\061\000\
    \064\000\065\000\070\000\053\000\071\000\012\000\012\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\043\000\012\000\047\000\054\000\012\000\
    \066\000\072\000\073\000\074\000\066\000\076\000\077\000\047\000\
    \054\000\012\000\043\000\078\000\012\000\012\000\012\000\012\000\
    \012\000\012\000\012\000\012\000\012\000\012\000\012\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\079\000\080\000\044\000\081\000\084\000\086\000\048\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\044\000\048\000\087\000\088\000\090\000\091\000\
    \000\000\009\000\011\000\012\000\021\000\012\000\012\000\012\000\
    \012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
    \012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
    \012\000\012\000\012\000\012\000\012\000\012\000\012\000\017\000\
    \023\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\039\000\039\000\039\000\039\000\039\000\
    \039\000\039\000\039\000\039\000\039\000\092\000\093\000\094\000\
    \095\000\096\000\097\000\098\000\106\000\024\000\024\000\033\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\040\000\040\000\040\000\040\000\040\000\040\000\
    \040\000\040\000\040\000\040\000\024\000\107\000\108\000\024\000\
    \109\000\110\000\255\255\255\255\255\255\113\000\114\000\255\255\
    \255\255\024\000\035\000\057\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\029\000\
    \255\255\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\255\255\255\255\255\255\064\000\255\255\
    \255\255\255\255\255\255\024\000\012\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\036\000\
    \036\000\083\000\255\255\100\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\101\000\101\000\083\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\036\000\255\255\
    \255\255\036\000\100\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\101\000\036\000\036\000\101\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \101\000\255\255\255\255\255\255\100\000\255\255\255\255\036\000\
    \255\255\255\255\036\000\036\000\255\255\036\000\255\255\255\255\
    \036\000\255\255\036\000\036\000\036\000\036\000\036\000\255\255\
    \036\000\036\000\255\255\255\255\255\255\036\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\036\000\255\255\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\041\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\106\000\255\255\024\000\255\255\255\255\255\255\
    \041\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \041\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
    \041\000\041\000\041\000\113\000\114\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\041\000\
    \255\255\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
    \041\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
    \041\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
    \041\000\041\000\041\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\036\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\101\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255";
  Lexing.lex_base_code =
   "";
  Lexing.lex_backtrk_code =
   "";
  Lexing.lex_default_code =
   "";
  Lexing.lex_trans_code =
   "";
  Lexing.lex_check_code =
   "";
  Lexing.lex_code =
   "";
}

let rec dater_tag pos lexbuf =
   __ocaml_lex_dater_tag_rec pos lexbuf 0
and __ocaml_lex_dater_tag_rec pos lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 192 "stk/STKCompiler.mll"
                ( dater_tag (next_line pos) lexbuf )
# 459 "stk/STKCompiler.ml"

  | 1 ->
# 193 "stk/STKCompiler.mll"
                ( dater_tag (next_lexeme pos) lexbuf )
# 464 "stk/STKCompiler.ml"

  | 2 ->
# 194 "stk/STKCompiler.mll"
                ( dater_tag (next_line pos) lexbuf )
# 469 "stk/STKCompiler.ml"

  | 3 ->
# 196 "stk/STKCompiler.mll"
                (
    error pos (Lexing.lexeme lexbuf) "Syntax error: unexpected ':'" lexbuf
  )
# 476 "stk/STKCompiler.ml"

  | 4 ->
# 200 "stk/STKCompiler.mll"
                (
    let tag = Lexing.lexeme lexbuf in
    fprintf output "%s:\n" tag;
    dater_points (next_lexeme pos) tag lexbuf
  )
# 485 "stk/STKCompiler.ml"

  | 5 ->
# 206 "stk/STKCompiler.mll"
                (
    let token = Lexing.lexeme lexbuf in
    error pos token (fmt "Syntax error: found `%s` without a tag" token) lexbuf
  )
# 493 "stk/STKCompiler.ml"

  | 6 ->
# 211 "stk/STKCompiler.mll"
                ()
# 498 "stk/STKCompiler.ml"

  | 7 ->
# 213 "stk/STKCompiler.mll"
                ( 
    error (next_lexeme pos) (Lexing.lexeme lexbuf)
      "Syntax error: only tag definitions are allowed after '.data'" lexbuf
  )
# 506 "stk/STKCompiler.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_dater_tag_rec pos lexbuf __ocaml_lex_state

and dater_points pos previous lexbuf =
   __ocaml_lex_dater_points_rec pos previous lexbuf 12
and __ocaml_lex_dater_points_rec pos previous lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 222 "stk/STKCompiler.mll"
                ( dater_points (next_line pos) previous lexbuf )
# 518 "stk/STKCompiler.ml"

  | 1 ->
# 223 "stk/STKCompiler.mll"
                ( dater_points (next_lexeme pos) previous lexbuf )
# 523 "stk/STKCompiler.ml"

  | 2 ->
# 224 "stk/STKCompiler.mll"
                ( dater_points (next_line pos) previous lexbuf )
# 528 "stk/STKCompiler.ml"

  | 3 ->
# 225 "stk/STKCompiler.mll"
                ( dater_data (next_lexeme pos) previous lexbuf )
# 533 "stk/STKCompiler.ml"

  | 4 ->
# 227 "stk/STKCompiler.mll"
                (
    error (next_lexeme pos) (Lexing.lexeme lexbuf)
      (fmt "Syntax error: tag '%s' was not given a value" previous) lexbuf
  )
# 541 "stk/STKCompiler.ml"

  | 5 ->
# 232 "stk/STKCompiler.mll"
                (
    let token = Lexing.lexeme lexbuf in
    error (next_lexeme pos) token (fmt 
        "Syntax error: found data '%s' directly after tag '%s'. You may have forgotten ':'." 
        token previous) 
      lexbuf
  )
# 552 "stk/STKCompiler.ml"

  | 6 ->
# 240 "stk/STKCompiler.mll"
                (
    raise (Compilation_Error(pos, (Lexing.lexeme lexbuf), 
      fmt "Syntax error: tag '%s' was not given a value" previous))
  )
# 560 "stk/STKCompiler.ml"

  | 7 ->
# 245 "stk/STKCompiler.mll"
                ( 
    error (next_lexeme pos) (Lexing.lexeme lexbuf)
      (fmt "Syntax error while looking for ':' after declaration of tag '%s'" previous) lexbuf
  )
# 568 "stk/STKCompiler.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_dater_points_rec pos previous lexbuf __ocaml_lex_state

and dater_data pos previous lexbuf =
   __ocaml_lex_dater_data_rec pos previous lexbuf 24
and __ocaml_lex_dater_data_rec pos previous lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 254 "stk/STKCompiler.mll"
                ( dater_data (next_line pos) previous lexbuf )
# 580 "stk/STKCompiler.ml"

  | 1 ->
# 255 "stk/STKCompiler.mll"
                ( dater_data (next_lexeme pos) previous lexbuf )
# 585 "stk/STKCompiler.ml"

  | 2 ->
# 256 "stk/STKCompiler.mll"
                ( dater_data (next_line pos) previous lexbuf )
# 590 "stk/STKCompiler.ml"

  | 3 ->
# 258 "stk/STKCompiler.mll"
                ( 
    error (next_lexeme pos) (Lexing.lexeme lexbuf) "Syntax error: duplicate ':'" lexbuf
  )
# 597 "stk/STKCompiler.ml"

  | 4 ->
# 262 "stk/STKCompiler.mll"
                (
    let token = Lexing.lexeme lexbuf in
    error (next_lexeme pos) token
      (fmt "Syntax error: found tag '%s', expected a value" token) lexbuf
  )
# 606 "stk/STKCompiler.ml"

  | 5 ->
# 268 "stk/STKCompiler.mll"
                (
    fprintf output "%s\n" (Lexing.lexeme lexbuf);
    dater_tag (next_lexeme pos) lexbuf
  )
# 614 "stk/STKCompiler.ml"

  | 6 ->
# 273 "stk/STKCompiler.mll"
                (
    raise (Compilation_Error(pos, (Lexing.lexeme lexbuf),
      fmt "Syntax error: tag '%s' was not given a value" previous))
  )
# 622 "stk/STKCompiler.ml"

  | 7 ->
# 278 "stk/STKCompiler.mll"
                ( 
    error (next_lexeme pos) (Lexing.lexeme lexbuf)
      (fmt "Syntax error while looking for the value of tag '%s'" previous) lexbuf
  )
# 630 "stk/STKCompiler.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_dater_data_rec pos previous lexbuf __ocaml_lex_state

and texter pos lexbuf =
   __ocaml_lex_texter_rec pos lexbuf 36
and __ocaml_lex_texter_rec pos lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 287 "stk/STKCompiler.mll"
                ( dater_tag (next_lexeme pos) lexbuf )
# 642 "stk/STKCompiler.ml"

  | 1 ->
# 290 "stk/STKCompiler.mll"
                ( texter (next_line pos) lexbuf )
# 647 "stk/STKCompiler.ml"

  | 2 ->
# 291 "stk/STKCompiler.mll"
                ( texter (next_lexeme pos) lexbuf )
# 652 "stk/STKCompiler.ml"

  | 3 ->
# 292 "stk/STKCompiler.mll"
                ( texter (next_line pos) lexbuf )
# 657 "stk/STKCompiler.ml"

  | 4 ->
# 294 "stk/STKCompiler.mll"
                        ( 
    fprintf output "%s\n" (Lexing.lexeme lexbuf);
    texter (next_lexeme pos) lexbuf
  )
# 665 "stk/STKCompiler.ml"

  | 5 ->
# 299 "stk/STKCompiler.mll"
                        (
    fprintf output "%s %s\n" (Lexing.lexeme lexbuf) acc;
    pop1 pos error;
    texter (next_lexeme pos) lexbuf
  )
# 674 "stk/STKCompiler.ml"

  | 6 ->
# 305 "stk/STKCompiler.mll"
                (
    no_push pos error;
    fprintf output "READ %s %s\n" acc acc; 
    texter (next_lexeme pos) lexbuf
  )
# 683 "stk/STKCompiler.ml"

  | 7 ->
# 311 "stk/STKCompiler.mll"
                (
    pop2_no_return 0 (fun dest1 -> 
        fprintf output "WRITE %s %s\n" dest1 acc)
       pos error;
    texter (next_lexeme pos) lexbuf
  )
# 693 "stk/STKCompiler.ml"

  | 8 ->
# 318 "stk/STKCompiler.mll"
                (
    pop2_no_return 0 (fun dest1 -> 
        fprintf output "JUMP %s WHEN %s\n" dest1 acc)
      pos error;
    texter (next_lexeme pos) lexbuf
  )
# 703 "stk/STKCompiler.ml"

  | 9 ->
# 325 "stk/STKCompiler.mll"
                          (
    no_push pos error;
    fprintf output "%s %s %s\n" (Lexing.lexeme lexbuf) acc acc;
    texter (next_lexeme pos) lexbuf
  )
# 712 "stk/STKCompiler.ml"

  | 10 ->
# 334 "stk/STKCompiler.mll"
                (
    let dest1 = pop2_return 0 pos error in
    fprintf output "%s %s %s %s\n" (Lexing.lexeme lexbuf) acc dest1 acc;
    texter (next_lexeme pos) lexbuf
  )
# 721 "stk/STKCompiler.ml"

  | 11 ->
# 341 "stk/STKCompiler.mll"
                     (
    let tag = extract_tag (Lexing.lexeme lexbuf) in
    if tag = "stack_pointer" then
      error pos tag 
        "'stack_pointer' is a reserved word and can not be used to name new tags" lexbuf
    else
    begin
      fprintf output "%s:\n" tag;
      texter (next_lexeme pos) lexbuf
    end
  )
# 736 "stk/STKCompiler.ml"

  | 12 ->
# 354 "stk/STKCompiler.mll"
                (
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
  )
# 754 "stk/STKCompiler.ml"

  | 13 ->
# 370 "stk/STKCompiler.mll"
                ( 
    let pos' = push (Int (Lexing.lexeme lexbuf)) pos in
    texter (next_lexeme pos') lexbuf 
  )
# 762 "stk/STKCompiler.ml"

  | 14 ->
# 375 "stk/STKCompiler.mll"
                ()
# 767 "stk/STKCompiler.ml"

  | 15 ->
# 377 "stk/STKCompiler.mll"
                ( 
    error (next_lexeme pos) (Lexing.lexeme lexbuf) "Syntax error" lexbuf
  )
# 774 "stk/STKCompiler.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_texter_rec pos lexbuf __ocaml_lex_state

and lexer pos lexbuf =
   __ocaml_lex_lexer_rec pos lexbuf 101
and __ocaml_lex_lexer_rec pos lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 385 "stk/STKCompiler.mll"
              ( texter (next_lexeme pos) lexbuf )
# 786 "stk/STKCompiler.ml"

  | 1 ->
# 386 "stk/STKCompiler.mll"
              ( lexer (next_line pos) lexbuf )
# 791 "stk/STKCompiler.ml"

  | 2 ->
# 387 "stk/STKCompiler.mll"
              ( lexer (next_lexeme pos) lexbuf )
# 796 "stk/STKCompiler.ml"

  | 3 ->
# 388 "stk/STKCompiler.mll"
              ( lexer (next_line pos) lexbuf )
# 801 "stk/STKCompiler.ml"

  | 4 ->
# 389 "stk/STKCompiler.mll"
              (
    raise (Compilation_Error (pos, "end of file", "Reached unexpected end of file"))
  )
# 808 "stk/STKCompiler.ml"

  | 5 ->
# 392 "stk/STKCompiler.mll"
              ( 
    error (next_lexeme pos) (Lexing.lexeme lexbuf)
      (fmt "Syntax error while looking for '.text'") lexbuf
  )
# 816 "stk/STKCompiler.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_lexer_rec pos lexbuf __ocaml_lex_state

and error pos token msg lexbuf =
   __ocaml_lex_error_rec pos token msg lexbuf 114
and __ocaml_lex_error_rec pos token msg lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 402 "stk/STKCompiler.mll"
              ( raise (Compilation_Error(pos, token, msg)) )
# 828 "stk/STKCompiler.ml"

  | 1 ->
# 403 "stk/STKCompiler.mll"
              ( error (next_lexeme pos) token msg lexbuf )
# 833 "stk/STKCompiler.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_error_rec pos token msg lexbuf __ocaml_lex_state

;;

# 406 "stk/STKCompiler.mll"
 
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
      printf "[ERROR] The compilation failed. Error : %s\n" msg;
      exit 1

# 858 "stk/STKCompiler.ml"
