# 1 "ocamllex/STKCompiler.mll"
 
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

# 149 "ocamllex/STKCompiler.ml"
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
# 159 "ocamllex/STKCompiler.mll"
                ( dater_tag (next_line pos) lexbuf )
# 426 "ocamllex/STKCompiler.ml"

  | 1 ->
# 160 "ocamllex/STKCompiler.mll"
                ( dater_tag (next_lexeme pos) lexbuf )
# 431 "ocamllex/STKCompiler.ml"

  | 2 ->
# 161 "ocamllex/STKCompiler.mll"
                ( dater_tag (next_line pos) lexbuf )
# 436 "ocamllex/STKCompiler.ml"

  | 3 ->
# 163 "ocamllex/STKCompiler.mll"
                (
    error pos "Syntax error: unexpected ':'" lexbuf
  )
# 443 "ocamllex/STKCompiler.ml"

  | 4 ->
# 167 "ocamllex/STKCompiler.mll"
                (
    let tag = Lexing.lexeme lexbuf in
    fprintf output "%s:\n" tag;
    dater_points (next_lexeme pos) tag lexbuf
  )
# 452 "ocamllex/STKCompiler.ml"

  | 5 ->
# 173 "ocamllex/STKCompiler.mll"
                (
    error pos (fmt "Syntax error: found `%s` without a tag" (Lexing.lexeme lexbuf)) lexbuf
  )
# 459 "ocamllex/STKCompiler.ml"

  | 6 ->
# 177 "ocamllex/STKCompiler.mll"
                ()
# 464 "ocamllex/STKCompiler.ml"

  | 7 ->
# 179 "ocamllex/STKCompiler.mll"
                ( 
    error (next_lexeme pos) "Syntax error: only tag definitions are allowed after '.data'" lexbuf
  )
# 471 "ocamllex/STKCompiler.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_dater_tag_rec pos lexbuf __ocaml_lex_state

and dater_points pos previous lexbuf =
   __ocaml_lex_dater_points_rec pos previous lexbuf 12
and __ocaml_lex_dater_points_rec pos previous lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 187 "ocamllex/STKCompiler.mll"
                ( dater_points (next_line pos) previous lexbuf )
# 483 "ocamllex/STKCompiler.ml"

  | 1 ->
# 188 "ocamllex/STKCompiler.mll"
                ( dater_points (next_lexeme pos) previous lexbuf )
# 488 "ocamllex/STKCompiler.ml"

  | 2 ->
# 189 "ocamllex/STKCompiler.mll"
                ( dater_points (next_line pos) previous lexbuf )
# 493 "ocamllex/STKCompiler.ml"

  | 3 ->
# 190 "ocamllex/STKCompiler.mll"
                ( dater_data (next_lexeme pos) previous lexbuf )
# 498 "ocamllex/STKCompiler.ml"

  | 4 ->
# 192 "ocamllex/STKCompiler.mll"
                (
   error (next_lexeme pos) (fmt "Syntax error: tag '%s' was not given a value" previous) lexbuf
  )
# 505 "ocamllex/STKCompiler.ml"

  | 5 ->
# 196 "ocamllex/STKCompiler.mll"
                (
    error (next_lexeme pos) (fmt "Syntax error: found data '%s' directly after tag '%s'. You may have forgotten ':'." (Lexing.lexeme lexbuf) previous) lexbuf
  )
# 512 "ocamllex/STKCompiler.ml"

  | 6 ->
# 200 "ocamllex/STKCompiler.mll"
                (
    raise (Compilation_Error(pos, fmt "Syntax error: tag '%s' was not given a value" previous))
  )
# 519 "ocamllex/STKCompiler.ml"

  | 7 ->
# 204 "ocamllex/STKCompiler.mll"
                ( 
    error (next_lexeme pos) (fmt "Syntax error while looking for ':' after declaration of tag '%s'" previous) lexbuf
  )
# 526 "ocamllex/STKCompiler.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_dater_points_rec pos previous lexbuf __ocaml_lex_state

and dater_data pos previous lexbuf =
   __ocaml_lex_dater_data_rec pos previous lexbuf 24
and __ocaml_lex_dater_data_rec pos previous lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 212 "ocamllex/STKCompiler.mll"
                ( dater_data (next_line pos) previous lexbuf )
# 538 "ocamllex/STKCompiler.ml"

  | 1 ->
# 213 "ocamllex/STKCompiler.mll"
                ( dater_data (next_lexeme pos) previous lexbuf )
# 543 "ocamllex/STKCompiler.ml"

  | 2 ->
# 214 "ocamllex/STKCompiler.mll"
                ( dater_data (next_line pos) previous lexbuf )
# 548 "ocamllex/STKCompiler.ml"

  | 3 ->
# 216 "ocamllex/STKCompiler.mll"
                ( 
    error (next_lexeme pos) "Syntax error: duplicate ':'" lexbuf
  )
# 555 "ocamllex/STKCompiler.ml"

  | 4 ->
# 220 "ocamllex/STKCompiler.mll"
                (
   error (next_lexeme pos) (fmt "Syntax error: found tag '%s', expected a value" (Lexing.lexeme lexbuf)) lexbuf
  )
# 562 "ocamllex/STKCompiler.ml"

  | 5 ->
# 224 "ocamllex/STKCompiler.mll"
                (
    fprintf output "%s\n" (Lexing.lexeme lexbuf);
    dater_tag (next_lexeme pos) lexbuf
  )
# 570 "ocamllex/STKCompiler.ml"

  | 6 ->
# 229 "ocamllex/STKCompiler.mll"
                (
    raise (Compilation_Error(pos, fmt "Syntax error: tag '%s' was not given a value" previous))
  )
# 577 "ocamllex/STKCompiler.ml"

  | 7 ->
# 233 "ocamllex/STKCompiler.mll"
                ( 
    error (next_lexeme pos) (fmt "Syntax error while looking for the value of tag '%s'" previous) lexbuf
  )
# 584 "ocamllex/STKCompiler.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_dater_data_rec pos previous lexbuf __ocaml_lex_state

and texter pos lexbuf =
   __ocaml_lex_texter_rec pos lexbuf 36
and __ocaml_lex_texter_rec pos lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 241 "ocamllex/STKCompiler.mll"
                ( dater_tag (next_lexeme pos) lexbuf )
# 596 "ocamllex/STKCompiler.ml"

  | 1 ->
# 244 "ocamllex/STKCompiler.mll"
                ( texter (next_line pos) lexbuf )
# 601 "ocamllex/STKCompiler.ml"

  | 2 ->
# 245 "ocamllex/STKCompiler.mll"
                ( texter (next_lexeme pos) lexbuf )
# 606 "ocamllex/STKCompiler.ml"

  | 3 ->
# 246 "ocamllex/STKCompiler.mll"
                ( texter (next_line pos) lexbuf )
# 611 "ocamllex/STKCompiler.ml"

  | 4 ->
# 248 "ocamllex/STKCompiler.mll"
                        ( 
    fprintf output "%s\n" (Lexing.lexeme lexbuf);
    texter (next_lexeme pos) lexbuf
  )
# 619 "ocamllex/STKCompiler.ml"

  | 5 ->
# 253 "ocamllex/STKCompiler.mll"
                        (
    fprintf output "%s %s\n" (Lexing.lexeme lexbuf) acc;
    pop1 ();
    texter (next_lexeme pos) lexbuf
  )
# 628 "ocamllex/STKCompiler.ml"

  | 6 ->
# 259 "ocamllex/STKCompiler.mll"
                (
    (*pop_n 1;
    (* on doit mettre à jour la valeur de stack_pointer, au cas où c'est ce qu'on veut lire *)
    fprintf output "WRITE %s %s\n" spa sp;
    fprintf output "READ $r1 $r0\n";
    push (Reg 1);*)
    fprintf output "WRITE %s %s\n" spa sp;
    fprintf output "READ %s %s\n" acc acc; 
    texter (next_lexeme pos) lexbuf
  )
# 642 "ocamllex/STKCompiler.ml"

  | 7 ->
# 270 "ocamllex/STKCompiler.mll"
                (
    (*pop_n 2;
    fprintf output "WRITE $r0 $r1\n";*)
    pop2_no_return 0 (fun dest1 -> fprintf output "WRITE %s %s\n" dest1 acc);
    texter (next_lexeme pos) lexbuf
  )
# 652 "ocamllex/STKCompiler.ml"

  | 8 ->
# 277 "ocamllex/STKCompiler.mll"
                (
    (*pop_n 2;
    fprintf output "JUMP $r0 WHEN $r1\n";*)
    pop2_no_return 0 (fun dest1 -> fprintf output "JUMP %s WHEN %s\n" dest1 acc);
    texter (next_lexeme pos) lexbuf
  )
# 662 "ocamllex/STKCompiler.ml"

  | 9 ->
# 284 "ocamllex/STKCompiler.mll"
                          (
    (*pop_n 1;
    fprintf output "NEG $r0 $r0\n";
    push (Reg 0);*)
    fprintf output "%s %s %s\n" (Lexing.lexeme lexbuf) acc acc;
    texter (next_lexeme pos) lexbuf
  )
# 673 "ocamllex/STKCompiler.ml"

  | 10 ->
# 295 "ocamllex/STKCompiler.mll"
                (
    (*pop_n 2;
    fprintf output "%s $r0 $r0 $r1\n" (Lexing.lexeme lexbuf);
    push (Reg 0);*)
    let dest1 = pop2_return 0 in
    fprintf output "%s %s %s %s\n" (Lexing.lexeme lexbuf) acc dest1 acc;
    texter (next_lexeme pos) lexbuf
  )
# 685 "ocamllex/STKCompiler.ml"

  | 11 ->
# 305 "ocamllex/STKCompiler.mll"
                     (
    fprintf output "%s\n" (Lexing.lexeme lexbuf);
    texter (next_lexeme pos) lexbuf 
  )
# 693 "ocamllex/STKCompiler.ml"

  | 12 ->
# 311 "ocamllex/STKCompiler.mll"
                (
    let pos' = push (Tag (Lexing.lexeme lexbuf)) pos in
    texter (next_lexeme pos') lexbuf
  )
# 701 "ocamllex/STKCompiler.ml"

  | 13 ->
# 317 "ocamllex/STKCompiler.mll"
                ( 
    let pos' = push (Int (Lexing.lexeme lexbuf)) pos in
    texter (next_lexeme pos') lexbuf 
  )
# 709 "ocamllex/STKCompiler.ml"

  | 14 ->
# 322 "ocamllex/STKCompiler.mll"
                ()
# 714 "ocamllex/STKCompiler.ml"

  | 15 ->
# 324 "ocamllex/STKCompiler.mll"
                ( 
    error (next_lexeme pos) "Syntax error" lexbuf
  )
# 721 "ocamllex/STKCompiler.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_texter_rec pos lexbuf __ocaml_lex_state

and lexer pos lexbuf =
   __ocaml_lex_lexer_rec pos lexbuf 101
and __ocaml_lex_lexer_rec pos lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 332 "ocamllex/STKCompiler.mll"
              ( texter (next_lexeme pos) lexbuf )
# 733 "ocamllex/STKCompiler.ml"

  | 1 ->
# 333 "ocamllex/STKCompiler.mll"
              ( lexer (next_line pos) lexbuf )
# 738 "ocamllex/STKCompiler.ml"

  | 2 ->
# 334 "ocamllex/STKCompiler.mll"
              ( lexer (next_lexeme pos) lexbuf )
# 743 "ocamllex/STKCompiler.ml"

  | 3 ->
# 335 "ocamllex/STKCompiler.mll"
              ( lexer (next_line pos) lexbuf )
# 748 "ocamllex/STKCompiler.ml"

  | 4 ->
# 336 "ocamllex/STKCompiler.mll"
              (
    raise (Compilation_Error (pos, "Reached unexpected end of file"))
  )
# 755 "ocamllex/STKCompiler.ml"

  | 5 ->
# 339 "ocamllex/STKCompiler.mll"
              ( 
    error (next_lexeme pos) (fmt "Syntax error while looking for '.text'") lexbuf
  )
# 762 "ocamllex/STKCompiler.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_lexer_rec pos lexbuf __ocaml_lex_state

and error pos msg lexbuf =
   __ocaml_lex_error_rec pos msg lexbuf 114
and __ocaml_lex_error_rec pos msg lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 348 "ocamllex/STKCompiler.mll"
              ( raise (Compilation_Error(pos, msg)) )
# 774 "ocamllex/STKCompiler.ml"

  | 1 ->
# 349 "ocamllex/STKCompiler.mll"
              ( error (next_lexeme pos) msg lexbuf )
# 779 "ocamllex/STKCompiler.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_error_rec pos msg lexbuf __ocaml_lex_state

;;

# 352 "ocamllex/STKCompiler.mll"
 
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

# 806 "ocamllex/STKCompiler.ml"
