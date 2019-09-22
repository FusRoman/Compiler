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
    fprintf output "WRITE %s %s\n" sp (register_str x)
    (*fprintf output "WRITE %s %s\n" spa sp*)

  let pop dest =
    let rec pop_rec l =
      match l with
      | [] -> 
        (*fprintf output "WRITE %s %s\n" spa sp*)
        ()
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

# 120 "ocamllex/STKCompiler.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\248\255\249\255\250\255\011\000\021\000\031\000\253\255\
    \254\255\001\000\255\255\002\000\164\000\248\255\249\255\250\255\
    \106\000\127\000\192\000\253\255\254\255\004\000\255\255\032\000\
    \068\001\248\255\249\255\250\255\137\000\175\000\032\001\253\255\
    \254\255\079\000\255\255\114\000\182\001\237\255\238\255\011\001\
    \042\001\040\002\008\000\126\000\166\000\018\000\007\000\129\000\
    \166\000\008\000\012\000\027\000\041\000\083\000\129\000\252\255\
    \253\255\115\000\027\000\068\000\050\000\070\000\255\255\254\255\
    \158\000\088\000\120\000\243\255\251\255\242\255\097\000\088\000\
    \250\255\128\000\124\000\119\000\249\255\128\001\137\000\248\255\
    \133\000\127\000\164\000\247\255\157\000\156\000\150\000\166\000\
    \183\000\175\000\245\255\178\000\177\000\240\000\243\000\244\255\
    \243\000\004\001\005\001\008\001\245\000\241\255\186\001\194\001\
    \250\255\251\255\252\255\253\255\066\001\241\000\001\001\240\000\
    \245\000\255\255\254\255\099\001\100\001\254\255\255\255";
  Lexing.lex_backtrk =
   "\255\255\255\255\255\255\255\255\004\000\007\000\003\000\255\255\
    \255\255\007\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \004\000\007\000\003\000\255\255\255\255\007\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\004\000\007\000\003\000\255\255\
    \255\255\007\000\255\255\255\255\255\255\255\255\255\255\016\000\
    \018\000\015\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\255\255\
    \255\255\018\000\018\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\009\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\005\000\005\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255";
  Lexing.lex_default =
   "\001\000\000\000\000\000\000\000\255\255\255\255\255\255\000\000\
    \000\000\011\000\000\000\011\000\013\000\000\000\000\000\000\000\
    \255\255\255\255\255\255\000\000\000\000\023\000\000\000\023\000\
    \025\000\000\000\000\000\000\000\255\255\255\255\255\255\000\000\
    \000\000\035\000\000\000\035\000\037\000\000\000\000\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
    \000\000\064\000\255\255\255\255\255\255\255\255\000\000\000\000\
    \064\000\255\255\255\255\000\000\000\000\000\000\255\255\255\255\
    \000\000\255\255\255\255\255\255\000\000\255\255\255\255\000\000\
    \255\255\255\255\255\255\000\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\000\000\255\255\255\255\255\255\255\255\000\000\
    \255\255\255\255\255\255\255\255\255\255\000\000\255\255\104\000\
    \000\000\000\000\000\000\000\000\115\000\255\255\255\255\255\255\
    \255\255\000\000\000\000\115\000\117\000\000\000\000\000";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\008\000\003\000\010\000\010\000\000\000\022\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \008\000\000\000\000\000\009\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\022\000\000\000\000\000\005\000\000\000\000\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\007\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\006\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\034\000\069\000\100\000\099\000\084\000\080\000\006\000\
    \077\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\006\000\073\000\034\000\063\000\006\000\059\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\069\000\060\000\061\000\062\000\
    \063\000\069\000\071\000\070\000\072\000\020\000\015\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\069\000\020\000\098\000\065\000\021\000\
    \068\000\074\000\075\000\076\000\067\000\079\000\081\000\097\000\
    \066\000\017\000\069\000\082\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\019\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\083\000\085\000\069\000\086\000\087\000\088\000\092\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\069\000\091\000\089\000\090\000\096\000\093\000\
    \002\000\255\255\255\255\018\000\255\255\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \255\255\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\039\000\039\000\039\000\039\000\039\000\
    \039\000\039\000\039\000\039\000\039\000\094\000\095\000\069\000\
    \069\000\069\000\069\000\069\000\114\000\032\000\027\000\255\255\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\039\000\039\000\039\000\039\000\039\000\039\000\
    \039\000\039\000\039\000\039\000\032\000\110\000\111\000\033\000\
    \112\000\113\000\000\000\000\000\000\000\114\000\118\000\000\000\
    \000\000\029\000\255\255\255\255\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\031\000\030\000\
    \000\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\000\000\000\000\000\000\255\255\000\000\
    \000\000\000\000\000\000\030\000\014\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\056\000\
    \055\000\078\000\000\000\102\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\107\000\106\000\069\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\056\000\000\000\
    \000\000\057\000\102\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\107\000\040\000\058\000\108\000\039\000\039\000\
    \039\000\039\000\039\000\039\000\039\000\039\000\039\000\039\000\
    \109\000\000\000\000\000\000\000\101\000\000\000\000\000\047\000\
    \000\000\000\000\045\000\053\000\000\000\043\000\000\000\000\000\
    \049\000\000\000\044\000\048\000\054\000\042\000\052\000\000\000\
    \051\000\046\000\000\000\000\000\000\000\050\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\041\000\000\000\041\000\
    \041\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
    \041\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
    \041\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
    \041\000\102\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\255\255\000\000\026\000\000\000\000\000\000\000\
    \102\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \041\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
    \041\000\041\000\101\000\255\255\118\000\000\000\000\000\000\000\
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
    \000\000\000\000\105\000\000\000\000\000\000\000\000\000\000\000\
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
    \000\000\000\000\000\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\006\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\033\000\042\000\045\000\046\000\049\000\050\000\000\000\
    \051\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\052\000\035\000\057\000\006\000\058\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\053\000\059\000\060\000\061\000\
    \064\000\065\000\070\000\053\000\071\000\012\000\012\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\043\000\012\000\047\000\054\000\012\000\
    \066\000\073\000\074\000\075\000\066\000\078\000\080\000\047\000\
    \054\000\012\000\043\000\081\000\012\000\012\000\012\000\012\000\
    \012\000\012\000\012\000\012\000\012\000\012\000\012\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\082\000\084\000\044\000\085\000\086\000\087\000\048\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\044\000\048\000\088\000\089\000\091\000\092\000\
    \000\000\009\000\011\000\012\000\021\000\012\000\012\000\012\000\
    \012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
    \012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
    \012\000\012\000\012\000\012\000\012\000\012\000\012\000\018\000\
    \023\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\039\000\039\000\039\000\039\000\039\000\
    \039\000\039\000\039\000\039\000\039\000\093\000\094\000\096\000\
    \097\000\098\000\099\000\100\000\108\000\024\000\024\000\033\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\040\000\040\000\040\000\040\000\040\000\040\000\
    \040\000\040\000\040\000\040\000\024\000\109\000\110\000\024\000\
    \111\000\112\000\255\255\255\255\255\255\115\000\116\000\255\255\
    \255\255\024\000\035\000\057\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\030\000\
    \255\255\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\255\255\255\255\255\255\064\000\255\255\
    \255\255\255\255\255\255\024\000\012\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\036\000\
    \036\000\077\000\255\255\102\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\103\000\103\000\077\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\036\000\255\255\
    \255\255\036\000\102\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\103\000\036\000\036\000\103\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \103\000\255\255\255\255\255\255\102\000\255\255\255\255\036\000\
    \255\255\255\255\036\000\036\000\255\255\036\000\255\255\255\255\
    \036\000\255\255\036\000\036\000\036\000\036\000\036\000\255\255\
    \036\000\036\000\255\255\255\255\255\255\036\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\036\000\255\255\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\041\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\108\000\255\255\024\000\255\255\255\255\255\255\
    \041\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \041\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
    \041\000\041\000\041\000\115\000\116\000\255\255\255\255\255\255\
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
    \255\255\255\255\103\000\255\255\255\255\255\255\255\255\255\255\
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
# 128 "ocamllex/STKCompiler.mll"
                ( dater_tag (next_line pos) lexbuf )
# 397 "ocamllex/STKCompiler.ml"

  | 1 ->
# 129 "ocamllex/STKCompiler.mll"
                ( dater_tag (next_lexeme pos) lexbuf )
# 402 "ocamllex/STKCompiler.ml"

  | 2 ->
# 130 "ocamllex/STKCompiler.mll"
                (
    error pos "Syntax error: unexpected ':'" lexbuf
  )
# 409 "ocamllex/STKCompiler.ml"

  | 3 ->
# 133 "ocamllex/STKCompiler.mll"
                (
    let tag = Lexing.lexeme lexbuf in
    fprintf output "%s:\n" tag;
    dater_points (next_lexeme pos) tag lexbuf
  )
# 418 "ocamllex/STKCompiler.ml"

  | 4 ->
# 138 "ocamllex/STKCompiler.mll"
                (
    error pos (fmt "Syntax error: found `%s` without a tag" (Lexing.lexeme lexbuf)) lexbuf
  )
# 425 "ocamllex/STKCompiler.ml"

  | 5 ->
# 141 "ocamllex/STKCompiler.mll"
                ( dater_tag (next_line pos) lexbuf )
# 430 "ocamllex/STKCompiler.ml"

  | 6 ->
# 142 "ocamllex/STKCompiler.mll"
                ()
# 435 "ocamllex/STKCompiler.ml"

  | 7 ->
# 143 "ocamllex/STKCompiler.mll"
                ( 
    error (next_lexeme pos) "Syntax error: only tag definitions are allowed after '.data'" lexbuf
  )
# 442 "ocamllex/STKCompiler.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_dater_tag_rec pos lexbuf __ocaml_lex_state

and dater_points pos previous lexbuf =
   __ocaml_lex_dater_points_rec pos previous lexbuf 12
and __ocaml_lex_dater_points_rec pos previous lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 149 "ocamllex/STKCompiler.mll"
                ( dater_points (next_line pos) previous lexbuf )
# 454 "ocamllex/STKCompiler.ml"

  | 1 ->
# 150 "ocamllex/STKCompiler.mll"
                ( dater_points (next_lexeme pos) previous lexbuf )
# 459 "ocamllex/STKCompiler.ml"

  | 2 ->
# 151 "ocamllex/STKCompiler.mll"
                ( dater_data (next_lexeme pos) previous lexbuf )
# 464 "ocamllex/STKCompiler.ml"

  | 3 ->
# 152 "ocamllex/STKCompiler.mll"
                (
   error (next_lexeme pos) (fmt "Syntax error: tag '%s' was not given a value" previous) lexbuf
  )
# 471 "ocamllex/STKCompiler.ml"

  | 4 ->
# 155 "ocamllex/STKCompiler.mll"
                (
    error (next_lexeme pos) (fmt "Syntax error: found data '%s' directly after tag '%s'. You may have forgotten ':'." (Lexing.lexeme lexbuf) previous) lexbuf
  )
# 478 "ocamllex/STKCompiler.ml"

  | 5 ->
# 158 "ocamllex/STKCompiler.mll"
                ( dater_points (next_line pos) previous lexbuf )
# 483 "ocamllex/STKCompiler.ml"

  | 6 ->
# 159 "ocamllex/STKCompiler.mll"
                (
    raise (Compilation_Error(pos, fmt "Syntax error: tag '%s' was not given a value" previous))
  )
# 490 "ocamllex/STKCompiler.ml"

  | 7 ->
# 162 "ocamllex/STKCompiler.mll"
                ( 
    error (next_lexeme pos) (fmt "Syntax error while looking for ':' after declaration of tag '%s'" previous) lexbuf
  )
# 497 "ocamllex/STKCompiler.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_dater_points_rec pos previous lexbuf __ocaml_lex_state

and dater_data pos previous lexbuf =
   __ocaml_lex_dater_data_rec pos previous lexbuf 24
and __ocaml_lex_dater_data_rec pos previous lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 168 "ocamllex/STKCompiler.mll"
                ( dater_data (next_line pos) previous lexbuf )
# 509 "ocamllex/STKCompiler.ml"

  | 1 ->
# 169 "ocamllex/STKCompiler.mll"
                ( dater_data (next_lexeme pos) previous lexbuf )
# 514 "ocamllex/STKCompiler.ml"

  | 2 ->
# 170 "ocamllex/STKCompiler.mll"
                ( 
    error (next_lexeme pos) "Syntax error: duplicate ':'" lexbuf
  )
# 521 "ocamllex/STKCompiler.ml"

  | 3 ->
# 173 "ocamllex/STKCompiler.mll"
                (
   error (next_lexeme pos) (fmt "Syntax error: found tag '%s', expected a value" (Lexing.lexeme lexbuf)) lexbuf
  )
# 528 "ocamllex/STKCompiler.ml"

  | 4 ->
# 176 "ocamllex/STKCompiler.mll"
                (
    fprintf output "%s\n" (Lexing.lexeme lexbuf);
    dater_tag (next_lexeme pos) lexbuf
  )
# 536 "ocamllex/STKCompiler.ml"

  | 5 ->
# 180 "ocamllex/STKCompiler.mll"
                ( dater_data (next_line pos) previous lexbuf )
# 541 "ocamllex/STKCompiler.ml"

  | 6 ->
# 181 "ocamllex/STKCompiler.mll"
                (
    raise (Compilation_Error(pos, fmt "Syntax error: tag '%s' was not given a value" previous))
  )
# 548 "ocamllex/STKCompiler.ml"

  | 7 ->
# 184 "ocamllex/STKCompiler.mll"
                ( 
    error (next_lexeme pos) (fmt "Syntax error while looking for the value of tag '%s'" previous) lexbuf
  )
# 555 "ocamllex/STKCompiler.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_dater_data_rec pos previous lexbuf __ocaml_lex_state

and texter pos lexbuf =
   __ocaml_lex_texter_rec pos lexbuf 36
and __ocaml_lex_texter_rec pos lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 190 "ocamllex/STKCompiler.mll"
                ( dater_tag (next_lexeme pos) lexbuf )
# 567 "ocamllex/STKCompiler.ml"

  | 1 ->
# 193 "ocamllex/STKCompiler.mll"
                ( texter (next_line pos) lexbuf )
# 572 "ocamllex/STKCompiler.ml"

  | 2 ->
# 194 "ocamllex/STKCompiler.mll"
                ( texter (next_lexeme pos) lexbuf )
# 577 "ocamllex/STKCompiler.ml"

  | 3 ->
# 195 "ocamllex/STKCompiler.mll"
                ( texter (next_line pos) lexbuf )
# 582 "ocamllex/STKCompiler.ml"

  | 4 ->
# 198 "ocamllex/STKCompiler.mll"
                ( 
    fprintf output "NOP\n";
    texter (next_lexeme pos) lexbuf
  )
# 590 "ocamllex/STKCompiler.ml"

  | 5 ->
# 202 "ocamllex/STKCompiler.mll"
                ( 
    fprintf output "EXIT\n";
    texter (next_lexeme pos) lexbuf
  )
# 598 "ocamllex/STKCompiler.ml"

  | 6 ->
# 208 "ocamllex/STKCompiler.mll"
                (
    pop [0];
    fprintf output "PRINT $r0\n";
    texter (next_lexeme pos) lexbuf
  )
# 607 "ocamllex/STKCompiler.ml"

  | 7 ->
# 215 "ocamllex/STKCompiler.mll"
                (
    pop_n 1;
    (* on doit mettre à jour la valeur de stack_pointer, au cas où c'est ce qu'on veut lire *)
    fprintf output "WRITE %s %s\n" spa sp;
    fprintf output "READ $r1 $r0\n";
    push (Reg 1);
    texter (next_lexeme pos) lexbuf
  )
# 619 "ocamllex/STKCompiler.ml"

  | 8 ->
# 224 "ocamllex/STKCompiler.mll"
                (
    pop_n 2;
    fprintf output "WRITE $r0 $r1\n";
    texter (next_lexeme pos) lexbuf
  )
# 628 "ocamllex/STKCompiler.ml"

  | 9 ->
# 231 "ocamllex/STKCompiler.mll"
                (
    pop_n 1;
    fprintf output "JUMP $r0\n";
    texter (next_lexeme pos) lexbuf
  )
# 637 "ocamllex/STKCompiler.ml"

  | 10 ->
# 236 "ocamllex/STKCompiler.mll"
                (
    pop_n 2;
    fprintf output "JUMP $r0 WHEN $r1\n";
    texter (next_lexeme pos) lexbuf
  )
# 646 "ocamllex/STKCompiler.ml"

  | 11 ->
# 243 "ocamllex/STKCompiler.mll"
                (  
    pop_n 1;
    fprintf output "MINUS $r0 $r0\n";
    push (Reg 0);
    texter (next_lexeme pos) lexbuf
  )
# 656 "ocamllex/STKCompiler.ml"

  | 12 ->
# 249 "ocamllex/STKCompiler.mll"
                (
    pop_n 1;
    fprintf output "NEG $r0 $r0\n";
    push (Reg 0);
    texter (next_lexeme pos) lexbuf
  )
# 666 "ocamllex/STKCompiler.ml"

  | 13 ->
# 260 "ocamllex/STKCompiler.mll"
                (
    pop_n 2;
    fprintf output "%s $r0 $r0 $r1\n" (Lexing.lexeme lexbuf);
    push (Reg 0);
    texter (next_lexeme pos) lexbuf
  )
# 676 "ocamllex/STKCompiler.ml"

  | 14 ->
# 268 "ocamllex/STKCompiler.mll"
                     (
    fprintf output "%s\n" (Lexing.lexeme lexbuf);
    texter (next_lexeme pos) lexbuf 
  )
# 684 "ocamllex/STKCompiler.ml"

  | 15 ->
# 272 "ocamllex/STKCompiler.mll"
                (
    push (Tag (Lexing.lexeme lexbuf));
    texter (next_lexeme pos) lexbuf
  )
# 692 "ocamllex/STKCompiler.ml"

  | 16 ->
# 276 "ocamllex/STKCompiler.mll"
                ( 
    push (Int (Lexing.lexeme lexbuf));
    texter (next_lexeme pos) lexbuf 
  )
# 700 "ocamllex/STKCompiler.ml"

  | 17 ->
# 281 "ocamllex/STKCompiler.mll"
                ()
# 705 "ocamllex/STKCompiler.ml"

  | 18 ->
# 283 "ocamllex/STKCompiler.mll"
                ( 
    error (next_lexeme pos) "Syntax error" lexbuf
  )
# 712 "ocamllex/STKCompiler.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_texter_rec pos lexbuf __ocaml_lex_state

and lexer pos lexbuf =
   __ocaml_lex_lexer_rec pos lexbuf 103
and __ocaml_lex_lexer_rec pos lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 289 "ocamllex/STKCompiler.mll"
              ( texter (next_lexeme pos) lexbuf )
# 724 "ocamllex/STKCompiler.ml"

  | 1 ->
# 290 "ocamllex/STKCompiler.mll"
              ( lexer (next_line pos) lexbuf )
# 729 "ocamllex/STKCompiler.ml"

  | 2 ->
# 291 "ocamllex/STKCompiler.mll"
              ( lexer (next_lexeme pos) lexbuf )
# 734 "ocamllex/STKCompiler.ml"

  | 3 ->
# 292 "ocamllex/STKCompiler.mll"
              ( lexer (next_line pos) lexbuf )
# 739 "ocamllex/STKCompiler.ml"

  | 4 ->
# 293 "ocamllex/STKCompiler.mll"
              (
    raise (Compilation_Error (pos, "Reached unexpected end of file"))
  )
# 746 "ocamllex/STKCompiler.ml"

  | 5 ->
# 296 "ocamllex/STKCompiler.mll"
              ( 
    error (next_lexeme pos) (fmt "Syntax error while looking for '.text'") lexbuf
  )
# 753 "ocamllex/STKCompiler.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_lexer_rec pos lexbuf __ocaml_lex_state

and error pos msg lexbuf =
   __ocaml_lex_error_rec pos msg lexbuf 116
and __ocaml_lex_error_rec pos msg lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 303 "ocamllex/STKCompiler.mll"
              ( raise (Compilation_Error(pos, msg)) )
# 765 "ocamllex/STKCompiler.ml"

  | 1 ->
# 304 "ocamllex/STKCompiler.mll"
              ( 
    error (next_lexeme pos) msg lexbuf
  )
# 772 "ocamllex/STKCompiler.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_error_rec pos msg lexbuf __ocaml_lex_state

;;

# 309 "ocamllex/STKCompiler.mll"
 
  let _ =
    let beginning = Sys.time () in
    fprintf output "ADDRESS %s stack_pointer\n" spa;
    fprintf output "READ %s %s\n" sp spa;
    try
      lexer first_position lexbuf;
      (* si succès -> *)
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

# 800 "ocamllex/STKCompiler.ml"
