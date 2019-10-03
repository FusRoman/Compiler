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


  let fmt = sprintf

  type no_arg = Nop | Exit
  type unary_proc = Print | Jump
  type binary_proc = JumpWhen | Write
  type unary_fun = Read | Minus | Not
  type binary_fun =
    | Add   | Sub  | Mult  
    | Div   | Rem  | Eq 
    | Neq   | Lt   | Le    
    | Gt    | Ge   | And  
    | Or

  type instr =
    | DefineTag of string
    | PushCte of int (** int*)
    | PushTag of string (** int*)
    | StackPointer
    | NoArg of no_arg
    | UnaryProc of unary_proc (** int*)
    | BinaryProc of binary_proc (** int * int*)
    | UnaryFun of unary_fun (** int * int*)
    | BinaryFun of binary_fun (** int * int * int*)

  type data =
    | Cte of int
    | Address of string
    | Value of string
    | UnnamedValue
    | ArithmeticResult (* of suite d'instructions *)
    | Unknown

  (* Contient des infos sur la ligne courante, utile pour les messages d'erreur *)
  type line_info = {
    num: int;
    mutable line: string
  }

  (* Structure contenant toutes les informations utiles à la première passe *)
  type parser_info = {
    current_line: line_info;
    tags: TagSet.t;
    instr: (line_info * instr) Queue.t;
    data: (string * string) Queue.t;
  }

  (* Structure pour la seconde passe, qui correspond à l'optimisation *)
  type compiler_state = {
    acc_empty: bool; (* indique si acc est disponible sans avoir à push dans la pile *)
    first_push: bool; (* Vaut faux tant qu'on n'a jamais mis une valeur dans l'acc *)
    defined_tags: TagSet.t; (* Ensemble des tags définis et donc utilisables *)
    remaining_instr: (line_info * instr) Queue.t; (* Liste des instructions à compiler *)
    remaining_data: (string * string) Queue.t (* Liste des déclarations de données à compiler *)
  }

  type instr_info = {
    pop: int; (* Combien d'éléments sont retirés de la liste *)
    push: bool; (* Vrai si un élément est replacé dans la pile, faux sinon *)
    ignore: bool (* Vrai s'il faut ignorer la prochaine commande complètement. Surtout utile pour DefineTag *)
  }

  type pushable = 
    | Tag of string 
    | Int of int 
    | Reg of string

  exception Compilation_Error of line_info * string * string

  let initial_info () = {
    current_line = {num = 1; line = ""};
    tags = TagSet.empty;
    instr = Queue.create ();
    data = Queue.create ()
  }

  let initial_state info = {
    acc_empty = true;
    first_push = false;
    defined_tags = info.tags;
    remaining_instr = info.instr;
    remaining_data = info.data
  }

  let next_line info = 
    {info with current_line = {
        num = info.current_line.num + 1;
        line = ""
      }
    }

  let next_lexeme info =
    info.current_line.line <- info.current_line.line ^ (Lexing.lexeme lexbuf);
    info

  let add_instr info i =
    Queue.add (info.current_line, i) info.instr;
    info    

  let add_tag info tag =
    let info' = add_instr info (DefineTag tag) in
    {info' with tags = TagSet.add tag info.tags}

  let add_data info tag value =
    Queue.add (tag, value) info.data;
    {info with tags = TagSet.add tag info.tags}


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


  (* Retourne quelques informations sur une instruction, utiles pour compiler efficacement l'instruction d'avant *)
  let get_info i =
    match i with
    | DefineTag _ ->
      {pop = 0; push = false; ignore = true}
    | PushCte _ | PushTag _ | StackPointer ->
      {pop = 0; push = true; ignore = false}
    | NoArg _ ->
      {pop = 0; push = false; ignore = false}
    | UnaryProc _ ->
      {pop = 1; push = false; ignore = false}
    | BinaryProc _ ->
      {pop = 2; push = false; ignore = false}
    | UnaryFun _ ->
      {pop = 1; push = true; ignore = false}
    | BinaryFun _ ->
      {pop = 2; push = true; ignore = false}

   (* Renvoie le nom de l'instruction STK *)
  let get_name_no_arg i =
    match i with
    | Nop -> "NOP"
    | Exit -> "EXIT"

  let get_name_unary_proc i =
    match i with
    | Jump -> "JUMP"
    | Print -> "PRINT"

  let get_name_binary_proc i =
    match i with
    | Write -> "WRITE"
    | JumpWhen -> "JUMPWHEN"

  let get_name_unary_fun i =
    match i with
    | Read -> "READ"
    | Minus -> "MINUS"
    | Not -> "NOT"

  let get_name_binary_fun i =
    match i with
    | Add -> "ADD"
    | Sub -> "SUB"
    | Mult -> "MULT"
    | Div -> "DIV"
    | Rem -> "REM"
    | Eq -> "EQ"
    | Neq -> "NEQ"
    | Lt -> "LT"
    | Le -> "LE"
    | Gt -> "GT"
    | Ge -> "GE"
    | And -> "AND"
    | Or -> "OR"
    
  let get_name_instr i =
    match i with
    | DefineTag tag ->
      tag ^ ":"
    | PushCte cte ->
      fmt "push %d" cte
    | PushTag tag ->
      fmt "push '%s'" tag
    | StackPointer ->
      "stack_pointer"
    | NoArg i' ->
      get_name_no_arg i'
    | UnaryProc i' ->
      get_name_unary_proc i'
    | BinaryProc i' ->
      get_name_binary_proc i'
    | UnaryFun i' ->
      get_name_unary_fun i'
    | BinaryFun i' ->
      get_name_binary_fun i'

  (*
    Renvoie vrai si la prochaine instruction est un push (PushCte, PushTag, StackPointer).
    Idéalement on ignorerait les NOP, de façon à ce que ce soit la prochaine instruction
    avec un effet qui soit prise en compte. Cependant, parce que la structure de queue utilisée
    n'est pas fonctionnelle et qu'elle ne propose pas non plus d'accès aléatoire, ce n'est pas très pratique.
    De plus le cas est rare vu qu'on évite de mettre des NOP inutiles en général...
    (TODO utiliser les cycles de Splashoon ?)
  *)
  let is_next_instr_push state =
    if Queue.is_empty state.remaining_instr then
      false
    else
      let (_, i) = Queue.peek state.remaining_instr in
      match i with
      | PushTag _ | PushCte _ | StackPointer ->
        true
      | _ -> false

  (*
    Un programme doit dans les faits démarrer par un push ; avant, il ne peut faire que des NOP ou des EXIT.
    A partir du premier élément dans la pile, il devient possible de faire un JUMP et vérifier que la pile est 
    nécessairement vide à un certain endroit du programme est trop compliqué (voire impossible).
    En revanche, pour le premier push, on sait que la pile est nécessairement vide donc on peut agir en conséquence.
  *)
  let push pushable state =
    let fill_acc () =
        match pushable with
        | Tag s ->
          fprintf output "ADDRESS %s %s\n" acc s
        | Int v ->
          fprintf output "CONST %s %d\n" acc v
        | Reg s ->
          fprintf output "MOVE %s %s\n" acc s
    in
    let _ =
      if state.acc_empty then
      begin
        fill_acc ();
        if not state.first_push then
          fprintf output "DECR %s 1\n" sp
      end
      else
      begin
        fprintf output "WRITE %s %s\n" sp acc;
        fill_acc ();
        fprintf output "DECR %s 1\n" sp;
      end
    in
    (* Inutile de mettre à jour la pile si la prochaine instruction pop *)
    (*fprintf output "DECR %s 1\n" sp;*)
    {state with acc_empty = false; first_push = true}

  (* Si on tombe sur un pop avant le premier push, on sait qu'il y a erreur. *)
  let no_push state line instr =
    let name = get_name_instr instr in
    if not state.first_push then
      raise (Compilation_Error(line, name, fmt "No push instruction before '%s'" name))
    else if state.acc_empty then
      raise (Failure "push: both acc_empty and first_push are true")

  (*
    Utilisé par les instructions qui prennent un élément sur la pile et n'en remettent pas (PRINT et JUMP).
    Diminue la pile de 1 et met le nouveau sommet dans acc.
    A appeler après avoir utilisé acc comme l'ancien sommet de la pile, donc à la fin en général.
    Pour les instructions à 1 argument et retournant un résultat, il suffit de stocker le résultat dans acc ;
    aucun appel à une fonction pop n'est nécessaire dans ce cas.
  *)
  let pop dest =
    fprintf output "INCR %s 1\n" sp;
    fprintf output "READ %s %s\n" dest sp

  (* Traduit une instruction i et l'écrit dans le fichier output *)
  let compile_line state line instr =
    match instr with
    | DefineTag tag ->
      fprintf output "%s:\n" tag;
      state (* plus tard, amorcer un nouveau bloc *)

    | PushCte v ->
      push (Int v) state

    | PushTag s ->
      if TagSet.mem s state.defined_tags then
        push (Tag s) state
      else
        raise (Compilation_Error(line, s, fmt "Undefined tag '%s'" s))

    | StackPointer ->
      (* Comment traiter le cas particulier stack_pointer ?
        => empiler la valeur du registre 
        ou calculer la vraie taille de la pile en incluant les registres intermédiaires (info plus utile haut niveau) ? *)
      push (Reg sp) state

    | NoArg i ->
      fprintf output "%s\n" (get_name_no_arg i);
      state

    | UnaryProc Print ->
      no_push state line instr;
      fprintf output "PRINT %s\n" acc;
      if is_next_instr_push state then
        {state with acc_empty = true}
      else
        (pop acc; state)

    | UnaryProc Jump ->
      no_push state line instr;
      let r0 = register_str 0 in
      fprintf output "MOVE %s %s\n" r0 acc;
      pop acc; (* la pile doit être parfaitement à jour avant le saut *)
      fprintf output "JUMP %s\n" r0;
      state

    | BinaryProc Write ->
      (*pop2_no_return 0 (fun dest1 -> 
          fprintf output "WRITE %s %s\n" dest1 acc)
        state line instr;
      state*)
      no_push state line instr;
      let r0 = register_str 0 in
      (*fprintf output "INCR %s 1\n" sp;
      fprintf output "READ %s %s\n" r0 sp;*)
      pop r0;
      fprintf output "WRITE %s %s\n" r0 acc;
      if is_next_instr_push state then
        {state with acc_empty = true}
      else
        (*fprintf output "INCR %s 1\n" sp;
        fprintf output "READ %s %s\n" acc sp;*)
        (pop acc; state)

    | BinaryProc JumpWhen ->
      no_push state line instr;
      let r0 = register_str 0 in
      let r1 = register_str 1 in
      fprintf output "MOVE %s %s\n" r1 acc;
      (*fprintf output "INCR %s 1\n" sp;
      fprintf output "READ %s %s\n" r0 sp;
      fprintf output "INCR %s 1\n" sp;
      fprintf output "READ %s %s\n" acc sp;*)
      pop r0;
      pop acc;
      fprintf output "JUMP %s WHEN %s\n" r0 r1;
      state

    | UnaryFun i ->
      fprintf output "%s %s %s\n" (get_name_unary_fun i) acc acc;
      state

    | BinaryFun i ->
      (*let dest1 = pop2_return 0 state line instr in
      fprintf output "%s %s %s %s\n" (get_name_binary_fun i) acc dest1 acc;
      state*)
      no_push state line instr;
      let r0 = register_str 0 in
      pop r0;
      fprintf output "%s %s %s %s\n" (get_name_binary_fun i) acc r0 acc;
      state

  let compile state =
    let rec compile_instr state =
      if not (Queue.is_empty state.remaining_instr) then
        let l, i = Queue.take state.remaining_instr in
        let state' = compile_line state l i in
        compile_instr state'
    in

    let rec compile_data state =
      if not (Queue.is_empty state.remaining_data) then
        let (tag, value) = Queue.take state.remaining_data in
        fprintf output "%s:\n%s\n" tag value;
        compile_data state
    in

    fprintf output "CONST %s 65535\n" sp;
    compile_instr state;
    compile_data state


  (* Extrait le nom du tag depuis une déclaration de tag (avec ':') *)
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
let comment = '#' [^ '\n']* ('\n' | eof)


(* Appelé en entrant dans la section .data. Appelle data_points dès qu'un tag est trouvé. *)
rule dater_tag info =
  parse
  | comment     { dater_tag (next_line info) lexbuf }
  | blank       { dater_tag (next_lexeme info) lexbuf }
  | '\n'        { dater_tag (next_line info) lexbuf }

  | ':'         {
    error info (Lexing.lexeme lexbuf) "Syntax error: unexpected ':'" lexbuf
  }

  | tag         {
    let tag = Lexing.lexeme lexbuf in
    dater_points (next_lexeme info) tag lexbuf
  }

  | integer     {
    let token = Lexing.lexeme lexbuf in
    error info token (fmt "Syntax error: found `%s` without tag" token) lexbuf
  }

  | eof         { info }

  | _           { 
    error (next_lexeme info) (Lexing.lexeme lexbuf)
      "Syntax error: only tag definitions are allowed after '.data'" lexbuf
  }


(* Appelé par dater_tag dès qu'un tag est trouvé. Appelle dater_data dès que ':' est trouvé. *)
and dater_points info previous =
  parse
  | comment     { dater_points (next_line info) previous lexbuf }
  | blank       { dater_points (next_lexeme info) previous lexbuf }
  | '\n'        { dater_points (next_line info) previous lexbuf }
  | ':'         { dater_data (next_lexeme info) previous lexbuf }

  | tag         {
    error (next_lexeme info) (Lexing.lexeme lexbuf)
      (fmt "Syntax error: tag '%s' was not given a value" previous) lexbuf
  }

  | integer     {
    let token = Lexing.lexeme lexbuf in
    error (next_lexeme info) token (fmt 
        "Syntax error: found data '%s' directly after tag '%s'. You may have forgotten ':'." 
        token previous) 
      lexbuf
  }

  | eof         {
    raise (Compilation_Error(info.current_line, (Lexing.lexeme lexbuf), 
      fmt "Syntax error: tag '%s' was not given a value" previous))
  }

  | _           { 
    error (next_lexeme info) (Lexing.lexeme lexbuf)
      (fmt "Syntax error while looking for ':' after declaration of tag '%s'" previous) lexbuf
  }


(* Appelé par dater_points quand un tag suivi de deux points a été trouvé. Cherche la donnée correspondante, puis rappelle dater_tag. *)
and dater_data info previous =
  parse
  | comment     { dater_data (next_line info) previous lexbuf }
  | blank       { dater_data (next_lexeme info) previous lexbuf }
  | '\n'        { dater_data (next_line info) previous lexbuf }

  | ':'         { 
    error (next_lexeme info) (Lexing.lexeme lexbuf) "Syntax error: duplicate ':'" lexbuf
  }

  | tag         {
    let token = Lexing.lexeme lexbuf in
    error (next_lexeme info) token
      (fmt "Syntax error: found tag '%s', expected a value" token) lexbuf
  }

  | integer     {
    let value = Lexing.lexeme lexbuf in
    let info' = add_data info previous value in
    dater_tag (next_lexeme info') lexbuf
  }

  | eof         {
    raise (Compilation_Error(info.current_line, (Lexing.lexeme lexbuf),
      fmt "Syntax error: tag '%s' was not given a value" previous))
  }

  | _           { 
    error (next_lexeme info) (Lexing.lexeme lexbuf)
      (fmt "Syntax error while looking for the value of tag '%s'" previous) lexbuf
  }


(* Analyseur de la section .text. Appelle dater_tag dès que '.data' a été trouvé. *)
and texter info = 
  parse
  | ".data"     { dater_tag (next_lexeme info) lexbuf }

  (* Détection des espaces blanc *)
  | comment     { texter (next_line info) lexbuf }
  | blank       { texter (next_lexeme info) lexbuf }
  | '\n'        { texter (next_line info) lexbuf }

  | "NOP" | "EXIT" {
    let token = Lexing.lexeme lexbuf in
    let op =
      if token = "NOP" then Nop
      else if token = "EXIT" then Exit
      else raise (Failure (fmt "Unknown no-parameter instruction '%s'" token))
    in
    let info' = add_instr info (NoArg op) in
    texter (next_lexeme info') lexbuf
  }

  | "PRINT" | "JUMP" {
    let token = Lexing.lexeme lexbuf in
    let op =
      if token = "PRINT" then Print
      else if token = "JUMP" then Jump
      else raise (Failure (fmt "Unknown unary procedure '%s'" token))
    in
    let info' = add_instr info (UnaryProc op) in
    texter (next_lexeme info') lexbuf
  }

  | "WRITE" | "JUMPWHEN" {
    let token = Lexing.lexeme lexbuf in
    let op =
      if token = "WRITE" then Write
      else if token = "JUMPWHEN" then JumpWhen
      else raise (Failure (fmt "Unknown binary procedure '%s'" token))
    in
    let info' = add_instr info (BinaryProc op) in
    texter (next_lexeme info') lexbuf
  }

  | "READ" | "MINUS" | "NOT" {
    let token = Lexing.lexeme lexbuf in
    let op =
      if token = "READ" then Read
      else if token = "MINUS" then Minus
      else if token = "NOT" then Not
      else raise (Failure (fmt "Unknown unary function '%s'" token))
    in
    let info' = add_instr info (UnaryFun op) in
    texter (next_lexeme info') lexbuf
  }

  | "ADD" | "SUB" | "MULT" | "DIV"
  | "REM" | "EQ"  | "NEQ"  | "LT"
  | "LE"  | "GT"  | "GE"   | "AND"
  | "OR" {
    let token = Lexing.lexeme lexbuf in
    let op =
      if token = "ADD" then Add
      else if token = "SUB" then Sub
      else if token = "MULT" then Mult
      else if token = "DIV" then Div
      else if token = "REM" then Rem
      else if token = "EQ" then Eq
      else if token = "NEQ" then Neq
      else if token = "LT" then Lt
      else if token = "LE" then Le
      else if token = "GT" then Gt
      else if token = "GE" then Ge
      else if token = "AND" then And
      else if token = "OR" then Or
      else raise (Failure (fmt "Unknown binary function '%s'" token))
    in
    let info' = add_instr info (BinaryFun op) in
    texter (next_lexeme info') lexbuf
  }

  (* Définition de tag *)
  | tag blank* ':'   {
    let tag = extract_tag (Lexing.lexeme lexbuf) in
    if tag = "stack_pointer" then
      error info tag 
        "'stack_pointer' is a reserved tag and can not be used to name new tags" lexbuf
    else
      if TagSet.mem tag info.tags then
        error info tag
          (fmt "Duplicate tag '%s'" tag) lexbuf
      else
      begin
        let info' = add_tag info tag in
        texter (next_lexeme info') lexbuf
      end
  } 

  (* Empilement de tag *)
  | tag         {
    let tag = Lexing.lexeme lexbuf in
    let info' = 
      if tag = "stack_pointer" then
        add_instr info StackPointer
      else
        add_instr info (PushTag tag)
    in
    texter (next_lexeme info') lexbuf
  }

  (* Empilement d'entier *)
  | integer     { 
    let v = int_of_string (Lexing.lexeme lexbuf) in
    let info' = add_instr info (PushCte v) in
    texter (next_lexeme info') lexbuf 
  }

  | eof         { info }

  | _           { 
    error (next_lexeme info) (Lexing.lexeme lexbuf) "Syntax error" lexbuf
  }


(* Première règle à être appelée. Accepte les commentaires et lignes et appelle texter quand '.text' est rencontré. N'accepte rien d'autre. *)
and lexer info =
  parse
  | ".text"   { texter (next_lexeme info) lexbuf }
  | comment   { lexer (next_line info) lexbuf }
  | blank     { lexer (next_lexeme info) lexbuf }
  | '\n'      { lexer (next_line info) lexbuf }
  | eof       {
    raise (Compilation_Error (info.current_line, "end of file", "Reached unexpected end of file while looking for '.text'"))
  }
  | _         { 
    error (next_lexeme info) (Lexing.lexeme lexbuf)
      (fmt "Syntax error while looking for '.text'") lexbuf
  }


(* Règle d'erreur dont le seul rôle est de parcourir le reste de la ligne pour avoir un message d'erreur plus complet. *)
and error info token msg =
  parse
  | '\n'      
  | eof       { raise (Compilation_Error(info.current_line, token, msg)) }
  | _         { error (next_lexeme info) token msg lexbuf }


{
  let _ =
    let beginning = Sys.time () in
    try
      let info = lexer (initial_info ()) lexbuf in
      let state = initial_state info in
      compile state;
      close_out output;
      printf "Compilation STK -> ASM successful (%fs)\n" (Sys.time () -. beginning);
      exit 0
    with
    | Compilation_Error (line, token, msg) ->
      printf "[ERROR] Line %d, token '%s':\n%s\n%s\n" line.num token line.line msg;
      exit 1
    | Failure msg ->
      printf "[ERROR] The compilation failed due to an internal error. Error : %s\n" msg;
      exit 1
}