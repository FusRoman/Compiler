{
  (* Version Acc (avec 1 seul registre accumulateur) *)
  open Printf
  open Cycle

  let max_uint16 = 65535

  (*
    La compilation se passe en deux étapes.

    1) La lecture du fichier
      Le but de cette étape est de transformer le fichier en une suite d'instructions abstraites
      (voir le type instr). Au passage :
      - on repère les erreurs de syntaxe
        La syntaxe du langage étant très simple, tout est réalisable avec le lexeur. Chaque règle
        est comme l'état d'un automate.
      - on note les tags qui ont été définis
        stack_pointer est un cas particulier puisqu'il correspond en fait à un registre
      - on réalise quelques optimisations préalables
        Certains ADD et SUB sont transformés en INCR et DECR, 
        empiler un tag puis READ se transforme en READ
      - le fichier est traduit à l'aide du type instr en quelque chose de plus facilement utilisable
        pour la deuxième étape.

    2) La compilation proprement dite
      On a maintenant toutes nos instructions. On les traduit donc une par une en ASM.
      Additionnellement, lorsqu'on compile une instruction i, on peut regarder l'instruction i+1 
      pour d'éventuelles optimisations. On utilise pour l'instant ce mécanisme pour ne pas mettre 
      à jour la pile lorsqu'on doit retirer un élément, mais qu'on sait que la prochaine instruction 
      est un empilement.

    On ne peut pas avoir une seule passe sans sacrifier les optimisations rétroactives (pour l'instant,
    la seule est décrite dans la phase 2). Si nous avons le temps, nous souhaiterions implémenter
    une optimisation non demandée qui consiste à repérer les séquences d'instruction avec un effet 
    identique sur la pile et stocker le résultat dans un registre afin de le réaliser plus tard.
    Cette optimisation fera un usage plus poussé encore des deux passes.
  *)

  (* **********************************************************************************************
   *
   * Initialisation
   *
   * *********************************************************************************************)

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

  (* **********************************************************************************************
   *
   * Types représentant les instructions ou les données
   *
   * *********************************************************************************************)

  type instr =
    | DefineTag of string
    | PushCte of int
    | PushTag of string
    | Nop
    | Exit
    | Print
    | Jump
    | JumpWhen
    | Write
    | Read
    | DirectRead of string
    | Minus
    | Cpl
    | Not
    | Incr of int
    | Decr of int
    | Add
    | Sub
    | Mult
    | Div
    | Rem
    | Eq
    | Neq
    | Lt
    | Le
    | Gt
    | Ge
    | And
    | Or

  type pushable = 
    | Tag of string
    | DirectTag of string
    | Int of int 
    | Reg of string


  (* **********************************************************************************************
   *
   * Autres types
   *
   * *********************************************************************************************)

  (* Contient des infos sur la ligne courante, utile pour les messages d'erreur *)
  type line_info = {
    num: int;
    mutable line: string
  }

  (* Lancée lorsqu'une erreur dans le programme à compiler est détectée en seconde passe. *)
  exception Compilation_Error of line_info * string * string

  (* Structure contenant toutes les informations utiles à la première passe *)
  type parser_info = {
    current_line: line_info;
    tags: Tagset.t;
    instr: (line_info * instr) cycle;
    data: (string * string) cycle;
    previous_is_cte: bool;
    previous_is_tag: bool
  }

  (* Structure pour la seconde passe, qui correspond à l'optimisation *)
  type compiler_state = {
    stack_depth: int; (* Profondeur de pile *)
    unused_regs: int; (* Nombre de registres libres *)
    commit: bool; (* Vaut true s'il faut mettre à jour stack_pointer à la fin de l'instruction *)
    defined_tags: Tagset.t; (* Ensemble des tags définis et donc utilisables *)
    remaining_instr: (line_info * instr) cycle; (* Liste des instructions à compiler *)
    remaining_data: (string * string) cycle (* Liste des déclarations de données à compiler *)
  }

  (* **********************************************************************************************
   *
   * Fonctions sur les instructions
   *
   * *********************************************************************************************)

  (* Renvoie le nom d'une instruction STK *)
  let get_name_instr i =
    match i with
    | DefineTag tag ->
      tag ^ ":"
    | PushCte cte ->
      fmt "push %d" cte
    | PushTag tag ->
      fmt "push '%s'" tag
    | DirectRead tag ->
      fmt "DIRECTREAD '%s'" tag
    | Nop -> "NOP"
    | Exit -> "EXIT"
    | Jump -> "JUMP"
    | Print -> "PRINT"
    | Write -> "WRITE"
    | JumpWhen -> "JUMPWHEN"
    | Read -> "READ"
    | Minus -> "MINUS"
    | Cpl -> "CPL"
    | Not -> "NOT"
    | Decr n -> fmt "DECR %d" n
    | Incr n -> fmt "INCR %d" n
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

  (*
    Renvoie vrai si la prochaine instruction est un push (PushCte, PushTag).
    Ignore les NOP rencontrés.
  *)
  let is_next_instr_push state =
    let rec next_instr c =
      if c = empty_cycle then
        false
      else
        let ((_, i), c') = take c in
        match i with
        | PushTag _ | PushCte _ | DirectRead _ -> true
        | Nop -> next_instr c'
        | _ -> false
    in
    next_instr state.remaining_instr


  (* **********************************************************************************************
   *
   * Fonctions de lecture
   *
   * *********************************************************************************************)

  let initial_info () = {
    current_line = {num = 1; line = ""};
    tags = Tagset.singleton "stack_pointer";
    instr = empty_cycle;
    data = empty_cycle;
    previous_is_cte = false;
    previous_is_tag = false
  }

  let next_line info =
    let token = Lexing.lexeme lexbuf in
    let length = String.length token in
    if length > 0 then
    begin
      if length > 1 && token.[length - 1] = '\n' then
        info.current_line.line <- info.current_line.line ^ (String.sub token 0 (length - 1))
      else
        info.current_line.line <- info.current_line.line ^ token
    end;
    {info with current_line = {
        num = info.current_line.num + 1;
        line = ""
      }
    }

  let next_lexeme info =
    info.current_line.line <- info.current_line.line ^ (Lexing.lexeme lexbuf);
    info    

  (* 
    Ajoute une instruction dans la liste des instructions à compiler.
    Quelques optimisations sont faites au passage :
    - empiler un tag puis appeler READ sera compilé comme un seul DIRECTREAD
      En bytecode, cela résultera exactement au même donc au final, on a surtout gagné quelques rares lignes dans le fichier .asm.

    - empiler une constante, puis un nombre arbitraire de NOP, et enfin ADD ou SUB résultera en assembleur en la même suite de NOP suivie d'un INCR ou d'un DECR
      Les NOP sont surtout utiles pour le débug, pour se repérer dans un code compilé. J'ai donc préféré les laisser.
      Cela dit, puisque l'empilement de la constante est bien déplacé, l'ordre des opérations change en fait.

      Il est possible de généraliser cette optimisation en gardant une liste des instructions, chacune associée à une pile virtuelle.
      Une pile virtuelle est remise à zéro à chaque déclaration d'étiquette, contient la hauteur de pile générée par le code depuis la dernière déclaration d'étiquette,
      et son contenu constitué soit de constantes, soit d'autres valeurs auxquelles on ne s'intéresse pas car il n'y a pas grand-chose à optimiser.
      En rajoutant une instruction, on mettrait à jour la pile virtuelle. Cela permet ensuite de détecter qu'une opération arithmétique ou logique n'a pour arguments que des
      constantes. Dans ce cas, on peut alors remplacer l'opération et ses arguments par un seul empilement de constante.

      Remplacer les instructions implique de parcourir au moins une partie du code déjà parcourur auparavant, augmentant considérablement la complexité.
      Cela dit en pratique cela ne devrait pas trop être un problème vu que les cas problématiques consistent à d'abord empiler les constantes, mettre quelques instructions
      indépendantes au milieu, puis seulement après les instructions qui utilisent les constantes. Les compilateurs de langages plus haut niveau devraient organiser le code 
      de manière plus intuitive donc je ne pense pas que ce soit réellement un problème.

      La vraie raison pour laquelle je n'ai pas implémenté cette optimisation est qu'il me semble que les compilateurs qui manipulent un arbre de syntaxe (comme ART) sont plus
      adaptés à la tâche car la détection d'opérations sur des constantes et leur remplacement y sont bien moins coûteux et lourds algorithmiquement.
      Ils pourront aussi étendre le précalcul de constantes à d'éventuelles fonctions prédéfinies.

      De plus, bien qu'en STK on puisse faire '96 1 10 PRINT ADD PRINT' qui, avec un compilateur plus optimisé, pourrait donner '97 10 PRINT PRINT' ou '10 PRINT 97 PRINT',
      ce genre de calculs avec des "calculs indépendants intermédiaires" (ici 10 PRINT) ne peut pas être fait en ART grâce à sa grammaire : le seul code équivalent
      y est 'print(10); print(97);'. Les expressions arithmétiques et booléennes sont délimitées, ce qui nous arrange.

      On gagne donc du temps à ne pas s'en occuper ici. En revanche, la rédaction de commentaire a peut-être pris de temps que l'implémentation de l'optimisation.
  *)
  let add_instr info i =
    (* Enlève le dernier pushcte (éventuellement derrière des Nop) *)
    let rec remove_last_push filter not_found instr =
      if instr = empty_cycle then
        raise (Failure not_found)
      else
        let (l, i), s = take_last instr in
        match filter i with
        | None -> 
          let (c, s') = remove_last_push filter not_found s in
          (c, append s' (l, i))
        | Some c ->
          (c, s)
    in

    let remove_last_push_cte = 
      remove_last_push (fun i ->
        match i with
        | PushCte c -> Some c
        | Nop -> None
        | _ -> 
          raise (Failure (fmt "previous_is_cte is true but found instruction '%s' before any PushCte" (get_name_instr i)))
      ) "previous_is_cte is true but could not find any PushCte" 
    in

    let remove_last_push_tag = 
      remove_last_push (fun i ->
        match i with
        | PushTag t -> Some t
        | Nop -> None
        | _ -> 
          raise (Failure (fmt "previous_is_tag is true but found instruction '%s' before any PushTag" (get_name_instr i)))
      ) "previous_is_tag is true but could not find any PushTag" 
    in

    (* La dernière instruction est-elle PushCte ou PushTag, en ignorant Nop ? *)
    let is_cte, is_tag = 
      match i with
      | PushCte _ -> (true, false)
      | PushTag _ -> (false, true)
      | Nop -> (info.previous_is_cte, info.previous_is_tag)
      | _ -> (false, false)
    in

    (* Rajoute l'instruction, avec de légères optimisations si possible *)
    let instr =
      match i with
      | Sub when info.previous_is_cte ->
        let (c, rem_instr) = remove_last_push_cte info.instr in
        if c = 0 then
          rem_instr (* c = 0 donc l'opération ne faisait en fait rien *)
        else if c < 0 && -c > max_uint16 then
          (* c est négatif donc c'est une addition. Sa valeur absolue est représentable avec 16 bits. *)
          append rem_instr (info.current_line, Incr c)
        else if c < max_uint16 then
          (* c est représentable sur 16 bits *)
          append rem_instr (info.current_line, Decr c)
        else
          (* c n'est pas représentable sur 16 bits. Il va falloir un registre pour contenir le tout. *)
          append info.instr (info.current_line, i)

      | Add when info.previous_is_cte ->
        let (c, rem_instr) = remove_last_push_cte info.instr in
        if c = 0 then
          (* rien à faire *)
          rem_instr
        else if c < 0 && -c > max_uint16 then
          append rem_instr (info.current_line, Decr c)
        else if c < max_uint16 then
          append rem_instr (info.current_line, Incr c)
        else 
          append info.instr (info.current_line, i)

      (* Précalculer les calculs sur des constantes sera fait sur des langages de plus haut niveau;
        on ne s'en occupe pas ici même si on pourrait déjà s'occuper facilement de quelques cas avec NOT, CPL, et MINUS *)

      | Read when info.previous_is_tag ->
        let (t, instr) = remove_last_push_tag info.instr in
        append instr (info.current_line, DirectRead t)

      | _ -> 
        append info.instr (info.current_line, i)
    in
    {info with 
      instr = instr;
      previous_is_cte = is_cte;
      previous_is_tag = is_tag
    }

  let add_tag info tag =
    let info' = add_instr info (DefineTag tag) in
    {info' with tags = Tagset.add tag info.tags}

  let add_data info tag value =
    {info with 
      tags = Tagset.add tag info.tags;
      data = append info.data (tag, value)
    }


  (* **********************************************************************************************
   *
   * Procédures de compilation
   *
   * *********************************************************************************************)

  (* Retourne la représentation en assembleur du registre reg *)
  let register_str reg =
    if reg < 0 || reg >= 16 then
      raise (Failure (fmt "Invalid register number: %d" reg))
    else
      "$r" ^ (string_of_int reg)

  (* Registre spécial spa : contient l'adresse de stack_pointer à tout moment (et n'est donc jamais modifié) *)
  let spa = register_str 15

  (* Registre spécial sp : contient la valeur de stack_pointer à tout moment *)
  let sp = register_str 14

  (* Nombre de registres que l'on peut utiliser pour les calculs courants *)
  let max_reg = 14

  let initial_state info = {
    unused_regs = max_reg;
    stack_depth = 0;
    commit = false;
    defined_tags = info.tags;
    remaining_instr = info.instr;
    remaining_data = info.data
  }

  let reset_state state =
    {state with
      stack_depth = 0;
      unused_regs = max_reg;
      commit = false
    }

  (* Affiche un warning à destination de l'utilisateur *)
  let warning line token msg =
    printf "[WARNING] Line %d, token '%s':\n%s\n%s\n" line.num token line.line msg

  (* 
    Ecris la nouvelle valeur de sp dans stack_pointer, 
    de façon à être visible dans les langages plus haut niveau.
    Cette fonction doit être appelée à chaque fois que la pile physique est modifiée.
  *)
  let commit_stack state =
    if state.commit then
      fprintf output "WRITE %s %s\n" spa sp

  (* 
    Renvoie (s, r) avec s le nouvel état et r le numéro du registre actuel dans state.
    Le registre renvoyé est d'abord libéré si besoin.
    On suppose qu'on a besoin d'y stocker un résultat, donc unused_regs vaudra
    max(ancien unused_regs - 1, 0) au retour stack_depth est de toute façon incrémenté,
    donc le prochain registre courant sera le suivant.
  *)
  let get_current_register state =
    let current_reg = register_str (state.stack_depth mod max_reg) in
    let stack_depth = state.stack_depth + 1 in
    if state.unused_regs > 0 then
      (* Est-ce que le registre pointé actuellement est libre ? Si oui, on peut le renvoyer directement. *)
      ({state with unused_regs = state.unused_regs - 1; stack_depth}, current_reg)
    else
    begin
      (* Le registre n'est pas libre. Il faut d'abord empiler son ancienne valeur avant de le renvoyer. *)
      fprintf output "WRITE %s %s\n" sp current_reg;
      fprintf output "DECR %s 1\n" sp;
      ({state with stack_depth; commit = true}, current_reg)
    end

  (* 
    Réalise un push fantôme. La valeur a en fait déjà été écrite dans le registre courant,
    on met juste à jour l'état pour que le compilateur se comporte bien.
    A appeler à la fin de la compilation d'une fonction (une instruction qui dépile 1 ou 2 valeurs
    et en empile 1).
  *)
  let finish_fun state =
    fst (get_current_register state)
    (* Concrètement, revient à faire stack_depth++ et unused_regs-- *)


  (* Une fonction qui réunit tous les cas d'empilement possibles. commit_stack est appelé. *)
  let push pushable state =
    let (s, r) = get_current_register state in
    let () =
      commit_stack s;
      match pushable with
      | Tag s ->
        fprintf output "ADDRESS %s %s\n" r s
      | DirectTag s ->
        fprintf output "DIRECTREAD %s %s\n" r s
      | Int i ->
        if i >= 0 then
          fprintf output "CONST %s %d\n" r i
        else
        begin
          fprintf output "CONST %s %d\n" r (-i);
          fprintf output "MINUS %s %s\n" r r
        end
      | Reg r2 ->
        (* Devrait être inutile maintenant que stack_pointer n'est plus un registre *)
        fprintf output "MOVE %s %s\n" r r2
    in s

  (* 
    Retourne (s, r) avec :
    - s l'état mis à jour
    - r le registre contenant la valeur désirée, à utiliser avant tout push (sous forme de string)
    stack_depth sera toujours décrémenté. unused_regs sera incrémenté sauf si tous les
    registres étaient déjà inutilisés.
  *)
  let pop state line instr =
    (* Rien à pop ! On lance une exception, même si le programme pourrait en fait être valide. *)
    if state.stack_depth = 0 then
      raise (Compilation_Error(line, get_name_instr instr, 
        "Attempted pop with an empty stack. This compiler considers the stack to be empty at each label definition. " ^
        "This error should not occur if the STK source comes from the compilation of an ART program."));
    let stack_depth = state.stack_depth - 1 in
    let previous_reg = register_str (stack_depth mod max_reg) in
    if state.unused_regs < max_reg then
      (* La valeur est bien dans le registre considéré : on peut le renvoyer tel quel *)
      ({state with stack_depth; unused_regs = state.unused_regs + 1}, previous_reg)
    else
    begin
      (* Sinon elle se trouve dans la pile ; on doit d'abord charger la valeur dans le registre avant de le renvoyer *)
      fprintf output "INCR %s 1\n" sp;
      fprintf output "READ %s %s\n" previous_reg sp;
      ({state with stack_depth; commit = true}, previous_reg)
    end

  (* 
    Retourne (s, r1, r2) avec :
    - s l'état mis à jour
    - r1 le registre contenant la première valeur (la plus ancienne dans la pile)
    - r2 le registre contenant la deuxième (la plus récente)
  *)
  let pop2 state line instr =
    let (s', r2) = pop state line instr in
    let (s'', r1) = pop s' line instr in
    (s'', r1, r2)

  (* Traduit une instruction i et l'écrit dans le fichier output *)
  let compile_instr state line instr =
    match instr with
    | DefineTag tag ->
      fprintf output "%s:\n" tag;
      reset_state state

    (* Push *)
    | PushCte v ->
      push (Int v) state

    | PushTag s ->
      if Tagset.mem s state.defined_tags then
        push (Tag s) state
      else
        raise (Compilation_Error(line, s, fmt "Undefined tag '%s'" s))

    | DirectRead s ->
      push (DirectTag s) state

    (* Instructions sans effet sur la pile *)
    | Nop | Exit ->
      fprintf output "%s\n" (get_name_instr instr);
      state

    (* Procédures unaires *)
    | Print | Jump ->
      let (s, r) = pop state line instr in
      commit_stack s;
      fprintf output "%s %s\n" (get_name_instr instr) r;
      s

    (* Procédures binaires *)
    | Write ->
      let (s, r1, r2) = pop2 state line instr in
      commit_stack s;
      fprintf output "WRITE %s %s\n" r1 r2;
      s

    | JumpWhen ->
      let (s, r1, r2) = pop2 state line instr in
      commit_stack s;
      fprintf output "JUMP %s WHEN %s\n" r1 r2;
      s

    (* Fonctions unaires *)
    | Incr n ->
      let (s, r) = pop state line instr in
      commit_stack s;
      fprintf output "INCR %s %d\n" r n;
      finish_fun s

    | Decr n ->
      let (s, r) = pop state line instr in
      commit_stack s;
      fprintf output "DECR %s %d\n" r n;
      finish_fun s

    | Read | Minus | Cpl | Not ->
      let (s, r) = pop state line instr in
      commit_stack s;
      fprintf output "%s %s %s\n" (get_name_instr instr) r r;
      finish_fun s

    (* Fonctions binaires *)
    | _ ->
      let (s, r1, r2) = pop2 state line instr in
      commit_stack s;
      fprintf output "%s %s %s %s\n" (get_name_instr instr) r1 r1 r2;
      finish_fun s

  let compile state =
    let rec compile_instrs state =
      if state.remaining_instr <> empty_cycle then
        let (l, i), c' = take state.remaining_instr in
        let state' = {state with remaining_instr = c'} in
        let state'' = compile_instr state' l i in
        compile_instrs {state'' with commit = false}
    in

    let rec compile_data data =
      if data <> empty_cycle then
        let (tag, value), data' = take data in
        fprintf output "%s:\n%s\n" tag value;
        compile_data data'
    in

    fprintf output "ADDRESS %s stack_pointer\n" spa;
    fprintf output "READ %s %s\n" sp spa;
    compile_instrs state;
    compile_data state.remaining_data;
    fprintf output "stack_pointer:\n65535"

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
    let info' = add_instr info op in
    texter (next_lexeme info') lexbuf
  }

  | "PRINT" | "JUMP" {
    let token = Lexing.lexeme lexbuf in
    let op =
      if token = "PRINT" then Print
      else if token = "JUMP" then Jump
      else raise (Failure (fmt "Unknown unary procedure '%s'" token))
    in
    let info' = add_instr info op in
    texter (next_lexeme info') lexbuf
  }

  | "WRITE" | "JUMPWHEN" {
    let token = Lexing.lexeme lexbuf in
    let op =
      if token = "WRITE" then Write
      else if token = "JUMPWHEN" then JumpWhen
      else raise (Failure (fmt "Unknown binary procedure '%s'" token))
    in
    let info' = add_instr info op in
    texter (next_lexeme info') lexbuf
  }

  | "READ" | "MINUS" | "CPL" | "NOT" {
    let token = Lexing.lexeme lexbuf in
    let op =
      if token = "READ" then Read
      else if token = "MINUS" then Minus
      else if token = "CPL" then Cpl
      else if token = "NOT" then Not
      else raise (Failure (fmt "Unknown unary function '%s'" token))
    in
    let info' = add_instr info op in
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
    let info' = add_instr info op in
    texter (next_lexeme info') lexbuf
  }

  (* Définition de tag *)
  | tag blank* ':'   {
    let tag = extract_tag (Lexing.lexeme lexbuf) in
    if tag = "stack_pointer" then
      error info tag 
        "'stack_pointer' is a reserved tag and can not be used to name new tags" lexbuf
    else
      if Tagset.mem tag info.tags then
        error info tag
          (fmt "Tag '%s' is declared at least twice" tag) lexbuf
      else
      begin
        let info' = add_tag info tag in
        texter (next_lexeme info') lexbuf
      end
  } 

  (* Empilement de tag *)
  | tag         {
    let tag = Lexing.lexeme lexbuf in
    let info' = add_instr info (PushTag tag) in
    texter (next_lexeme info') lexbuf
  }

  (* Empilement d'entier *)
  | integer     {
    let token = Lexing.lexeme lexbuf in
    let v = 
      try
        int_of_string token
      with
      | Failure _ ->
        ignore (error (next_lexeme info) token 
          (fmt "String '%s' cannot be converted to int. Representable integers range from %d to %d." 
              token min_int max_int)
          lexbuf);
        0 (* Juste pour contenter le typage, inutile évidemment *)
    in
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