{
  (* Version Acc (avec 1 seul registre accumulateur) *)
  open Printf
  open Cycle

  module TagSet = Set.Make(String)

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

  (* La représentation des instructions est bizarre en prévision de STKCompilerAlloc,
    comme chaque constructeur aura un nombre d'arguments déterminé par sa signature *)
  type no_arg = Nop | Exit
  type unary_proc = Print | Jump
  type binary_proc = JumpWhen | Write
  type unary_fun = Read 
    | Minus | Cpl | Not 
    | Incr of int 
    | Decr of int
  type binary_fun = Add   
    | Sub | Mult | Div   
    | Rem | Eq   | Neq  
    | Lt  | Le   | Gt    
    | Ge  | And  | Or

  type instr =
    | DefineTag of string
    | PushCte of int (** int*)
    | PushTag of string (** int*)
    | StackPointer
    | DirectRead of string
    | NoArg of no_arg
    | UnaryProc of unary_proc (** int*)
    | BinaryProc of binary_proc (** int * int*)
    | UnaryFun of unary_fun (** int * int*)
    | BinaryFun of binary_fun (** int * int * int*)

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
    tags: TagSet.t;
    instr: (line_info * instr) cycle;
    data: (string * string) cycle;
    previous_is_cte: bool;
    previous_is_tag: bool
  }

  (* Structure pour la seconde passe, qui correspond à l'optimisation *)
  type compiler_state = {
    acc_empty: bool; (* indique si acc est disponible sans avoir à push dans la pile *)
    first_push: bool; (* Vaut faux tant qu'on n'a jamais mis une valeur dans l'acc *)
    defined_tags: TagSet.t; (* Ensemble des tags définis et donc utilisables *)
    remaining_instr: (line_info * instr) cycle; (* Liste des instructions à compiler *)
    remaining_data: (string * string) cycle (* Liste des déclarations de données à compiler *)
  }

  (* **********************************************************************************************
   *
   * Fonctions sur les instructions
   *
   * *********************************************************************************************)

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
    | Cpl -> "CPL"
    | Not -> "NOT"
    | Decr n -> fmt "DECR %d" n
    | Incr n -> fmt "INCR %d" n

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
    | DirectRead tag ->
      fmt "DIRECTREAD '%s'" tag
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
    Ignore les NOP rencontrés.
  *)
  let is_next_instr_push state =
    let rec next_instr c =
      if c = empty_cycle then
        false
      else
        let ((_, i), c') = take c in
        match i with
        | PushTag _ | PushCte _ | StackPointer | DirectRead _ -> true
        | NoArg Nop -> next_instr c'
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
    tags = TagSet.empty;
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
        | NoArg Nop -> None
        | _ -> 
          raise (Failure (fmt "previous_is_cte is true but found instruction '%s' before any PushCte" (get_name_instr i)))
      ) "previous_is_cte is true but could not find any PushCte" 
    in

    let remove_last_push_tag = 
      remove_last_push (fun i ->
        match i with
        | PushTag t -> Some t
        | NoArg Nop -> None
        | _ -> 
          raise (Failure (fmt "previous_is_tag is true but found instruction '%s' before any PushTag" (get_name_instr i)))
      ) "previous_is_tag is true but could not find any PushTag" 
    in

    (* La dernière instruction est-elle PushCte ou PushTag, en ignorant Nop ? *)
    let is_cte, is_tag = 
      match i with
      | PushCte _ -> (true, false)
      | PushTag _ -> (false, true)
      | NoArg Nop -> (info.previous_is_cte, info.previous_is_tag)
      | _ -> (false, false)
    in

    (* Rajoute l'instruction, avec de légères optimisations si possible *)
    let instr =
      match i with
      | BinaryFun Sub when info.previous_is_cte ->
        let (c, rem_instr) = remove_last_push_cte info.instr in
        if c = 0 then
          rem_instr (* c = 0 donc l'opération ne faisait en fait rien *)
        else if c < 0 && -c > max_uint16 then
          (* c est négatif donc c'est une addition. Sa valeur absolue est représentable avec 16 bits. *)
          append rem_instr (info.current_line, UnaryFun (Incr c))
        else if c < max_uint16 then
          (* c est représentable sur 16 bits *)
          append rem_instr (info.current_line, UnaryFun (Decr c))
        else
          (* c n'est pas représentable sur 16 bits. Il va falloir un registre pour contenir le tout. *)
          append info.instr (info.current_line, i)

      | BinaryFun Add when info.previous_is_cte ->
        let (c, rem_instr) = remove_last_push_cte info.instr in
        if c = 0 then
          (* rien à faire *)
          rem_instr
        else if c < 0 && -c > max_uint16 then
          append rem_instr (info.current_line, UnaryFun (Decr c))
        else if c < max_uint16 then
          append rem_instr (info.current_line, UnaryFun (Incr c))
        else 
          append info.instr (info.current_line, i)

      (* Précalculer les calculs sur des constantes sera fait sur des langages de plus haut niveau;
        on ne s'en occupe pas ici même si on pourrait déjà s'occuper facilement de quelques cas avec NOT, CPL, et MINUS *)

      | UnaryFun Read when info.previous_is_tag ->
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
    {info' with tags = TagSet.add tag info.tags}

  let add_data info tag value =
    {info with 
      tags = TagSet.add tag info.tags;
      data = append info.data (tag, value)
    }


  (* **********************************************************************************************
   *
   * Procédures de compilation
   *
   * *********************************************************************************************)

  let initial_state info = {
    acc_empty = true;
    first_push = false;
    defined_tags = info.tags;
    remaining_instr = info.instr;
    remaining_data = info.data
  }

  (* Affiche un warning à destination de l'utilisateur *)
  let warning line token msg =
    printf "[WARNING] Line %d, token '%s':\n%s\n%s\n" line.num token line.line msg

  (* Retourne la représentation en assembleur du registre reg *)
  let register_str reg =
    if reg < 0 || reg >= 16 then
      raise (Failure (fmt "Invalid register number: %d" reg))
    else
      "$r" ^ (string_of_int reg)

  (* 
    Registre dans lequel on met l'adresse du sommet de la pile
    Auparavant, stack_pointer était une donnée automatiquement rajoutée à la fin des fichiers ASM,
    de façon à ce que les programmes plus haut niveau puissent y accéder.
    Mais ceci est en fait réalisable en traitant le tag 'stack_pointer' comme un cas particulier dans ce fichier.
    On peut ainsi stocker stack_pointer dans un registre, ce qui est plus pratique.
    Cela dit ce n'est pas sans conséquence en haut niveau puisqu'il ne faudra pas mettre READ juste après,
    au contraire des autres étiquettes. Il faudra voir si cela pose problème.
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

  (*
    Un programme doit dans les faits démarrer par un push ; avant, il ne peut faire que des NOP ou des EXIT.
    A partir du premier élément dans la pile, il devient possible de faire un JUMP et vérifier que la pile est 
    nécessairement vide à un certain endroit du programme est trop compliqué (voire impossible).
    En revanche, pour le premier push, on sait que la pile est nécessairement vide donc on peut agir en conséquence.
  *)
  let push pushable state =
    (* r : valeur positive à empiler; dest : registre destination; i : appeler avec 0 *)
    let rec push_positive dest i r =
      if r <= max_uint16 then
        (* r est suffisament petit pour tenir sur 16 bits (non signé) *)
        fprintf output "CONST %s %d\n" dest r
      else
      begin
        (* Si c'est trop grand, on va devoir procéder en plusieurs étapes *)
        fprintf output "CONST %s %d\n" dest max_uint16;
        let factor = r / max_uint16 in
        let r' = 
          if factor > 1 then (* Concrétement, factor <> 1 car r > max_uint16 *)
          begin
            (*fprintf output "CONST $r0 %d\n" factor;*)
            (* 
              Première étape : multiplication 
              On met le facteur dans r0 en utilisant des sous-registres.
              On a assez de registres libres pour que le calcul du facteur change les valeurs des registres spéciaux.
            *)
            let reg_factor = register_str i in
            push_positive reg_factor (i+1) factor;
            fprintf output "MULT %s %s %s\n" dest dest reg_factor;
            r - factor * max_uint16
          end
          else
            r - max_uint16
        in
        (* Deuxième étape : addition du modulo par max_uint16 *)
        if r' <> 0 then
          fprintf output "INCR %s %d\n" dest r'
      end
    in

    let fill_acc () =
        match pushable with
        | Tag s ->
          fprintf output "ADDRESS %s %s\n" acc s
        | Int v ->
          (* Si l'entier est trop grand ou trop petit, on doit charger sa valeur en plusieurs instructions *)
          if v < 0 then
          begin
            let absv =
              if v = min_int then
                max_int
              else
                -v
            in
            push_positive acc 0 absv;
            fprintf output "MINUS %s %s\n" acc acc;
            if v = min_int then
              (* Dans ce cas particulier, on peut faire moins d'instructions,
              mais vu la rareté du cas peu importe *)
              fprintf output "DECR %s 1\n" acc
          end
          else
            push_positive acc 0 v
        | Reg s ->
          fprintf output "MOVE %s %s\n" acc s
        | DirectTag s ->
          fprintf output "DIRECTREAD %s %s\n" acc s
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
        fprintf output "DECR %s 1\n" sp
      end
    in
    {state with acc_empty = false; first_push = true}

  (* Si on tombe sur un pop avant le premier push, on sait qu'il y a erreur. *)
  let no_push state line instr =
    let name = get_name_instr instr in
    if not state.first_push then
      raise (Compilation_Error(line, name, fmt "No push instruction before '%s'" name))
    else if state.acc_empty then
      raise (Failure "push: both acc_empty and first_push are true")

  (* Print les instructions utiles à un pop. *)
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

    | DirectRead s ->
      push (DirectTag s) state

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
      no_push state line instr;
      let r0 = register_str 0 in
      pop r0;
      fprintf output "WRITE %s %s\n" r0 acc;
      if is_next_instr_push state then
        {state with acc_empty = true}
      else
        (pop acc; state)

    | BinaryProc JumpWhen ->
      no_push state line instr;
      let r0 = register_str 0 in
      let r1 = register_str 1 in
      fprintf output "MOVE %s %s\n" r1 acc;
      pop r0;
      pop acc;
      fprintf output "JUMP %s WHEN %s\n" r0 r1;
      state

    | UnaryFun (Incr n) ->
      fprintf output "INCR %s %d\n" acc n;
      state

    | UnaryFun (Decr n) ->
      fprintf output "DECR %s %d\n" acc n;
      state

    | UnaryFun i ->
      fprintf output "%s %s %s\n" (get_name_unary_fun i) acc acc;
      state

    | BinaryFun i ->
      no_push state line instr;
      let r0 = register_str 0 in
      pop r0;
      fprintf output "%s %s %s %s\n" (get_name_binary_fun i) acc r0 acc;
      state

  let compile state =
    let rec compile_instr state =
      if state.remaining_instr <> empty_cycle then
        let (l, i), c' = take state.remaining_instr in
        let state' = {state with remaining_instr = c'} in
        let state'' = compile_line state' l i in
        compile_instr state''
    in

    let rec compile_data data =
      if data <> empty_cycle then
        let (tag, value), data' = take data in
        fprintf output "%s:\n%s\n" tag value;
        compile_data data'
    in

    fprintf output "CONST %s 65535\n" sp;
    compile_instr state;
    compile_data state.remaining_data


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

  | "READ" | "MINUS" | "CPL" | "NOT" {
    let token = Lexing.lexeme lexbuf in
    let op =
      if token = "READ" then Read
      else if token = "MINUS" then Minus
      else if token = "CPL" then Cpl
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
    let token = Lexing.lexeme lexbuf in
    let v = 
      try
        int_of_string token
      with
      | Failure _ ->
        ignore (error (next_lexeme info) token 
          (fmt "String '%s' cannot be converted to int. 
              Representable integers range from %d to %d." token min_int max_int)
          lexbuf);
        0 (* Juste pour contenter le typage, inutile évidemmebt *)
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