(** Constantes de l'architecture : mémoire de taille 2^16, et 16 registres *)
let memory_size = 65536
let nb_registers = 16

(** État de la machine :
    - mémoire adressable représentée par un tableau, le contenu d'un mot
      mémoire (32 bits) étant représenté par un entier natif de Caml (soit 63
      bits disponibles sur une architecture 64 bits),
    - ensemble des registres représenté par un tableau de même,
    - pointeur de code considéré comme un registre séparé, représenté par une
      simple référence
*)
let memory = Array.make memory_size 0
let registers = Array.make nb_registers 0
let program_counter = ref 0
  
(** Primitives de décodage : extraction du code d'opération et des paramètres
    d'une instruction *)
let op_code i = i lsr 24
let dest i = (i lsr 16) land 0xff
let op1 i = (i lsr 8) land 0xff
let op2 i = i land 0xff
let const_op i = i land 0xffff

(** Décodage et exécution d'une instruction *)
let exec_instruction i =
  (* On récupère le code d'opération sous la forme d'un nombre entier, puis on
     fait un cas par code *)
  match op_code i with
    | 0 -> (* NOP : ne rien faire *)
      ()

    | 1 -> (* EXIT : arrêter l'exécution *)
      exit 0

    | 2 -> (* PRINT : afficher un caractère *)
      print_char (char_of_int registers.(op1 i))

    | 3 -> (* CONST : charger une constante dans un registre *)
      registers.(dest i) <- const_op i
        
    | 4 -> (* READ : lire une valeur en mémoire *)
      registers.(dest i) <- memory.(registers.(op1 i))

    | 5 -> (* WRITE : écrire une valeur en mémoire *)
      memory.(registers.(dest i)) <- registers.(op1 i)

    | 6 -> (* READLAB : lire à une adresse immédiate *)
      registers.(dest i) <- memory.(const_op i)

    | 7 -> (* WRITELAB : écrire à une adresse immédiate *)
      memory.(const_op i) <- registers.(dest i)
        
    | 8 -> (* JUMP : donner une nouvelle valeur au PC *)
      program_counter := registers.(op1 i) - 1
    (* Le PC sera incrémenté de 1 après exécution de l'instruction, d'où
       décalage de la valeur lue. Alternativement on pourrait imposer comme
       convention que tous les sauts doivent pointer sur l'instruction
       précédent l'instruction cible souhaitée. *)

    | 9 -> (* JUMPWHEN : donner éventuellement une nouvelle valeur au PC *)
      if registers.(op2 i) <> 0
      then program_counter := registers.(op1 i) - 1

    | 11 ->
      failwith "unassigned opcode"

    | op when 12 <= op && op <= 14 -> (* Op arithmétique unaire *)
      let unop = match op with
        | 12 -> fun x -> x (* MOVE *)
        | 13 -> (~-)       (* MINUS *)
        | 14 -> lnot       (* NOT *)
        | _ -> assert false
      in
      registers.(dest i) <- unop registers.(op1 i)

    | 15 ->
      failwith "unassigned opcode"
        
    | op when 16 <= op && op <= 28 -> (* Op arithmétique binaire *)
      let bti f = fun a b -> if f a b then 1 else 0 in
      let binop = match op with
        | 16 -> (+)   (* ADD *)
        | 17 -> (-)   (* SUB *)
        | 18 -> ( * ) (* MULT *)
        | 19 -> (/)   (* DIV *)
        | 20 -> (mod) (* REM *)
        | 21 -> bti (=)  (* EQ *)
        | 22 -> bti (<>) (* NEQ *)
        | 23 -> bti (<)  (* LT *)
        | 24 -> bti (<=) (* LE *)
        | 25 -> bti (>)  (* GT *)
        | 26 -> bti (>=) (* GE *)
        | 27 -> (land) (* AND *)
        | 28 -> (lor)  (* OR *)
        | _ -> assert false
      in
      registers.(dest i) <- binop registers.(op1 i) registers.(op2 i)

    | 30 | 31 as op ->
      let binop = match op with
        | 30 -> (+) (* INCR *)
        | 31 -> (-) (* DECR *)
        | _ -> assert false
      in
      registers.(dest i) <- binop registers.(dest i) (const_op i)
        
    | _ ->
      failwith "unassigned opcode"

(** Lecture d'un fichier binaire et copie de son contenu en mémoire,
    par blocs de 4 octets *)
let _ =
  let file = Sys.argv.(1) in
  if not (Filename.check_suffix file ".btc") then
    failwith "expected .btc extension";
  let in_channel = open_in_bin file in
  let i = ref 0 in
  try
    while true do
      memory.(!i) <- input_binary_int in_channel;
      incr i
    done
  with
    | End_of_file ->
      close_in in_channel
      (* memory.(!i) <- memory_size; *)
      (* stack_pointer := !i *)

(** Exécution :
    1/ lire l'instruction courante
    2/ la décoder et l'exécuter
    3/ incrémenter le pointeur de code
    4/ recommencer
    La boucle ne s'arrête jamais d'elle-même, mais peut être interrompue par
    une instruction exit *)
let _ =
  while true do
    exec_instruction memory.(!program_counter);
    incr program_counter
  done
