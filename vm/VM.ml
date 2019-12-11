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

let get_mem i =
  if i < 0 || i >= memory_size then
    raise (Invalid_argument 
      (Printf.sprintf "cannot read memory cell %d: index out of bounds" i));
  memory.(i)

let set_mem i v =
  if i < 0 || i >= memory_size then
    raise (Invalid_argument 
      (Printf.sprintf "cannot write memory cell %d: index out of bounds" i));
  memory.(i) <- v

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
    begin
      try
        print_char (char_of_int registers.(op1 i))
        (*Printf.printf "%d\n" registers.(op1 i)*) 
      with
      | Invalid_argument msg ->
        raise (Invalid_argument (Printf.sprintf "char_of_int: invalid value %d" registers.(op1 i)))
    end

    | 3 -> (* CONST : charger une constante dans un registre *)
      registers.(dest i) <- const_op i
        
    | 4 -> (* READ : lire une valeur en mémoire *)
      registers.(dest i) <- get_mem registers.(op1 i)

    | 5 -> (* WRITE : écrire une valeur en mémoire *)
      set_mem registers.(dest i) registers.(op1 i)

    | 6 -> (* READLAB : lire à une adresse immédiate *)
      registers.(dest i) <- get_mem (const_op i)

    | 7 -> (* WRITELAB : écrire à une adresse immédiate *)
      set_mem (const_op i) registers.(dest i)
        
    | 8 -> (* JUMP : donner une nouvelle valeur au PC *)
      program_counter := registers.(op1 i) - 1
    (* Le PC sera incrémenté de 1 après exécution de l'instruction, d'où
       décalage de la valeur lue. Alternativement on pourrait imposer comme
       convention que tous les sauts doivent pointer sur l'instruction
       précédent l'instruction cible souhaitée. *)

    | 9 -> (* JUMPWHEN : donner éventuellement une nouvelle valeur au PC *)
      if registers.(op2 i) <> 0
      then program_counter := registers.(op1 i) - 1

    | op when 12 <= op && op <= 15 -> (* Op arithmétique unaire *)
      let unop = match op with
        | 12 -> fun x -> x (* MOVE *)
        | 13 -> Arith.minus      (* MINUS *)
        | 14 -> Arith.cpl        (* CPL *)
        | 15 -> Arith.anot       (* NOT *)
        | _ -> assert false
      in
      registers.(dest i) <- unop registers.(op1 i)
        
    | op when 16 <= op && op <= 28 -> (* Op arithmétique binaire *)
      let binop = match op with
        | 16 -> Arith.add       (* ADD *)
        | 17 -> Arith.sub       (* SUB *)
        | 18 -> Arith.multiply  (* MULT *)
        | 19 -> Arith.divise    (* DIV *)
        | 20 -> Arith.modulo    (* REM *)
        | 21 -> Arith.equal     (* EQ *)
        | 22 -> Arith.different (* NEQ *)
        | 23 -> Arith.lt        (* LT *)
        | 24 -> Arith.le        (* LE *)
        | 25 -> Arith.gt        (* GT *)
        | 26 -> Arith.ge        (* GE *)
        | 27 -> Arith.bit_and   (* AND *)
        | 28 -> Arith.bit_or    (* OR *)
        | _ -> assert false
      in
      registers.(dest i) <- binop registers.(op1 i) registers.(op2 i)

    | 30 | 31 as op ->
      let binop = match op with
        | 30 -> Arith.add       (* INCR *)
        | 31 -> Arith.sub       (* DECR *)
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
      close_in in_channel;
      (* memory.(!i) <- memory_size; *)
      (* stack_pointer := !i *)

      (* Initialisation de argv, qui doit valoir &argc + 1 *)
      memory.(!i - 2) <- !i;

      (* Initialisation de argc *)
      let argc = (Array.length Sys.argv) - 1 in (* on ignore vm *)
      let argc_pos = !i - 1 in
      memory.(argc_pos) <- argc;
      i := argc_pos + argc + 1;

      (* Empilement de argv *)
      for a = 1 to argc do
        let arg = Sys.argv.(a) in
        let arg_len = String.length arg in
        memory.(!i) <- arg_len;
        incr i;
        memory.(argc_pos + a) <- !i; (* les arrays pointent en fait sur leur deuxième élément, le premier étant leur taille *)
        for j = 0 to arg_len - 1 do
          memory.(!i) <- int_of_char arg.[j];
          incr i
        done
      done

(** Exécution :
    1/ lire l'instruction courante
    2/ la décoder et l'exécuter
    3/ incrémenter le pointeur de code
    4/ recommencer
    La boucle ne s'arrête jamais d'elle-même, mais peut être interrompue par
    une instruction exit *)
let _ =
  try
    while true do
      let i = 
        try
          memory.(!program_counter)
        with Invalid_argument _ ->
          raise (Invalid_argument "program_counter out of bounds")
      in
      exec_instruction i;
      incr program_counter
    done;
    exit 0
  with
  | Invalid_argument msg ->
    Printf.printf "[ERROR] %s\n" msg;
    Printf.printf "Current state:\n";
    Printf.printf "program_counter: %d\n" !program_counter;
    for i = 0 to nb_registers - 1 do
      Printf.printf "$r%d\t%d\n" i registers.(i)
    done