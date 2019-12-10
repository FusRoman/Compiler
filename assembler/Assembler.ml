(** Rôles de l'assembleur :
    - expliciter les adresses symboliques représentées par les étiquettes
    - coder chaque instruction dans le format binaire du langage machine
*)

(** Tableau destiné à contenir les instructions lues dans le fichier source,
    une ligne par case, sous forme de chaîne de caractères *)
let instructions = Array.make 65536 ""

(** Table d'association entre étiquettes symboliques et adresses effectives *)
let label_map = Hashtbl.create 17

exception End_of_program

let int_of_string s =
  try int_of_string s
  with Failure _ -> failwith ("int_of_string : " ^ s)
  
(** Un registre est représenté par une chaîne "$r%i" où %i donne le numéro du
    registre *)
let get_register_number s =
  int_of_string (String.sub s 2 (String.length s - 2))

(** Opération de construction de l'entier codant une instruction à partir de
    son opcode et des valeurs à donner aux trois octets suivants.
    On peut donner des valeurs arbitraires aux octets non utilisés par une
    instruction (ici ce sera systématiquement 0). Pour inclure une valeur
    immédiate de 16 bits regroupant les octets 3 et 4, on place la valeur en
    quatriième paramètre en laissant le troisième à 0. *)
let mk_binary opcode rd r1 r2 =
  opcode lsl 24 + rd lsl 16 + r1 lsl 8 + r2
    
(** Encodage des instructions assembleur :
    - prend en paramètre une  chaîne de caractères représentant une ligne
    - renvoie l'entier de 32 bits codant cette instruction
*)
let encode_line l =
  (* On extrait la liste des éléments hors espaces *)
  let tokens = List.filter (fun s -> s <> "") (String.split_on_char ' ' l) in
  (* Cas sur le premier élément de la liste *)
  match tokens with
    | [ "NOP" ] ->
      mk_binary 0 0 0 0
      
    | [ "EXIT" ] ->
      mk_binary 1 0 0 0
      
    | [ "PRINT"; r1 ] ->
      let r1 = get_register_number r1 in
      mk_binary 2 0 r1 0
        
    | [ "CONST"; rd; c ] ->
      let rd = get_register_number rd in
      let c = int_of_string c in
      if c < 0 || c >= 65536 then
      begin
        Printf.printf "CONST: an immediate value must be between 0 and 65535 included\n";
        exit 1
      end;
      mk_binary 3 rd 0 c
        
    | [ "ADDRESS"; rd; lab ] ->
      let rd = get_register_number rd in
      let address = Hashtbl.find label_map lab in
      mk_binary 3 rd 0 address
        
    | [ "READ" | "WRITE" as memop; rd; r1 ] ->
      let opcode = match memop with
        | "READ" -> 4
        | "WRITE" -> 5
        | _ -> assert false
      in
      let rd = get_register_number rd in
      let r1 = get_register_number r1 in
      mk_binary opcode rd r1 0

    | [ "DIRECTREAD"; rd; lab ] ->
      let rd = get_register_number rd in
      let address = Hashtbl.find label_map lab in
      mk_binary 6 rd 0 address

    | [ "DIRECTWRITE"; lab; r ] ->
      let address = Hashtbl.find label_map lab in
      let r = get_register_number r in
      mk_binary 7 r 0 address

    | [ "JUMP"; r1 ] ->
      let r1 = get_register_number r1 in
      mk_binary 8 0 r1 0

    | [ "JUMP"; r1; "WHEN"; r2 ] ->
      let r1 = get_register_number r1 in
      let r2 = get_register_number r2 in
      mk_binary 9 0 r1 r2

    | [ "MOVE" | "MINUS" | "CPL" | "NOT" as unop; rd; r1 ] ->
      let opcode = match unop with
        | "MOVE" -> 12
        | "MINUS" -> 13
        | "CPL" -> 14
        | "NOT" -> 15
        | _ -> assert false
      in
      let rd = get_register_number rd in
      let r1 = get_register_number r1 in
      mk_binary opcode rd r1 0

    | [ "ADD" | "SUB" | "MULT" | "DIV" | "REM"
      | "EQ" | "NEQ" | "LT" | "LE" | "GT" | "GE"
      | "AND" | "OR" as binop; rd; r1; r2 ] ->
      let opcode = match binop with
        | "ADD" -> 16
        | "SUB" -> 17
        | "MULT" -> 18
        | "DIV" -> 19
        | "REM" -> 20
        | "EQ" -> 21
        | "NEQ" -> 22
        | "LT" -> 23
        | "LE" -> 24
        | "GT" -> 25
        | "GE" -> 26
        | "AND" -> 27
        | "OR" -> 28
        | _ -> assert false
      in
      let rd = get_register_number rd in
      let r1 = get_register_number r1 in
      let r2 = get_register_number r2 in
      mk_binary opcode rd r1 r2

    | [ "INCR" | "DECR" as updop; r; c ] ->
      let opcode = match updop with
        | "INCR" -> 30
        | "DECR" -> 31
        | _ -> assert false
      in
      let r = get_register_number r in
      let c = int_of_string c in
      mk_binary opcode r 0 c
        
    | [ n ] -> int_of_string n
        
    | [] -> raise End_of_program

    | _ -> failwith ("invalid line " ^ l)
        
(** Test pour repérer les lignes qui définissent des étiquettes *)
let is_label l =
  l.[String.length l - 1] = ':'
let get_label l =
  String.sub l 0 (String.length l - 1)

(** Test pour repérer les commentaires et les lignes vides *)
let is_comment l =
  try
    let length = String.length l in
    let i = ref 0 in
    while !i < length && (l.[!i] = ' ' || l.[!i] = '\t') do
      incr i
    done;
    !i >= length || l.[!i] = '#' || l.[!i] = '\n'
  with
  | Invalid_argument msg ->
    failwith (Printf.sprintf "Raised an 'Invalid_argument %s' while parsing line '%s'" msg l)
    
(* Nombre de lignes hors étiquettes *)
let line_count = ref 0

(** Chargement des instructions d'un seul fichier dans le tableau [instructions] *)
let load_file input_file =
  if not (Filename.check_suffix input_file ".asm") then
    failwith "expected .asm extension";
  let input = open_in input_file in
  try
    while true do
      (* Lecture ligne à ligne *)
      let line = input_line input in
      (* Une étiquette est ajoutée à la table des étiquettes, associée au
         compte de lignes courant, tandis que tout autre ligne est ajoutée
         dans le tableau d'instructions après la précédente et incrémente
         le compteur *)
      if is_comment line
      then ()
      else if is_label line
      then
        let label = get_label line in
        if Hashtbl.mem label_map label then
          failwith (Printf.sprintf "Duplicate tag '%s'" label)
        else
          Hashtbl.add label_map label !line_count
      else begin
        instructions.(!line_count) <- line;
        incr line_count
      end
    done
  with
    | End_of_file -> close_in input

let argc = Array.length Sys.argv

let _ =
  load_file Sys.argv.(argc - 1);
  for i = 1 to argc - 2 do
    load_file Sys.argv.(i)
  done

let _ =
  let output_file = (Filename.chop_suffix Sys.argv.(argc - 1) ".asm") ^ ".btc" in
  let output = open_out_bin output_file in
  try
    Array.iter (fun l -> output_binary_int output (encode_line l)) instructions
  with
    | End_of_program -> close_out output; exit 0
