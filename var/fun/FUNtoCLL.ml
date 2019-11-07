(* Dans les expressions, remplace les paramètres formels d'une fonction par 
   le calcul d'adresse correspondant. *)
let rec translate_expression expr param_table =
  failwith "not implemented"


(* Instructions ne faisant pas référence aux fonctions : traduction iso *)
(* Pour Call et Return, appliquer le protocole *)
let rec translate_instruction instr param_table =
  failwith "not implemented"

    
let translate_function_definition fdef =
  failwith "not implemented"

    
let translate_program prog =
  { CLL.text = List.map translate_function_definition FUN.(prog.text);
    CLL.data = failwith "not implemented" }
