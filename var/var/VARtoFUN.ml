open FUNInstr

(* Remplace les références à des variables locales par les calculs
   d'adresse *)
let rec translate_expression expr alloc_table =
  failwith "not implemented"


(* Instructions et séquences : traduction iso *)
let rec translate_instruction instr alloc_table = match instr with
  | Nop -> Nop  
  | Print(e) -> Print(translate_expression e alloc_table)
  | Exit -> Exit
    
  | Write(e1, e2) ->
    Write(translate_expression e1 alloc_table,
          translate_expression e2 alloc_table)
      
  | If(c, s1, s2) ->
    If(translate_expression c alloc_table,
       translate_sequence s1 alloc_table,
       translate_sequence s2 alloc_table)
  | While(c, s) ->
    While(translate_expression c alloc_table,
          translate_sequence s alloc_table)
    
  | Call(d, f, args) ->
    Call(translate_expression d alloc_table,
         translate_expression f alloc_table,
         List.map (fun a -> translate_expression a alloc_table) args)
      
  | Return(e) ->
    Return(translate_expression e alloc_table)

and translate_sequence seq alloc_table =
  List.map (fun i -> translate_instruction i alloc_table) seq


(* Ajoute au code habituel l'initialisation des variables locales *)
let translate_function_definition fdef =
  failwith "not implemented"
  

let translate_program prog =
  { FUN.text = List.map translate_function_definition VAR.(prog.text);
    FUN.data = VAR.(prog.globals) }
