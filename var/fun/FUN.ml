type function_definition = {
  name: string;
  code: FUNInstr.sequence;
  parameters: string list;
}
    
type program = {
  text: function_definition list;
  data: (string * int) list;
}

let rec params_to_string = function
  | [] -> ""
  | p :: ps -> p ^ ", " ^ (params_to_string ps)
    
let fdef_to_string fdef =
  fdef.name ^ "(" ^ (params_to_string fdef.parameters) ^ ") {\n"
  ^ (FUNInstr.sequence_to_string fdef.code)
  ^ "}\n\n"

let rec fdefs_to_string = function
  | [] -> ""
  | fdef :: fdefs -> (fdef_to_string fdef) ^ (fdefs_to_string fdefs)
    
let prog_to_string prog =
  ".text\n" ^ (fdefs_to_string prog.text)
  ^ ".data\n" ^ (ART.data_to_string prog.data)
