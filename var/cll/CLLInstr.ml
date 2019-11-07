type instruction =
  | Debug
  | Nop
  | Print of IMPExpr.expression
  | Exit
  | Write of IMPExpr.expression * IMPExpr.expression
  | If    of IMPExpr.expression * sequence * sequence
  | While of IMPExpr.expression * sequence
  (* Fonctions *)
  | Call of IMPExpr.expression (* adresse *)
  | Return
and sequence = instruction list
    
let push e =
  IMPExpr.([ Write(Name "stack_pointer",
                   sub (Deref(Name "stack_pointer")) (Immediate 1)) ;
             Write(Deref(Name "stack_pointer"), e) ])

let pop e =
  IMPExpr.([ Write(e, Deref(Deref(Name "stack_pointer"))) ;
             Write(Name "stack_pointer",
                   add (Deref(Name "stack_pointer")) (Immediate 1)) ])

let rec sequence_to_string = function
  | [] -> ""
  | i::seq -> (instr_to_string i) ^ (sequence_to_string seq)

and instr_to_string = function
  | Debug -> "debug;\n"
  | Nop -> ""
  | Print(e) -> "print(" ^ (IMPExpr.to_string e) ^ ");\n"
  | Exit -> "exit;\n"
  | Write(le, e) -> (IMPExpr.le_to_string le) ^ " := " ^ (IMPExpr.to_string e) ^ ";\n"
  | If(c, s1, s2) ->
    "if (" ^ (IMPExpr.to_string c) ^ ") {\n"
    ^ (sequence_to_string s1) ^ "} else {\n"
    ^ (sequence_to_string s2) ^ "}\n"
  | While(c, s) ->
    "while (" ^ (IMPExpr.to_string c) ^ ") {\n"
    ^ (sequence_to_string s) ^ "}\n"
  | Call(f) ->
    "call (" ^ (IMPExpr.le_to_string f) ^ ");\n"
  | Return ->
    "return;\n"
