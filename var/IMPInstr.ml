type instruction =
  | Nop
  | Print of IMPExpr.expression
  | Exit
  | Write of IMPExpr.expression (* adresse *) * IMPExpr.expression (* valeur *)
  | If    of IMPExpr.expression * sequence * sequence
  | While of IMPExpr.expression * sequence
  | Label of string
  | Goto  of IMPExpr.expression (* adresse *)

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
  | Nop -> ""
  | Print(e) -> "print(" ^ (IMPExpr.to_string e) ^ ");\n"
  | Exit -> "exit;\n"
  | If(c, s1, s2) ->
    "if (" ^ (IMPExpr.to_string c) ^ ") {\n"
    ^ (sequence_to_string s1) ^ "} else {\n"
    ^ (sequence_to_string s2) ^ "}\n"
  | While(c, s) ->
    "while (" ^ (IMPExpr.to_string c) ^ ") {\n"
    ^ (sequence_to_string s) ^ "}\n"
  | Label(lab) -> lab ^ ":\n"
  | Goto(e) -> "goto(" ^ (IMPExpr.le_to_string e) ^ ");\n"
  | Write(le, e) -> (IMPExpr.le_to_string le) ^ " := " ^ (IMPExpr.to_string e) ^ ";\n"
