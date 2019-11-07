type instruction =
  | Nop
  | Print of IMPExpr.expression
  | Exit
  | Write of IMPExpr.expression * IMPExpr.expression
  | If    of IMPExpr.expression * sequence * sequence
  | While of IMPExpr.expression * sequence
  | Call of IMPExpr.expression (* adresse *)
    * IMPExpr.expression (* fonction *)
    * IMPExpr.expression list (* paramètres *)
  | Return of IMPExpr.expression
and sequence = instruction list

let push e =
  IMPExpr.([ Write(Name "stack_pointer",
                   sub (Deref(Name "stack_pointer")) (Immediate 1)) ;
             Write(Deref(Name "stack_pointer"), e) ])

let rec sequence_to_string = function
  | [] -> ""
  | i::seq -> (instr_to_string i) ^ (sequence_to_string seq)

and instr_to_string = function
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
  | Call(d, f, args) ->
    (IMPExpr.le_to_string d) ^ " := "
    ^ (IMPExpr.to_string f) ^ "("
    ^ (args_to_string args) ^ ");\n"
  | Return(e) ->
    "return (" ^ (IMPExpr.to_string e) ^ ");\n"

and args_to_string = function
  | [] -> ""
  | [a] -> IMPExpr.to_string a
  | a::args -> (IMPExpr.to_string a) ^ ", " ^ (args_to_string args)
