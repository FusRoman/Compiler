type instruction =
  | Debug
  (* Base *)
  | Nop
  | Print of IMPExpr.expression
  | Exit
  (* Sauts *)
  | Label    of string
  | Jump     of IMPExpr.expression (* adresse *)
  | JumpWhen of IMPExpr.expression (* adresse *)
              * IMPExpr.expression (* condition *)
  (* Accès mémoire *)
  | Write of IMPExpr.expression (* adresse *) * IMPExpr.expression (* valeur *)
      
type program = { text: instruction list; data: (string * int) list }

let rec instr_to_string = function
  | Debug -> "debug;"
  | Nop -> "nop;"
  | Print(e) -> "print " ^ (IMPExpr.to_string e) ^ ";"
  | Exit -> "exit;"
  | Label(id) -> id ^ ":"
  | Jump(e) -> "jump " ^ (IMPExpr.le_to_string e) ^ ";"
  | JumpWhen(e1, e2) -> "jump " ^ (IMPExpr.le_to_string e1)
                      ^ " when " ^ (IMPExpr.to_string e2) ^ ";"
  | Write(e1, e2) -> (IMPExpr.le_to_string e1) ^ " := "
                   ^ (IMPExpr.to_string e2) ^ ";"

let rec seq_to_string = function
  | [] -> ""
  | i::s -> (instr_to_string i) ^ "\n" ^ (seq_to_string s)

(* let rec values_to_string = function *)
(*   | [] -> "" *)
(*   | v::s -> (string_of_int v) ^ " " ^ (values_to_string s) *)
    
let rec data_to_string = function
  | [] -> ""
  | (id,v)::s -> id ^ ": " ^ (string_of_int v) ^ "\n" ^ (data_to_string s)
    
let to_string p =
  ".text\n" ^ (seq_to_string p.text) ^ "\n.data\n" ^ (data_to_string p.data)
    
