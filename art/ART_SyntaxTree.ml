type binop = 
  |ADD
  |SUB
  |MULT
  |DIV
  |AND
  |OR
  |REM
  |INF
  |INF_EQUAL
  |SUP
  |SUP_EQUAL
  |EQUAL
  |NOT_EQUAL

type unop =
  |MINUS
  |NOT

type prog =
  |Prog_Data of instrs * datas
  |Prog of instrs

and instrs =
  |Instrs of instr * instrs
  |Instrs_with_tag of tag * instrs
  |Empty_instr

and tag =
  |Tag of string

and instr = 
  |Print of expression
  |Affect of l_expr * expression
  |Nop
  |Exit
  |Jump of l_expr
  |JumpWhen of l_expr * expression

and datas = 
  |Datas of data * datas
  |Empty_data

and data = 
  |Data of string * int

and expression =
  |Expr_parenthese of expression
  |Int of int
  |Bool of bool
  |L_expr of l_expr
  |Binop of expression * binop * expression
  |Unop of unop * expression

and l_expr = 
  |Id of string
  |L_star of l_expr;;

let string_of_binop op = 
  match op with
  |ADD -> "ADD"
  |SUB -> "SUB"
  |MULT -> "MULT"
  |DIV -> "DIV"
  |REM -> "REM"
  |EQUAL -> "EQ"
  |NOT_EQUAL -> "NEQ"
  |INF -> "LT"
  |INF_EQUAL -> "LE"
  |SUP -> "GT"
  |SUP_EQUAL -> "GE"
  |AND -> "AND"
  |OR -> "OR"

let string_of_unop op =
  match op with
  |MINUS -> "MINUS"
  |NOT -> "NOT"

let rec compile_l_expr file l_e =
  match l_e with
  |Id i -> Printf.fprintf file "%s\n" i;
    Printf.fprintf file "READ\n"
  |L_star s_l_e -> compile_l_expr file s_l_e;
    Printf.fprintf file "READ\n"

let rec compile_exprs file e =
  match e with
  |Int i -> Printf.fprintf file "%s\n" (string_of_int i)
  |Bool b -> Printf.fprintf file "%s\n" (string_of_bool b)
  |Binop (e1,op,e2) -> compile_exprs file e1;
    compile_exprs file e2;
    Printf.fprintf file "%s\n" (string_of_binop op)
  |Unop (op,e) -> compile_exprs file e;
    Printf.fprintf file "%s\n" (string_of_unop op)
  |Expr_parenthese e -> compile_exprs file e
  |L_expr l_e -> compile_l_expr file l_e

let rec compile_l_expr_without_read file l_e = 
  match l_e with
  |Id i -> Printf.fprintf file "%s\n" i
  |L_star s_l_e -> compile_l_expr_without_read file s_l_e

let compile_instr file instr = 
  match instr with
  |Nop -> Printf.fprintf file "NOP\n"
  |Exit -> Printf.fprintf file "EXIT\n"
  |Print e -> compile_exprs file e;
    Printf.fprintf file "PRINT\n"
  |Jump l_e -> compile_l_expr_without_read file l_e;
    Printf.fprintf file "JUMP\n"
  |JumpWhen (l_e,e) -> compile_l_expr_without_read file l_e;
    compile_exprs file e;
    Printf.fprintf file "JUMPWHEN\n"
  |Affect (l_e,e) -> compile_l_expr_without_read file l_e;
    compile_exprs file e;
    Printf.fprintf file "WRITE\n"

let compile_tag file t =
  match t with
  |Tag t -> Printf.fprintf file "%s\n" t

let rec compile_instrs file tree_instr = 
  match tree_instr with
  |Instrs (i, is) -> compile_instr file i;
    compile_instrs file is
  |Instrs_with_tag (t,is) -> compile_tag file t;
    compile_instrs file is
  |Empty_instr -> ()

let compile_data file data =
  match data with
  |Data (s,i) ->
    Printf.fprintf file "%s\n" s;
    Printf.fprintf file ":";
    Printf.fprintf file "%s\n" (string_of_int i)

let rec compile_datas file datas =
  match datas with
  |Datas (d,dss) -> compile_data file d;
    compile_datas file dss
  |Empty_data -> ()

let rec compile file tree = 
  match tree with
  |Prog is -> Printf.fprintf file ".text\n";
    compile_instrs file is
  |Prog_Data (is,ds) -> Printf.fprintf file ".text\n";
    compile_instrs file is;
    Printf.fprintf file ".data\n";
    compile_datas file ds

