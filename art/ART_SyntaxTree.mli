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

val compile : out_channel -> prog -> unit 
