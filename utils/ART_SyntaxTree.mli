exception SyntaxError of string * int * int

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
  |Tag of string * int * int

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
  |Data of string * int * int * int

and expression =
  |Expr_parenthese of expression
  |Int of int
  |Bool of bool
  |L_expr of l_expr
  |STACK_POINTER
  |Binop of expression * binop * expression
  |Unop of unop * expression

and l_expr = 
  |Id of string * int * int
  |L_star of l_expr;;

type 'a compiler_type = {
  tag_set: Tagset.t;
  syntax_tree: 'a
}

val compile : out_channel -> string list -> prog -> unit 
