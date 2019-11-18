open ARTTree
open IMPTree
open FUNTree
open VARTree

type rec_expression =
  | Int of int
  | Bool of bool
  | Id of string node
  | Deref of rec_expression
  | Unop of unop * rec_expression
  | Binop of rec_expression * binop * rec_expression
  | Call of rec_expression * (rec_expression list)
  | RecordAccess of rec_expression * rec_expression

type variable = string node * rec_expression

(** Instructions en REC *)
type rec_instr =
  | Nop
  | Exit
  | Return of rec_expression
  | Break of unit node
  | Continue of unit node
  | Print of rec_expression
  | UnopAssign of rec_expression * assign_unop
  | BinopAssign of rec_expression * assign_binop * rec_expression
  | IfElse of rec_expression * rec_instrs * rec_instrs
  | If of rec_expression * rec_instrs
  | While of rec_expression * rec_instrs
  | For of rec_instrs * rec_expression * rec_instrs * rec_instrs
  | Call of rec_expression * (rec_expression list)
  | Declaration of variable
  | NewRecord of rec_expression * rec_expression

and rec_instrs = rec_instr list

type rec_function = rec_instrs function_definition

type global_declaration =
  | Fun of rec_function
  | Var of variable

type rec_prog = global_declaration list


val rec_to_tpl : rec_prog compiler_type -> var_prog compiler_type 