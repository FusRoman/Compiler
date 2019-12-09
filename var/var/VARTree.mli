open ARTTree
open IMPTree
open CLLTree
open FUNTree

val var_variables : Tagset.t

type var_expression =
  | Int of int
  | Bool of bool
  | Id of string node
  | Deref of var_expression
  | Unop of unop * var_expression
  | Binop of var_expression * binop * var_expression
  | Call of var_expression * (var_expression list)

type variable = string node * var_expression

(** Instructions en VAR *)
type var_instr =
  | Nop
  | Exit
  | Return of var_expression
  | Break of unit node
  | Continue of unit node
  | Print of var_expression
  | UnopAssign of var_expression * assign_unop
  | BinopAssign of var_expression * assign_binop * var_expression
  | IfElse of var_expression * var_instrs * var_instrs
  | If of var_expression * var_instrs
  | While of var_expression * var_instrs
  | For of var_instrs * var_expression * var_instrs * var_instrs
  | Call of var_expression * (var_expression list)
  | Declaration of variable

and var_instrs = var_instr list

type var_function = var_instrs function_definition

(* 
  Les variables globales sont limitées aux valeurs immédiates par la grammaire.
  Ce n'était pas notre choix initial donc le type ne le reflète pas.
*)
type global_declaration =
  | Fun of var_function
  | Var of variable

type var_prog = global_declaration list

val var_to_fun : bool -> var_prog compiler_type -> fun_prog compiler_type

val write_var_right_expr : out_channel -> var_expression -> unit

val write_var_left_expr : out_channel -> var_expression -> unit

val write_var : out_channel -> var_prog compiler_type -> unit