open ARTTree
open IMPTree
open CLLTree
open FUNTree

(** VAR ne définit aucune nouvelle variable par rapport à FUN *)
val var_variables : Tagset.t

(*type fun_expr =
  | Int of int
  | Bool of bool
  | Id of string node
  | Address of fun_expr
  | Deref of fun_expr
  | Unop of unop * fun_expr
  | Binop of fun_expr * binop * fun_expr
  | Call of fun_expr * (fun_expr list)*)

type variable = string node * expression

(** Instructions en VAR *)
type var_instr =
  | Nop
  | Exit
  | Return of expression
  | Break of unit node
  | Continue of unit node
  | Print of expression
  | UnopAssign of expression * assign_unop
  | BinopAssign of expression * assign_binop * expression
  | IfElse of expression * var_instrs * var_instrs
  | If of expression * var_instrs
  | While of expression * var_instrs
  | For of var_instrs * expression * var_instrs * var_instrs
  | SetCall of expression * assign_binop * expression * (expression list)
  | Call of expression * (expression list)
  | Declaration of variable

and var_instrs = var_instr list

type var_function = var_instrs function_definition

type global_declaration =
  | Fun of var_function
  | Var of variable

type var_prog = global_declaration list

(** 
  Analogue à ARTTree.check_expression.
  'check_fun_expression e fct lenv genv' vérifie que chaque ID est dans l'environnement local,
  sinon global. Si l'id est absent des deux ensembles, une exception est lancée.
*)
(*val check_fun_expression : fun_expr -> function_definition -> Tagset.t -> Tagset.t -> unit*)

val var_to_fun : var_prog compiler_type -> fun_prog compiler_type

val write_var : out_channel -> var_prog compiler_type -> unit