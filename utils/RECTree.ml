open ARTTree
open VARTree
open TPLTree
open VARParser
open VARLexer
open IMPTree
open FUNTree

type rec_expression =
  | Int of int
  | Bool of bool
  | Id of string node
  | Deref of var_expression
  | Unop of unop * var_expression
  | Binop of var_expression * binop * var_expression
  | Call of var_expression * (var_expression list)

type variable = string node * var_expression

(** Instructions en REC *)
type rec_instr =
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

and rec_instrs = rec_instr list

type rec_function = rec_instrs function_definition

type global_declaration =
  | Fun of rec_function
  | Var of variable

type rec_prog = global_declaration list

let rec_variables = tpl_variables

let rec_to_tpl rec_prog =
  let tag_set = Tagset.union_duplicate rec_variables rec_prog.tag_set in
  let rec_lib = program token (Lexing.from_channel (open_in "typ/rec/RECLib.tpl")) in
  let syntax_tree = rec_lib.syntax_tree @ rec_prog.syntax_tree in
  {tag_set; syntax_tree}