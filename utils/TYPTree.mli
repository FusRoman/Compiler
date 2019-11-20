open ARTTree
open VARTree
open TPLTree
open TYPParser
open TYPLexer
open IMPTree
open FUNTree

module StringMap: Map.S with type key = string

type env = _type StringMap.t
and _type =
  |Int of int
  |Pointer of _type
  |Array of _type
  |String of string
  |Fun of (_type list) * _type
  |Record of env
  |Tuple of int * _type

type typ_expression =
  | Int of int
  | Bool of bool
  | Id of string node
  | Deref of typ_expression
  | Unop of unop * typ_expression
  | Binop of typ_expression * binop * typ_expression
  | Call of typ_expression * (typ_expression list)
  | RecordAccess of typ_expression * string

type variable = string node * typ_expression

(** Instructions en REC *)
type typ_instr =
  | Nop
  | Exit
  | Return of typ_expression
  | Break of unit node
  | Continue of unit node
  | Print of typ_expression
  | UnopAssign of typ_expression * assign_unop
  | BinopAssign of typ_expression * assign_binop * typ_expression
  | IfElse of typ_expression * typ_instrs * typ_instrs
  | If of typ_expression * typ_instrs
  | While of typ_expression * typ_instrs
  | For of typ_instrs * typ_expression * typ_instrs * typ_instrs
  | Call of typ_expression * (typ_expression list)
  | Declaration of variable
  | Newtypord of typ_expression * string

and typ_instrs = typ_instr list

type typ_function = typ_instrs function_definition

type global_declaration =
  | Fun of typ_function
  | Var of variable

type typ_prog = global_declaration list

type 'a program = {
  genv: env;
  _type: env;
  tree: 'a
}

val typ_to_tpl : typ_prog program -> var_prog compiler_type