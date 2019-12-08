open ARTTree
open VARTree
open TPLTree
open IMPTree
open FUNTree

module StringMap: Map.S with type key = string

type env = _type StringMap.t
and record_env = (_type * int) StringMap.t

and _type =
  |TInt
  |TPointer of _type
  |TArray of _type
  |TFun of (_type list) * _type
  |TRecord of record_env
  |TTuple of _type list
  |TAlias of string node

type record_field = string node * typ_expression
and typ_expression =
  | Int of int
  | Bool of bool
  | Id of string node
  | Deref of typ_expression
  | Unop of unop * typ_expression
  | Binop of typ_expression * binop * typ_expression
  | Call of typ_expression * (typ_expression list)
  | RecordAccess of typ_expression * string node
  | NewRecord of _type * record_field list
  | ArrayAccess of typ_expression * typ_expression
  | NewArray of typ_expression * typ_expression
  | TupleAccess of typ_expression * int
  | NewTuple of typ_expression list
  | InitArray of typ_expression list

type variable = _type * string node * typ_expression
type declaration_type = string node * _type
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

and typ_instrs = typ_instr list

type parameter = {
  name : string node;
  reference : bool;
  params_type : _type
}

type 'a function_definition = {
  name : string node;
  params: parameter list;
  block: 'a;
  return_type : _type
}

type typ_function = typ_instrs function_definition

type global_declaration =
  | Fun of typ_function
  | Var of variable
  | Type of declaration_type

type typ_prog = global_declaration list
  
  
  (**
    genv est une map permettant d'étiqueter chaque label avec son type.
    _type est une map de tous les types et alias de type déclaré dans le programme.
    tree est l'AST du programme ecrit en langage typ
  *)
  type 'a program = {
    genv: env;
    _type: env;
    tree: 'a
  }
  
(** Renvoie un nom de variable qui n'existe pas encore dans l'environnement donné. *)
val make_var_node : 'a -> 'a StringMap.t -> 'a StringMap.t * string node

val typ_to_tpl : typ_prog program -> var_prog compiler_type