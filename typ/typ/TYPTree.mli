open ARTTree
open VARTree
open TPLTree
open IMPTree
open FUNTree

module StringMap: Map.S with type key = string

type env = _type StringMap.t
and record_env = (_type * int) StringMap.t

and _type =
  | TNull
  | TInt
  | TPointer of _type
  | TArray of _type
  | TFun of (_type list) * _type
  | TRecord of record_env
  | TTuple of _type list
  | TAlias of string node

and declarable_type =
  | TRegular of _type
  | TExtended of _type node * (string node * _type) list

type typ_binop = ARTBinop of binop | Seq | NSeq

type record_field = string node * typ_expression node
and typ_expression =
  | Null
  | Int of int
  | Bool of bool
  | Id of string node
  | Deref of typ_expression node
  | Unop of unop * typ_expression node
  | Binop of typ_expression node * typ_binop * typ_expression node
  | Call of typ_expression node * (typ_expression node list)
  | RecordAccess of typ_expression node * string node
  | NewRecord of _type node * record_field list
  | ArrayAccess of typ_expression node * typ_expression node
  | NewArray of typ_expression node * typ_expression node
  | TupleAccess of typ_expression node * int
  | NewTuple of typ_expression node list
  | InitArray of typ_expression node list

type variable = _type * string node * typ_expression node
type type_declaration = string node * declarable_type
(** Instructions en TYP *)
type typ_instr =
  | Nop
  | Exit
  | Return of typ_expression node
  | Break of unit node
  | Continue of unit node
  | Print of typ_expression node
  | UnopAssign of typ_expression node * assign_unop
  | BinopAssign of typ_expression node * assign_binop * typ_expression node
  | IfElse of typ_expression node * typ_instrs * typ_instrs
  | If of typ_expression node * typ_instrs
  | While of typ_expression node * typ_instrs
  | For of typ_instrs * typ_expression node * typ_instrs * typ_instrs
  | Call of typ_expression node * (typ_expression node list)
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
  | Type of type_declaration

type typ_prog = global_declaration list
  
  
(**
  genv est une map associant chaque variable à son type.
  types est une liste de déclarations de type.
  tree est l'AST du programme ecrit en langage typ
*)
type 'a program = {
  genv: env;
  types: (string node * declarable_type) list;
  tree: 'a
}

exception TypeError of string * int * int

val typ_genv: _type StringMap.t

(** Ajoute typ_genv à l'environnement, en favorisant ce dernier en cas de conflit *)
val add_typ_genv: _type StringMap.t -> _type StringMap.t
  
(** Renvoie un nom de variable qui n'existe pas encore dans l'environnement donné. *)
val make_var_node: 'a -> 'a StringMap.t -> 'a StringMap.t * string node

(** Vérifie que le type donné de nom alias n'utilise bien que des types déjà définis, rassemblés dans le premier argument *)
val check_type: _type StringMap.t -> string node -> _type -> unit

val translate_extended: _type StringMap.t -> string node -> _type node -> (string node * _type) list -> _type

val typ_to_tpl: typ_prog program -> var_prog compiler_type