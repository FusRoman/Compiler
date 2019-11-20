open ARTTree
open VARTree
open TPLTree
open TYPParser
open TYPLexer
open IMPTree
open FUNTree

module StringMap = Map.Make(String)

type env = _type StringMap.t

(**
   Type des types des expressions
   Unknown peut être retourné par le parser lorsqu'il rencontre un nom
   alors qu'il attendait un type. N'ayant pas encore forcément parsé la 
   déclaration du type, il ne peut pas savoir exactement quoi renvoyer. 
   Il faudra, dans la première passe du compilateur, remplacer les Unknown
   par leurs vrais types puis vérifier que les expressions sont bien typées.
   Rencontrer des Unknown en-dehors de la première passe est une erreur
   et ne devrait jamais arriver.
*)
and _type =
  |Int
  |Pointer of _type
  |Array of _type
  |Fun of (_type list) * _type
  |Record of env
  |Tuple of int * _type
  |Unknown of string node

type record_field = string node * typ_expression
and typ_expression =
  | Int of int
  | Bool of bool
  | Id of string node
  | Deref of typ_expression
  | Unop of unop * typ_expression
  | Binop of typ_expression * binop * typ_expression
  | Call of typ_expression * (typ_expression list)
  | RecordAccess of typ_expression * string
  | NewRecord of record_field list
  | ArrayAccess of typ_expression * typ_expression
  | NewArray of typ_expression * typ_expression

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
  | NewRecord of typ_expression * string

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


(*
  genv est une map permettant d'étiqueter chaque label avec son type.
  _type est une map de tous les types et alias de type déclaré dans le programme.
  tree est l'AST du programme ecrit en langage typ
*)
type 'a program = {
  genv: env;
  _type: env;
  tree: 'a
}

