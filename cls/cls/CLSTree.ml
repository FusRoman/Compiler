open ARTTree
open VARTree
open TPLTree
open IMPTree
open FUNTree
open TYPTree



type record_field = string node * cls_expression node
and cls_expression =
  | Int of int
  | Bool of bool
  | Id of string node
  | Deref of cls_expression node
  | Unop of unop * cls_expression node
  | Binop of cls_expression node * typ_binop * cls_expression node
  | Call of cls_expression node * (cls_expression node list)
  | RecordAccess of cls_expression node * string node
  | NewRecord of _type * record_field list
  | ArrayAccess of cls_expression node * cls_expression node
  | NewArray of cls_expression node * cls_expression node
  | TupleAccess of cls_expression node * int
  | NewTuple of cls_expression node list
  | InitArray of cls_expression node list
  | MethodAccess of cls_expression node * string node * cls_expression node list

type variable = _type * string node * cls_expression node
type not_init_variable = _type * string node
type declaration_type = string node * _type
(** Instructions en CLS *)
type cls_instr =
  | Nop
  | Exit
  | Return of cls_expression node
  | Break of unit node
  | Continue of unit node
  | Print of cls_expression node
  | UnopAssign of cls_expression node * assign_unop
  | BinopAssign of cls_expression node * assign_binop * cls_expression node
  | IfElse of cls_expression node * cls_instrs * cls_instrs
  | If of cls_expression node * cls_instrs
  | While of cls_expression node * cls_instrs
  | For of cls_instrs * cls_expression node * cls_instrs * cls_instrs
  | Call of cls_expression node * (cls_expression node list)
  | Declaration of variable

and cls_instrs = cls_instr list

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

type cls_function = cls_instrs function_definition

type attribute = 
  |InitVar of variable
  |Var of not_init_variable

type class_element =
  |Method of cls_function
  |Attribute of attribute

type global_declaration =
  | Fun of cls_function
  | Var of variable
  | Type of declaration_type
  | Class of string node * class_element list
  | ClassFille of string node * string * class_element list

type cls_prog = global_declaration list


(**
   genv est une map permettant d'étiqueter chaque label avec son type.
   _type est une map de tous les types et alias de type déclaré dans le programme.
   tree est l'AST du programme ecrit en langage typ
*)
type 'a program = {
  class_env: TYPTree.env;
  tree: 'a
}

exception TypeError of string * int * int

let rec translate_expression class_env e =
  match e with
  

let cls_to_typ (cls_prog: cls_prog program) =
  {
    genv = StringMap.empty;
    types = [];
    tree = [TYPTree.Var (TInt, default_node "toto", default_node (TYPTree.Int 0))]
  }