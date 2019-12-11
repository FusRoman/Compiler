open ARTTree
open VARTree
open TPLTree
open IMPTree
open FUNTree
open TYPTree

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

  type typ_binop = ARTBinop of binop | Seq

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
  
  type variable = _type * string node * cls_expression node
  type declaration_type = string node * _type
  (** Instructions en TYP *)
  type cls_instr =
    | Nop
    | Exit
    | Return of cls_expression node
    | Break of unit node
    | Continue of unit node
    | Print of cls_expression node
    | UnopAssign of cls_expression node * assign_unop
    | BinopAssign of cls_expression node * assign_binop * cls_expression node
    | IfElse of cls_expression node * cls_instrs * cls_instr
    | If of cls_expression node * cls_instr
    | While of cls_expression node * cls_instr
    | For of cls_instr * cls_expression node * cls_instr * cls_instr
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

type attribute = Var of variable list
type methode = Fun of cls_function list

type global_declaration =
  | Fun of cls_function
  | Var of variable
  | Type of declaration_type
  | Class of attribute * methode

type cls_prog = global_declaration list
  
  
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

  exception TypeError of string * int * int
  
(** Renvoie un nom de variable qui n'existe pas encore dans l'environnement donné. *)
val make_var_node : 'a -> 'a StringMap.t -> 'a StringMap.t * string node

val typ_to_tpl : cls_prog program -> typ_prog compiler_type