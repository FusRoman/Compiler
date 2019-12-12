open ARTTree
open VARTree
open TPLTree
open IMPTree
open FUNTree
open TYPTree

module StringMap: Map.S with type key = string

type env = _type StringMap.t
and record_env = (cls_type * int) StringMap.t

and cls_type = _type

type record_field = string node * cls_expression node
and cls_expression = typ_expression

type cls_variable = cls_type * string node * cls_expression node
type type_declaration = string node * cls_type

type cls_instr = typ_instr
type cls_instrs = cls_instr list

type cls_function = cls_instrs TYPTree.function_definition

type class_attribute = {
  name: string node;
  _type: cls_type;
  _public: bool;
  _static: bool;
  init: cls_expression;
}

type class_method = {
  _public: bool;
  _static: bool;
  _fun: cls_function;
}

type class_field =
  | Method of class_method
  | Attribute of class_attribute

type cls_class = {
  fields: class_field list;
  parent: string node option
}

type cls_declarable_type =
  | TRegular of cls_type
  | TExtended of cls_type node * (string node * cls_type) list
  | TClass of cls_class

type global_declaration =
  | Fun of cls_function
  | Var of cls_variable
  | Type of string node * cls_declarable_type

type cls_prog = global_declaration list


type cls_program = {
  genv: cls_type StringMap.t;
  types: cls_declarable_type list
  tree: 'a
}

val cls_to_typ : cls_program -> typ_prog TYPTree.program