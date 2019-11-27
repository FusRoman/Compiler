open ARTTree
open IMPTree
open CLLTree

(**
  Une exception levée lorsque qu'une variable n'a pas été déclarée avant utilisation.
  Le premier argument est le nom de la fonction où se trouve l'erreur, le deuxième la variable.
*)
exception UnboundValue of (string node) * (string node)

(** FUN définit function_result par-dessus CLL *)
val fun_variables : Tagset.t

(*type fun_expr =
  | Int of int
  | Bool of bool
  | Id of string node
  | Address of fun_expr
  | Deref of fun_expr
  | Unop of unop * fun_expr
  | Binop of fun_expr * binop * fun_expr
  | Call of fun_expr * (fun_expr list)*)

(**
  Les instructions en FUN, identiques à celles de CLL sauf pour Call et SetCall qui prend maintenant des arguments.

  TerminalCall ne doit paŝ être utilisé quand le langage source est FUN. En effet, l'appel terminal va chercher
  à reprendre l'espace donné aux arguments de la fonction appelante pour stocker ceux de la fonction appelée.
  Il faut donc faire attention à ne remplacer l'argument i quand celui-ci est utilisé dans d'autres expressions plus tard,
  ce qu'il est possible de faire en FUN et requiert l'allocation de "variables locales anonymes" pour stocker les valeurs des arguments.
  Ce faisant, on risque d'effacer des variables locales utilisées dans les expressions des arguments. Il est plus efficace
  de gérer ceci dans VAR (quitte à s'y occuper des paramètres aussi).
  C'est la raison pour laquelle TerminalCall n'accepte pas la liste des arguments, car c'est VAR qui s'en occupe.
*)
type fun_instr =
  | Nop
  | Exit
  | Return of expression
  | Break of unit node
  | Continue of unit node
  | Print of expression
  | UnopAssign of expression * assign_unop
  | BinopAssign of expression * assign_binop * expression
  | IfElse of expression * fun_instrs * fun_instrs
  | If of expression * fun_instrs
  | While of expression * fun_instrs
  | For of fun_instrs * expression * fun_instrs * fun_instrs
  | SetCall of expression * assign_binop * expression * (expression list)
  | Call of expression * (expression list)
  | TerminalCall of expression

and fun_instrs = fun_instr list

type parameter = {
  name: string node;
  reference: bool;
}

(* Le type des déclarations de fonctions en FUN *)
type 'a function_definition = {
  name: string node; 
  params: parameter list;
  block: 'a
}

type fun_function = fun_instrs function_definition

type fun_prog = (fun_function list) * ((string node * int) list)

(** 
  Analogue à ARTTree.check_expression.
  'check_fun_expression e fct lenv genv' vérifie que chaque ID est dans l'environnement local,
  sinon global. Si l'id est absent des deux ensembles, une exception est lancée.
*)
(*val check_fun_expression : fun_expr -> function_definition -> Tagset.t -> Tagset.t -> unit*)

val fun_to_cll : fun_prog compiler_type -> cll_prog compiler_type

val write_fun : out_channel -> fun_prog compiler_type -> unit