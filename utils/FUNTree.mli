open ARTTree
open CLLTree

(** Alias dont le seul intérêt est de ne plus avoir à ouvrir ARTTree dans les langages suivants. *)
exception SyntaxError = ARTTree.SyntaxError

(**
  Une exception levée lorsque qu'une variable n'a pas été déclarée avant utilisation.
  Le premier argument est le nom de la fonction où se trouve l'erreur, le deuxième la variable.
*)
exception UnboundValue of (string node) * (string node)

(** FUN définit function_result par-dessus CLL *)
val fun_variables : Tagset.t

type fun_expr =
  | Int of int
  | Bool of bool
  | Id of string node
  | Address of fun_expr
  | Deref of fun_expr
  | Unop of unop * fun_expr
  | Binop of fun_expr * binop * fun_expr
  | Call of fun_expr * (fun_expr list)

(**
  Les instructions en FUN, identiques à celles de CLL sauf pour Call qui prend maintenant des arguments
*)
type fun_instr =
  | Nop
  | Exit
  | Return of fun_expr
  | Break of unit node
  | Continue of unit node
  | Print of fun_expr
  | UnopAssign of fun_expr * assign_unop
  | BinopAssign of fun_expr * assign_binop * fun_expr
  | IfElse of fun_expr * fun_instrs * fun_instrs
  | If of fun_expr * fun_instrs
  | While of fun_expr * fun_instrs
  | For of cll_instrs * fun_expr * fun_instrs * fun_instrs

and fun_instrs = fun_instr list

type parameter = {
  name: string;
  reference: bool;
}

(* Le type des déclarations de fonctions en FUN *)
type function_definition = {
  name: string node; 
  params: parameter list;
  block: fun_instrs
}

and fun_prog = (function_definition list) * datas

(** 
  Analogue à ARTTree.check_expression.
  'check_fun_expression e fct lenv genv' vérifie que chaque ID est dans l'environnement local,
  sinon global. Si l'id est absent des deux ensembles, une exception est lancée.
*)
val check_fun_expression : fun_expr -> function_definition -> Tagset.t -> Tagset.t -> unit

(*val fun_to_cll : fun_prog compiler_type -> cll_prog compiler_type*)