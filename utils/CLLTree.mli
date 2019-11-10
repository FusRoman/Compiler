open ARTTree
open IMPTree

(** 
  Ensemble des tags réservés de CLL. Les langages plus haut niveau les utilisent donc aussi indirectement. 
  Contient stack_pointer, return_address et frame_pointer.
*)
val cll_variables : Tagset.t

(**
  Les instructions en CLL, ressemblant à celles en IMP à part les différences suivantes :
  - Disparition de goto et des déclarations d'étiquettes
  - Apparition des définitions des procédures sans paramètres possibles
  - Ajout des appels de procédure et de return
*)
type cll_instr =
  | Nop
  | Exit
  | Return
  | Break of unit node
  | Continue of unit node
  | Print of expression
  | UnopAssign of expression * assign_unop
  | BinopAssign of expression * assign_binop * expression
  | IfElse of expression * cll_instrs * cll_instrs
  | If of expression * cll_instrs
  | While of expression * cll_instrs
  | For of cll_instrs * expression * cll_instrs * cll_instrs
  | Call of expression

(** Analogue à son équivalent IMP *)
and cll_instrs = cll_instr Cycle.cycle

(* Le type des déclarations de procédure en cll *)
type procedure_definition = {name:string node; block:cll_instrs}

and procedure_definitions = procedure_definition Cycle.cycle

and cll_prog =
  | ProcedureDefinitionData of procedure_definitions * datas
  | ProcedureDefinition of procedure_definitions

(**
  Transforme un arbre de syntaxe CLL en un arbre de syntaxe IMP.
  En plus des contraintes grammaticales, le programme doit vérifier les critères suivants :
  - pas de mention à une procédure ou une variable globale non déclarée
  - 'break' et 'continue' doivent toujours être placés dans une boucle
  - toutes les procédures doivent forcément exécuter return ou exit à un moment.
  Sur ce dernier point, la vérification est assez grossière.
  Les procédures comme 'if (true) exit; print(10);' ne compileront pas
  (en revanche, 'if (c) exit; else return;' compilent)
*)
val cll_to_imp : cll_prog compiler_type -> imp_prog compiler_type