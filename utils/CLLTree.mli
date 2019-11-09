open ARTTree

(**
  Les instructions en CLL, ressemblant à celles en IMP à part les différences suivantes :
  - Disparition de GOTO et des déclarations d'étiquettes
  - Apparition des définitions des procedures sans paramètres possibles
  - Ajout des appels de procédure
*)
type cll_instr =
  | Nop
  | Exit
  | Return
  | Break of unit node
  | Continue of unit node
  | Print of expression
  | Assign of expression * expression
  | IfElse of expression * cll_instrs * cll_instrs
  | If of expression * cll_instrs
  | While of expression * cll_instrs
  | Call of string node
  | DataDeclaration of string node

(** Analogue à son équivalent ART *)
and cll_instrs = cll_instr Cycle.cycle

(* Le type des déclarations de procédure en cll *)
type procedure_definition = {name:string; block:cll_instrs}

and procedure_definitions = procedure_definition Cycle.cycle

(** Analogue à son équivalent ART *)
and cll_prog =
  | Procedure_Definition_Data of procedure_definitions * datas
  | Procedure_Definition of procedure_definitions

(**
  Transforme un arbre de syntaxe CLL en un arbre de syntaxe IMP, qu'il est ensuite possible
  d'écrire dans un fichier ou de compiler en STK directement.
  Vérifie la correction du programme et tente de l'optimiser.
*)
val cll_to_imp : cll_prog compiler_type -> imp_prog compiler_type