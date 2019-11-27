open ARTTree
open IMPTree

(** 
  Ensemble des tags réservés de CLL. Les langages plus haut niveau les utilisent donc aussi indirectement. 
  Contient stack_pointer, frame_pointer et function_result.
  function_result devrait être défini dans FUN mais est utilisé par CLL pour stocker temporairement
  l'adresse de retour pour certains appels terminaux. Les deux rôles ne se chevauchent pas, il n'est donc pas
  problématique d'utiliser uniquement function_result au lieu d'utiliser return_address très peu de fois.
*)
val cll_variables : Tagset.t

(**
  Les instructions en CLL, ressemblant à celles en IMP à part les différences suivantes :
  - Disparition de goto et des déclarations d'étiquettes
  - Apparition des définitions des procédures sans paramètres possibles
  - Ajout des appels de procédure et de return
  - Ajout de TerminalCall, un return spécial pour les appels terminaux.
    Syntaxe : return f().
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
  | TerminalCall of expression

(** Analogue à son équivalent IMP *)
and cll_instrs = cll_instr Cycle.cycle

(* Le type des déclarations de procédure en cll *)
type procedure_definition = {name:string node; block:cll_instrs}

and procedure_definitions = procedure_definition Cycle.cycle

and cll_prog = procedure_definitions * datas

(**
  Transforme un arbre de syntaxe CLL en un arbre de syntaxe IMP.
  En plus des contraintes grammaticales, le programme doit vérifier les critères suivants :
  - pas de mention à une procédure ou une variable globale non déclarée
  - 'break' et 'continue' doivent toujours être placés dans une boucle
  - toutes les procédures doivent forcément exécuter return ou exit à un moment.
  Sur ce dernier point, la vérification est assez grossière.
  Les procédures comme 'if (true) exit; print(10);' ne compileront pas
  (en revanche, 'if (c) exit; else return;' compilent).

  La procédure main est toujours traduite en premier.
  Il n'y a donc pas besoin de s'inquiéter de l'ordre des procédures dans les langages qui compilent vers CLL.
  De plus elle est autorisée, comme les autres procédures, à finir par return.
  Si cette instruction est exécutée alors qu'il n'y a qu'une seule cellule dans la pile d'appel, 
  le programme crashe. C'est à l'utilisateur de s'assurer que cela n'arrive pas.
*)
val cll_to_imp : cll_prog compiler_type -> imp_prog compiler_type

(** Ecris un programme CLL dans le fichier désigné. *)
val write_cll : out_channel -> cll_prog compiler_type -> unit