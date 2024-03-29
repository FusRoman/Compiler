open ARTTree
open IMPTree

(** En plus des variables réservés d'IMP, rajoute frame_pointer. *)
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
  (en revanche, 'if (c) exit; else return;' compile).

  La procédure main est toujours traduite en premier.
  Il n'y a donc pas besoin de s'inquiéter de l'ordre des procédures dans les langages qui compilent vers CLL.
  De plus elle est autorisée, comme les autres procédures, à finir par return.
  Si cette instruction est exécutée alors qu'il n'y a qu'une seule cellule dans la pile d'appel, 
  le programme crashe. C'est à l'utilisateur de s'assurer que cela n'arrive pas.

  Le premier argument doit valeur true si le programme compilé est une librairie.
  Si c'est le cas, aucune vérification n'est faite sur la présence ou l'abscence d'un procédure main.
  Si ce n'est pas une librairie, une procédure main doit être définie et elle sera toujours compilée
  en premier.

  Le deuxième est le nom du fichier. Il est utilisé pour produire des tags automatiquement en garantissant
  qu'ils soient différents de tous ceux de programmes avec un nom ou un emplacement différents,
  garantie dont on a besoin à cause des librairies.
*)
val cll_to_imp : string -> bool -> cll_prog compiler_type -> imp_prog compiler_type

(** Ecris un programme CLL dans le fichier désigné. *)
val write_cll : out_channel -> cll_prog compiler_type -> unit