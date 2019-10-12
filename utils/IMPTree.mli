open ARTTree

(**
  Les instructions en IMP, ressemblant à celles en ART à part les différences suivantes :
  - Disparition de JumpWhen (émulable avec un if)
  - Remplacement de Jump par Goto (simple changement de nom, il s'agit de la même chose en réalité)
  - Apparition des conditionnelles If et IfElse, de While, de Break et de Continue.
*)
type imp_instr =
  | Nop
  | Exit
  | Break of unit node
  | Continue of unit node
  | Print of expression
  | Goto of l_expr
  | Assign of l_expr * expression
  | IfElse of expression * imp_instrs * imp_instrs
  | If of expression * imp_instrs
  | While of expression * imp_instrs
  | TagDeclaration of string node

(** Analogue à son équivalent ART *)
and imp_instrs = imp_instr Cycle.cycle

(** Analogue à son équivalent ART *)
and imp_prog =
  | TextData of imp_instrs * datas
  | Text of imp_instrs

(** 
  Traduit une boucle for en une boucle while IMP.
  'for_to_while init cond it block' représente la boucle 'for (init; cond; it) block'.
  init et it peuvent contenir plusieurs assignements. Si une seule instruction donnée
  n'est pas Assign, une erreur sera lancée.
*)
val for_to_while : imp_instrs -> expression -> imp_instrs -> imp_instrs -> imp_instrs

(**
  Transforme un arbre de syntaxe IMP en un arbre de syntaxe ART, qu'il est ensuite possible
  d'écrire dans un fichier ou de compiler en STK directement.
  Vérifie la correction du programme et tente de l'optimiser.
*)
val imp_to_art : imp_prog compiler_type -> art_prog compiler_type

(**
  'write_imp imp output' écrit le programme imp dans le fichier output.
*)
val write_imp : out_channel -> imp_prog -> unit