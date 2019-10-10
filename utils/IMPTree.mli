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
  | Break
  | Continue
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
  Transforme un arbre de syntaxe IMP en un arbre de syntaxe ART, qu'il est ensuite possible
  d'écrire dans un fichier ou de compiler en STK directement.
  Vérifie la correction du programme et tente de l'optimiser.
*)
val imp_to_art : imp_prog compiler_type -> art_prog compiler_type

(**
  'write_imp imp output' écrit le programme imp dans le fichier output.
*)
(*val write_imp : imp_prog -> out_channel -> unit*)