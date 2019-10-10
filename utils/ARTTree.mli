exception SyntaxError of string * int * int

type binop = 
  | Add
  | Sub 
  | Mult
  | Div
  | And
  | Or
  | Rem
  | Lt
  | Le
  | Gt
  | Ge
  | Eq
  | Neq

type unop =
  | Minus
  | Not
  | Cpl

(* 
  Contient un arbre enfant mais aussi des informations au cas où on lever une erreur.
  Pour l'instant, uniquement utilisé dès lors qu'on déclare ou qu'on utilise un tag ;
  on pourrait lui trouver d'autres utilisations à l'avenir.
*)
type 'a node = {
  line: int;
  column: int;
  contents: 'a
}

(**
  Type des arbres "de compilation".
  Il ne s'agit pas vraiment d'arbres de syntaxe puisque ce n'est pas une traduction exacte de la grammaire,
  mais ils en sont proches.
*)
type art_prog =
  | ProgData of art_instrs * datas
  | Prog of art_instrs

and art_instrs = art_instr Cycle.cycle

and art_instr = 
  | Print of expression
  | Assign of l_expr * expression
  | Nop
  | Exit
  | Jump of l_expr
  | JumpWhen of l_expr * expression
  | TagDeclaration of string node 

and data = (string * int) node

and datas = data Cycle.cycle

and expression =
  | Int of int
  | Bool of bool
  | LExpr of l_expr
  | StackPointer
  | Binop of expression * binop * expression
  | Unop of unop * expression
  | Address of l_expr

and l_expr = 
  | Id of string node
  | LStar of l_expr

type 'a compiler_type = {
  tag_set: Tagset.t;
  syntax_tree: 'a
}

(** Renvoie la fonction correspondant à l'opérateur binaire donné *)
val binop_fun : binop -> int -> int -> int

(** Renvoie la fonction correspondant à l'opérateur unaire donné *)
val unop_fun : unop -> int -> int

(**
  Optimise une expression :
  - tant qu'il s'agit de calculs de constantes, simplifie
  - annule les double NOT / MINUS / CPL
  - optimise l'ordre de calcul des expression arithmetique pour minimiser l'utilisation des registres.    
  Les expressions en IMP ont le même type que celles en ART, donc il n'y a pas de traduction à faire.

  Retourne (sub, v) où sub est l'expression optimisée, et v contient un entier si l'expression est une constante 
  ou a été optimisée en tant que telle, et vaut None sinon.

  Au final puisque IMP a besoin de cette fonction pour optimiser non seulement ses expressions mais aussi
  ses structures de contrôle, utiliser cette fonction dans la compilation de ART serait redondant 
  (puisque ce serait déjà optimisé) et ferait donc juste perdre du temps.


  Si l'on a une expression de la forme (e1 * e2) ou (e1 + e2) et que r1 désigne le nombre de registres requis pour
  l'exécution de e1 et r2 le nombre de registres requis pour l'execution de e2, alors il faut max(r1,r2 + 1) registres 
  pour l'execution de l'expression complète : il faut un registre pour enregistrer le résultat de e1 
  lors du calcul de e2, d'où r2 + 1.
  Comme on veut minimiser max(r1, r2 + 1), on veut que r2 soit plus petit que r1 si possible.
  Si r1 < r2 alors on inverse donc les opérandes sinon on laisse tel quel.
  Cette optimisation ne peut naturellement qu'être effectuée sur les opérations commutatives. 
*)
val optimize_expression : expression -> expression * int option

val compile : out_channel -> string list -> art_prog -> unit 
