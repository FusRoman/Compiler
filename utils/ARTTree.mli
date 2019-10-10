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

val compile : out_channel -> string list -> art_prog -> unit 
