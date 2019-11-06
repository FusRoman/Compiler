type unop  = Minus | Not
type binop = Add | Sub | Mult | Div | Rem | Eq | Neq | Lt | Le | Gt | Ge | And | Or

let interpret_unop = function
  | Minus -> (~-)
  | Not -> lnot

let bti f = fun a b -> if f a b then 1 else 0
let interpret_binop = function
  | Add -> (+)
  | Sub -> (-)
  | Mult -> ( * )
  | Div -> (/)
  | Rem -> (mod)
  | Eq -> bti (=)
  | Neq -> bti (<>)
  | Lt -> bti (<)
  | Le -> bti (<=)
  | Gt -> bti (>)
  | Ge -> bti (>=)
  | And -> (land)
  | Or -> (lor)

    
let uop_to_string = function
  | Minus -> "-"
  | Not -> "!"

let bop_to_string = function
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Rem -> "%"
  | Eq -> "=="
  | Neq -> "!="
  | Lt -> "<"
  | Le -> "<="
  | Gt -> ">"
  | Ge -> ">="
  | And -> "&&"
  | Or -> "||"
