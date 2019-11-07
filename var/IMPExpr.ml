type expression =
  | Immediate of int
  | Name  of string
  | Unop  of Op.unop * expression
  | Binop of Op.binop * expression * expression
  | Deref of expression (* adresse *)
      
let rec to_string = function
  | Immediate(i) -> string_of_int i
  | Unop(op, e)       -> "(" ^ (Op.uop_to_string op) ^ (to_string e) ^ ")"
  | Binop(op, e1, e2) -> "(" ^ (to_string e1) ^ (Op.bop_to_string op) ^ (to_string e2) ^ ")"
  | Deref(e) -> le_to_string e
  | Name(id) -> id

and le_to_string = function
  | Name(id) -> id
  | e -> "*" ^ (to_string e)

let add e1 e2 =
  Binop(Add, e1, e2)

let sub e1 e2 =
  Binop(Sub, e1, e2)
