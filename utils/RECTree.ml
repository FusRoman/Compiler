open ARTTree
open VARTree
open TPLTree
open VARParser
open VARLexer
open IMPTree
open FUNTree

type rec_expression =
  | Int of int
  | Bool of bool
  | Id of string node
  | Deref of rec_expression
  | Unop of unop * rec_expression
  | Binop of rec_expression * binop * rec_expression
  | Call of rec_expression * (rec_expression list)
  | RecordAccess of rec_expression * string

type variable = string node * rec_expression

(** Instructions en REC *)
type rec_instr =
  | Nop
  | Exit
  | Return of rec_expression
  | Break of unit node
  | Continue of unit node
  | Print of rec_expression
  | UnopAssign of rec_expression * assign_unop
  | BinopAssign of rec_expression * assign_binop * rec_expression
  | IfElse of rec_expression * rec_instrs * rec_instrs
  | If of rec_expression * rec_instrs
  | While of rec_expression * rec_instrs
  | For of rec_instrs * rec_expression * rec_instrs * rec_instrs
  | Call of rec_expression * (rec_expression list)
  | Declaration of variable
  | NewRecord of rec_expression * string

and rec_instrs = rec_instr list

type rec_function = rec_instrs function_definition

type global_declaration =
  | Fun of rec_function
  | Var of variable

type rec_prog = global_declaration list

let rec_variables = tpl_variables

let assoc_record = Hashtbl.create 10

let rec translate_expression e =
  match e with
  |RecordAccess (e, s) -> e
  |expre -> expre

let translate_instruction i =
  match i with
  |NewRecord (e, s) -> let name_record = translate_left_expression e in
  Hashtbl.add assoc_record (name_record, s) 
  |instr -> instr

let rec_to_tpl rec_prog =
  let tag_set = Tagset.union_duplicate rec_variables rec_prog.tag_set in
  let rec_lib = program token (Lexing.from_channel (open_in "typ/rec/RECLib.tpl")) in
  let syntax_tree = rec_lib.syntax_tree @ rec_prog.syntax_tree in
  {tag_set; syntax_tree}