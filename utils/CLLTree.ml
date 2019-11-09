open ARTTree
open Tagset
open Cycle
open IMPTree

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

(** Analogue à son équivalent IMP *)
and cll_instrs = cll_instr Cycle.cycle

(* Le type des déclarations de procédure en cll *)
type procedure_definition = {name:string; block:cll_instrs}

and procedure_definitions = procedure_definition Cycle.cycle

and cll_prog =
  | Procedure_Definition_Data of procedure_definitions * datas
  | Procedure_Definition of procedure_definitions

let for_to_while init cond it block =
  let rec append_assign assigns acc =
    if assigns = Cycle.empty_cycle then
      acc
    else
      let (a, s) = Cycle.take assigns in
      match a with
      | Assign(l, e) ->
        append_assign s (Cycle.append acc a) 
      | _ ->
        failwith "for_to_while: only assignments are allowed in init and it"
  in
  let result = append_assign init Cycle.empty_cycle in
  let block' = append_assign it block in
  Cycle.append result (While(cond, block'))


(**
   Transforme un arbre de syntaxe CLL en un arbre de syntaxe IMP, qu'il est ensuite possible
   d'écrire dans un fichier ou de compiler en STK directement.
   Vérifie la correction du programme et tente de l'optimiser.
*) 
let cll_to_imp prog =
  { tag_set = empty; syntax_tree = Text Cycle.empty_cycle }