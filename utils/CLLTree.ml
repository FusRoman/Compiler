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
  | Call of expression

(** Analogue à son équivalent IMP *)
and cll_instrs = cll_instr Cycle.cycle

(* Le type des déclarations de procédure en cll *)
type procedure_definition = {name:string node; block:cll_instrs}

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

let rec make_tag e = 
  match e with
  |Id s -> TagDeclaration {line=0;column=0;contents=("return_" ^ s.contents)}
  |LStar e -> make_tag e
  |_ -> failwith "bad tag declaration" 

let rec translate_instruction i =
  match i with
  |Nop -> IMPTree.Nop
  |Exit -> IMPTree.Exit
  |Break n -> IMPTree.Break n
  |Continue n -> IMPTree.Continue n
  |Print e -> IMPTree.Print e
  |Assign (e1,e2) -> IMPTree.Assign (e1,e2)
  |IfElse (e1, i1, i2) -> IMPTree.IfElse (e1, translate_instructions i1, translate_instructions i2)
  |If (e,i) -> IMPTree.If (e,translate_instructions i)
  |While (e,i) -> IMPTree.While (e,translate_instructions i)
  |_ -> failwith "erreur sur l'appel à translate_instruction"

and translate_instructions is =
  iter is (
    fun cll_instr imp_cycle ->
      match cll_instr with
      |Call e -> append (append imp_cycle (IMPTree.Goto e)) (make_tag e)
      |Return -> append imp_cycle IMPTree.Exit
      |other_instruction -> append imp_cycle (translate_instruction other_instruction)
  ) empty_cycle

and translate_procedure proc_def imp_instr_cycle =
  let tag_proc = TagDeclaration proc_def.name in
  let imp_proc_block = translate_instructions proc_def.block in
  extend (append imp_instr_cycle tag_proc) imp_proc_block 

(**
   Transforme un arbre de syntaxe CLL en un arbre de syntaxe IMP, qu'il est ensuite possible
   d'écrire dans un fichier ou de compiler en STK directement.
   Vérifie la correction du programme et tente de l'optimiser.
*)
let cll_to_imp cll_prog =
  let return_adress = {line = 0;column = 0; contents = ("return_adress", 0)} in
  let frame_pointer = {line = 0;column = 0; contents = ("frame_pointer", 0)} in
  let syntax_tree,data = match cll_prog.syntax_tree with
    |Procedure_Definition_Data (proc_def_cycle,data_cycle) -> 
      let new_data_cycle = prepend (prepend data_cycle return_adress) frame_pointer in
      let proc_def_to_imp = iter proc_def_cycle translate_procedure empty_cycle in
      (proc_def_to_imp,new_data_cycle)
    |Procedure_Definition proc_def_cycle ->
      let data_cycle = prepend (prepend empty_cycle return_adress) frame_pointer in
      let proc_def_to_imp = iter proc_def_cycle translate_procedure empty_cycle in
      (proc_def_to_imp, data_cycle)
  in
  let tag_set = cll_prog.tag_set in
  { tag_set; syntax_tree = TextData (syntax_tree, data) }