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
  | UnopAssign of expression * unop_assign
  | BinopAssign of expression * assign_binop * expression
  | IfElse of expression * cll_instrs * cll_instrs
  | If of expression * cll_instrs
  | While of expression * cll_instrs
  | For of cll_instrs * expression * cll_instrs * cll_instrs
  | Call of expression

(** Analogue à son équivalent IMP *)
and cll_instrs = cll_instr Cycle.cycle

(* Le type des déclarations de procédure en cll *)
type procedure_definition = {name:string node; block:cll_instrs}

and procedure_definitions = procedure_definition Cycle.cycle

and cll_prog =
  | ProcedureDefinitionData of procedure_definitions * datas
  | ProcedureDefinition of procedure_definitions

(* Guigui : j'ai pas trop regardé mais je suppose que c'est inutile vu qu'on a For maintenant *)
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
  (* cette fonction prend en parametre le nom d'une procedure et creer son etiquette 
  de retour, ne devrait normalement pas poser de problème de duplication de tag vis a vis
  du programmeur vu qu'il n'est pas possible de déclarer d'étiquette en CLL *)
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
    (* On parcours chaque instrcution du bloc d'instruction d'une procedure
    et on le traduit en code IMP.
        - Call et Return ne sont pas terminé, pour l'instant, j'ajoute le saut vers la procédure
            appelé et l'etiquette de retour une fois l'execution de la procédure terminé.contents
            Du coup, il manque tous les appels a stack_pointer et les modifs de return_adress
            et frame_pointer *)
      match cll_instr with
      |Call e -> append (append imp_cycle (IMPTree.Goto e)) (make_tag e)
      |Return -> append imp_cycle IMPTree.Exit
      |other_instruction -> append imp_cycle (translate_instruction other_instruction)
  ) empty_cycle

and translate_procedure proc_def imp_instr_cycle =
(* Ajoute le tag correspondant a la procedure dans le code IMP *)
  let tag_proc = TagDeclaration proc_def.name in
  (* Traduit le bloc d'instruction de la procédure en code IMP *)
  let imp_proc_block = translate_instructions proc_def.block in
  extend (append imp_instr_cycle tag_proc) imp_proc_block 

(**
   Transforme un arbre de syntaxe CLL en un arbre de syntaxe IMP, qu'il est ensuite possible
   d'écrire dans un fichier ou de compiler en STK directement.
   Vérifie la correction du programme et tente de l'optimiser.
*)
let cll_to_imp cll_prog =
  (* - return_address sert à transmettre à une procédure appelée l’adresse à laquelle elle
      devra revenir une fois qu’elle aura terminé son exécution
     - frame_pointer contient l’adresse de la cellule courante de la chaîne d’appels
     Ces deux données sont ajouté au .data du fichier IMP de sortie *)
  let return_adress = {line = 0;column = 0; contents = ("return_adress", 0)} in
  let frame_pointer = {line = 0;column = 0; contents = ("frame_pointer", 0)} in
  let syntax_tree,data = match cll_prog.syntax_tree with
    |ProcedureDefinitionData (proc_def_cycle,data_cycle) -> 
      let new_data_cycle = append (prepend data_cycle return_adress) frame_pointer in
      (* Parcours le cycle de procedure et traduit chaque procedure en code IMP correspondant *)
      let proc_def_to_imp = iter proc_def_cycle translate_procedure empty_cycle in
      (proc_def_to_imp,new_data_cycle)
    |ProcedureDefinition proc_def_cycle ->
      let data_cycle = append (prepend empty_cycle return_adress) frame_pointer in
      let proc_def_to_imp = iter proc_def_cycle translate_procedure empty_cycle in
      (proc_def_to_imp, data_cycle)
  in
  let tag_set = cll_prog.tag_set in
  { tag_set; syntax_tree = TextData (syntax_tree, data) }