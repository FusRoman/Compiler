open ARTTree
open Tagset
open Cycle
open IMPTree

let cll_variables =
  add "frame_pointer" (add "return_address" (singleton "stack_pointer"))

let stack_pointer = {contents = "stack_pointer"; line = -1; column = -1}
let return_address = {contents = "return_address"; line = -1; column = -1}
let frame_pointer = {contents = "frame_pointer"; line = -1; column = -1}

type cll_instr =
  | Nop
  | Exit
  | Return
  | Break of unit node
  | Continue of unit node
  | Print of expression
  | UnopAssign of expression * assign_unop
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

let lol = ref 0
let rec make_tag () = 
  (* A refaire pour s'assurer que c'est pas déjà pris *)
  incr lol;
  {line = -1; column = -1; contents = "return_" ^ (string_of_int (!lol - 1))}

(* 
  Vérifie si :
  - les ids utilisés sont bien tous déclarés
  - les break et continue sont situés dans des boucles
  - les procédures finissent par un return dans toutes les branches
  Contrairement à ce qu'indique l'énoncé, main peut finir avec return.
  De cette façon, d'autres procédures peuvent l'appeler.
  La façon dont la procédure est déterminée valide ou pas est assez grossière.
  Par exemple, le code 'if (true) exit; print(10);' est considéré comme invalide,
  alors qu'en pratique exit serait toujours exécuté.
  Des exit ou des return non nécessaires seront alors nécessaires.
  En pratique, ça ne devrait pas être un trop grand problème.
  Si besoin, on peut ignorer les erreurs de return / exit,
  mais la vérification des expressions restent nécessaires
  (du point de vue de l'utilisateur ; IMP s'en charge aussi, mais les messages d'erreurs
  seront peu explicites pour l'utilisateur).
*)
let check_procedure proc tag_set =
  let rec check_rec loop is =
    if count_1 is then
      let (x, is') = take is in
      match x with
      | Exit | Return -> 
        (* On veut vérifier les expressions qui suivent. 
          Par ailleurs les instructions suivantes sont inutiles et pourraient être omises. *)
        ignore (check_rec loop is'); 
        true

      | Nop -> 
        check_rec loop is'

      | Print e | Call e | UnopAssign(e, _) ->
        check_expression e tag_set;
        check_rec loop is'

      | BinopAssign(e1, _, e2) ->
        check_expression e1 tag_set;
        check_expression e2 tag_set;
        check_rec loop is'

      | If(c, b) ->
        check_expression c tag_set;
        ignore (check_rec loop b);
        check_rec loop is'

      | IfElse(c, t, e) ->
        check_expression c tag_set;
        (check_rec loop t && check_rec loop e) || check_rec loop is'

      | While(c, b) ->
        check_expression c tag_set;
        ignore (check_rec true b);
        check_rec loop is'

      | For(init, c, it, b) ->
        check_expression c tag_set;
        ignore (check_rec loop init); (* placer un break ou un continue est absurde mais autorisé (en pratique la grammaire ne l'autorise pas cela dit), mais elle devrait) *)
        ignore (check_rec true it);
        ignore (check_rec true b);
        check_rec loop is'

      | Break _ | Continue _ when loop ->
        check_rec loop is'

      | Break n | Continue n ->
        raise (SyntaxError("'break' and 'continue' statements are only allowed within loops.", n.line, n.column))
    else 
      false
  in
  if not (check_rec false proc.block) then
    raise (SyntaxError(
      Printf.sprintf "Procedure '%s' does not end with 'return;' or 'exit;' in at least one branch" proc.name.contents, 
      proc.name.line, proc.name.column
    ))

let rec translate_instruction tag_set i acc =
  match i with
  | Nop -> 
    append acc IMPTree.Nop
  | Exit -> 
    append acc Exit
  | Break n -> 
    append acc (Break n)
  | Continue n -> 
    append acc (Continue n)
  | Print e -> 
    check_expression e tag_set;
    append acc (Print e)
  | BinopAssign (e1,op,e2) -> 
    check_expression e1 tag_set;
    check_expression e2 tag_set;
    append acc (simplify_assign_binop e1 op e2)
  | UnopAssign (e,op) -> 
    check_expression e tag_set;
    append acc (simplify_assign_unop e op)
  | IfElse (e1, i1, i2) -> 
    check_expression e1 tag_set;
    append acc (IfElse (e1, translate_instructions tag_set i1, translate_instructions tag_set i2))
  | If (e,i) -> 
    check_expression e tag_set;
    append acc (If (e,translate_instructions tag_set i))
  | While (e,i) -> 
    check_expression e tag_set;
    append acc (While (e,translate_instructions tag_set i))

  | For (init, c, it, b) ->
    let imp_init = translate_instructions tag_set init in
    check_expression c tag_set;
    let imp_it = translate_instructions tag_set it in
    let imp_b  = translate_instructions tag_set b in
    extend acc (for_to_while imp_init c imp_it imp_b)

  | Call e ->
    (* Etape 1 du protocole d'appel *)
    let return = make_tag () in
    let acc = append acc (Assign(Id return_address, Address return)) in (* *stack_pointer := &return; *)
    let acc = append acc (Goto e) in (* goto(e); *)
    append acc (TagDeclaration return) (* return: *)

  | Return ->
    (* Etape 4 du protocle d'appel *)
    let acc = append acc (Assign(Id stack_pointer, Id frame_pointer)) in (* stack_pointer := frame_pointer; *)
    let acc = append acc (Assign(Id frame_pointer, LStar(Binop(Id stack_pointer, Sub, Int 1)))) in (* frame_pointer := *(stack_pointer - 1) *)
    (* Pas besoin de mettre return_address à jour *)
    append acc (Goto(LStar(LStar(Id stack_pointer)))) (* goto( **stack_pointer ) (goto utilise une expression gauche, les deux déréférencements sont donc explicites) *)

and translate_instructions tag_set is =
  iter is (fun i acc -> translate_instruction tag_set i acc) empty_cycle

and translate_procedure tag_set proc_def acc =
  check_procedure proc_def tag_set;
  (* Ajoute le tag correspondant a la procedure dans le code IMP *)
  let acc = append acc (TagDeclaration proc_def.name) in
  (* Etape 2 du protocole d'appel *)
  let deref_sp = LStar (Id stack_pointer) in
  let decr_sp = simplify_assign_unop (Id stack_pointer) Decr in
  let acc = append acc (Assign(deref_sp, Id return_address)) in (* *stack_pointer := return_adress; *)
  let acc = append acc decr_sp in (* stack_pointer--; *)
  let acc = append acc (Assign(deref_sp, Id frame_pointer)) in (* *stack_pointer := frame_pointer; *)
  let acc = append acc decr_sp in (* stack_pointer--; *)
  let acc = append acc (Assign(Id frame_pointer, Binop(Id stack_pointer, Add, Int 2))) in (* frame_pointer := stack_pointer + 2; *)
  (* Traduit le bloc d'instruction de la procédure en code IMP *)
  extend acc (translate_instructions tag_set proc_def.block)

let rec translate_procedures tag_set procs acc =
  if count_1 procs then
    let (p, s) = take procs in
    let acc' = translate_procedure tag_set p acc in
    translate_procedures tag_set s acc'
  else
    acc

let cll_to_imp cll_prog =
  (* Les variables frame_pointer, return_address et stack_pointer ont été rajoutés à tag_set dans CLLParser *)
  let add_variables data =
    let add_variable data var =
      let var' = {line = var.line; column = var.column; contents = (var.contents, 0)} in
      append data var'
    in
    let data' = add_variable data return_address in
    add_variable data' frame_pointer
  in

  let syntax_tree, data = 
    match cll_prog.syntax_tree with
    | ProcedureDefinitionData(procs, data) -> 
      let data = add_variables data in
      let instr = translate_procedures cll_prog.tag_set procs empty_cycle in
      (instr, data)
    | ProcedureDefinition procs ->
      let data = add_variables empty_cycle in
      let instr = translate_procedures cll_prog.tag_set procs empty_cycle in
      (instr, data)
  in

  let tag_set = cll_prog.tag_set in
  { tag_set; syntax_tree = TextData(syntax_tree, data) }