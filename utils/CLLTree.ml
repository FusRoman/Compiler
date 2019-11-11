open ARTTree
open Tagset
open Cycle
open IMPTree

let cll_variables =
  add "frame_pointer" (add "return_address" (singleton "stack_pointer"))

let stack_pointer = default_node "stack_pointer"
let return_address = default_node "return_address"
let frame_pointer = default_node "frame_pointer"

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

let rec translate_instruction tag_set maker i acc =
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
    append acc (Print e)
  | BinopAssign (e1,op,e2) -> 
    append acc (simplify_assign_binop e1 op e2)
  | UnopAssign (op, e) -> 
    append acc (simplify_assign_unop op e)
  | IfElse (e1, i1, i2) -> 
    append acc (IfElse (e1, translate_instructions tag_set maker i1, translate_instructions tag_set maker i2))
  | If (e,i) -> 
    append acc (If (e,translate_instructions tag_set maker i))
  | While (e,i) -> 
    append acc (While (e,translate_instructions tag_set maker i))

  | For (init, c, it, b) ->
    let imp_init = translate_instructions tag_set maker init in
    let imp_it = translate_instructions tag_set maker it in
    let imp_b  = translate_instructions tag_set maker b in
    extend acc (for_to_while imp_init c imp_it imp_b)

  | Call e ->
    (* Etape 1 du protocole d'appel *)
    let return = maker () in
    let acc = append acc (Assign(Id return_address, Id return)) in (* *stack_pointer := &return; *)
    let acc = append acc (Goto e) in (* goto(e); *)
    append acc (TagDeclaration return) (* return: *)

  | Return ->
    (* Etape 4 du protocole d'appel *)
    let acc = append acc (Assign(Id stack_pointer, LStar(Id frame_pointer))) in (* stack_pointer := frame_pointer; *)
    let acc = append acc (Assign(Id frame_pointer, LStar(Binop(LStar(Id stack_pointer), Sub, Int 1)))) in (* frame_pointer := *(stack_pointer - 1) *)
    (* Pas besoin de mettre return_address à jour *)
    append acc (Goto(LStar(LStar(Id stack_pointer)))) (* goto( **stack_pointer ) (goto utilise une expression gauche, les deux déréférencements sont donc explicites) *)

and translate_instructions tag_set maker is =
  iter is (fun i acc -> translate_instruction tag_set maker i acc) empty_cycle

and translate_procedure tag_set maker proc_def acc =
  check_procedure proc_def tag_set;
  (* Ajoute le tag correspondant a la procedure dans le code IMP *)
  let acc = append acc (TagDeclaration proc_def.name) in
  (* Etape 2 du protocole d'appel *)
  let deref_sp = LStar(Id stack_pointer) in
  let decr_sp = simplify_assign_unop (Id stack_pointer) Decr in
  let acc = append acc (Assign(deref_sp, LStar(Id return_address))) in (* *stack_pointer := return_adress; *)
  let acc = append acc decr_sp in (* stack_pointer--; *)
  let acc = append acc (Assign(deref_sp, LStar(Id frame_pointer))) in (* *stack_pointer := frame_pointer; *)
  let acc = append acc decr_sp in (* stack_pointer--; *)
  let acc = append acc (Assign(Id frame_pointer, Binop(LStar(Id stack_pointer), Add, Int 2))) in (* frame_pointer := stack_pointer + 2; *)
  (* Traduit le bloc d'instruction de la procédure en code IMP *)
  extend acc (translate_instructions tag_set maker proc_def.block)

let translate_procedures tag_set maker procs acc =
  (* Compile toujours le main en premier *)
  let rec tr_main tag_set maker procs acc =
    if count_1 procs then
      let (p, s) = take procs in
      if p.name.contents = "main" then
        translate_procedure tag_set maker p acc
      else
        tr_main tag_set maker s acc
    else
      raise (SyntaxError("No 'main' procedure defined.", 0, 0))
  in

  (* L'ordre des autres fonctions importe peu *)
  let rec tr_not_main tag_set maker procs acc =
    if count_1 procs then
      let (p, s) = take procs in
      let acc' =
        if p.name.contents = "main" then
          acc
        else
          translate_procedure tag_set maker p acc
      in
      tr_not_main tag_set maker s acc'
    else
      acc
  in

  let acc' = tr_main tag_set maker procs acc in
  tr_not_main tag_set maker procs acc'

let cll_to_imp cll_prog =
  let tag_set = union cll_variables cll_prog.tag_set in
  let maker = make_node_maker tag_set in

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
      let instr = translate_procedures tag_set maker procs empty_cycle in
      (instr, data)
    | ProcedureDefinition procs ->
      let data = add_variables empty_cycle in
      let instr = translate_procedures tag_set maker procs empty_cycle in
      (instr, data)
  in

  { tag_set; syntax_tree = TextData(syntax_tree, data) }