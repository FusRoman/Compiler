open ARTTree
open Tagset
open Cycle
open CLLTree

(* Il reste quoi à faire quand on aura résolu le problème des call ?
  - calcul des adresses (je sais pas ce que ça représente exactement)
  - passage par référence (pareil)
  - appel terminal (devrait être plutôt facile, suffit de réécrire sur la cellule courante, s'inspirer de CLL)
  - régler la grammaire

  Après ça VAR devrait être facile.
  Y aura le truc avec les déclarations dans le for qui devrait être chiant cela dit.
*)

exception UnboundValue of string node
exception SyntaxError = ARTTree.SyntaxError

let fun_variables = add "function_result" cll_variables

let stack_pointer = default_node "stack_pointer"
let frame_pointer = default_node "frame_pointer"
let function_result = default_node "function_result"

type fun_expr =
  | Int of int
  | Bool of bool
  | Id of string node
  | Address of fun_expr
  | Deref of fun_expr
  | Unop of unop * fun_expr
  | Binop of fun_expr * binop * fun_expr
  | Call of fun_expr * (fun_expr list)

type fun_instr =
  | Nop
  | Exit
  | Return of expression
  | Break of unit node
  | Continue of unit node
  | Print of expression
  | UnopAssign of expression * assign_unop
  | BinopAssign of expression * assign_binop * expression
  | IfElse of expression * fun_instrs * fun_instrs
  | If of expression * fun_instrs
  | While of expression * fun_instrs
  | For of cll_instrs * expression * fun_instrs * fun_instrs

and fun_instrs = fun_instr list

type parameter = {
  name: string;
  reference: bool;
}

type fun_definition = {
  name: string node; 
  params: parameter list;
  block: fun_instrs
}

and fun_prog = (function_definition list) * datas

let check_fun_expression e fct lenv genv =
  let check_id id =
    if not (mem id.contents lenv || mem id.contents genv) then
      raise (UnboundValue(fct.name, id))
  in
  let rec check_expr e =
    match e with
    | Int _ | Bool _ -> ()
    | Id id ->
      check_id id
    | Address e | Deref e | Unop(_, e) ->
      check_expr e
    | Binop(e1, _, e2) ->
      check_expr e1;
      check_expr e2
    | Call(f, args) ->
      check_expr f;
      List.fold_left (fun () e -> check_expr e) () args
  in
  check_expr e

(* 
  A l'instar de CLLTree.check_procedure, vérifie si :
    - les procédures finissent bien par exit ou return
    - les break et continue sont à l'intérieur de boucles
    - les expressions n'utilisent que des variables accessibles à ce moment
*)
let check_function genv fct =
  let rec compute_lenv env params =
    match params with
    | [] -> env
    | x::s ->
      compute_env (add x.name env) s
  in
  let lenv = compute_env Tagset.empty fct.params in
  let rec check_rec loop is =
    match is with
    | [] -> false
    | x::is' ->
      match x with
      | Exit ->
        ignore (check_rec loop is'); 
        true

      | Return e ->
        check_fun_expression e fct lenv genv;
        ignore (check_rec loop is');
        true

      | Nop -> 
        check_rec loop is'

      | Print e | UnopAssign(e, _) ->
        check_fun_expression e fct lenv genv;
        check_rec loop is'

      (*| Call(d, op, f, es) ->
        check_fun_expression d fct lenv genv;
        check_fun_expression f fct lenv genv;
        List.iter (fun e -> check_fun_expression e fct lenv genv) es;
        check_rec loop is'*)

      | BinopAssign(e1, _, e2) ->
        check_fun_expression e1 fct lenv genv;
        check_fun_expression e2 fct lenv genv;
        check_rec loop is'

      | If(c, b) ->
        check_fun_expression c fct lenv genv;
        ignore (check_rec loop b);
        check_rec loop is'

      | IfElse(c, t, e) ->
        check_fun_expression c fct lenv genv;
        (check_rec loop t && check_rec loop e) || check_rec loop is'

      | While(c, b) ->
        check_fun_expression c fct lenv genv;
        ignore (check_rec true b);
        check_rec loop is'

      | For(init, c, it, b) ->
        check_fun_expression c fct lenv genv;
        ignore (check_rec loop init); (* placer un break ou un continue est absurde mais autorisé (en pratique la grammaire ne l'autorise pas cela dit), mais elle devrait) *)
        ignore (check_rec true it);
        ignore (check_rec true b);
        check_rec loop is'

      | Break _ | Continue _ when loop ->
        check_rec loop is'

      | Break n | Continue n ->
        raise (SyntaxError("'break' and 'continue' statements are only allowed within loops.", n.line, n.column))
  in
  if not (check_rec false fct.block) then
    raise (SyntaxError(
      Printf.sprintf "Procedure '%s' does not end with 'return;' or 'exit;' in at least one branch" fct.name.contents, 
      fct.name.line, fct.name.column
    ))

(* Empile les arguments d'un appel sur la pile *)
let stack_args args acc =
  List.fold_right (fun e acc ->
    let acc' = append acc (AssignBinop(LStar(Id stack_pointer), Standard, e)) in
    append acc (AssignUnop(Id stack_pointer, Decr))
  ) args acc

(* 
  Renvoie (b, e, a) avec :
  - b : les instructions à exécuter avant l'évaluation de l'expression
  - e : l'expression traduite
  - a : le nombre de variables locales utilisées, autrement dit il faudra faire stack_pointer += a pour les effacer

  Ordre des appels de fonctions :
  On parcourt l'expression en DFS. Le premier appel rencontré sera le premier exécuté.
  L'adresse de la fonction sera calculée en premier, ensuite ce seront ses arguments, dans l'ordre et de manière récursive.
  Le reste de l'expression parent sera calculé en dernier.

  Si immediate vaut vrai, le dernier appel de fonction sera remplacée dans e par function_result.
*)
let translate_expression e fct genv immediate =
  let nb_params = List.length fct.params in

  (* Remplace les paramètres par le calcul de leur adresse *)
  let rec translate_id id params i =
    match params with
    | [] ->
      if mem id.contents genv then)
        ARTTree.Id id
      else
        (* Ne devrait jamais arriver puisqu'on vérifie avant *)
        raise (UnboundValue(fct.name, id))
    | x::s ->
      if x = id then
        Binop(LStar(Id frame_pointer), Add, Int(nb_params - i))
      else
        translate_id id s (i+1)
  in

  (* 
    Gère les appels de fonction.
    TODO j'en suis là et je galère comme un porc
  *)
  and call f args last =
    let b, e, a, _ =
      List.fold_right (fun e (bacc, eacc, aacc, last) ->
        let (b, e, a, l) = translate_expr e last in
        (extend b bacc, e::eacc, a + aacc, l)
      ) (f::args) (empty_cycle, true)
    in
    (* Ici stacker les résultats *)
    if last then
      (* 
        l'expression à renvoyer est LStar(Id function_result) 
        Et le nombre de variables locales 0
      *)
    else
      (* 
        Alors je sais pas. On peut pas utiliser frame_pointer parce qu'on connaît pas la distance de stack_pointer, 
        stack_pointer peut être modifié, return_address risque pas de nous aider.
        En tout le nombre de variables locales est 1
      *)
      (extend )
  in

  (* 
    e : expression à traduire
    may_be_last : e est susceptible de contenir le dernier appel de fonction
    Renvoie (b, e, a, l) avec b, e et a comme expliqués avant.
    l vaut true si le dernier appel n'a pas été rencontré.
  *)
  and translate_expr e may_be_last =
    match e with
    | Int i -> 
      (empty_cycle, ARTTree.Int i, 0, may_be_last)
    | Bool b -> 
      (empty_cycle, Bool b, 0, may_be_last)
    | Id id ->
      (empty_cycle, translate_id fct.params 0, 0, may_be_last)
    | Deref e ->
      let (b, e, a, l) = translate_expr e may_be_last in
      (b, LStar e, a, l)
    | Unop(op, e) ->
      let (b, e, a, l) = translate_expr e may_be_last in
      (b, Unop(op, e), a, l)
    | Binop(e1, op, e2) ->
      let (b2, e2, a2, l2) = translate_expr e2 may_be_last in
      let (b1, e1, a1, l1) = translate_expr e1 l2 in
      (extend b1 b2, Binop(e1, op, e2), a1 + a2, l1 && l2)
    | Address id ->
      failwith "not implemented"
    | Call(f, args) ->
      let lol = call f args may_be_last in
      (, , , false)
  in

  let (b, e, a, _) = translate_expr e immediate in
  (b, e, a)

let incr_sp a =
  AssignBinop(Id stack_pointer, Add, Int a)

(* TODO Vérifier si ça fait pas n'importe quoi avec les optimisations d'IMP *)
let rec translate_instruction genv fct i acc =
  match i with
  | Nop -> 
    append acc CLLTree.Nop
  | Exit -> 
    append acc Exit
  | Break n -> 
    append acc (Break n)
  | Continue n -> 
    append acc (Continue n)
  | Return e ->
    let (b, e', a) = translate_expression e genv fct true in
    let acc = extend acc b in
    let acc = append acc (AssignBinop(Id function_result, Standard, e')) in
    append acc Return
    (* Pas besoin de rajouter incr_sp ; return réinitialisera stack_pointer de toute façon *)

  | Print e ->
    let (b, e', a) = translate_expression e genv fct true in
    let acc = extend acc b in
    let acc = append acc (Print e') in
    append acc (incr_sp a) 

  | BinopAssign(e1, op, e2) -> 
    let (b1, e1', a1) = translate_expression e1 genv fct false in
    let (b2, e2', a2) = translate_expression e2 genv fct true in
    let acc = extend acc b1 in
    let acc = extend acc b2 in
    let acc = append acc (BinopAssign(e1', op, e2')) in
    append acc (incr_sp (a1 + a2))

  | UnopAssign(e, op) -> 
    let (b, e', a) = translate_expression e genv fct true in
    let acc = extend acc b in
    let acc = append acc (UnopAssign(e', op)) in
    append acc (incr_sp a)

  | If(e, i) -> 
    let (b, e', a) = translate_expression e genv fct true in
    let incr = incr_sp a in
    let acc  = extend acc b in
    let i' = translate_instructions genv fct i in
    let i' = prepend i' incr in
    (* Pas optimisé je pense *)
    append acc (IfElse(e', i', from_elt incr))

  | IfElse(e, i1, i2) -> 
    let (b, e', a) = translate_expression e genv fct true in
    let incr = incr_sp a in
    let acc = extend acc b in
    let i1' = translate_instructions genv fct i1 in
    let i1' = prepend i1' incr in
    let i2' = translate_instructions genv fct i2 in
    let i2' = prepend i2' incr in
    append acc (IfElse(e', i1', i2'))

  | While(e, i) -> 
    let (b, e', a) = translate_expression e genv fct true in
    let incr = incr_sp a in
    let acc = extend acc b in
    let i' = translate_instructions genv fct i in
    let i' = extend i' b in
    let i' = prepend i' incr in
    let acc = append acc (While(e', i')) in
    append acc incr

  | For (init, c, it, b) ->
    let (b, cll_c, a) = translate_expression e genv fct in
    let incr = incr_sp a in
    let cll_init = translate_instructions genv fct init in
    let cll_init = extend cll_init b
    let cll_it = translate_instructions genv fct it in
    let cll_b = translate_instructions genv fct b in
    let cll_b = prepend cll_b incr in
    let acc = append acc (For(cll_init, cll_c, cll_it, cll_b)) in
    append acc incr

  (*| Call(d, op, e, args) ->
    let d' = translate_expression d genv fct in
    let acc = AssignBinop(d', op, Id function_result)::acc in
    let acc = (Call e)::acc in
    stack_args args acc*)
    
and translate_instructions genv fct is =
  List.fold_left (fun acc i ->
    translate_instruction genv fct i acc
  ) empty_cycle is

(* Pas fait
let translate_procedure genv fct acc =
  check_function genv fct;
  (* TODO en fait CLL utilise encore les cycles, donc faut traduire avec les cycles. Relou *)

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
*)