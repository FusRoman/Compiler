open ARTTree
open Tagset
open Cycle
open CLLTree

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

(* 
  Renvoie (b, e, a) avec :
  - b : les instructions à exécuter avant l'évaluation de l'expression
  - e : l'expression traduite
  - a : les instructions à exécuter après
*)
let translate_expression e fct genv =
  let nb_params = List.length fct.params in
  let rec address_id id params i =
    match params with
    | [] ->
      if mem id.contents genv then
        Id (id)
      else
        (* Ne devrait jamais arriver puisqu'on vérifie avant *)
        raise (UnboundValue(fct.name, id))
    | x::s ->
      if x = id then
        Binop(Id frame_pointer), Add, Int(nb_params - i)
      else
        address_id id s (i+1)
  in
  let rec translate_expr e =
    match e with
    | Int _ | Bool _ -> e
    | Id id ->
      LStar (translate_id fct.params 0)
    | LStar e -> 
      LStar (translate_expr e)
    | Unop(op, e) ->
      Unop(op, translate_expr e)
    | Binop(e1, op, e2) ->
      Binop(translate_expr e1, op, translate_expr e2)
    | Address id ->
  in
  translate_expr e

let stack_args args acc =
  List.fold_right (fun e acc ->
    (AssignBinop(LStar(Id stack_pointer), Standard, e))::
    (AssignUnop(Id stack_pointer, Decr))::
    acc
  ) args acc

let rec translate_instruction genv fct i acc =
  match i with
  | Nop -> 
    CLLTree.Nop::acc
  | Exit -> 
    Exit::acc
  | Break n -> 
    (Break n)::acc
  | Continue n -> 
    (Continue n)::acc
  | Return e ->
    let acc' = Return::acc in
    let e' = translate_expression e genv fct in
    (AssignBinop(Id function_result, Standard, e'))::acc'
  | Print e ->
    (Print e)::acc
  | BinopAssign(e1, op, e2) -> 
    (Binop(e1, op, e2))::acc
  | UnopAssign(op, e) -> 
    (UnopAssign(op, e))::acc
  | IfElse (e1, i1, i2) -> 
    let cll_i1 = translate_instructions genv fct i1 in
    let cll_i2 = translate_instructions genv fct i2 in
    (IfElse(e1, cll_i1, cll_i2))::acc
  | If (e,i) -> 
    (If(e, translate_instructions genv fct i))::acc
  | While (e,i) -> 
    (While(e, translate_instructions genv fct i))::acc
  | For (init, c, it, b) ->
    let cll_init = translate_instructions genv fct init in
    let cll_it = translate_instructions genv fct it ini
    let cll_b  = translate_instructions genv fct b in
    (For(cll_init, c, cll_it, cll_b))::acc
  | Call(d, op, e, args) ->
    let d' = translate_expression d genv fct in
    let acc = AssignBinop(d', op, Id function_result)::acc in
    let acc = (Call e)::acc in
    stack_args args acc
    
and translate_instructions genv fct is =
  List.fold_right (fun i acc ->
    translate_instruction genv fct i acc
  ) is []

and translate_procedure genv fct acc =
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