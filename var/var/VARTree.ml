open Printf
open Cycle
open Tagset
open ARTTree
open IMPTree
open CLLTree
open FUNTree

let var_variables = fun_variables

let stack_pointer = default_node "stack_pointer"
let frame_pointer = default_node "frame_pointer"
let function_result = default_node "function_result"

type var_expression =
  | Int of int
  | Bool of bool
  | Id of string node
  | Deref of var_expression
  | Unop of unop * var_expression
  | Binop of var_expression * binop * var_expression
  | Call of var_expression * (var_expression list)

type variable = string node * var_expression

type var_instr =
  | Nop
  | Exit
  | Return of var_expression
  | Break of unit node
  | Continue of unit node
  | Print of var_expression
  | UnopAssign of var_expression * assign_unop
  | BinopAssign of var_expression * assign_binop * var_expression
  | IfElse of var_expression * var_instrs * var_instrs
  | If of var_expression * var_instrs
  | While of var_expression * var_instrs
  | For of var_instrs * var_expression * var_instrs * var_instrs
  | Call of var_expression * (var_expression list)
  | Declaration of variable

and var_instrs = var_instr list

type var_function = var_instrs function_definition

type global_declaration =
  | Fun of var_function
  | Var of variable

type var_prog = global_declaration list

(* ************************************************************************************************
 *
 * Fonctions de contrôle
 *
 * ***********************************************************************************************)

(* 
  Vérifie la structure d'une fonction, i.e. les 'break' et 'continue' sont bien placés
  et la fonction finit toujours par exit ou return.
*)
let check_function fct genv =
  let rec check_rec loop is =
    match is with
    | [] -> false
    | x::is' ->
      match x with
      | Exit | Return _ ->
        ignore (check_rec loop is'); 
        true

      | Nop | Print _ 
      | UnopAssign _ | BinopAssign _
      | Call _ | Declaration _ ->
        check_rec loop is'

      (* La grammaire interdit les exit et les return dans l'en-tête du for *)
      | If(_, b) | While(_, b) | For(_, _, _, b) ->
        ignore (check_rec loop b);
        check_rec loop is'

      | IfElse(_, t, e) ->
        (check_rec loop t && check_rec loop e) || check_rec loop is'

      | Break _ | Continue _ when loop ->
        check_rec loop is'

      | Break n | Continue n ->
        raise (SyntaxError("'break' and 'continue' statements are only allowed within loops.", n.line, n.column))
  in
  if fct.name.contents = "main" && fct.params != [] then
    raise (SyntaxError("Function 'main' can not have any parameter.", fct.name.line, fct.name.column));
  if not (check_rec false fct.block) then
    raise (SyntaxError(
      Printf.sprintf "Function '%s' does not end with 'return;' or 'exit;' in at least one branch" fct.name.contents, 
      fct.name.line, fct.name.column
    ))


(* ************************************************************************************************
 *
 * Fonctions de traduction
 *
 * ***********************************************************************************************)

module Env = Map.Make(String)

(* Un type contenant diverses données pour la gestion des variables locales *)
type var_compiler = {
  genv: Tagset.t;
  lenv: int Env.t;
  variables: int;
  local: int;
  fct: var_function;
}

let global_compiler genv = {
  genv;
  lenv = Env.empty;
  variables = 0;
  local = 0;
  fct = {
    name = {
      line = -1; 
      column = -1; 
      contents = "top-level"
    }; 
    params = []; 
    block = []
  }
}

let enter_function fct genv =
  (* On rajoute à l'environnement global car FUN se charge de la traduction des paramètres *)
  let fct_genv = List.fold_left (fun acc (p: parameter) ->
    add_duplicate p.name.contents acc
  ) genv fct.params in
  {
    genv = fct_genv;
    lenv = Env.empty;
    variables = 0;
    local = 0;
    fct;
  }

let enter_block compiler =
  {compiler with local = 0}

let quit_block acc compiler =
  if compiler.local = 0 then
    acc
  else
    append acc (FUNTree.BinopAssign(Id stack_pointer, AddAssign, Int compiler.local))

let decr_sp = FUNTree.UnopAssign(Id stack_pointer, Decr)

(*
  Renvoie (init, e, c, reevaluate, compiler) avec :
  - init : les instructions à exécuter avant l'évaluation de l'expression
  - e : l'expression traduite
  - reevaluate : les instructions à exécuter si on veut réévaluer l'expression
  - compiler : l'état du compilateur après l'évaluation de l'expression. 
    Des variables locales anonymes ont pu être crées lors de l'évaluation de l'expression.
    On peut vouloir appeler enter_block avant l'évaluation de l'expression pour pouvoir ensuite
    supprimer ces variables sans affecter le reste du bloc.

  Ordre d'évaluation :
  On parcourt l'expression en DFS à gauche. Les fonctions sont une exception puisqu'on évalue
  les arguments de gauche à droite puis ensuite seulement l'adresse de la fonction.
*)
let translate_expression e compiler =
  let translate_id id compiler =
    if Env.mem id.contents compiler.lenv then
      let index = Env.find id.contents compiler.lenv in
      ARTTree.Binop(LStar(Id frame_pointer), Sub, Int(index + 2))
    else if mem id.contents compiler.genv then
      Id id
    else
      raise (UnboundValue(compiler.fct.name, id))
  in

  (* Gère les appels de fonctions dans les expressions *)
  let rec call f args old_compiler =
    let compiler = enter_block old_compiler in
    let init, reeval, compiler =
      List.fold_left (fun (init_acc, reeval_acc, compiler) e ->
        let (b, e, r, c) = translate_expr e compiler in
        let init_acc = extend init_acc b in
        let reeval_acc = extend reeval_acc r in
        let stack_arg = from_list FUNTree.[
          BinopAssign(LStar(Id stack_pointer), Standard, e);
          decr_sp
        ] in
        let init_acc = extend init_acc stack_arg in
        let reeval_acc = extend reeval_acc stack_arg in
        let compiler = {compiler with local = compiler.local + 1; variables = compiler.variables + 1} in
        (init_acc, reeval_acc, compiler)
      ) (empty_cycle, empty_cycle, compiler) args
    in
    let finit, fe, fr, compiler = translate_expr f compiler in
    let init = extend init finit in
    let reeval = extend reeval fr in
    let call = FUNTree.Call(fe, []) in
    let init = quit_block (append init call) compiler in
    let reeval = quit_block (append reeval call) compiler in
    (init, LStar(Id function_result), reeval, old_compiler)

  (* 
    e : expression à traduire
    Renvoie (b, e, r, c) avec b, e, r et c comme expliqués avant.
  *)
  and translate_expr e compiler =
    match e with
    | Int i -> 
      (empty_cycle, ARTTree.Int i, empty_cycle, compiler)
    | Bool b -> 
      (empty_cycle, Bool b, empty_cycle, compiler)
    | Id id ->
      (empty_cycle, translate_id id compiler, empty_cycle, compiler)
    | Deref e ->
      let (b, e, r, c) = translate_expr e compiler in
      (b, LStar e, r, c)
    | Unop(op, e) ->
      let (b, e, r, c) = translate_expr e compiler in
      (b, Unop(op, e), r, c)
    | Binop(e1, op, e2) ->
      let (b1, e1, r1, c) = translate_expr e1 compiler in
      let (b2, e2, r2, c) = translate_expr e2 c in
      (extend b1 b2, Binop(e1, op, e2), extend r1 r2, c)
    | Call(f, args) ->
      let (init, e, r, c) = call f args compiler in
      (init, e, r, c)
  in
  translate_expr e compiler

let declare_variable acc v e compiler =
  let (init, e', _, c) = translate_expression e compiler in
  let acc = extend acc init in
  let acc = append acc FUNTree.(BinopAssign(LStar(Id stack_pointer), Standard, e')) in
  let acc = append acc decr_sp in
  let c = {c with
    local = compiler.local + 1;
    variables = compiler.variables + 1;
    lenv = Env.add v.contents compiler.variables compiler.lenv
  } in
  (acc, c)

(*
  Renomme les variables déclarées dans un bloc de boucle, et fait sortir toutes les déclarations locales
  pour optimiser.
  La capture de variable est autorisée, il faut donc modifier les noms des variables et les rendre uniques afin de s'assurer
  que le programme soit sémantiquement équivalent.
  Renvoie (decl, acc) avec decl la liste des 
*)
let extract_declarations loop =
  let last_num = ref 0 in

  let rec rename_expression map e =
    match e with 
    | Int _ | Bool _ -> e
    | Deref e ->
      Deref (rename_expression map e)
    | Unop(op, e) ->
      Unop(op, rename_expression map e)
    | Binop(e1, op, e2) ->
      Binop(rename_expression map e1, op, rename_expression map e2)
    | Call(f, args) ->
      let args' = List.map (rename_expression map) args in
      Call(rename_expression map f, args')
    | Id id ->
      match Env.find_opt id.contents map with
      | None -> e
      | Some name ->
        Id {id with contents = name}
  in

  let rec rename_instruction (acc, decl, map) i =
    let return i =
      (append acc i, decl, map)
    in
    match i with
    | Nop | Exit | Break _ | Continue _ -> 
      return i
    | Print e ->
      return (Print (rename_expression map e))
    | Return e ->
      return (Return (rename_expression map e))
    | UnopAssign(e, op) ->
      return (UnopAssign(rename_expression map e, op))
    | BinopAssign(d, op, e) ->
      return (BinopAssign(rename_expression map d, op, rename_expression map e))
    | IfElse(c, t, e) ->
      let (t, decl, _) = rename_instructions decl map t in
      let (e, decl, _) = rename_instructions decl map e in
      let acc = append acc (IfElse(rename_expression map c, t, e)) in
      (acc, decl, map)
    | If(c, t) ->
      let (t, decl, _) = rename_instructions decl map t in
      let acc = append acc (If(rename_expression map c, t)) in
      (acc, decl, map)
    | While(c, b) ->
      let (b, decl, _) = rename_instructions decl map b in
      let acc = append acc (While(rename_expression map c, b)) in
      (acc, decl, map)
    | For(init, c, it, b) ->
      let (init', decl, map_init) = rename_instructions decl map init in
      let c' = rename_expression map_init c in
      let (b', decl, _) = rename_instructions decl map_init b in
      let (it', decl, _) = rename_instructions decl map_init it in
      (append acc (For(init', c', it', b')), decl, map)
    | Call(f, args) ->
      let args' = List.map (rename_expression map) args in
      return (Call(rename_expression map f, args'))
    | Declaration(id, e) ->
      let e' = rename_expression map e in
      let num = !last_num in
      incr last_num;
      let name = id.contents ^ "$" ^ (string_of_int num) in
      let new_id = {id with contents = name} in
      let map = Env.add id.contents name map in
      let decl = append decl (Declaration(new_id, Int 0)) in
      let acc = append acc (BinopAssign(Id new_id, Standard, e')) in
      (acc, decl, map)

  and rename_instructions decl map block =
    let acc, decl, map =
      List.fold_left rename_instruction (empty_cycle, decl, map) block
    in
    (to_list acc, decl, map)
  in

  let (acc, decl, _) = rename_instruction (empty_cycle, empty_cycle, Env.empty) loop in
  (decl, fst (take acc)) (* On a la garantie d'avoir toujours un seul élement renvoyé si c'est une boucle *)

(*
  Utilisé pour optimiser les appels terminaux.
  - remplace les adresses des paramètres par le calcul de leur adresse AVANT d'optimiser (voir le README)
  - renvoie la liste des arguments qui doivent être copiés au sommet du stack
  Renvoie (acc, f, args, compiler) avec :
  - acc la liste des instructions avec potentiellement des déclarations en plus
  - f la nouvelle expression de l'adresse de la fonction
  - args la liste des expressions potentiellement modifiées
  - compiler le nouvel état du compilateur
*)
let rec preprocess_terminal_call acc f args compiler =
  let nb_new_args = List.length args in
  let nb_old_args = List.length compiler.fct.params in
  let not_overwritten = nb_old_args - nb_new_args in

  let rec find_param (params: parameter list) id i =
    match params with
    | [] -> None
    | x::_ when x.name.contents = id.contents -> Some(i, x)
    | _::s -> find_param s id (i + 1)
  in

  (* is_param utilise le compiler originel, pas l'accumulé *)
  let is_param id =
    if Env.mem id.contents compiler.lenv then
      (* lenv ne contient que les variables locales, pas les paramètres *)
      None
    else
      find_param compiler.fct.params id 0
  in

  (* 
    Renvoie l'ensemble des arguments précédents vu dans cette expression, et l'argument transformé 
    e : l'argument à transformer
    i : la position de cet argument
  *)
  let rec preprocess_expression e i =
    match e with
    | Int _ | Bool _ -> (Tagset.empty, e)
    | Unop(op, e) ->
      let depends, e' = preprocess_expression e i in 
      (depends, Unop(op, e'))
    | Binop(e1, op, e2) ->
      let depends1, e1' = preprocess_expression e1 i in
      let depends2, e2' = preprocess_expression e2 i in
      (Tagset.union_duplicate depends1 depends2, Binop(e1', op, e2'))
    | Call(f, args) ->
      let depends, f' = preprocess_expression f i in
      let depends, args' =
        List.fold_left (fun (depends, args') e ->
          let (depends2, e') = preprocess_expression e i in
          (Tagset.union_duplicate depends depends2, e'::args')
        ) (depends, []) args
      in
      (depends, Call(f', List.rev args'))
    | Deref (Id id) ->
    begin
      match is_param id with
      | None -> (Tagset.empty, e)
      | Some(j, x) ->
        if j >= not_overwritten && j < i then
          (* j sera réécrit avant i *)
          (Tagset.singleton x.name.contents, e)
        else
          (* j ne sera pas réécrit, ou après i *)
          (Tagset.empty, e)
    end
    | Deref e ->
      let depends, e' = preprocess_expression e i in
      (depends, Deref e')
    | Id id ->
      (* On veut juste l'adresse de id *)
      match is_param id with
      | None -> (Tagset.empty, e) (* variable globale ou locale *)
      | Some(j, x) ->
        (* 
          Ancienne version qui faisait en sorte que le paramètre puisse être modifié, ce qui est en fait le contraire de ce qu'on veut.
          Seul cas où on modifiait l'expression, donc en fait l'expression rendue est maintenant identique. Changer la fonction ?
        *)
        (*let base = Binop(Deref(Id frame_pointer), Add, Int(nb_old_args - j)) in
        (Tagset.empty, if x.reference then Deref base else base)*)
        (Tagset.singleton id.contents, e)
  in

  (* Traduction préliminaire des expressions et détection des conflits *)
  let _, f' = preprocess_expression f (-1) in
  let depends, args', _ = 
    List.fold_left (fun (depends, args, i) e ->
      let (depends2, e') = preprocess_expression e i in
      (Tagset.union_duplicate depends depends2, e'::args, i + 1)
    ) (Tagset.empty, [], not_overwritten) args
  in (* not_overwritten parce que c'est la position en mémoire qui nous intéresse, pas la position dans l'appel *)
  let args' = List.rev args' in

  (* Résolution des conflits *)
  let acc, compiler =
    Tagset.fold (fun id (acc, c) ->
      let id_node = default_node id in
      (*translate_instruction acc (Declaration(id_node, Deref (Id id_node))) c*)
      declare_variable acc id_node (Deref (Id id_node)) c
    ) depends (acc, compiler)
  in
  (acc, f', args', compiler)

(* Pour l'instant, les conditions peuvent rajouter des désallocations de variables locales inutiles *)
and translate_instruction acc i compiler =
  match i with
  | Nop -> 
    (append acc FUNTree.Nop, compiler)

  | Exit -> 
    (append acc Exit, compiler)

  | Break n ->
    let acc = quit_block acc compiler in
    (append acc (Break n), compiler)

  | Continue n -> 
    let acc = quit_block acc compiler in
    (append acc (Continue n), compiler)

  | Return(Call(f, args)) ->
    let n = List.length args in
    let space = List.length compiler.fct.params in
    let not_overwritten = space - n in
    if n <= space then
      (* 
        Appel terminal
        1) On transforme les expressions pour que les adresses de paramètres renvoient bien celles des paramètres,
          et pas celles des arguments
        2) On déclare les nouvelles variables nécessaires pour gérer les conflits de mémoire :
          on veut éviter qu'un paramètre soit réécrit puis utilisé dans les arguments suivants
        3) On réécrit les paramètres
      *)
      let (acc, f', args', c) = preprocess_terminal_call acc f args compiler in
      let (finit, f', _, c) = translate_expression f' c in
      let acc = extend acc finit in
      let acc, c, _ =
        List.fold_left (fun (acc, c, i) e ->
          let overwritten_param = List.nth compiler.fct.params (i + not_overwritten) in
          match e with
          | Deref (Id id) when id.contents = overwritten_param.name.contents ->
            (* Un des cas où l'affectation n'a aucun effet, on peut l'éviter *)
            (acc, c, i + 1)
          | _ ->
            let (init, e', _, c) = translate_expression e c in
            let acc = extend acc init in
            let acc = append acc (BinopAssign(Binop(LStar(Id frame_pointer), Add, Int(n - i)), Standard, e')) in
            (acc, c, i + 1)
          ) (acc, c, 0) args'
      in
      (append acc (TerminalCall f'), c)
    else
      (* 
        Appel normal ou presque, puisqu'on n'a pas besoin de libérer les paramètres. 
        Il n'y a pas non plus besoin de reset function_result, mais malheureusement
        il faudrait implémenter les appels terminaux depuis FUN pour avoir le contrôle sur function_result.
        Or FUN ne peut pas gérer les variables locales tel qu'il est actuellement.
      *)
      let (init, e', _, c) = translate_expression (Call(f, args)) compiler in
      let acc = extend acc init in
      (append acc (Return e'), c)

  | Return e ->
    let (init, e', _, c) = translate_expression e compiler in
    let acc = extend acc init in
    (*let acc = quit_block acc c in*)
    (append acc (Return e'), compiler)

  | Print e ->
    let (init, e', _, c) = translate_expression e compiler in
    let acc = extend acc init in
    (append acc (Print e'), c)

  | BinopAssign(e1, op, e2) -> 
    let (init1, e1', _, c)  = translate_expression e1 compiler in
    let (init2, e2', _, c) = translate_expression e2 c in
    let acc = extend acc init1 in
    let acc = extend acc init2 in
    (append acc (BinopAssign(e1', op, e2')), c)

  | UnopAssign(e, op) -> 
    let (init, e', _, c) = translate_expression e compiler in
    let acc = extend acc init in
    (append acc (UnopAssign(e', op)), c)

  | If(e, i) -> 
    let (init, e', _, compiler) = translate_expression e compiler in
    let ci = enter_block compiler in
    let (i', ci) = translate_instructions (from_list i) ci in
    let i' = to_list (quit_block i' ci) in
    let acc = extend acc init in
    (append acc (If(e', i')), compiler)

  | IfElse(e, i1, i2) ->
    let (init, e', _, compiler) = translate_expression e compiler in
    let ci = enter_block compiler in
    let (i1', ci1) = translate_instructions (from_list i1) ci in
    let i1' = to_list (quit_block i1' ci1) in
    let (i2', ci2) = translate_instructions (from_list i2) ci in
    let i2' = to_list (quit_block i2' ci2) in
    let acc = extend acc init in
    (append acc (IfElse(e', i1', i2')), compiler)

  | While _ ->
  begin
    match extract_declarations i with
    | (decl, While(e, b)) ->
      let c_while = enter_block compiler in
      let (decl', c_while) = translate_instructions decl c_while in
      let acc = extend acc decl' in
      let (init, c', reeval, c_while) = translate_expression e c_while in
      let acc = extend acc init in
      let (b', c_while) = translate_instructions (from_list b) c_while in
      let b' = extend b' reeval in
      let acc = append acc (While(c', to_list b')) in
      (quit_block acc c_while, compiler)
    | _ -> 
      failwith "translate_instruction: expected extract_declarations to return a while loop"
  end


  | For _ ->
  begin
    match extract_declarations i with
    | (decl, For(init, c, it, b)) ->
      let c_for = enter_block compiler in
      let (decl', c_for) = translate_instructions decl c_for in
      let acc = extend acc decl' in
      let (init', c_for) = translate_instructions (from_list init) c_for in
      let (initc, c', reeval, c_for) = translate_expression c c_for in
      let init' = extend init' initc in
      let (b', c_for) = translate_instructions (from_list b) c_for in
      let (it', c_for) = translate_instructions (from_list it) c_for in
      let it' = extend it' reeval in
      let acc = append acc (For(to_list init', c', to_list it', to_list b')) in
      (quit_block acc c_for, compiler)

    | _ ->
      failwith "translate_instruction: expected extract_declarations to return a for loop"
  end

  | Call(e, args) ->
    let (init, _, _, c) = translate_expression (Call(e, args)) compiler in
    (extend acc init, c)

  | Declaration(id, e) ->
    declare_variable acc id e compiler
    
and translate_instructions is compiler =
  iter is (fun i (acc, compiler) ->
    translate_instruction acc i compiler
  ) (empty_cycle, compiler)

let translate_function acc fct genv =
  let compiler = enter_function fct genv in
  let (block, _) = translate_instructions (from_list fct.block) compiler in
  {name = fct.name; block = to_list block; params = fct.params}::acc 

let translate_global_declaration fct_acc data_acc init_acc decl main genv =
  match decl with
  | Fun fct ->
    check_function fct genv;
    if fct.name.contents = "main" then
    begin
      main := Some fct;
      (fct_acc, data_acc, init_acc)
    end
    else
      let fct_acc = translate_function fct_acc fct genv in
      (fct_acc, data_acc, init_acc)

  | Var(v, e) ->
    match e with
    | Int i -> 
      (fct_acc, (v, i)::data_acc, init_acc)
    | Bool b -> 
      (fct_acc, (v, Arith.int_of_bool b)::data_acc, init_acc)
    | _ ->
      let init_acc = (BinopAssign(Id v, Standard, e))::init_acc in
      (fct_acc, (v, 0)::data_acc, init_acc)

let var_to_fun var =
  let genv = union_duplicate var_variables var.tag_set in
  let main = ref None in
  let (fct, data, init) = List.fold_left (fun (fct_acc, data_acc, init_acc) decl -> 
    translate_global_declaration fct_acc data_acc init_acc decl main genv
  ) ([], [], []) var.syntax_tree in
  match !main with
  | None -> 
    raise (SyntaxError("No function 'main' defined.", 0, 0))
  | Some main_fct ->
    let main = {main_fct with block = (List.rev init) @ main_fct.block} in
    let fct = translate_function fct main genv in
    {syntax_tree = (List.rev fct, List.rev data); tag_set = genv}


(* ************************************************************************************************
 *
 * Fonctions de print
 *
 * ***********************************************************************************************)

let write_tabs file depth =
  for i = 1 to depth do
    fprintf file "\t"
  done

let rec write_args file args =
  match args with
  | [] -> ()
  | x::[] ->
    write_var_right_expr file x
  | x::y::s ->
    write_var_right_expr file x;
    fprintf file ",";
    write_args file (y::s)

and write_var_right_expr file e =
  match e with
  | Int i -> 
    fprintf file "%d" i
  | Bool b -> 
    fprintf file "%b" b
  | Binop(e1, op, e2) -> 
    fprintf file "(";
    write_var_right_expr file e1;
    fprintf file " %s " (string_of_binop op);
    write_var_right_expr file e2;
    fprintf file ")"
  | Unop(op, e) -> 
    fprintf file "(";
    fprintf file "%s" (string_of_unop op); 
    write_var_right_expr file e;
    fprintf file ")"
  | Deref(Id i) ->
    fprintf file "%s" i.contents
  | Id i ->
    fprintf file "(&%s)" i.contents
  | Deref e -> 
    fprintf file "*(";
    write_var_right_expr file e;
    fprintf file ")"
  | Call(f, args) ->
    write_var_left_expr file f;
    fprintf file "(";
    write_args file args;
    fprintf file ")"

and write_var_left_expr file e =
  match e with
  | Id i ->
    fprintf file "%s" i.contents
  | Deref(Id i) ->
    fprintf file "*(%s)" i.contents
  | Deref e ->
    fprintf file "*(*(";
    write_var_right_expr file e;
    fprintf file "))"
  | _ ->
    fprintf file "*(";
    write_var_right_expr file e;
    fprintf file ")"

and write_assign file i depth =
  match i with
  | Nop -> 
    fprintf file "nop"
  | Exit -> 
    fprintf file "exit"
  | Print e ->
    fprintf file "print(";
    write_var_right_expr file e;
    fprintf file ")"
  | Return e ->
    fprintf file "return ";
    write_var_right_expr file e
  | Break _ ->
    fprintf file "break"
  | Continue _ ->
    fprintf file "continue"
  | UnopAssign(e, op) ->
    write_var_left_expr file e;
    fprintf file "%s" (string_of_assign_unop op)
  | BinopAssign(d, op, e) ->
    write_var_left_expr file d;
    fprintf file " %s " (string_of_assign_binop op);
    write_var_right_expr file e
  | Call(f, args) ->
    write_var_left_expr file f;
    fprintf file "(";
    write_args file args;
    fprintf file ")"
  | Declaration(v, e) ->
    fprintf file "var %s := " v.contents;
    write_var_right_expr file e
  | _ ->
    fprintf file "\n";
    write_instruction file i (depth + 1)

and write_assigns file is depth =
  match is with
  | x::y::s ->
    write_assign file x depth;
    fprintf file ", ";
    write_assigns file (y::s) depth
  | x::[] ->
    write_assign file x depth
  | [] -> ()

and write_params file params =
  match params with
  | x::y::s ->
    if x.reference then
      fprintf file "&";
    fprintf file "%s, " x.name.contents;
    write_params file (y::s)
  | x::[] ->
    if x.reference then
      fprintf file "&";
    fprintf file "%s" x.name.contents
  | [] -> ()

and write_instruction file i depth =
  write_tabs file depth;
  match i with
  | Nop -> 
    fprintf file "nop;\n"
  | Exit -> 
    fprintf file "exit;\n"
  | Print e ->
    fprintf file "print(";
    write_var_right_expr file e;
    fprintf file ");\n"
  | Return e ->
    fprintf file "return ";
    write_var_right_expr file e;
    fprintf file ";\n"
  | Break _ ->
    fprintf file "break;\n"
  | Continue _ ->
    fprintf file "continue;\n"
  | UnopAssign _ | BinopAssign _ ->
    write_assign file i depth;
    fprintf file ";\n"
  | IfElse(c, t, e) ->
    fprintf file "if (";
    write_var_right_expr file c;
    fprintf file ") {\n";
    write_instructions file t (depth + 1);
    write_tabs file depth;
    fprintf file "} else {\n";
    write_instructions file e (depth + 1);
    write_tabs file depth;
    fprintf file "}\n"
  | If(c, t) ->
    fprintf file "if (";
    write_var_right_expr file c;
    fprintf file ") {\n";
    write_instructions file t (depth + 1);
    write_tabs file depth;
    fprintf file "}\n"
  | While(c, b) ->
    fprintf file "while (";
    write_var_right_expr file c;
    fprintf file ") {\n";
    write_instructions file b (depth + 1);
    write_tabs file depth;
    fprintf file "}\n"
  | For(init, c, it, b) ->
    fprintf file "for (";
    write_assigns file init depth;
    fprintf file "; ";
    write_var_right_expr file c;
    fprintf file "; ";
    write_assigns file it depth;
    fprintf file ") {\n";
    write_instructions file b (depth + 1);
    write_tabs file depth;
    fprintf file "}\n";
  | Call(f, args) ->
    write_var_left_expr file f;
    fprintf file "(";
    write_args file args;
    fprintf file ");\n"
  | Declaration(v, e) ->
    fprintf file "var %s := " v.contents;
    write_var_right_expr file e;
    fprintf file ";\n"

and write_instructions file is depth =
  List.iter (fun i -> write_instruction file i depth) is

let write_var file var =
  List.iter (fun decl ->
    match decl with
    | Fun fct ->
      fprintf file "%s(" fct.name.contents;
      write_params file fct.params;
      fprintf file ") {\n";
      write_instructions file fct.block 1;
      fprintf file "}\n\n"
    | Var(v, e) ->
      fprintf file "var %s := " v.contents;
      write_var_right_expr file e;
      fprintf file ";\n"
  ) var.syntax_tree