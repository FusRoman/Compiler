open Printf
open Cycle
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

(* 
  Un type contenant diverses données pour la gestion des variables locales 
  fct : la fonction en train d'être compilée
  genv : environnement global, comprenant toutes les variables globales, toutes les fonctions,
    et les paramètres de fct
  lenv : les variables locales déclarées et utilisables là où se situe dans le code.
  variables : le nombre total de variables locales utilisées. Ces variables peuvent avoir
    été déclarées par le programme ou par le compilateur.
  local : nombre de variables locales utilisées dans le bloc actuel, autrement dit le nombre de mots
    de la pile à libérer à la fin du bloc
*)
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

(* 
  Les deux fonctions suivantes étaient originellement censées être appelées respectivement au début et à la fin d'un bloc d'instructions.
  Cependant, maintenant que toutes les déclarations de variables locales sont extraites du code de la fonction et pas juste dans les boucles,
  elle sont maintenant utilisées par les expressions qui ont besoin d'empiler des valeurs (à cause d'appels de fonction).
*)
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
    else if Tagset.mem id.contents compiler.genv then
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
  Renomme les variables déclarées dans un bloc. Les déclarations sont en fait laissées dans le code, mais au lieu de les traduire
  en réservant de l'espace sur la pile comme les anciennes versions de VARTree, elles servent maintenant juste à donner la position 
  de la variable par rapport à frame_pointer dans la seconde passe de compilation (translate_instruction). Elles sont traduites
  comme des affectations par ailleurs.
  La capture de variable est autorisée, il faut donc modifier les noms des variables et les rendre uniques afin de s'assurer
  que le programme soit sémantiquement équivalent.
  Renvoie (block, variables) avec block le bloc d'instructions transformées, et variables le nombre minimum de variables 
  que l'on peut utiliser.
*)
let extract_declarations block =
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

  (*
    acc: instructions transformées
    variables: nombre de variables déclarées dans le bloc courant
    subvariables: nombre maximum de variables déclarées dans un sous-bloc du bloc courant
    map: associe les variables à leur nouveau nom
  *)
  let rec rename_instruction (acc, variables, subvariables, map) i =
    let return i =
      (append acc i, variables, subvariables, map)
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
      let (t, vt, _) = rename_instructions t map in
      let (e, ve, _) = rename_instructions e map in
      let acc = append acc (IfElse(rename_expression map c, to_list t, to_list e)) in
      (acc, variables, max subvariables (max vt ve), map)
    | If(c, t) ->
      let (t, vt, _) = rename_instructions t map in
      let acc = append acc (If(rename_expression map c, to_list t)) in
      (acc, variables, max subvariables vt, map)
    | While(c, b) ->
      let (b, vb, _) = rename_instructions b map in
      let acc = append acc (While(rename_expression map c, to_list b)) in
      (acc, variables, max subvariables vb, map)
    | For(init, c, it, b) ->
      let (init', vinit, map_init) = rename_instructions init map in
      let c' = rename_expression map_init c in
      let (b', vb, _) = rename_instructions b map_init in
      let (it', vit, _) = rename_instructions it map_init in
      let acc = append acc (For(to_list init', c', to_list it', to_list b')) in
      (acc, variables, max subvariables (vinit + vb + vit), map)
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
      let acc = append acc (Declaration(new_id, e')) in
      (acc, variables + 1, subvariables, map)

  and rename_instructions block map =
    let block, variables, subvariables, map =
      List.fold_left rename_instruction (empty_cycle, 0, 0, map) block
    in
    (block, variables + subvariables, map)
  in

  let (acc, v, _) = rename_instructions block Env.empty in
  (acc, v)

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
let preprocess_terminal_call acc f args compiler =
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
      declare_variable acc id_node (Deref (Id id_node)) c
    ) depends (acc, compiler)
  in
  (acc, f', args', compiler)

(* Pour l'instant, les conditions peuvent rajouter des désallocations de variables locales inutiles *)
let rec translate_instruction acc i compiler =
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
    let (i', _) = translate_instructions i compiler in
    let acc = extend acc init in
    (append acc (If(e', to_list i')), compiler)

  | IfElse(e, i1, i2) ->
    let (init, e', _, compiler) = translate_expression e compiler in
    let (i1', _) = translate_instructions i1 compiler in
    let (i2', _) = translate_instructions i2 compiler in
    let acc = extend acc init in
    (append acc (IfElse(e', to_list i1', to_list i2')), compiler)

  | While(e, b) ->
    let c_while = compiler in
    let (init, c', reeval, c_while) = translate_expression e c_while in
    let acc = extend acc init in
    let (b', c_while) = translate_instructions b c_while in
    let b' = extend b' reeval in
    let acc = append acc (While(c', to_list b')) in
    (acc, compiler)

  | For(init, c, it, b) ->
    let c_for = compiler in
    let (init', c_for) = translate_instructions init c_for in
    let (initc, c', reeval, c_for) = translate_expression c c_for in
    let init' = extend init' initc in
    let (b', c_for) = translate_instructions b c_for in
    let (it', c_for) = translate_instructions it c_for in
    let it' = extend it' reeval in
    let acc = append acc (For(to_list init', c', to_list it', to_list b')) in
    (acc, compiler)

  | Call(e, args) ->
    let (init, _, _, c) = translate_expression (Call(e, args)) compiler in
    (extend acc init, c)

  | Declaration(id, e) ->
    (*declare_variable acc id e compiler*)
    (* 
      Suite à extract_declarations, Declaration a changé de sens : maintenant le compilateur s'en sert juste pour savoir
      comment traduire les variables. L'espace a déjà été réservé sur le stack_pointer au début de la fonction.
    *)
    let compiler = {compiler with
      local = compiler.local; (* Espace déjà réservé, aucune nouvelle variable à libérer *)
      variables = compiler.variables + 1;
      lenv = Env.add id.contents compiler.variables compiler.lenv
    } in
    translate_instruction acc (BinopAssign(Id id, Standard, e)) compiler
    
and translate_instructions is compiler =
  List.fold_left (fun (acc, compiler) i ->
    translate_instruction acc i compiler
  ) (empty_cycle, compiler) is

and translate_instructions_cycle is compiler =
  iter is (fun i (acc, compiler) ->
    translate_instruction acc i compiler
  ) (empty_cycle, compiler)

let translate_function acc fct genv =
  check_function fct genv;
  (* On rajoute à l'environnement global car FUN se charge de la traduction des paramètres *)
  let genv = 
    List.fold_left (fun acc (p: parameter) ->
      Tagset.add_duplicate p.name.contents acc
    ) genv fct.params 
  in
  let (block, variables) = extract_declarations fct.block in
  let block = 
    if variables = 0 then
      block
    else
      prepend block (BinopAssign(Id stack_pointer, SubAssign, Int variables)) 
  in
  let compiler = {
    genv; fct; 
    lenv = Env.empty; 
    local = 0;
    variables = 0
  } in
  let (block, _) = translate_instructions_cycle block compiler in
  {name = fct.name; block = to_list block; params = fct.params}::acc 

let translate_global_declaration fct_acc data_acc decl main genv =
  match decl with
  | Fun fct ->
    if fct.name.contents = "main" then
      main := true;
    let fct_acc = translate_function fct_acc fct genv in
    (fct_acc, data_acc)

  | Var(v, e) ->
    match e with
    | Int i -> 
      (fct_acc, (v, i)::data_acc)
    | Bool b -> 
      (fct_acc, (v, Arith.int_of_bool b)::data_acc)
    | _ ->
      raise (SyntaxError("Initializing global variables with an expression other than an immediate value is not allowed in VAR.", 0, 0))

let var_to_fun lib var =
  let genv = Tagset.union_duplicate var_variables var.tag_set in
  let main = ref false in
  let (fct, data) = 
    List.fold_left (fun (fct_acc, data_acc) decl -> 
      translate_global_declaration fct_acc data_acc decl main genv
    ) ([], []) var.syntax_tree 
  in
  match (!main, lib) with
  | (false, false) -> 
    raise (SyntaxError("No function 'main' defined.", 0, 0))
  | (true, true) ->
    raise (SyntaxError("Libraries are not allowed to define a function 'main'.", 0, 0))
  | (false, true) | (true, false) ->
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
      fprintf file ";\n\n"
  ) var.syntax_tree