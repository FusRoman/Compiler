open Printf
open Cycle
open Tagset
open ARTTree
open IMPTree
open CLLTree
open FUNTree

(* A faire :
  - appels dans les expressions 
*)

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
  Renvoie (init, e, c, reevaluate, ompiler) avec :
  - init : les instructions à exécuter avant l'évaluation de l'expression
  - e : l'expression traduite
  - reevaluate : les instructions à exécuter si on veut réévaluer l'expression
  - compiler : l'état du compilateur après l'évaluation de l'expression. 
    Des variables locales anonymes ont pu être crées lors de l'évaluation de l'expression.
    On peut vouloir appeler enter_block avant l'évaluation de l'expression pour pouvoir ensuite
    supprimer ces variables sans affecter le reste du bloc.

  Ordre des appels de fonctions :
  On parcourt l'expression en DFS. Le premier appel rencontré sera le premier exécuté.
  L'adresse de la fonction sera calculée en premier, ensuite ce seront ses arguments, dans l'ordre et de manière récursive.
  Le reste de l'expression parent sera calculé en dernier.

  Si immediate vaut vrai, le dernier appel de fonction sera remplacée dans e par function_result.
*)
let translate_expression e compiler immediate =
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
  let rec call f args compiler last =
    let (finit, fe, fr, fcompiler, _) = translate_expr f compiler false in
    let init, args', reeval, compiler =
      List.fold_left (fun (init_acc, args_acc, reeval_acc, compiler) e ->
        let (b, e, r, c, l) = translate_expr e compiler last in
        (extend init_acc b, append args_acc e, extend reeval_acc r, c)
      ) (finit, empty_cycle, fr, fcompiler) args
    in
    let arg_list = to_list args' in
    if last then
      (* Dernier appel de fonction avant utilisation du résultat ; on peut utiliser function_result directement *)
      (append init (FUNTree.Call(fe, arg_list)), LStar(Id function_result), reeval, compiler)
    else
      (* Sinon on doit déclarer une variables anonyme pour stocker le résultat *)
      (* Déclaration à l'envers. C'est vraiment le bordel... Je ferais mieux d'utiliser des cycles en interne *)
      let init = append init (SetCall(LStar(Id stack_pointer), Standard, fe, arg_list)) in
      let init = append init decr_sp in
      let anonymous = ARTTree.Binop(LStar(Id frame_pointer), Sub, Int(compiler.variables + 2)) in
      let reeval = append reeval (SetCall(anonymous, Standard, fe, arg_list)) in
      let compiler = {compiler with local = compiler.local + 1; variables = compiler.variables + 1} in
      (init, LStar anonymous, reeval, compiler)

  (* 
    e : expression à traduire
    may_be_last : e est susceptible de contenir le dernier appel de fonction
    Renvoie (b, e, r, c, l) avec b, e, r et c comme expliqués avant.
    l vaut true si le dernier appel n'a pas été rencontré.
  *)
  and translate_expr e compiler may_be_last =
    match e with
    | Int i -> 
      (empty_cycle, ARTTree.Int i, empty_cycle, compiler, may_be_last)
    | Bool b -> 
      (empty_cycle, Bool b, empty_cycle, compiler, may_be_last)
    | Id id ->
      (empty_cycle, translate_id id compiler, empty_cycle, compiler, may_be_last)
    | Deref e ->
      let (b, e, r, c, l) = translate_expr e compiler may_be_last in
      (b, LStar e, r, c, l)
    | Unop(op, e) ->
      let (b, e, r, c, l) = translate_expr e compiler may_be_last in
      (b, Unop(op, e), r, c, l)
    | Binop(e1, op, e2) ->
      let (b1, e1, r1, c, l1) = translate_expr e1 compiler may_be_last in
      let (b2, e2, r2, c, l2) = translate_expr e2 c l1 in
      (extend b1 b2, Binop(e1, op, e2), extend r1 r2, c, l1 && l2)

    | Call(f, args) ->
      let (init, e, r, c) = call f args compiler may_be_last in
      (init, e, r, c, false)
  in

  let (b, e, r, c, _) = translate_expr e compiler immediate in
  (b, e, r, c)

let declare_variable acc compiler v e =
  let (init, e', _, c) = translate_expression e compiler true in
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
  Extrait les déclarations d'un bloc d'instructions, sans considérer les sous-blocs (il reste possible de le faire, assez facilement).
  Renvoie (decl, block) avec block la suite des instructions déjà inversées,
  decl la liste des déclarations trouvées dans l'ordre.
  Permet d'éviter aux boucles d'éviter de réserver de l'espace pour des variables locales puis de les supprimer
  de manière répétée.
*)
let extract_declarations block =
  let (decl, block') = iter block (fun i (decl_acc, block_acc) ->
    match i with
    | Declaration(v, e) ->
    begin
      match e with
      | Int _ | Bool _ ->
        (* Si l'expression initiale est une constante, on peut optimiser encore un peu plus *)
        (append decl_acc (Declaration(v, e)), block_acc)
      | _ ->
        (* Sinon on est obligé de laisser une affectation, à cause des potentiels effets de bord *)
        (append decl_acc (Declaration(v, Int 0)), append block_acc (BinopAssign(Id v, Standard, e)))
    end
    | _ ->
      (decl_acc, append block_acc i)
  ) (empty_cycle, empty_cycle) in
  (decl, block')

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
        (* copié-collé depuis FUN... (translate_id dans translate_expression) *)
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
      translate_instruction acc (Declaration(id_node, Deref (Id id_node))) c
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
      let (finit, f', _, c) = translate_expression f' c false in
      let acc = extend acc finit in
      let acc, c, _ =
        List.fold_left (fun (acc, c, i) e ->
          let overwritten_param = List.nth compiler.fct.params (i + not_overwritten) in
          match e with
          | Deref (Id id) when id.contents = overwritten_param.name.contents ->
            (* Un des cas où l'affectation n'a aucun effet, on peut l'éviter *)
            (acc, c, i + 1)
          | _ ->
            let (init, e', _, c) = translate_expression e c true in
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
      let (init, e', _, c) = translate_expression (Call(f, args)) compiler true in
      let acc = extend acc init in
      (append acc (Return e'), c)

  | Return e ->
    let (init, e', _, c) = translate_expression e compiler true in
    let acc = extend acc init in
    (*let acc = quit_block acc c in*)
    (append acc (Return e'), compiler)

  | Print e ->
    let (init, e', _, c) = translate_expression e compiler true in
    let acc = extend acc init in
    (append acc (Print e'), c)

  | BinopAssign(e1, op, e2) -> 
    let (init1, e1', _, c)  = translate_expression e1 compiler false in
    let (init2, e2', _, c) = translate_expression e2 c true in
    let acc = extend acc init1 in
    let acc = extend acc init2 in
    (append acc (BinopAssign(e1', op, e2')), c)

  | UnopAssign(e, op) -> 
    let (init, e', _, c) = translate_expression e compiler true in
    let acc = extend acc init in
    (append acc (UnopAssign(e', op)), c)

  | If(e, i) -> 
    let (init, e', _, compiler) = translate_expression e compiler true in
    let ci = enter_block compiler in
    let (i', ci) = translate_instructions (from_list i) ci in
    let i' = to_list (quit_block i' ci) in
    let acc = extend acc init in
    (append acc (If(e', i')), compiler)

  | IfElse(e, i1, i2) -> 
    let (init, e', _, compiler) = translate_expression e compiler true in
    let ci = enter_block compiler in
    let (i1', ci1) = translate_instructions (from_list i1) ci in
    let i1' = to_list (quit_block i1' ci1) in
    let (i2', ci2) = translate_instructions (from_list i2) ci in
    let i2' = to_list (quit_block i2' ci2) in
    let acc = extend acc init in
    (append acc (IfElse(e', i1', i2')), compiler)

  | While(e, b) ->
    let c = enter_block compiler in
    let (init, e', reeval, c) = translate_expression e c false in
    let (decl, b_extracted) = extract_declarations (from_list b) in
    let (decl, c) = translate_instructions decl c in
    let (b_fun, c) = translate_instructions b_extracted c in
    let b_fun = extend b_fun reeval in
    let acc = extend acc decl in
    let acc = extend acc init in
    let acc = append acc (While(e', to_list b_fun)) in
    (quit_block acc c, compiler)

  | For (init, cond, it, b) ->
    let cfor = enter_block compiler in
    let (decl_b, b_extracted) = extract_declarations (from_list b) in
    let (decl_it, it_extracted) = extract_declarations (from_list it) in

    let init = extend (from_list init) (extend decl_b decl_it) in
    let (init', cfor) = translate_instructions init cfor in
    let (initcond, cond', reeval, cfor) = translate_expression cond cfor false in
    let init' = extend init' initcond in

    let (b', cfor) = translate_instructions b_extracted cfor in
    let (it', cfor) = translate_instructions it_extracted cfor in
    let it' = extend it' reeval in

    let acc = append acc (For(to_list init', cond', to_list it', to_list b')) in
    (quit_block acc cfor, compiler)

  | Call(e, args) ->
    let (init, _, _, c) = translate_expression (Call(e, args)) compiler true in
    (extend acc init, c)

  | Declaration(id, e) ->
    declare_variable acc compiler id e
    
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

let write_assign file i =
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
    failwith "VARTree.write_assign: while writing the header of a for loop, found a control block"

let rec write_assigns file is =
  match is with
  | x::y::s ->
    write_assign file x;
    fprintf file ", ";
    write_assigns file (y::s)
  | x::[] ->
    write_assign file x
  | [] -> ()

let rec write_args file args =
  match args with
  | x::y::s ->
    write_var_right_expr file x;
    fprintf file ", ";
    write_args file (y::s)
  | x::[] ->
    write_var_right_expr file x
  | [] -> ()

let rec write_params file params =
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

let rec write_instruction file i depth =
  write_tabs file depth;
  match i with
  | Nop -> 
    fprintf file "nop;\n"
  | Exit -> 
    fprintf file "exit;\n"
  | Print e ->
    fprintf file "print ";
    write_var_right_expr file e;
    fprintf file ";\n"
  | Return e ->
    fprintf file "return(";
    write_var_right_expr file e;
    fprintf file ");\n"
  | Break _ ->
    fprintf file "break;\n"
  | Continue _ ->
    fprintf file "continue;\n"
  | UnopAssign _ | BinopAssign _ ->
    write_assign file i;
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
    write_assigns file init;
    fprintf file "; ";
    write_var_right_expr file c;
    fprintf file "; ";
    write_assigns file it;
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