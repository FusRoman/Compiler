open Printf
open Tagset
open ARTTree
open IMPTree
open CLLTree
open FUNTree

(* A faire :
  - optimisation des blocs pour les boucles
  - appels dans les expressions
*)

let var_variables = fun_variables

let stack_pointer = default_node "stack_pointer"
let frame_pointer = default_node "frame_pointer"
let function_result = default_node "function_result"

(*type fun_expr =
  | Int of int
  | Bool of bool
  | Id of string node
  | Address of fun_expr
  | Deref of fun_expr
  | Unop of unop * fun_expr
  | Binop of fun_expr * binop * fun_expr
  | Call of fun_expr * (fun_expr list)*)

type variable = string node * expression

type var_instr =
  | Nop
  | Exit
  | Return of expression
  | Break of unit node
  | Continue of unit node
  | Print of expression
  | UnopAssign of expression * assign_unop
  | BinopAssign of expression * assign_binop * expression
  | IfElse of expression * var_instrs * var_instrs
  | If of expression * var_instrs
  | While of expression * var_instrs
  | For of var_instrs * expression * var_instrs * var_instrs
  | SetCall of expression * assign_binop * expression * (expression list)
  | Call of expression * (expression list)
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
      | SetCall _ | Call _
      | Declaration _ ->
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
  Ajoute une instruction à la fin d'une liste d'instructions. NB : 
  à chaque fois qu'on a fini de traduire un bloc, il faut appeler List.rev.
*)
let append acc i = i::acc

(* 
  Ajoute une séquence d'instructions à la fin d'une liste d'instructions.
  Si on appelle cette fonction avec [i1; i2; i3], le résultat sera
  [i3; i2; i1] @ acc ; la liste à ajouter est dans un ordre intuitif pour un humain,
  pas à l'envers.
*)
let extend acc is =
  List.fold_left (fun acc i -> i::acc) acc is

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

let rec translate_expression e compiler =
  match e with
  | Int _ | Bool _ -> e
  | Id id -> 
    if Env.mem id.contents compiler.lenv then
      let index = Env.find id.contents compiler.lenv in
      Binop(LStar(Id frame_pointer), Sub, Int(index + 2))
    else if mem id.contents compiler.genv then
      e
    else
    begin
      printf "Local environment:\n";
      Env.iter (fun k v -> printf "%s: %d\n" k v) compiler.lenv;
      raise (UnboundValue(compiler.fct.name, id))
    end
  | LStar e -> 
    LStar(translate_expression e compiler)
  | Unop(op, e) ->
    Unop(op, translate_expression e compiler)
  | Binop(e1, op, e2) ->
    Binop(translate_expression e1 compiler, op, translate_expression e2 compiler)

let declare_variable acc compiler v e =
  let e' = translate_expression e compiler in
  let acc = extend acc FUNTree.[
    BinopAssign(LStar(Id stack_pointer), Standard, e');
    UnopAssign(Id stack_pointer, Decr)
  ] in
  let compiler = {compiler with
    local = compiler.local + 1;
    variables = compiler.variables + 1;
    lenv = Env.add v.contents compiler.variables compiler.lenv
  } in
  (acc, compiler)

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
(*let translate_expression e fct genv immediate =
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

  (* Gère les appels de fonction. *)
  and call f args last =
    let (fb, fe, fa, _) = translate_expr e true in
    let b, a, _ =
      List.fold_right (fun e (bacc, aacc) ->
        let (b, e, a, l) = translate_expr e true in
        (* stock l'argument dans la pile *)
        let b = append b (AssignBinop(LStar(Id stack_pointer), Standard, e)) in
        let b = append b (AssignUnop(Id stack_pointer, Decr)) in 
        (* Calcul de l'argument puis stockage avant de passer au reste des expressions *)
        (extend b bacc, a + 1 + aacc)
      ) (args) (empty_cycle, fa)
    in
    let bc = extend fb b in
    let bc = append b (Call fe) in
    if last then
      (* Dernier appel de fonction dans la liste. La valeur de function_result ne devrait pas être écrasée. *)
      (bc, LStar(Id function_result), a)
    else
      (* 
        Comment connaître l'adresse de cet argument ?
        On sait pas où est frame_pointer à partir de stack_pointer
        stack_pointer peut être modifié au plein milieu d'une instruction
        return_address va pas nous aider
        function_result peut être modifié par appel de fonction
        Donc il faut une nouvelle variable je pense, qui pourra d'ailleurs être utilisée par VAR.
        Ah oui mais si on faisait tout dans VAR, on n'aurait pas besoin de variables du tout.
        En tout le nombre de variables locales est 1
      *)
      (bc, ..., a + 1)
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
  (b, e, a)*)

(* 
  Extrait les déclarations d'un bloc d'instructions, sans considérer les sous-blocs (il reste possible de le faire, assez facilement).
  Renvoie (decl, block) avec block la suite des instructions déjà inversées,
  decl la liste des déclarations trouvées inversées elles aussi.
  Permet d'éviter aux boucles d'éviter de réserver de l'espace pour des variables locales puis de les supprimer
  de manière répétée.
*)
let extract_declarations block =
  let (decl, block') = List.fold_left (fun (decl_acc, block_acc) i ->
    match i with
    | Declaration(v, e) ->
    begin
      match e with
      | Int _ | Bool _ ->
        (* Si l'expression initiale est une constante, on peut optimiser encore un peu plus *)
        ((Declaration(v, e))::decl_acc, block_acc)
      | _ ->
        (* Sinon on est obligé de laisser une affectation, à cause des potentiels effets de bord *)
        ((Declaration(v, Int 0))::decl_acc, (BinopAssign(Id v, Standard, e))::block_acc)
    end
    | _ ->
      (decl_acc, i::block_acc)
  ) ([], []) block in
  (List.rev decl, List.rev block')

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

  | Return e ->
    let e' = translate_expression e compiler in
    let acc = quit_block acc compiler in
    (append acc (Return e'), compiler)

  | Print e ->
    let e' = translate_expression e compiler in
    (append acc (Print e'), compiler)

  | BinopAssign(e1, op, e2) -> 
    let e1' = translate_expression e1 compiler in
    let e2' = translate_expression e2 compiler in
    (append acc (BinopAssign(e1', op, e2')), compiler)

  | UnopAssign(e, op) -> 
    let e' = translate_expression e compiler in
    (append acc (UnopAssign(e', op)), compiler)

  | If(e, i) -> 
    let e' = translate_expression e compiler in
    let (i', compiler') = translate_instructions i compiler in
    let i' = List.rev (quit_block i' compiler') in
    (append acc (If(e', i')), compiler)

  | IfElse(e, i1, i2) -> 
    let e' = translate_expression e compiler in
    let (i1', compiler1) = translate_instructions i1 compiler in
    let i1' = List.rev (quit_block i1' compiler1) in
    let (i2', compiler2) = translate_instructions i2 compiler in
    let i2' = List.rev (quit_block i2' compiler2) in
    (append acc (IfElse(e', i1', i2')), compiler)

  | While(e, b) -> 
    let e' = translate_expression e compiler in
    let (decl, b_extracted) = extract_declarations b in
    let (decl', compiler') = translate_instructions decl compiler in
    let (b_fun, compiler') = translate_instructions b_extracted compiler' in
    let acc = decl' @ acc in
    let acc = append acc (While(e', List.rev b_fun)) in
    (quit_block acc compiler', compiler)

  | For (init, c, it, b) ->
    (* init est déjà placé en-dehors de la boucle par IMP ; il n'y a aucun problème à le traduire directement
      Pas de déclarations dans it *)
    let (decl, b_extracted) = extract_declarations b in
    let init = init @ decl in
    let (init', compiler') = translate_instructions init compiler in
    let c' = translate_expression c compiler' in
    let (b', compiler') = translate_instructions b_extracted compiler' in
    (* pas de déclaration dans le bloc principal de b_extracted, donc on peut faire dans cet ordre *)
    let (it', compiler') = translate_instructions it compiler' in
    let acc = append acc (For(List.rev init', c', List.rev it', List.rev b')) in
    (quit_block acc compiler', compiler)

  | SetCall(d, op, e, args) ->
    let d' = translate_expression d compiler in
    let e' = translate_expression e compiler in
    let args' = List.fold_right (fun e acc -> 
      (translate_expression e compiler)::acc
    ) args [] in
    (append acc (SetCall(d', op, e', args')), compiler)

  | Call(e, args) ->
    let e' = translate_expression e compiler in
    let args' = List.fold_right (fun e acc -> 
      (translate_expression e compiler)::acc
    ) args [] in
    (append acc (Call(e', args')), compiler)

  | Declaration(id, e) ->
    printf "Adding variable %s\n" id.contents;
    declare_variable acc compiler id e
    
and translate_instructions is compiler =
  List.fold_left (fun (acc, compiler) i ->
    translate_instruction acc i compiler
  ) ([], compiler) is

let translate_function acc fct genv =
  let compiler = enter_function fct genv in
  let (block, _) = translate_instructions fct.block compiler in
  append acc {name = fct.name; block = List.rev block; params = fct.params}

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
      let e' = translate_expression e (global_compiler genv) in
      let init_acc = append init_acc (BinopAssign(Id v, Standard, e')) in
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

let write_assign file i =
  match i with
  | UnopAssign(e, op) ->
    write_art_left_expr file e;
    fprintf file "%s" (string_of_assign_unop op)
  | BinopAssign(d, op, e) ->
    write_art_left_expr file d;
    fprintf file " %s " (string_of_assign_binop op);
    write_art_right_expr file e
  | Declaration(v, e) ->
    fprintf file "var %s := " v.contents;
    write_art_right_expr file e
  | _ ->
    failwith "FUNTree.write_assign: not an assignment"

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
    write_art_right_expr file x;
    fprintf file ", ";
    write_args file (y::s)
  | x::[] ->
    write_art_right_expr file x
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
    fprintf file "print(";
    write_art_right_expr file e;
    fprintf file ");\n"
  | Return e ->
    fprintf file "return(";
    write_art_right_expr file e;
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
    write_art_right_expr file c;
    fprintf file ") {\n";
    write_instructions file t (depth + 1);
    write_tabs file depth;
    fprintf file "} else {\n";
    write_instructions file e (depth + 1);
    write_tabs file depth;
    fprintf file "}\n"
  | If(c, t) ->
    fprintf file "if (";
    write_art_right_expr file c;
    fprintf file ") {\n";
    write_instructions file t (depth + 1);
    write_tabs file depth;
    fprintf file "}\n"
  | While(c, b) ->
    fprintf file "while (";
    write_art_right_expr file c;
    fprintf file ") {\n";
    write_instructions file b (depth + 1);
    write_tabs file depth;
    fprintf file "}\n"
  | For(init, c, it, b) ->
    fprintf file "for (";
    write_assigns file init;
    fprintf file "; ";
    write_art_right_expr file c;
    fprintf file "; ";
    write_assigns file it;
    fprintf file ") {\n";
    write_instructions file b (depth + 1);
    write_tabs file depth;
    fprintf file "}\n";
  | Call(f, args) ->
    write_art_left_expr file f;
    fprintf file "(";
    write_args file args;
    fprintf file ");\n"
  | SetCall(d, op, f, args) ->
    write_art_left_expr file d;
    fprintf file " %s " (string_of_assign_binop op);
    write_art_left_expr file f;
    fprintf file "(";
    write_args file args;
    fprintf file ");\n"
  | Declaration(v, e) ->
    fprintf file "var %s := " v.contents;
    write_art_right_expr file e;
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
      write_art_right_expr file e;
      fprintf file ";\n"
  ) var.syntax_tree