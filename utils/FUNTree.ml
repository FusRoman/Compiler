open Printf
open Cycle
open Tagset
open ARTTree
open IMPTree
open CLLTree

(*
  - calcul des adresses (je sais pas ce que ça représente exactement)
  - passage par référence (pareil)

  Après ça VAR devrait être facile.
  Y aura le truc avec les déclarations dans le for qui devrait être chiant cela dit.
*)

exception UnboundValue of (string node) * (string node)

let fun_variables = add "function_result" cll_variables

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
  | For of fun_instrs * expression * fun_instrs * fun_instrs
  | SetCall of expression * assign_binop * expression * (expression list)
  | Call of expression * (expression list)

and fun_instrs = fun_instr list

type parameter = {
  name: string node;
  reference: bool;
}

type function_definition = {
  name: string node; 
  params: parameter list;
  block: fun_instrs
}

and fun_prog = (function_definition list) * datas

(*let check_fun_expression e fct lenv genv =
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
  check_expr e*)

(* 
  A l'instar de CLLTree.check_procedure, vérifie si :
    - les procédures finissent bien par exit ou return
    - les break et continue sont à l'intérieur de boucles
    - les expressions n'utilisent que des variables accessibles à ce moment
*)
let check_function fct genv =
  let rec compute_env env (params:parameter list) =
    match params with
    | [] -> env
    | x::s ->
      compute_env (add x.name.contents env) s
  in
  let env = compute_env genv fct.params in
  let rec check_expression e =
    match e with
    | Int _ | Bool _ -> ()
    | Id id ->
      if not (mem id.contents env) then
        raise (UnboundValue(fct.name, id))  
    | Unop(_, e) | LStar e ->
      check_expression e
    | Binop(e1, _, e2) ->
      check_expression e1;
      check_expression e2
  in
  let rec check_rec loop is =
    match is with
    | [] -> false
    | x::is' ->
      match x with
      | Exit ->
        ignore (check_rec loop is'); 
        true

      | Return e ->
        check_expression e;
        ignore (check_rec loop is');
        true

      | Nop -> 
        check_rec loop is'

      | Print e | UnopAssign(e, _) ->
        check_expression e;
        check_rec loop is'

      | SetCall(d, op, f, es) ->
        check_expression d;
        check_expression f;
        List.iter check_expression es;
        check_rec loop is'

      | Call(f, es) ->
        check_expression f;
        List.iter check_expression es;
        check_rec loop is'

      | BinopAssign(e1, _, e2) ->
        check_expression e1;
        check_expression e2;
        check_rec loop is'

      | If(c, b) ->
        check_expression c;
        ignore (check_rec loop b);
        check_rec loop is'

      | IfElse(c, t, e) ->
        check_expression c;
        (check_rec loop t && check_rec loop e) || check_rec loop is'

      | While(c, b) ->
        check_expression c;
        ignore (check_rec true b);
        check_rec loop is'

      | For(init, c, it, b) ->
        check_expression c;
        ignore (check_rec loop init);
        ignore (check_rec true it);
        ignore (check_rec true b);
        check_rec loop is'

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

let translate_expression e fct genv =
  let nb_params = List.length fct.params in

  (* Remplace les paramètres par le calcul de leur adresse *)
  let rec translate_id id (params: parameter list) i =
    match params with
    | [] ->
      if mem id.contents genv then
        Id id
      else
        (* Ne devrait jamais arriver puisqu'on vérifie avant *)
        raise (UnboundValue(fct.name, id))
    | x::s ->
      if x.name.contents = id.contents then
        (* TODO pas sûr que ça marche ici ! *)
        let base = Binop(LStar(Id frame_pointer), Add, Int(nb_params - i)) in
        if x.reference = true then LStar base else base
      else
        translate_id id s (i+1)
  in

  let rec translate_expr e =
    match e with
    | Int _ | Bool _ -> e
    | Id id -> 
      translate_id id fct.params 0
    | LStar e -> 
      LStar(translate_expr e)
    | Unop(op, e) ->
      Unop(op, translate_expr e)
    | Binop(e1, op, e2) ->
      Binop(translate_expr e1, op, translate_expr e2)
  in
  translate_expr e

(* Empile les arguments d'un appel sur la pile *)
let stack_args args fct genv acc =
  List.fold_left (fun acc e ->
    let e' = translate_expression e fct genv in
    let acc' = append acc (CLLTree.BinopAssign(LStar(Id stack_pointer), Standard, e')) in
    append acc' (UnopAssign(Id stack_pointer, Decr))
  ) acc args

let incr_sp a =
  BinopAssign(Id stack_pointer, AddAssign, Int a)

let rec translate_instruction i fct genv acc =
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
    let e'= translate_expression e fct genv in
    let acc = append acc (BinopAssign(Id function_result, Standard, e')) in
    append acc Return

  | Print e ->
    let e' = translate_expression e fct genv in
    append acc (Print e')

  | BinopAssign(e1, op, e2) -> 
    let e1' = translate_expression e1 fct genv in
    let e2' = translate_expression e2 fct genv in
    append acc (BinopAssign(e1', op, e2'))

  | UnopAssign(e, op) -> 
    let e' = translate_expression e fct genv in
    append acc (UnopAssign(e', op))

  | If(e, i) -> 
    let e' = translate_expression e fct genv in
    let i' = translate_instructions i fct genv in
    append acc (If(e', i'))

  | IfElse(e, i1, i2) -> 
    let e' = translate_expression e fct genv in
    let i1' = translate_instructions i1 fct genv in
    let i2' = translate_instructions i2 fct genv in
    append acc (IfElse(e', i1', i2'))

  | While(e, i) -> 
    let e' = translate_expression e fct genv in
    let i' = translate_instructions i fct genv in
    append acc (While(e', i'))

  | For (init, c, it, b) ->
    let init' = translate_instructions init fct genv in
    let c' = translate_expression c fct genv in
    let it' = translate_instructions it fct genv in
    let b' = translate_instructions b fct genv in
    append acc (For(init', c', it', b'))

  | SetCall(d, op, e, args) ->
    let d' = translate_expression d fct genv in
    let e' = translate_expression e fct genv in
    let acc = stack_args args fct genv acc in
    let acc = append acc (Call e') in
    append acc (BinopAssign(d', op, LStar(Id function_result)))

  | Call(e, args) ->
    let e' = translate_expression e fct genv in
    let acc = stack_args args fct genv acc in
    append acc (Call e')
    
and translate_instructions is fct genv =
  List.fold_left (fun acc i ->
    translate_instruction i fct genv acc
  ) empty_cycle is

let translate_function fct genv acc =
  check_function fct genv;
  let block = translate_instructions fct.block fct genv in
  append acc {name = fct.name; block}

let fun_to_cll fun_prog =
  let genv = union_duplicate fun_variables fun_prog.tag_set in
  let functions, data = fun_prog.syntax_tree in
  let data = append data (default_node ("function_result", 0)) in
  let procedures = List.fold_left (fun acc fct -> translate_function fct genv acc) empty_cycle functions in
  {syntax_tree = ProcedureDefinitionData(procedures, data); tag_set = genv}

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

and write_instructions file is depth =
  List.iter (fun i -> write_instruction file i depth) is

let write_function file fct =
  fprintf file "%s(" fct.name.contents;
  write_params file fct.params;
  fprintf file ") {\n";
  write_instructions file fct.block 1;
  fprintf file "}\n\n"

let write_fun file prog =
  let (fct, data) = prog.syntax_tree in
  List.iter (fun f -> write_function file f) fct;
  fprintf file ".data\n";
  write_art_data file data