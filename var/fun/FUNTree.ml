open Printf
open Cycle
open Tagset
open ARTTree
open IMPTree
open CLLTree

exception UnboundValue of (string node) * (string node)

let fun_variables = add "function_result" cll_variables

let stack_pointer = default_node "stack_pointer"
let frame_pointer = default_node "frame_pointer"
let function_result = default_node "function_result"

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
  | TerminalCall of expression

and fun_instrs = fun_instr list

type parameter = {
  name: string node;
  reference: bool;
}

type 'a function_definition = {
  name: string node; 
  params: parameter list;
  block: 'a
}

type fun_function = fun_instrs function_definition

type fun_prog = (fun_function list) * ((string node * int) list)

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
      compute_env (add_duplicate x.name.contents env) s
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

      | Return e | TerminalCall e ->
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
        (* Semble marcher même quand les deux branches exécutent return ou exit...
          Voir test/fun/weird.fun *)
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
        let base = Binop(LStar(Id frame_pointer), Add, Int(nb_params - i)) in
        if x.reference then LStar base else base
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

let incr_sp acc a =
  if a <> 0 then
    append acc (CLLTree.BinopAssign(Id stack_pointer, AddAssign, Int a))
  else
    acc

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
    let acc = append acc (BinopAssign(d', op, LStar(Id function_result))) in
    incr_sp acc (List.length args)

  | Call(e, args) ->
    let e' = translate_expression e fct genv in
    let acc = stack_args args fct genv acc in
    let acc = append acc (Call e') in
    incr_sp acc (List.length args)

  | TerminalCall e ->
    append acc (TerminalCall e)
    
and translate_instructions is fct genv =
  List.fold_left (fun acc i ->
    translate_instruction i fct genv acc
  ) empty_cycle is

let translate_function fct genv main acc =
  check_function fct genv;
  if fct.name.contents = "main" then main := true;
  let block = translate_instructions fct.block fct genv in
  append acc {name = fct.name; block}

let fun_to_cll lib fun_prog =
  let genv = union_duplicate fun_variables fun_prog.tag_set in
  let functions, data = fun_prog.syntax_tree in
  let main = ref false in
  let procedures = 
    List.fold_left (fun acc fct -> 
      translate_function fct genv main acc
    ) empty_cycle functions 
  in
  let data_cycle = 
    List.fold_left (fun acc (t, v) -> 
      append acc {contents = (t.contents, v); line = t.line; column = t.column}
    ) empty_cycle data 
  in
  let data_cycle = append data_cycle (default_node ("function_result", 0)) in
  match (!main, lib) with
  | (true, false) | (false, true) ->
    {syntax_tree = (procedures, data_cycle); tag_set = genv}
  | (false, false) ->
    raise (SyntaxError("No 'main' function defined.", 0, 0))
  | (true, true) ->
    raise (SyntaxError("Libraries are not allowed to define a function 'main'.", 0, 0))

let write_tabs file depth =
  for i = 1 to depth do
    fprintf file "\t"
  done

(* On accepte aussi les appels de fonction pour simplifier VAR *)
let rec write_assign file i depth =
  match i with
  | Nop -> 
    fprintf file "nop"
  | Exit -> 
    fprintf file "exit"
  | Print e ->
    fprintf file "print(";
    write_art_right_expr file e;
    fprintf file ")"
  | Return e ->
    fprintf file "return ";
    write_art_right_expr file e
  | Break _ ->
    fprintf file "break"
  | UnopAssign(e, op) ->
    write_art_left_expr file e;
    fprintf file "%s" (string_of_assign_unop op)
  | BinopAssign(d, op, e) ->
    write_art_left_expr file d;
    fprintf file " %s " (string_of_assign_binop op);
    write_art_right_expr file e
  | Call(f, args) ->
    write_art_left_expr file f;
    fprintf file "(";
    write_args file args;
    fprintf file ")"
  | SetCall(d, op, f, args) ->
    write_art_left_expr file d;
    fprintf file " %s " (string_of_assign_binop op);
    write_art_left_expr file f;
    fprintf file "(";
    write_args file args;
    fprintf file ")"
  | TerminalCall f ->
    fprintf file "return ";
    write_art_left_expr file f;
    fprintf file "()"
  | _ ->
    (* control block *)
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

and write_args file args =
  match args with
  | x::y::s ->
    write_art_right_expr file x;
    fprintf file ", ";
    write_args file (y::s)
  | x::[] ->
    write_art_right_expr file x
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
    write_art_right_expr file e;
    fprintf file ");\n"
  | Return e ->
    fprintf file "return ";
    write_art_right_expr file e;
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
    write_assigns file init depth;
    fprintf file "; ";
    write_art_right_expr file c;
    fprintf file "; ";
    write_assigns file it depth;
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
  | TerminalCall f ->
    fprintf file "return ";
    write_art_left_expr file f;
    fprintf file "();\n"

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
  List.iter (fun (t, v) -> fprintf file "\t%s: %d\n" t.contents v) data