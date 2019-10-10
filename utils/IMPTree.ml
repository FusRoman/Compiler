open ARTTree

type imp_instr =
  | Nop
  | Exit
  | Break
  | Continue
  | Print of expression
  | Goto of l_expr
  | Assign of l_expr * expression
  | IfElse of expression * imp_instrs * imp_instrs
  | If of expression * imp_instrs
  | While of expression * imp_instrs
  | TagDeclaration of string node

(** Analogue à son équivalent ART *)
and imp_instrs = imp_instr Cycle.cycle

(** Analogue à son équivalent ART *)
and imp_prog =
  | TextData of imp_instrs * datas
  | Text of imp_instrs

(*
  Optimise une expression :
  - tant qu'il s'agit de calculs de constantes, simplifie
  - annule les double NOT / MINUS / CPL
  Les expressions en IMP ont le même type que celles en ART, donc il n'y a pas de traduction à faire.
*)
let optimize_expression e =
  let rec opt_inner e =
    match e with
    | Int v -> (e, Some v)
    | Bool b -> (e, Some (Arith.int_of_bool b))
    | LExpr _ | StackPointer -> (e, None)

    | Unop(op, e') ->
    begin
      let (sub, cte) = opt_inner e' in
      match cte with
      | Some v ->
        let v' = unop_fun op v in
        (Int v', Some v')
      | None -> 
        match sub with
        (* Branche de toutes les fonctions unaires qui ne sont pas leur propre inverse
        | Unop(ADDRESS, _) -> (Unop(op, sub), None) *)
        | Unop(op2, sub') when op2 = op -> (sub', None) (* sub' ne peut pas être une constante *)
        | _ -> (Unop(op, sub), None)
    end

    | Binop(e1, op, e2) ->
      let (sub1, cte1) = opt_inner e1 in
      let (sub2, cte2) = opt_inner e2 in
      let default = (Binop(sub1, op, sub2), None) in
      match (op, cte1, cte2) with
      | (And, Some v, _) | (And, _, Some v) -> 
        if not (Arith.bool_of_int v) then
          (Int Arith.false_int, Some Arith.false_int)
        else
          default

      (*| (Or, Some v, _) | (Or, _, Some v) -> 
        if Arith.bool_of_int v then
          (Int Arith.true_int, Some Arith.true_int)
        else
          default*)
      (* Vu qu'il s'agit d'un OR binaire, on ne peut pas garantir que le résultat sera 1 
        si l'une des opérandes est interprétée comme true *)

      | (_, Some v1, Some v2) ->
        let v = binop_fun op v1 v2 in
        (Int v, Some v)

      | _ -> default
  in
  opt_inner e

let opt_exp_sub e = 
  fst (optimize_expression e)

let to_cycle i =
  Cycle.from_elt i

(*
  Traduit une condition et tente de l'optimiser.
  La condition et les deux branches sont d'abord optimisées, puis :
  - si aucune branche n'est réellement intéressante, la condition n'est tout simplement pas compilée.
  - s'il n'y a qu'une seule branche intéressante, on regarde en fonction de la condition optimisée
    si on doit réellement la mettre et si oui, on la rend telle quelle, sans condition
  - si les deux branches sont intéressantes, on cherche à en virer une via la condition simplifiée
    et si c'est impossible, on rend la conditionnelle avec ses sous-parties simplifiées.
*)
let rec translate_condition c bthen belse =
  let no_else c branch =
    let (e, v) = optimize_expression c in
    match v with
    | Some t when Arith.bool_of_int t -> branch
    | Some f -> Cycle.empty_cycle
    | None -> to_cycle (If(e, branch))
  in
  (* Ce serait plus efficace de commencer par les conditions *)
  let opt_then = translate_instructions bthen in
  let opt_else = translate_instructions belse in
  match (opt_then = Cycle.empty_cycle, opt_else = Cycle.empty_cycle) with
  | (true, true) -> Cycle.empty_cycle
  | (_, true) -> no_else c opt_then
  | (true, _) -> no_else (Unop(Not, c)) opt_else
  | _ ->
    let (e, v) = optimize_expression c in
    match v with
    | Some t when Arith.bool_of_int t -> opt_then
    | Some f -> opt_else
    | _ -> to_cycle (IfElse(e, opt_then, opt_else))

(*
  Simplifie une instruction.
  Les conditionnelles, les boucles et les expressions sont les principales cibles de cette optimisation.
*)
and translate_instruction i =
  match i with
  | Nop | Exit | Break | Continue | Goto _ -> to_cycle i
  | Print e -> to_cycle (Print(opt_exp_sub e))
  | Assign(l, e) -> to_cycle (Assign(l, opt_exp_sub e))
  | IfElse(c, bthen, belse) -> translate_condition c bthen belse
  | If(c, bthen) -> translate_condition c bthen Cycle.empty_cycle
  | TagDeclaration t -> to_cycle i
  | While(e, body) ->
    let (e, v) = optimize_expression e in
    match v with
    | Some f when not (Arith.bool_of_int f) -> Cycle.empty_cycle
    (*| Some t when t = Arith.true_int -> Warning ? *)
    | _ ->
      let opt_body = translate_instructions body in
      if opt_body = Cycle.empty_cycle then
        Cycle.empty_cycle
      else
        to_cycle (While(e, opt_body))

and translate_instructions l =
  let rec tr_inner acc l =
    if l = Cycle.empty_cycle then
      acc
    else
      let (i, s) = Cycle.take l in
      let i' = translate_instruction i in
      tr_inner (Cycle.extend acc i') s
  in
  tr_inner Cycle.empty_cycle l

let imp_to_art imp =
  {
    tag_set = imp.tag_set;
    syntax_tree =
      match imp.syntax_tree with
      | Text l -> 
        (*let optl = optimize_instructions l in*)
        Prog Cycle.empty_cycle
      | TextData(l, d) -> 
        (*let optl = optimize_instructions l in*)
        ProgData(Cycle.empty_cycle, d)
  }