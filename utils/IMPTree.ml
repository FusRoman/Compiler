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
  | Print e -> to_cycle (Print(fst (optimize_expression e)))
  | Assign(l, e) -> to_cycle (Assign(l, fst (optimize_expression e)))
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