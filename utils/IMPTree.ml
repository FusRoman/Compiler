open ARTTree

type imp_instr =
  | Nop
  | Exit
  | Break of unit node
  | Continue of unit node
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
  Optimise une condition.
  La condition et les deux branches sont d'abord optimisées, puis :
  - si aucune branche n'est réellement intéressante, la condition n'est tout simplement pas compilée.
  - s'il n'y a qu'une seule branche intéressante, on regarde en fonction de la condition optimisée
    si on doit réellement la mettre et si oui, on la rend telle quelle, sans condition
  - si les deux branches sont intéressantes, on cherche à en virer une via la condition simplifiée
    et si c'est impossible, on rend la conditionnelle avec ses sous-parties simplifiées.

  On prépare également un peu l'étape de compilation vers ART.
  L'instruction if (cond) instr_then else instr_else est traduite par :
  jump else when (not cond);
    instr_then
  else:
    instr_else
  C'est donc not cond qu'on optimise et pas juste cond.
  Dans l'étape de compilation, la négation aura déjà été intégrée.
*)
let rec optimize_condition c bthen belse =
  let no_else c branch decl =
    let (e, v) = optimize_expression (Unop(Not, c)) in
    match v with
    | Some t when (not decl) && (Arith.bool_of_int t) -> 
      (* La condition est fausse (on a inversé) : le code ne sera jamais exécuté *)
      (Cycle.empty_cycle, false)
    | Some f -> 
      (* La condition est vraie, donc le code sera toujours exécutée, pas besoin de condition *)
      (branch, decl)
    | None -> (to_cycle (If(e, branch)), decl)
  in
  let (opt_then, then_decl) = optimize_instructions bthen in
  let (opt_else, else_decl) = optimize_instructions belse in
  let then_skippable = (opt_then = Cycle.empty_cycle) && (not then_decl) in
  let else_skippable = (opt_else = Cycle.empty_cycle) && (not else_decl) in
  match (then_skippable, else_skippable) with
  | (true, true) -> (Cycle.empty_cycle, false)
  | (false, true) -> no_else c opt_then then_decl
  | (true, false) -> no_else (Unop(Not, c)) opt_else else_decl
  | _ ->
    let (e, v) = optimize_expression (Unop(Not, c)) in
    match v with
    | Some t when (not then_decl) && (Arith.bool_of_int t) -> 
      (* La condition est toujours fausse *)
      (opt_else, else_decl)
    | Some f when not else_decl -> 
      (* La condition est toujours vraie *)
      (opt_then, then_decl)
    | _ -> 
      (to_cycle (IfElse(e, opt_then, opt_else)), else_decl || then_decl)

(*
  Simplifie une instruction.
  Les conditionnelles, les boucles et les expressions sont les principales cibles de cette optimisation.
*)
and optimize_instruction i =
  match i with
  | Nop | Exit | Break _ | Continue _ | Goto _ -> (to_cycle i, false)
  | Print e -> 
    (to_cycle (Print(fst (optimize_expression e))), false)
  | Assign(l, e) -> 
    (to_cycle (Assign(l, fst (optimize_expression e))), false)
  | IfElse(c, bthen, belse) ->
    optimize_condition c bthen belse
  | If(c, bthen) -> 
    optimize_condition c bthen Cycle.empty_cycle
  | TagDeclaration t -> 
    (to_cycle i, true)
  | While(e, body) ->
    let opt_body, decl = optimize_instructions body in
    let c, v = optimize_expression (Unop(Not, e)) in
    match v with
    | Some f when (not decl) && (Arith.bool_of_int f) ->
      (* On ne peut éviter la boucle que si sa condition est toujours fausse et qu'elle ne déclare pas d'étiquette *)
      (Cycle.empty_cycle, false)
    | Some f when decl && (Arith.bool_of_int f) ->
      (* Si la condition est fausse mais qu'il y a des étiquettes, on peut quand même virer la boucle *)
      (opt_body, decl)     
    | _ ->
      (to_cycle (While(c, opt_body)), decl)

and optimize_instructions l =
  let rec tr_inner acc l decl =
    if l = Cycle.empty_cycle then
      (acc, decl)
    else
      let (i, s) = Cycle.take l in
      let (i', i_decl) = optimize_instruction i in
      tr_inner (Cycle.extend acc i') s (decl || i_decl)
  in
  tr_inner Cycle.empty_cycle l false

(* Donne un générateur de tag non déjà déclarés (sauf si on utilise deux tag maker en même temps, évidemment) *)
let meta_tag_maker imp =
  let cpt = ref 0 in
  let max_length = Tagset.fold (fun t acc -> max acc (String.length t)) imp.tag_set 1 in
  let base = String.make max_length 'a' in
  (fun () -> 
    let t = base ^ (string_of_int !cpt) in
    incr cpt;
    {line = -1; column = -1; contents = t}
  )

let get_ends_loop t _break _continue =
  match (_break, _continue) with
  | (Some _end, Some _begin) ->
    (_end.contents, _begin.contents)
  | (None, None) ->
    raise (SyntaxError("break and continue are only allowed in a loop", t.line, t.column))
  | _ ->
    failwith "get_ends_loop: contradictory information from _break and _continue"

let rec translate_instruction i maker _break _continue acc =
  let append = Cycle.append acc in
  match i with
  | Nop -> append ARTTree.Nop
  | Exit -> append Exit
  | Goto t -> append (Jump t)
  | Print e -> append (Print e)
  | Assign(l, e) -> append (Assign(l, e))
  | TagDeclaration t -> append (TagDeclaration t)

  | Break t ->
    let _end = fst (get_ends_loop t _break _continue) in
    append (Jump (Id {line = t.line; column = t.column; contents = _end}))

  | Continue t ->
    let _begin = snd (get_ends_loop t _break _continue) in
    append (Jump (Id {line = t.line; column = t.column; contents = _begin}))

  | If(c, bthen) ->
    (* Durant la phase d'optimisation, la condition a été inversée *)
    let _end = maker () in
    let acc2 = append (JumpWhen(Id _end, c)) in
    let acc3 = translate_instructions bthen maker _break _continue acc2 in
    Cycle.append acc3 (ARTTree.TagDeclaration _end)

  | IfElse(c, bthen, belse) ->
    let _else = maker () in
    let _end = maker () in
    let acc2 = append (JumpWhen(Id _else, c)) in
    let acc3 = translate_instructions bthen maker _break _continue acc2 in
    let acc4 = Cycle.append acc3 (Jump (Id _end)) in
    let acc5 = Cycle.append acc4 (TagDeclaration _else) in
    let acc6 = translate_instructions belse maker _break _continue acc5 in
    Cycle.append acc6 (TagDeclaration _end)

  | While(c, body) ->
    let _end = maker () in
    let _begin = maker () in
    let acc2 = append (TagDeclaration _begin) in
    let acc3 = Cycle.append acc2 (JumpWhen(Id _end, c)) in
    let acc4 = translate_instructions body maker (Some _end) (Some _begin) acc3 in
    let acc5 = Cycle.append acc4 (Jump (Id _begin)) in
    Cycle.append acc5 (TagDeclaration _end)

and translate_instructions is maker _break _continue acc =
  if is = Cycle.empty_cycle then
    acc
  else
    let (i, s) = Cycle.take is in
    let acc' = translate_instruction i maker _break _continue acc in
    translate_instructions s maker _break _continue acc'

let imp_to_art imp =
  (* Phase d'optimisation *)
  let opt_instrs, data = 
    match imp.syntax_tree with
    | Text i -> 
      (fst (optimize_instructions i), Cycle.empty_cycle)
    | TextData(i, d) ->
      (fst (optimize_instructions i), d)
  in
  (* Phase de compilation vers ART *)
  let tag_maker = meta_tag_maker imp in
  let art_instrs = translate_instructions opt_instrs tag_maker None None Cycle.empty_cycle in
  {syntax_tree = ProgData(art_instrs, data); tag_set = imp.tag_set}