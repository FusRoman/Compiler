open Printf
open Tagset
open Arith

exception SyntaxError of string * int * int

type binop = 
  | Add
  | Sub 
  | Mult
  | Div
  | And
  | Or
  | Rem
  | Lt
  | Le
  | Gt
  | Ge
  | Eq
  | Neq

type unop =
  | Minus
  | Not
  | Cpl

type 'a node = {
  line: int;
  column: int;
  contents: 'a
}

type art_prog =
  | ProgData of art_instrs * datas
  | Prog of art_instrs

and art_instrs = art_instr Cycle.cycle

and art_instr = 
  | Print of expression
  | Assign of expression * expression
  | Nop
  | Exit
  | Jump of expression
  | JumpWhen of expression * expression
  | TagDeclaration of string node 

and data = (string * int) node

and datas = data Cycle.cycle

and expression =
  | Int of int
  | Bool of bool
  | StackPointer
  | Binop of expression * binop * expression
  | Unop of unop * expression
  | Address of string node
  | Id of string node
  | LStar of expression

type 'a compiler_type = {
  tag_set: Tagset.t;
  syntax_tree: 'a
}

let string_of_binop op = 
  match op with
  | Add -> "ADD"
  | Sub -> "SUB"
  | Mult -> "MULT"
  | Div -> "DIV"
  | Rem -> "REM"
  | Eq -> "EQ"
  | Neq -> "NEQ"
  | Lt -> "LT"
  | Le -> "LE"
  | Gt -> "GT"
  | Ge -> "GE"
  | And -> "AND"
  | Or -> "OR"

let string_of_unop op =
  match op with
  | Minus -> "MINUS"
  | Not -> "NOT"
  | Cpl -> "CPL"

let binop_fun op =
  match op with
  | Add -> Arith.add
  | Sub -> Arith.sub
  | Mult -> Arith.multiply
  | Div -> Arith.divise
  | Rem -> Arith.modulo
  | Eq -> Arith.equal
  | Neq -> Arith.different
  | Lt -> Arith.lt
  | Le -> Arith.le
  | Gt -> Arith.gt
  | Ge -> Arith.ge
  | And -> Arith.bit_and
  | Or -> Arith.bit_or

let unop_fun op =
  match op with
  | Minus -> Arith.minus
  | Not -> Arith.anot
  | Cpl -> Arith.cpl

let optimize_expression e =
  let rec opt_inner e =
    match e with
    | Int v -> (e, Some v, 1)
    | Bool b -> (e, Some (Arith.int_of_bool b), 1)
    | StackPointer | Address _ -> (e, None, 1)

    | Unop(op, e') ->
      begin
        let (sub, cte, nb_register) = opt_inner e' in
        match cte with
        | Some v ->
          let v' = unop_fun op v in
          (Int v', Some v', nb_register)
        | None ->
          let result r = (r, None, nb_register) in
          match op with
          | Not ->
          begin
            match sub with
            | Unop(Not, Unop(Not, sub')) -> result (Unop(Not, sub'))
            | Binop(e1, Eq, e2) -> result (Binop(e1, Neq, e2))
            | Binop(e1, Neq, e2) -> result (Binop(e1, Eq, e2))
            | Binop(e1, Lt, e2) -> result (Binop(e1, Ge, e2))
            | Binop(e1, Ge, e2) -> result (Binop(e1, Lt, e2))
            | Binop(e1, Le, e2) -> result (Binop(e1, Gt, e2))
            | Binop(e1, Gt, e2) -> result (Binop(e1, Le, e2))
            | _ -> result (Unop(op, sub))
          end
          | _ ->
            match sub with
            | Unop(op2, sub') -> result sub' (* Cas des fonctions qui sont leur inverse appliquées 2 fois *)
            | _ -> result (Unop(op, sub))
      end

    | Binop(e1, op, e2) ->
    begin
      let (sub1, cte1, nb_register_e1) = opt_inner e1 in
      let (sub2, cte2, nb_register_e2) = opt_inner e2 in
      let e1, e2, nb_register_e =
        match op with
        | Add | Mult when nb_register_e1 < nb_register_e2 ->
          ( e2, e1, max nb_register_e2 (nb_register_e1 + 1) )
        | _ ->
          ( e1, e2, max nb_register_e1 (nb_register_e2 + 1) )
      in
      let default = (Binop(sub1, op, sub2), None, nb_register_e) in
      match (op, cte1, cte2) with
      | (And, Some v, _) | (And, _, Some v) -> 
        if not (Arith.bool_of_int v) then
          (Int Arith.false_int, Some Arith.false_int, nb_register_e1 + nb_register_e2)
        else
          default
      | (_, Some v1, Some v2) ->
        let v = binop_fun op v1 v2 in
        (Int v, Some v, nb_register_e1 + nb_register_e2)
      | _ -> default
    end
    | Id i -> (Id i, None, 1)
    | LStar e ->
      let (opt, _, nb_register) = opt_inner e in
      (LStar opt, None, nb_register)
  in

  let (sub, v, _) = opt_inner e in
  (sub, v)

let opt_exp_sub e =
  fst (optimize_expression e)

let rec compile_exprs file tag_set e =
  match e with
  | Int i -> fprintf file "%s\n" (string_of_int i)
  | Bool b -> fprintf file "%s\n" (string_of_int (int_of_bool b))
  | Binop (e1,op,e2) -> compile_exprs file tag_set e1;
    compile_exprs file tag_set e2;
    fprintf file "%s\n" (string_of_binop op)
  | Unop (op,e) -> compile_exprs file tag_set e;
    fprintf file "%s\n" (string_of_unop op)
  | StackPointer -> fprintf file "stack_pointer\n"

  | Address {contents = i; line = line; column = column} -> 
    if Tagset.mem i tag_set then
      fprintf file "%s\n" i
    else
      raise (SyntaxError ("Tag '"^i^"' was not declared before", line, column))

  | Id {contents = i; line = line; column = column} -> 
    if Tagset.mem i tag_set then
      fprintf file "%s\n" i
    else
      raise (SyntaxError (("Tag '"^i^ "' was not declared before"), line, column))

  | LStar e -> 
    compile_exprs file tag_set e;
    fprintf file "READ\n"

let compile_instr file tag_set instr = 
  match instr with
  | Nop -> fprintf file "NOP\n"
  | Exit -> fprintf file "EXIT\n"
  | Print e -> compile_exprs file tag_set e;
    fprintf file "PRINT\n"
  | Jump l_e ->
    compile_exprs file tag_set l_e;
    fprintf file "JUMP\n"
  | JumpWhen (l_e, e) ->
    compile_exprs file tag_set l_e;
    compile_exprs file tag_set e;
    fprintf file "JUMPWHEN\n"
  | Assign (l_e,e) ->
    compile_exprs file tag_set l_e;
    compile_exprs file tag_set e;
    fprintf file "WRITE\n"
  | TagDeclaration t ->
    fprintf file "%s:\n" t.contents

let rec compile_instrs file tag_set instrs = 
  if instrs <> Cycle.empty_cycle then
    begin
      let (i, s) = Cycle.take instrs in
      compile_instr file tag_set i;
      compile_instrs file tag_set s
    end

let compile_data file data =
  match data with
  | {contents = (s, i); line = _; column = _} ->
    fprintf file "%s: %d\n" s i

let rec compile_datas file datas =
  if datas <> Cycle.empty_cycle then
    begin
      let (d, s) = Cycle.take datas in
      compile_data file d;
      compile_datas file s
    end

let rec compile file tag_set tree = 
  match tree with
  | Prog is -> fprintf file ".text\n";
    compile_instrs file tag_set is
  | ProgData (is,ds) -> fprintf file ".text\n";
    compile_instrs file tag_set is;
    fprintf file ".data\n";
    compile_datas file ds


let string_of_binop_direct op = 
  match op with
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Rem -> "%"
  | Eq -> "=="
  | Neq -> "!="
  | Lt -> "<"
  | Le -> "<="
  | Gt -> ">"
  | Ge -> ">="
  | And -> "&&"
  | Or -> "||"

let string_of_unop_direct op =
  match op with
  | Minus -> "-"
  | Not -> "!"
  | Cpl -> "~"

let rec write_art_expr file e =
  match e with
  | Int i -> fprintf file "%d" i
  | Bool b -> fprintf file "%b" b
  | Binop (e1,op,e2) -> 
    fprintf file "(";
    write_art_expr file e1;
    fprintf file " %s " (string_of_binop_direct op);
    write_art_expr file e2;
    fprintf file ")"
  | Unop (op,e) -> 
    fprintf file "(";
    fprintf file "%s" (string_of_unop_direct op); 
    write_art_expr file e;
    fprintf file ")"
  | StackPointer -> 
    fprintf file "stack_pointer"
  | Address {contents = i; line = line; column = column} -> 
    fprintf file "&%s" i
  | Id {contents = i; line = line; column = column} -> 
    fprintf file "%s" i
  | LStar e -> 
    fprintf file "*";
    write_art_expr file e

let write_art_data = compile_datas

let direct_print_instr file tag_set instr = 
  match instr with
  | Nop -> fprintf file "nop;\n"
  | Exit -> fprintf file "exit;\n"
  | Print e -> 
    fprintf file "print("; 
    write_art_expr file e; 
    fprintf file ");\n"
  | Jump l_e -> 
    fprintf file "jump "; 
    write_art_expr file l_e;
    fprintf file ";\n"
  | JumpWhen (l_e,e) -> 
    fprintf file "jump "; 
    write_art_expr file l_e; 
    fprintf file " when "; 
    write_art_expr file e;
    fprintf file ";\n"
  | Assign (l_e,e) -> 
    write_art_expr file l_e; 
    fprintf file " := "; 
    write_art_expr file e;
    fprintf file ";\n"
  | TagDeclaration t ->
    fprintf file "%s:\n" t.contents

let rec direct_print_instrs file tag_set instrs = 
  if instrs <> Cycle.empty_cycle then
  begin
    let (i, s) = Cycle.take instrs in
    direct_print_instr file tag_set i;
    direct_print_instrs file tag_set s
  end

let rec write_art file tag_set tree = 
  match tree with
  | Prog is -> fprintf file ".text\n";
    direct_print_instrs file tag_set is
  | ProgData (is,ds) -> fprintf file ".text\n";
    direct_print_instrs file tag_set is;
    fprintf file ".data\n";
    compile_datas file ds
