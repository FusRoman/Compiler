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
  | Assign of l_expr * expression
  | Nop
  | Exit
  | Jump of l_expr
  | JumpWhen of l_expr * expression
  | TagDeclaration of string node 

and data = (string * int) node

and datas = data Cycle.cycle

and expression =
  | Int of int
  | Bool of bool
  | LExpr of l_expr
  | StackPointer
  | Binop of expression * binop * expression
  | Unop of unop * expression
  | Address of l_expr

and l_expr = 
  | Id of string node
  | LStar of l_expr

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
    | LExpr _ | StackPointer | Address _ -> (e, None, 1)

    | Unop(op, e') ->
      begin
        let (sub, cte, nb_register) = opt_inner e' in
        match cte with
        | Some v ->
          let v' = unop_fun op v in
          (Int v', Some v', nb_register)
        | None -> 
          match sub with
          (* Branche de toutes les fonctions unaires qui ne sont pas leur propre inverse
             | Unop(ADDRESS, _) -> (Unop(op, sub), None) *)
          | Unop(op2, sub') when op2 = op -> (sub', None, nb_register) (* sub' ne peut pas être une constante *)
          | _ -> (Unop(op, sub), None, nb_register)
      end

    | Binop(e1, op, e2) ->
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
  in
  let (sub, v, _) = opt_inner e in
  (sub, v)

let opt_exp_sub e =
  fst (optimize_expression e)

let rec compile_l_expr file tag_set l_e =
  match l_e with
  | Id {contents = i; line = line; column = column} -> 
    if Tagset.mem i tag_set then
      begin
        fprintf file "%s\n" i;
        fprintf file "READ\n"
      end
    else
      raise (SyntaxError (("Tag '"^i^ "' was not declared before"), line, column))
  | LStar suite -> compile_l_expr file tag_set suite;
    fprintf file "READ\n"

let rec compile_l_expr_without_read file tag_set l_e = 
  match l_e with
  | Id {contents = i; line = line; column = column} -> 
    if Tagset.mem i tag_set then
      fprintf file "%s\n" i
    else
      raise (SyntaxError ("Tag '"^i^"' was not declared before", line, column))
  | LStar s_l_e -> compile_l_expr_without_read file tag_set s_l_e

let rec compile_exprs file tag_set e =
  match e with
  | Int i -> fprintf file "%s\n" (string_of_int i)
  | Bool b -> fprintf file "%s\n" (string_of_int (int_of_bool b))
  | Binop (e1,op,e2) -> compile_exprs file tag_set e1;
    compile_exprs file tag_set e2;
    fprintf file "%s\n" (string_of_binop op)
  | Unop (op,e) -> compile_exprs file tag_set e;
    fprintf file "%s\n" (string_of_unop op)
  | LExpr l_e -> compile_l_expr file tag_set l_e
  | StackPointer -> fprintf file "stack_pointer\n"
  | Address l_e -> compile_l_expr_without_read file tag_set l_e



let compile_instr file tag_set instr = 
  match instr with
  | Nop -> fprintf file "NOP\n"
  | Exit -> fprintf file "EXIT\n"
  | Print e -> compile_exprs file tag_set e;
    fprintf file "PRINT\n"
  | Jump l_e -> compile_l_expr_without_read file tag_set l_e;
    fprintf file "JUMP\n"
  | JumpWhen (l_e,e) -> compile_l_expr_without_read file tag_set l_e;
    compile_exprs file tag_set e;
    fprintf file "JUMPWHEN\n"
  | Assign (l_e,e) -> compile_l_expr_without_read file tag_set l_e;
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