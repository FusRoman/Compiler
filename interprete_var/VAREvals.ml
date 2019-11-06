open VAR
open FUNInstr
open IMPExpr
open Op

exception SyntaxError of string

(* Environnements à compléter *)
type global_env = { functions: (string, function_definition) Hashtbl.t; variables_globale: (string, int) Hashtbl.t }
type local_env = { variables_locale: (string, int) Hashtbl.t; }

exception Return of int

let bool_of_int i =
  if i > 0 then
    true
  else
    false

let reach_value lenv genv name_var f =
  match Hashtbl.find_opt lenv.variables_locale name_var with
  |None -> 
    begin
      match Hashtbl.find_opt genv.variables_globale name_var with
      |None -> raise (SyntaxError ("This tag "^name_var^" was not declared before. find"))
      |Some n -> f n 
    end
  |Some n ->
    f n

let eval_program prog =
  let functions = Hashtbl.create 17 in
  let variables_globale = Hashtbl.create 17 in
  List.iter (fun fdef -> Hashtbl.add functions fdef.name fdef) prog.text;
  List.iter (fun (name_var, value_var) -> Hashtbl.add variables_globale name_var value_var) prog.globals;

  let rec eval_function fdef params genv =
    let lenv = { variables_locale = Hashtbl.create 17 } in
    List.iter (fun (name_var,value_var) -> Hashtbl.add lenv.variables_locale name_var value_var) fdef.locals;
    List.iter2 (fun name_params val_params -> Hashtbl.add lenv.variables_locale name_params val_params) fdef.parameters params;
    eval_sequence fdef.code lenv genv

  and eval_sequence s lenv genv =
    List.iter (fun i -> eval_instruction i lenv genv) s

  and eval_instruction i lenv genv = match i with
    | Exit -> exit 0
    | Print(e) ->
      Printf.printf "%c" ( char_of_int (eval_expression e lenv genv))
    | Write (l_e, e) ->
      let name_var = eval_id l_e lenv genv in
      let val_var = eval_expression e lenv genv in
      begin
        match Hashtbl.find_opt lenv.variables_locale name_var with
        |None -> 
          begin
            match Hashtbl.find_opt genv.variables_globale name_var with
            |None -> raise (SyntaxError ("This tag "^name_var^" was not declared before. write"))
            |Some n -> Hashtbl.replace genv.variables_globale name_var val_var
          end
        |Some n ->
          Hashtbl.replace lenv.variables_locale name_var val_var
      end
    | If (e, s1, s2) -> 
      let v = eval_expression e lenv genv in
      if bool_of_int v then
        eval_sequence s1 lenv genv
      else
        eval_sequence s2 lenv genv

    | While (e, s) ->
    (*
      if bool_of_int (eval_expression e lenv genv) then
        eval_sequence (s::[While(e,s)]) lenv genv *)
      let rec fun_while e =
        if bool_of_int (eval_expression e lenv genv) then
          begin
            eval_sequence s lenv genv;
            fun_while e
          end in
      fun_while e

    | Call (adress, name_function, params) ->

      let val_params = List.map (fun a -> eval_expression a lenv genv) params in
      let name_function = eval_id name_function lenv genv in
      begin
        match Hashtbl.find_opt functions name_function with
        |None -> raise (SyntaxError ("This function "^name_function^" was not declared before."))
        | Some f -> try
            eval_function f val_params genv
          with 
          | Return n ->
            let function_return = eval_id adress lenv genv in
            if Hashtbl.mem lenv.variables_locale function_return then
              Hashtbl.replace lenv.variables_locale function_return n
            else
              raise (SyntaxError ("This tag "^function_return^" was not declared before"))
      end
    | Return e ->
      begin
        raise (Return (eval_expression e lenv genv))
      end
    | Nop -> ()

  and eval_expression e lenv genv = match e with
    | Immediate(n) -> n
    | Name s -> reach_value lenv genv s (fun a -> a)
    | Deref e -> eval_expression e lenv genv
    | Binop (op, e1, e2) ->
      let v1 = eval_expression e1 lenv genv in
      let v2 = eval_expression e2 lenv genv in
      interpret_binop op v1 v2
    | Unop (op, e1) ->
      let v = eval_expression e1 lenv genv in
      interpret_unop op v

  and eval_id e lenv genv = match e with
    |Name s -> s
    |Deref e -> eval_id e lenv genv
    | _ -> raise (SyntaxError ("unmatched expression"))

  in
  let main = Hashtbl.find functions "main" in
  let sys_length = Array.length Sys.argv in
  if sys_length > 2 then
    begin
      let rec fill_list tab i l =
        if i = 2 then
          l
        else
          begin
            let n = int_of_string Sys.argv.(i-1) in
            fill_list tab (i-1) (n::l)
          end in
      let main_params = fill_list Sys.argv sys_length [] in
      eval_function main main_params { functions; variables_globale }
    end
  else
      eval_function main [] { functions; variables_globale }
