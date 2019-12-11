open ARTTree
open IMPTree
open FUNTree
open VARTree
open Cycle

module StringMap = Map.Make(String)

type env = _type StringMap.t
and record_env = (_type * int) StringMap.t

and _type =
  |TInt
  |TPointer of _type 
  |TArray of _type
  |TFun of (_type list) * _type
  |TRecord of record_env
  |TTuple of _type list
  |TAlias of string node

type typ_binop = ARTBinop of binop | Seq | NSeq

type record_field = string node * typ_expression node
and typ_expression =
  | Int of int
  | Bool of bool
  | Id of string node
  | Deref of typ_expression node
  | Unop of unop * typ_expression node
  | Binop of typ_expression node * typ_binop * typ_expression node
  | Call of typ_expression node * (typ_expression node list)
  | RecordAccess of typ_expression node * string node
  | NewRecord of _type * record_field list
  | ArrayAccess of typ_expression node * typ_expression node
  | NewArray of typ_expression node * typ_expression node
  | TupleAccess of typ_expression node * int
  | NewTuple of typ_expression node list
  | InitArray of typ_expression node list

type variable = _type * string node * typ_expression node
type declaration_type = string node * _type
(** Instructions en TYP *)
type typ_instr =
  | Nop
  | Exit
  | Return of typ_expression node
  | Break of unit node
  | Continue of unit node
  | Print of typ_expression node
  | UnopAssign of typ_expression node * assign_unop
  | BinopAssign of typ_expression node * assign_binop * typ_expression node
  | IfElse of typ_expression node * typ_instrs * typ_instrs
  | If of typ_expression node * typ_instrs
  | While of typ_expression node * typ_instrs
  | For of typ_instrs * typ_expression node * typ_instrs * typ_instrs
  | Call of typ_expression node * (typ_expression node list)
  | Declaration of variable

and typ_instrs = typ_instr list

type parameter = {
  name : string node;
  reference : bool;
  params_type : _type
}

type 'a function_definition = {
  name : string node;
  params: parameter list;
  block: 'a;
  return_type : _type
}

type typ_function = typ_instrs function_definition

type global_declaration =
  | Fun of typ_function
  | Var of variable
  | Type of declaration_type

type typ_prog = global_declaration list


(*
  genv est une map permettant d'étiqueter chaque label avec son type.
  _type est une map de tous les types et alias de type déclaré dans le programme.
  tree est l'AST du programme ecrit en langage typ
*)
type 'a program = {
  genv: env;
  _type: env;
  tree: 'a
}

exception TypeError of string * int * int

let malloc = default_node "malloc"

let string_of_typ_binop op =
  match op with
  |ARTBinop op -> string_of_binop op
  |Seq -> "="
  |NSeq -> "<>"

let rec string_of_type t =
  let rec aux separator l =
    match l with
    |[] -> ""
    |[x] -> string_of_type x
    |x :: s ->
      (string_of_type x) ^ separator ^ (aux separator s) in
  match t with
  |TInt -> "int"
  |TPointer x ->
    "pointer of " ^ (string_of_type x)
  |TArray x ->
    "array of " ^ (string_of_type x)
  |TFun (params, return) ->
    "fun " ^ (aux "," params) ^ " -> " ^ (string_of_type return)
  |TRecord env ->
    let rec aux l = 
      match l with
      |[] -> ""
      |[(key, (_type, _))] -> key ^ "=" ^ (string_of_type _type)
      |(key, (_type, _)) :: s ->
        key ^ "=" ^(string_of_type _type) ^ ";" ^ (aux s) in
    "{" ^ (aux (StringMap.bindings env)) ^ "}"
  |TTuple t ->
    (aux "*" t)
  |TAlias s ->
    s.contents

let raise_type_error got expected line column =
  raise (TypeError(Printf.sprintf
                     "This expression has type %s, but an expression was expected of type %s."
                     (string_of_type got) expected,
                   line, column))

let rec find_type_alias type_env a =
  match a with
  |TInt -> a
  |TAlias alias ->
    begin
      match StringMap.find_opt alias.contents type_env with
      |None -> raise (TypeError 
                        ("type '" ^ alias.contents ^ "' was never declared before", 
                         alias.line, 
                         alias.column)
                     )
      |Some t ->
        find_type_alias type_env t
    end
  |TPointer t ->
    TPointer (find_type_alias type_env t)
  |TArray t ->
    TArray (find_type_alias type_env t)
  |TFun (args, return) ->
    TFun(List.map (find_type_alias type_env) args, find_type_alias type_env return)
  |TRecord renv ->
    TRecord (StringMap.map (fun (t, o) ->
        (find_type_alias type_env t, o)
      ) renv)
  |TTuple t ->
    TTuple(List.map (find_type_alias type_env) t)



let rec check_expression genv type_env e = 
  match e with
  |Int _ -> TInt
  |Bool _ -> TInt
  |Id s_node ->
    begin
      match StringMap.find_opt s_node.contents genv with
      |None ->
        raise (SyntaxError (
            (Printf.sprintf "tag %s undefined" s_node.contents),s_node.line,s_node.column 
          ))
      |Some t -> TPointer t
    end
  |Deref e ->
    begin
      let true_type = find_type_alias type_env (check_expression genv type_env e.contents) in
      match true_type with
      | TPointer t -> t
      | x -> 
        raise_type_error x "pointer" e.line e.column
    end
  |Unop (_, e) -> 
    begin
      let true_type = find_type_alias type_env (check_expression genv type_env e.contents) in
      match true_type with
      |TInt -> TInt
      |x -> 
        raise_type_error x "int" e.line e.column
    end
  |Binop (e1, op, e2) ->
    begin
      let true_type_e1 = find_type_alias type_env (check_expression genv  type_env e1.contents) in
      let true_type_e2 = find_type_alias type_env (check_expression genv type_env e2.contents) in
      match op with
      | Seq | NSeq | ARTBinop Neq | ARTBinop Eq ->
        TInt
      |other_op ->
        begin
          match (true_type_e1, true_type_e2) with
          |TInt, TInt -> TInt
          |TPointer t, TInt |TInt, TPointer t when other_op = ARTBinop Add || other_op = ARTBinop Sub ->
            TPointer t
          |x,y -> 
            let s_op = string_of_typ_binop other_op in
            raise (TypeError (
                Printf.sprintf "Cannot perform '%s' on values of type %s and %s" s_op (string_of_type x) (string_of_type y),
                e1.line,
                e1.column
              ))
        end
    end
  |Call (s, expr_list) ->
    begin
      let true_type = find_type_alias type_env (check_expression genv type_env s.contents) in
      match true_type with
      | TPointer (TFun (type_params, return_type)) ->
        begin try
            List.iter2 (
              fun expr param_type ->
                let actual_type = check_expression genv type_env expr.contents in
                let true_type = find_type_alias type_env actual_type in
                let true_type_param = find_type_alias type_env param_type in
                if not (true_type = true_type_param) then
                  raise_type_error actual_type (string_of_type param_type) expr.line expr.column
            ) expr_list type_params
          with Not_found ->
            raise (TypeError("Invalid number of arguments provided", s.line, s.column))
        end;
        return_type
      |x ->
        raise_type_error x "pointer of function" s.line s.column
    end
  |RecordAccess (expression, field) ->
    begin
      let true_type = find_type_alias type_env (check_expression genv type_env expression.contents) in
      match true_type with
      |TPointer (TRecord env) ->
        begin
          match StringMap.find_opt field.contents env with
          |None -> raise (TypeError 
                            ("this field" ^ field.contents ^ "was never declared before", 
                             field.line, 
                             field.column)
                         )
          |Some (_type,_) -> TPointer _type
        end
      |x ->
        raise_type_error x "pointer of record" expression.line expression.column
    end
  |ArrayAccess (name_tab, index) ->
    let type_index = find_type_alias type_env (check_expression genv type_env index.contents) in
    let type_name_tab = find_type_alias type_env (check_expression genv type_env name_tab.contents) in
    begin
      match type_index, type_name_tab with
      |TInt, TPointer (TArray _type_array) -> 
        TPointer _type_array
      |TInt,y ->
        raise_type_error y "pointer of array" name_tab.line name_tab.column
      |x, _ ->
        raise_type_error x "int" index.line index.column
    end
  |NewArray (size, array_elt) ->
    let type_size = find_type_alias type_env (check_expression genv type_env size.contents) in
    let type_array = find_type_alias type_env (check_expression genv type_env array_elt.contents) in
    begin
      match type_size, type_array with
      |TInt, t -> TPointer (TArray t)
      |x, _ ->
        raise_type_error x "int" size.line size.column
    end
  |NewRecord (type_record,list_field) ->
    begin
      let true_type_record = find_type_alias type_env type_record in
      match true_type_record with
      |TPointer (TRecord e) ->  
        List.iter (
          fun (name_node, expr) ->
            match StringMap.find_opt name_node.contents e with
            |None -> raise (TypeError 
                              ("This field " ^ name_node.contents ^ "was not declared in the 
                              record type declaration.", 
                               0, 
                               0)
                           )
            |Some (_type,_) ->
              let actual_type = check_expression genv type_env expr.contents in
              let true_type = find_type_alias type_env actual_type in
              let true_type_field = find_type_alias type_env _type in
              if not (true_type = true_type_field) then
                raise_type_error actual_type (string_of_type _type) expr.line expr.column
        ) list_field;
        type_record
      |_ ->
        raise_type_error type_record "pointer of record" 0 0
    end
  |InitArray expr_list ->
    let first_type = check_expression genv type_env (List.hd expr_list).contents in
    let first_true_type = find_type_alias type_env first_type in
    List.iter (
      fun expr ->
        let actual_type = check_expression genv type_env expr.contents in
        let true_type = find_type_alias type_env actual_type in
        if not (true_type = first_true_type) then
          raise_type_error actual_type (string_of_type first_type) expr.line expr.column
    ) (List.tl expr_list);
    TPointer (TArray first_true_type)
  |TupleAccess (tpl, i) ->
    let true_type = find_type_alias type_env (check_expression genv type_env tpl.contents) in
    begin
      match true_type with
      |TPointer TTuple t ->
        if i >= 0 || i < (List.length t) then
          TPointer (List.nth t i)
        else
          raise (TypeError
                   ("Tuple_Index_Out_Of_Bounds with index " ^ (string_of_int i), 
                    tpl.line, 
                    tpl.column)
                )
      |x ->
        raise_type_error x "pointer of tuple" tpl.line tpl.column
    end
  |NewTuple l ->
    let type_list = List.fold_right (
        fun e acc ->
          let true_type = find_type_alias type_env (check_expression genv type_env e.contents) in
          true_type::acc
      ) l [] in
    TPointer (TTuple type_list)


let rec check_instruction genv type_env f i =
  match i with
  |Nop | Exit | Break _ | Continue _ -> ()
  |Return e ->
    begin
      match f with
      |Some fonction ->
        let type_return = check_expression genv type_env e.contents in
        let true_type_return = find_type_alias type_env type_return in
        let true_type_fun_return = find_type_alias type_env fonction.return_type in
        if not (true_type_return = true_type_fun_return) then
          raise_type_error type_return (string_of_type fonction.return_type) e.line e.column
      |None -> 
        raise (SyntaxError ("No return found in this function",0,0) )
    end
  |Print e ->
    let actual_type = check_expression genv type_env e.contents in
    let true_type = find_type_alias type_env actual_type in
    if not (true_type = TInt) then
      raise_type_error actual_type "int" e.line e.column
  |UnopAssign (e, op) ->
    begin
      let actual_type = check_expression genv type_env e.contents in
      let true_type = find_type_alias type_env actual_type in
      match true_type with
      |TPointer TInt |TPointer (TPointer _) -> ()
      |x ->
        raise_type_error actual_type "pointer of int or pointer of pointer" e.line e.column
    end
  |BinopAssign (e1, op, e2) ->
    let t =
      let true_type = find_type_alias type_env (check_expression genv type_env e1.contents) in
      match true_type with
      |TPointer t -> t
      |x ->
        raise_type_error x "pointer" e1.line e1.column
    in
    let actual_type = check_expression genv type_env e2.contents in
    let true_type = find_type_alias type_env actual_type in
    let true_type_of_t = find_type_alias type_env t in
    if not (true_type = true_type_of_t) then
      raise_type_error actual_type (string_of_type t) e2.line e2.column
  |IfElse (cond, block_if, block_else) ->
    begin
      let true_type = find_type_alias type_env (check_expression genv type_env cond.contents) in
      match true_type with
      |TInt ->
        check_list_instruction genv type_env f block_if;
        check_list_instruction genv type_env f block_else
      |x -> 
        raise_type_error x "bool/int" cond.line cond.column
    end
  |If (cond, block_if) ->
    begin
      let true_type = find_type_alias type_env (check_expression genv type_env cond.contents) in
      match true_type with
      |TInt -> 
        check_list_instruction genv type_env f block_if
      |x ->
        raise_type_error x "bool/int" cond.line cond.column
    end
  |While (cond, block_while) ->
    begin
      let true_type = find_type_alias type_env (check_expression genv type_env cond.contents) in
      match true_type with
      |TInt -> 
        check_list_instruction genv type_env f block_while
      |x ->
        raise_type_error x "bool/int" cond.line cond.column
    end
  |For (init, cond, iteration, block_for) ->
    begin
      let local_env = check_list_instruction_with_return_new_env genv type_env f init in
      let true_type = find_type_alias type_env (check_expression local_env type_env cond.contents) in
      match true_type with
      |TInt ->
        check_list_instruction local_env type_env f iteration;
        check_list_instruction local_env type_env f block_for
      |x ->
        raise_type_error x "bool/int" cond.line cond.column
    end
  |Call (s, expr_list) ->
    begin
      let true_type = find_type_alias type_env (check_expression genv type_env s.contents) in
      match true_type with
      |TPointer (TFun (type_params, return_type)) ->
        begin try 
            List.iter2 (
              fun expr param_type ->
                let actual_type = check_expression genv type_env expr.contents in
                let true_type = find_type_alias type_env actual_type in
                let true_type_param = find_type_alias type_env param_type in
                if not (true_type = true_type_param) then
                  raise_type_error actual_type (string_of_type param_type) expr.line expr.column
            ) expr_list type_params
          with Not_found ->
            raise (TypeError("Invalid number of arguments provided", s.line, s.column))
        end
      |x ->
        raise_type_error x "pointer of function" s.line s.column
    end
  |Declaration (_type_var, name_node, expr) ->
    let actual_type = check_expression genv type_env expr.contents in
    let true_type = find_type_alias type_env actual_type in
    let true_type2 = find_type_alias type_env _type_var in
    if not (true_type = true_type2) then
      raise_type_error actual_type (string_of_type _type_var) expr.line expr.column

and check_list_instruction genv type_env f l =
  match l with
  |[] -> ()
  | x :: s ->
    begin
      match x with
      |Declaration (_type_var, name_node, expr) ->
        check_instruction genv type_env f x;
        check_list_instruction (StringMap.add name_node.contents _type_var genv) type_env f s
      |_ -> 
        check_instruction genv type_env f x;
        check_list_instruction genv type_env f s
    end

and check_list_instruction_with_return_new_env genv type_env f l =
  match l with
  |[] -> genv
  | x :: s ->
    begin
      match x with
      |Declaration (_type_var, name_node, expr) ->
        check_instruction genv type_env f x;
        check_list_instruction (StringMap.add name_node.contents _type_var genv) type_env f s;
        (StringMap.add name_node.contents _type_var genv)
      |_ -> 
        check_instruction genv type_env f x;
        check_list_instruction genv type_env f s;
        genv
    end

and check_global_declaration genv type_env l =
  List.iter (
    fun globals ->
      match globals with
      |Fun f -> 
        let env_with_param = List.fold_left (
            fun acc (p: parameter) ->
              StringMap.add p.name.contents p.params_type acc
          ) genv f.params in
        check_list_instruction env_with_param type_env (Some f) f.block
      |Var (_type_var, name_var, expr) ->
        (* Tout ça c'est pas à jour du tout, faudra virer *)
        begin
          match _type_var with
          |TInt ->
            let true_type = find_type_alias type_env _type_var in
            let type_expr = find_type_alias type_env (check_expression genv type_env expr.contents) in
            if not (type_expr = true_type) then
              raise (TypeError 
                       ("This expression has type " ^ (string_of_type true_type) ^
                        "\nbut an expression was expected of type " ^ (string_of_type _type_var), 
                        expr.line, 
                        expr.column)
                    )
          |x -> raise (TypeError 
                         ("This expression has type " ^ (string_of_type x) ^
                          "\nbut an expression was expected of type int, only int are allowed for the globals variable declaration", 
                          0, 
                          0)
                      )
        end
      |_ -> ()
  ) l

let make_var_node _type env =
  let max =
    StringMap.fold (fun k _ acc ->
        max acc (String.length k)
      ) env 0
  in
  let var = String.make (max + 1) 'a' in
  (StringMap.add var _type env, ARTTree.default_node var)

(*
    Optimisation
    Pour l'instant la compilation fait un truc un peu dégueu du genre
    var r := {a := e1; b := e2};
    ->
    var tmp := malloc(2);
    init e1;
    *(tmp+0) := e1';
    init e2;
    *(tmp+1) := e2';
    var r := tmp;

    On peut virer la dernière affectation en utilisant r à la place de tmp.
    Pour ce faire, sortir la traduction de NewArray, NewRecord et NewTuple de translate_expression et 
    les appeler dans translate_expression avec un argument en plus, une option d'expression.
    Si None -> faire comme déjà fait, si Some -> optimisation.
    Quand on détecte BinopAssign(d, op, NewTruc), on peut appeler avec Some d.
*)


let rec structural_equality genv type_env e1 e2 test_array from_array =
  match (check_expression genv type_env e1.contents), (check_expression genv type_env e2.contents) with
  | TInt, TInt ->
    let (init1, var_e1, reeval1, env1) = translate_expression genv type_env e1.contents in
    let (init2, var_e2, reeval2, env2) = translate_expression env1 type_env e2.contents in
    let test_array = match from_array with
      |Some t -> t
      |None -> false in
    if test_array then
      (
        Cycle.extend init1 init2,
        VARTree.Binop (var_e1, Neq, var_e2),
        Cycle.extend reeval1 reeval2,
        env2,
        false
      )
    else
      (
        Cycle.extend init1 init2,
        VARTree.Binop (var_e1, Eq, var_e2),
        Cycle.extend reeval1 reeval2,
        env2,
        false
      )

  |TPointer (TTuple l1), TPointer (TTuple l2) ->
    let (init,var_equal ,reeval, env, t, _) = List.fold_left (
        fun (acc_init,var_acc ,acc_reeval, acc_env, t , it) _ ->
          let access_t1 = TupleAccess (e1, it) in
          let access_t2 = TupleAccess (e2, it) in
          let (init, var, reeval, new_env,t) = structural_equality acc_env type_env (default_node access_t1) (default_node access_t2) test_array from_array in
          (
            extend acc_init init,
            append var_acc var,
            extend acc_reeval reeval,
            new_env,
            t,
            it + 1
          )
      ) (empty_cycle,empty_cycle ,empty_cycle, genv, false, 0) l1 in
    let (head, tail) = take var_equal in
    let test_tuple = 
      match from_array with
      |Some t -> t
      |None -> false 
    in
    let cond_expr = iter tail (
        fun simple_equal acc ->
          if test_tuple then
            VARTree.Binop (acc, Or, simple_equal)
          else
            VARTree.Binop (acc, And, simple_equal)
      ) head in
    (
      init,
      cond_expr,
      reeval,
      env,
      false
    )

  |TPointer (TArray t1), TPointer (TArray t2) ->
    let (init1, var_e1, reeval1, env1) = translate_expression genv type_env e1.contents in
    let (init2, var_e2, reeval2, env2) = translate_expression env1 type_env e2.contents in
    let (new_env, test_tag) = make_var_node TInt env2 in
    let (new_env,i) = make_var_node TInt new_env in
    let var_test = VARTree.Declaration (test_tag, Int 1) in
    let var_i = VARTree.Declaration (i, Int 0) in
    let access_size_array1 = VARTree.Binop (var_e1, Sub, Int 1) in
    let access_size_array2 = VARTree.Binop (var_e2, Sub, Int 1) in
    let test_size = VARTree.Binop (Deref access_size_array1, Neq, Deref access_size_array2) in
    let test_tag_param = 
      match test_array with
      |None -> test_tag
      |Some test_param -> test_param 
    in
    let block_if = VARTree.BinopAssign (Id test_tag_param, Standard, Int 0) in
    let access_array1 = ArrayAccess (e1, default_node (Deref (default_node (Id i)))) in
    let access_array2 = ArrayAccess (e2, default_node (Deref (default_node (Id i)))) in
    let (init_test, test_if_block_for, reeval_test, env_test, to_tab) = structural_equality new_env type_env (default_node access_array1) (default_node access_array2) (Some test_tag_param) (Some true) in
    let block_for = VARTree.If (test_if_block_for, [block_if]) in

    let block_for = if to_tab then init_test else (append init_test block_for) in
    let cond_for = VARTree.Binop (Deref (Id i), Lt, Deref access_size_array1) in
    let it_for = VARTree.UnopAssign (Id i, Incr) in
    let block_else = VARTree.For ([var_i], cond_for, [it_for], to_list block_for) in
    let block_struct_equal = VARTree.IfElse (test_size, [block_if], [block_else]) in
    let tmp_cycle = append empty_cycle var_test in
    (
      append tmp_cycle block_struct_equal,
      Deref (Id test_tag_param),
      reeval_test,
      env_test,
      true
    )

  |TPointer (TRecord env1), TPointer (TRecord env2) ->
    let access_all_field_r1 = map (from_list (StringMap.bindings env1)) (
        fun (s, _) ->
          default_node (RecordAccess (e1, default_node s))
      ) in
    let access_all_field_r2 = map (from_list (StringMap.bindings env2)) (
        fun (s, _) ->
          default_node (RecordAccess (e2, default_node s))
      ) in
    let (a_r1, tail1) = take access_all_field_r1 in
    let (a_r2, tail2) = take access_all_field_r2 in
    let (first_init, first_var_expr, first_reeval, first_env,test) = structural_equality genv type_env a_r1 a_r2 test_array from_array in
    iter2 tail1 tail2 (
      fun a_r1 a_r2 (init_acc, var_acc, reeval_acc, env_acc, test_acc)   ->
        let (init_r1, var_r1, reeval_r1, env_r1,t) = structural_equality env_acc type_env a_r1 a_r2 test_array from_array in
        let test = 
          match from_array with
          |Some t -> t
          |None -> false 
        in
        if test then
          (
            extend init_acc init_r1,
            VARTree.Binop (var_acc, Or, var_r1),
            extend reeval_acc reeval_r1,
            env_r1,
            t
          )
        else
          (
            extend init_acc init_r1,
            VARTree.Binop (var_acc, And, var_r1),
            extend reeval_acc reeval_r1,
            env_r1,
            t
          )
    ) (first_init, first_var_expr, first_reeval, first_env,test)  

  |TPointer a, TPointer b ->
    structural_equality genv type_env (default_node (Deref e1)) (default_node (Deref e2)) test_array from_array
  |_,_ -> failwith "erreur égalité de structure"


and translate_expression genv type_env e =
  match e with
  |TupleAccess (tpl, i) ->
    let (init_struct, name_tuple, reeval_struct, new_env) = translate_expression genv type_env tpl.contents in
    (
      init_struct,
      VARTree.Binop (name_tuple, Add, Int i),
      reeval_struct,
      new_env
    )

  |RecordAccess (name_record, field) ->
    begin
      match find_type_alias type_env (check_expression genv type_env name_record.contents) with
      |TPointer (TRecord env_record) ->
        begin
          match StringMap.find_opt field.contents env_record with
          |Some (_type, offset) ->
            let (init_struct, name_record, reeval_struct, new_env) = translate_expression genv type_env name_record.contents in
            (init_struct, VARTree.Binop (name_record, Add, VARTree.Int offset), reeval_struct, new_env)
          |None -> raise (TypeError 
                            ("This field " ^ field.contents ^ "was not declared before.",
                             field.line, 
                             field.column)
                         )
        end
      |x ->
        raise_type_error x "pointer of record" name_record.line name_record.column
    end


  |ArrayAccess (name_array, offset_expr) ->
    (* Rajouter les ifs *)
    let (init_struct1,name_array, reeval_struct1, new_env1) = translate_expression genv type_env name_array.contents in
    let (init_struct2,offset_expr, reeval_struct2, new_env2) = translate_expression new_env1 type_env offset_expr.contents in
    let tab_size = VARTree.Binop (name_array, Sub, Int 1) in
    let condition_size = VARTree.Binop (Deref tab_size, Le, offset_expr) in
    let test_size = VARTree.If (condition_size, [Exit]) in
    let cycle_init = Cycle.extend init_struct1 init_struct2 in
    (Cycle.append cycle_init test_size, VARTree.Binop (name_array, Add, offset_expr), Cycle.extend reeval_struct1 reeval_struct2, new_env2)


  |NewRecord (_type, record_field) ->

    let (new_map, record_tag) = make_var_node _type genv in

    (* Allocation mémoire de l'enregistrement *)
    let (call_to_malloc: var_expression) = VARTree.Call ( Id malloc, [Int (List.length record_field)] ) in
    let assign_return_malloc = VARTree.Declaration (record_tag, call_to_malloc) in


    (* Initialise chaque champ d'un enregistrement *)
    let (var_instr, new_env) = List.fold_left (
        fun (var_instr_acc, map_acc) (field_name, expr_field) ->
          let access_record = ARTTree.default_node (RecordAccess (
              default_node (Deref (ARTTree.default_node (Id record_tag))), field_name)) in
          let (instr_init_field:typ_instr) = BinopAssign (access_record, Standard, expr_field) in
          let new_var_instr, new_map_acc = translate_instruction var_instr_acc map_acc type_env instr_init_field in
          (
            new_var_instr,
            new_map_acc
          )
      ) (empty_cycle, new_map) record_field in

    (
      prepend var_instr assign_return_malloc,
      Deref (Id record_tag),
      var_instr,
      new_env
    )


  |NewArray (size_expr, elt_expr) ->
    let array_type = check_expression genv type_env elt_expr.contents in
    let (new_map, array_tag) = make_var_node array_type genv in

    (* On crée une variable temporaire contenant l'evaluation de l'expression de la taille du 
       tableau pour éviter que cette expression ne se fasse évaluer a chaque tour de boucle 
       et également dans l'appel du malloc*)

    let (init_size_expr, var_size_expr, reeval_size, map_size) = translate_expression new_map type_env size_expr.contents in
    let (new_map_with_tag_size, tmpsize_tag) = make_var_node TInt map_size in
    let assign_size = VARTree.Declaration (tmpsize_tag, var_size_expr) in



    (* Allocation memoire du tableau *)
    let var_size_expr_plus_un = VARTree.Binop (Deref (Id tmpsize_tag), Add, Int 1) in
    let (call_to_malloc: var_expression) = VARTree.Call (Id malloc, [var_size_expr_plus_un]) in
    let assign_return_malloc = VARTree.Declaration (array_tag, call_to_malloc) in

    let assign_array_size = VARTree.BinopAssign ((Deref (Id array_tag)) ,Standard, Deref (Id tmpsize_tag)) in


    (* Initialisation du tableau avec un for *)
    let i = default_node "i" in
    let init_for = VARTree.Declaration (i, Int 1) in
    let cond_for = VARTree.Binop (Deref (Id i), Le, Deref (Id tmpsize_tag)) in
    let it_for = VARTree.UnopAssign (Id i, Incr) in

    (*
      Creation d'une variable temporaire a l'exterieur du block for pour evaluer l'expression 
      des elements du tableau qu'une seul fois.
    *)
    let (init_elt_expr, var_elt_expr, reeval_elt_expr, map_elt_expr) = translate_expression new_map_with_tag_size type_env elt_expr.contents in
    let (new_map2, tmpvar_tag) = make_var_node (check_expression map_size type_env elt_expr.contents) map_elt_expr in
    let tmp_variable = VARTree.Declaration (tmpvar_tag, var_elt_expr) in

    let access_array = VARTree.Binop (Deref (Id array_tag), Add, Deref (Id i)) in
    let block_for = VARTree.BinopAssign (access_array, Standard, Deref (Id tmpvar_tag)) in



    let array_init_instr = VARTree.For ([init_for], cond_for, [it_for], [block_for]) in

    let tmp_cycle = Cycle.append init_size_expr assign_size in
    let tmp_cycle = Cycle.append tmp_cycle assign_return_malloc in
    let tmp_cycle = Cycle.append tmp_cycle assign_array_size in
    let tmp_cycle = Cycle.append tmp_cycle tmp_variable in
    let tmp_cycle = Cycle.append tmp_cycle array_init_instr in
    let reeval_cycle = Cycle.extend reeval_size reeval_elt_expr in
    let reeval_cycle = Cycle.append reeval_cycle tmp_variable in
    let reeval_cycle = Cycle.append reeval_cycle array_init_instr in
    (
      tmp_cycle,
      Binop (Deref (Id array_tag), Add, Int 1),
      reeval_cycle,
      map_elt_expr
    )

  |InitArray list_expr ->
    let array_type = check_expression genv type_env (List.hd list_expr).contents in
    let (new_map, array_tag) = make_var_node (TPointer (TArray array_type)) genv in

    let array_size = VARTree.Int ((List.length list_expr)+1) in
    let (call_to_malloc: var_expression) = VARTree.Call (Id malloc, [array_size]) in
    let assign_return_malloc = VARTree.Declaration (array_tag, call_to_malloc) in

    let access_array_size = VARTree.Binop (Deref (Id array_tag) ,Add, Int 0) in
    let assign_array_size = VARTree.BinopAssign (access_array_size ,Standard, Int (List.length list_expr)) in

    let (init, var_instr_cycle, reeval, new_env,_) = List.fold_left (
        fun (init_acc, var_acc, reeval_acc, env_acc, cpt) e ->
          let (init, var_expr, reeval, new_env) = translate_expression env_acc type_env e.contents in

          let access_array = VARTree.Binop (Deref (Id array_tag) ,Add, Int cpt) in
          let assign_array_elt = VARTree.BinopAssign (access_array ,Standard, var_expr) in

          (
            extend init_acc init,
            append var_acc assign_array_elt,
            extend reeval_acc reeval,
            new_env,
            cpt + 1
          )
      ) (empty_cycle, empty_cycle, empty_cycle, new_map, 1) list_expr in
    let tmp_cycle = append empty_cycle assign_return_malloc in
    let tmp_cycle = append tmp_cycle assign_array_size in
    let tmp_cycle = extend tmp_cycle init in
    (
      extend tmp_cycle var_instr_cycle,
      Binop (Deref (Id array_tag), Add, Int 1),
      reeval,
      new_env
    )


  |NewTuple l ->
    let type_list = List.fold_right (
        fun e acc ->
          let true_type = find_type_alias type_env (check_expression genv type_env e.contents) in
          true_type::acc
      ) l [] in
    let (new_map, tuple_tag) = make_var_node (TPointer (TTuple type_list)) genv in

    let tuple_size = VARTree.Int (List.length l) in
    let (call_to_malloc: var_expression) = VARTree.Call (Id malloc, [tuple_size]) in
    let assign_return_malloc = VARTree.Declaration (tuple_tag, call_to_malloc) in

    let (init, reeval, return_env, _) = List.fold_left (
        fun (init_acc, reeval_acc, map_acc, index) e ->
          let (init, var_expr, reeval, s_map) = translate_expression map_acc type_env e.contents in

          let access_array = VARTree.Binop (Deref (Id tuple_tag) ,Add, Int index) in
          let assign_expr_to_tuple = VARTree.BinopAssign (access_array, Standard, var_expr) in
          let tmp_cycle = extend init_acc init in
          let tmp_cycle = append tmp_cycle assign_expr_to_tuple in
          (
            tmp_cycle,
            extend reeval_acc reeval,
            s_map,
            index + 1
          )

      ) (empty_cycle, empty_cycle, new_map, 0) l in

    let tmp_cycle = append empty_cycle assign_return_malloc in
    let tmp_cycle = extend tmp_cycle init in

    (
      tmp_cycle,
      Deref (Id tuple_tag),
      reeval,
      return_env
    )


  |Int x ->
    (
      empty_cycle,
      Int x,
      empty_cycle,
      genv
    )
  |Bool b ->
    (
      empty_cycle,
      Bool b,
      empty_cycle,
      genv
    )
  |Id s ->
    (
      empty_cycle,
      Id s,
      empty_cycle,
      genv
    )
  |Deref e ->
    let (init, var_e, reeval, new_env) = translate_expression genv type_env e.contents in
    (
      init,
      Deref var_e,
      reeval,
      new_env
    )
  |Unop (op, e) ->
    let (init, var_e, reeval, new_env) = translate_expression genv type_env e.contents in
    (
      init,
      Unop (op, var_e),
      reeval,
      new_env
    )
  |Binop (e1, op, e2) ->
    begin
      match op with
      |ARTBinop op ->
        let (init1, var_e1, reeval1, new_env1) = translate_expression genv type_env e1.contents in
        let (init2, var_e2, reeval2, new_env2) = translate_expression new_env1 type_env e2.contents in
        (
          Cycle.extend init1 init2,
          Binop (var_e1, op, var_e2),
          Cycle.extend reeval1 reeval2,
          new_env2
        )
      |Seq ->
        let (init, v, reeval, env, _) = structural_equality genv type_env e1 e2 None None in
        (init, v, reeval, env)
      |NSeq ->
        let (init, v, reeval, env, _) = structural_equality genv type_env e1 e2 None None in
        (init, Unop(Not, v), reeval, env)
    end
  |Call (function_name, function_block) ->
    let (init_fun, var_fun, reeval_fun, env_fun) = translate_expression genv type_env function_name.contents in
    let (init_param, var_param, reeval_param, env_param) = List.fold_left (
        fun (acc_init, acc_var_param, acc_reeval, acc_env) typ_e ->
          let (init_p, var_p, reeval_p, env_p) = translate_expression acc_env type_env typ_e.contents in
          (
            Cycle.extend acc_init init_p,
            Cycle.append acc_var_param var_p,
            Cycle.extend acc_reeval reeval_p,
            env_p
          )
      ) (init_fun, empty_cycle, reeval_fun, env_fun) function_block in
    (
      Cycle.extend init_fun init_param,
      Call (var_fun, to_list var_param),
      Cycle.extend reeval_fun reeval_param,
      env_param
    )

and translate_instruction instr_acc genv type_env i =
  match i with
  |Nop ->
    (
      append instr_acc VARTree.Nop,
      genv
    )
  |Exit ->
    (
      append instr_acc VARTree.Exit,
      genv
    )
  |Break s ->
    (
      append instr_acc (VARTree.Break s),
      genv
    )
  |Continue s ->
    (
      append instr_acc (VARTree.Continue s),
      genv
    )
  |Return e ->
    let (init_e, var_e, reeval_e, new_env) = translate_expression genv type_env e.contents in
    let acc = extend instr_acc init_e in
    (
      append acc (Return var_e),
      new_env
    )
  |Print e ->
    let (init_e, var_e, reeval_e, new_env) = translate_expression genv type_env e.contents in
    let acc = extend instr_acc init_e in
    (
      append acc (Print var_e),
      new_env
    )
  |UnopAssign (e, op) ->
    let (init_e, var_e, reeval_e, new_env) = translate_expression genv type_env e.contents in
    let acc = extend instr_acc init_e in
    (
      append acc (VARTree.UnopAssign (var_e, op)),
      new_env
    )
  |BinopAssign (left_expr, assign_op, expression) ->
    let (init_l_e, var_l_e, reeval_l_e, new_env_l_e) = translate_expression genv type_env left_expr.contents in
    let (init_expr, var_e, reeval_e, new_env_e) = translate_expression new_env_l_e type_env  expression.contents in
    let acc = extend instr_acc init_l_e in
    let acc = extend acc init_expr in
    (
      append acc (VARTree.BinopAssign (var_l_e, assign_op, var_e)),
      new_env_e
    )
  |If (cond, block_if) ->
    let (init_cond, var_cond, reeval_cond, cond_env) = translate_expression genv type_env cond.contents in
    let (var_instr_cycle, block_env) = translate_instructions cond_env type_env block_if in
    let acc = extend instr_acc init_cond in
    (
      append acc (If (var_cond, (to_list var_instr_cycle))),
      cond_env
    )
  |IfElse (cond, block_if, block_else) ->
    let (init_cond, var_cond, reeval_cond, cond_env) = translate_expression genv type_env cond.contents in
    let (var_block_if ,env_block_if) = translate_instructions cond_env type_env block_if in
    let (var_block_else, env_block_else) = translate_instructions cond_env type_env block_else in
    let acc = extend instr_acc init_cond in
    (
      append acc (IfElse (var_cond, (to_list var_block_if), (to_list var_block_else))),
      cond_env
    )
  |Declaration (_type, s, e) ->
    let (init_e, var_e, reeval_e, env_expr) = translate_expression genv type_env e.contents in
    let acc = extend instr_acc init_e in
    (
      append acc (Declaration (s, var_e)),
      StringMap.add s.contents _type env_expr
    )
  |Call (function_name, function_param) ->
    let (init_fun, var_fun, reeval_fun, env_fun) = translate_expression genv type_env function_name.contents in
    let (init_param, var_param, reeval_param, env_param) = List.fold_left (
        fun (acc_init, acc_var, acc_reeval, acc_env) param ->
          let (init_p, var_p, reeval_p, env_p) = translate_expression acc_env type_env param.contents in
          (
            extend acc_init init_p,
            append acc_var var_p,
            extend acc_reeval reeval_p,
            env_p
          )
      ) (init_fun, empty_cycle, reeval_fun, env_fun) function_param in
    let acc = extend instr_acc init_fun in
    (
      append acc (Call (var_fun, (to_list var_param))),
      env_param
    )
  |While (cond, block) ->
    let (init_cond, var_cond, reeval_cond, env_cond) = translate_expression genv type_env cond.contents in
    let (var_block, env_block) = translate_instructions env_cond type_env block in
    let acc = extend instr_acc init_cond in
    let var_block = extend var_block reeval_cond in
    (
      append acc (While (var_cond, (to_list var_block))),
      env_cond
    )
  |For (init, cond, it, block) ->
    let (var_init, env_init) = translate_instructions genv type_env init in
    let (init_cond, var_cond, reeval_cond, env_cond) = translate_expression env_init type_env cond.contents in
    let (var_block, env_block) = translate_instructions env_cond type_env block in
    let (var_it, env_it) = translate_instructions env_block type_env it in
    let init = extend var_init init_cond in
    let var_it = extend var_it reeval_cond in
    (
      append instr_acc (For ((to_list init), var_cond, (to_list var_it), (to_list var_block))),
      genv
    )

and translate_instructions genv type_env is =
  List.fold_left (
    fun (acc_instr ,acc_env) i ->
      translate_instruction acc_instr acc_env type_env i
  ) (empty_cycle, genv) is

let translate_globals genv type_env g =
  let rec inter acc g =
    match g with
    |[] -> acc
    |x :: s ->
      begin
        match x with
        |Fun f -> 
          let env_with_param = List.fold_left (
              fun acc (p: parameter) ->
                StringMap.add p.name.contents p.params_type acc
            ) genv f.params in
          let (block_var_fun, new_env) = translate_instructions env_with_param type_env f.block in
          let new_params_list = List.map (
              fun (p: parameter) ->
                { reference = p.reference; name = p.name}
            ) f.params in
          let var_globals = VARTree.Fun {
              name = f.name; block = to_list block_var_fun; params = new_params_list
            } in
          inter (var_globals::acc) s
        |Var (_, name, t_expr) ->
          let init, var_expr, reeval, new_env = translate_expression genv type_env t_expr.contents in
          let var_globals = VARTree.Var (name, var_expr) in
          inter (var_globals::acc) s
        |Type t -> 
          inter acc s
      end in
  inter [] g


let typ_to_tpl typ_prog =
  check_global_declaration typ_prog.genv typ_prog._type typ_prog.tree;
  let syntax_tree = translate_globals typ_prog.genv typ_prog._type typ_prog.tree in
  let tag_set = List.fold_left (
      fun acc (name_global, _) ->
        Tagset.add name_global acc
    ) Tagset.empty (StringMap.bindings typ_prog.genv) in
  {syntax_tree; tag_set}