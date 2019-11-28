open ARTTree
open IMPTree
open VARTree
open Cycle

module StringMap = Map.Make(String)

type env = _type StringMap.t
and record_env = (_type * int) StringMap.t

(*
   Type des types des expressions
   Alias peut être retourné par le parser lorsqu'il rencontre un nom
   alors qu'il attendait un type. N'ayant pas encore forcément parsé la 
   déclaration du type, il ne peut pas savoir exactement quoi renvoyer. 
   Il faudra, dans la première passe du compilateur, remplacer les Alias
   par leurs vrais types puis vérifier que les expressions sont bien typées.
   Rencontrer des Alias en-dehors de la première passe est une erreur
   et ne devrait jamais arriver.
*)
and _type =
  |TInt
  |TPointer of _type
  |TArray of _type
  |TFun of (_type list) * _type
  |TRecord of record_env
  |TTuple of int * _type
  |TAlias of string node

type record_field = string node * typ_expression
and typ_expression =
  | Int of int
  | Bool of bool
  | Id of string node
  | Deref of typ_expression
  | Unop of unop * typ_expression
  | Binop of typ_expression * binop * typ_expression
  | Call of typ_expression * (typ_expression list)
  | RecordAccess of typ_expression * string
  | NewRecord of _type * record_field list
  | ArrayAccess of typ_expression * typ_expression
  | NewArray of typ_expression * typ_expression

type variable = _type * string node * typ_expression
type declaration_type = string node * _type
(** Instructions en REC *)
type typ_instr =
  | Nop
  | Exit
  | Return of typ_expression
  | Break of unit node
  | Continue of unit node
  | Print of typ_expression
  | UnopAssign of typ_expression * assign_unop
  | BinopAssign of typ_expression * assign_binop * typ_expression
  | IfElse of typ_expression * typ_instrs * typ_instrs
  | If of typ_expression * typ_instrs
  | While of typ_expression * typ_instrs
  | For of typ_instrs * typ_expression * typ_instrs * typ_instrs
  | Call of typ_expression * (typ_expression list)
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

exception TypeError

let malloc = default_node "malloc"

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
      match check_expression genv type_env e with
      | TPointer t -> t
      | _ -> raise TypeError
    end
  |Unop (_, e) -> 
    begin
      match check_expression genv type_env e with
      |TInt -> TInt
      |_ -> raise TypeError
    end
  |Binop (e1, op, e2) ->
    begin
      match (check_expression genv type_env e1, check_expression genv type_env e2) with
      |TInt, TInt -> TInt
      |TPointer t, TInt |TInt, TPointer t when op = Add || op = Sub ->
        TPointer t
      |_,_ -> raise TypeError
    end
  |Call (s, expr_list) ->

    begin 
      match check_expression genv type_env s with
      | TPointer (TFun (type_params, return_type)) ->
        List.iter2 (
          fun expr param_type ->
            if not ((check_expression genv type_env expr) = param_type) then
              raise TypeError
        ) expr_list type_params;
        return_type
      |_ -> failwith "grosse erreur sur type des fonctions !!!"
    end
  |RecordAccess (expression, field) ->
    begin
      match check_expression genv type_env expression with
      |TPointer (TRecord env) ->
        begin
          match StringMap.find_opt field env with
          |None -> raise TypeError
          |Some (_type,_) -> _type
        end
      |_ -> raise TypeError
    end
  |ArrayAccess (name_tab, index) ->
    let type_index = check_expression genv type_env index in
    let type_name_tab = check_expression genv type_env name_tab in
    begin
      match type_index, type_name_tab with
      |TInt, TPointer (TArray _type_array) -> 
        _type_array
      |_ -> raise TypeError
    end
  |NewArray (size, array_elt) ->
    let type_size = check_expression genv type_env size in
    let type_array = check_expression genv type_env array_elt in
    begin
      match type_size, type_array with
      |TInt, t -> t
      |_, _ -> raise TypeError
    end
  |NewRecord (type_record,list_field) ->
    begin
      match type_record with
      |TRecord e ->  
        List.iter (
          fun (name_node, expr) ->
            match StringMap.find_opt name_node.contents e with
            |None -> raise TypeError
            |Some (_type,_) -> 
              if not ((check_expression genv type_env expr) = _type) then
                raise TypeError
        ) list_field;
        type_record
      |_ -> failwith "erreur sur les records"
    end

let rec check_instruction genv type_env f i =
  match i with
  |Nop | Exit |Break _ |Continue _ -> ()
  |Return e ->
    begin
      match f with
      |Some fonction ->
        let type_return = check_expression genv type_env e in
        if not (type_return = fonction.return_type) then
          raise TypeError
      |None -> 
        raise (SyntaxError ("no return find in this block function",0,0) )
    end
  |Print e ->
    if not ((check_expression genv type_env e) = TInt) then
      raise TypeError
  |UnopAssign (e, op) ->
    begin
      match check_expression genv type_env e with
      |TPointer TInt |TPointer (TPointer _) -> ()
      |_ -> raise TypeError
    end
  |BinopAssign (e1, op, e2) ->
    let t =
      match check_expression genv type_env e1 with
      |TPointer t -> t
      |_ -> raise TypeError
    in
    if not ((check_expression genv type_env e2) = t) then
      raise TypeError
  |IfElse (cond, block_if, block_else) ->
    begin
      match check_expression genv type_env cond with
      |TInt ->
        check_list_instruction genv type_env f block_if;
        check_list_instruction genv type_env f block_else
      |_ -> raise TypeError
    end
  |If (cond, block_if) ->
    begin
      match check_expression genv type_env cond with
      |TInt -> 
        check_list_instruction genv type_env f block_if
      |_ -> raise TypeError
    end
  |While (cond, block_while) ->
    begin
      match check_expression genv type_env cond with
      |TInt -> 
        check_list_instruction genv type_env f block_while
      |_ -> raise TypeError
    end
  |For (init, cond, iteration, block_for) ->
    begin
      match check_expression genv type_env cond with
      |TInt ->
        check_list_instruction genv type_env f init;
        check_list_instruction genv type_env f iteration;
        check_list_instruction genv type_env f block_for
      |_ -> raise TypeError
    end
  |Call (s, expr_list) ->
    begin 
      match check_expression genv type_env s with
      |TFun (type_params, return_type) ->
        List.iter2 (
          fun expr param_type ->
            if not ((check_expression genv type_env expr) = param_type) then
              raise TypeError
        ) expr_list type_params
      |_ -> failwith "grosse erreur sur type des fonctions !!!"
    end
  |Declaration (_type_var, name_node, expr) ->
    if not ((check_expression genv type_env expr) = _type_var) then
      raise TypeError

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

and check_global_declaration genv type_env l =
  List.iter (
    fun globals ->
      match globals with
      |Fun f -> check_list_instruction genv type_env (Some f) f.block
      |Var (_type_var, name_var, expr) -> 
        if not ((check_expression genv type_env expr) = _type_var) then
          raise TypeError
      |_ -> ()
  )

let make_var_node _type env =
  let max =
    StringMap.fold (fun k _ acc ->
        max acc (String.length k)
      ) env 0
  in
  let var = String.make max 'a' in
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
let rec translate_expression genv type_env e =
  match e with
  |RecordAccess (name_record, field) ->
    begin
      match check_expression genv type_env name_record with
      |TRecord env_record ->
        begin
          match StringMap.find_opt field env_record with
          |Some (_type, offset) ->
            let (init_struct, name_record, reeval_struct, new_env) = translate_expression genv type_env name_record in
            (init_struct, VARTree.Binop (name_record, Add, VARTree.Int offset), reeval_struct, new_env)
          |None -> raise TypeError
        end
      |_ -> raise TypeError
    end


  |ArrayAccess (name_array, offset_expr) ->
    (* Rajouter les ifs *)
    let (init_struct1,name_array, reeval_struct1, new_env1) = translate_expression genv type_env name_array in
    let (init_struct2,offset_expr, reeval_struct2, new_env2) = translate_expression new_env1 type_env offset_expr in
    (Cycle.extend init_struct1 init_struct2, VARTree.Binop (name_array, Add, offset_expr), Cycle.extend reeval_struct1 reeval_struct2, new_env2)


  |NewRecord (_type, record_field) ->
    let (new_map, record_tag) = make_var_node _type genv in

    (* Allocation mémoire de l'enregistrement *)
    let (call_to_malloc: var_expression) = VARTree.Call ( Id malloc, [Int (List.length record_field)] ) in
    let assign_return_malloc = VARTree.BinopAssign (Id record_tag, Standard, call_to_malloc) in


    (* Initialise chaque champ d'un enregistrement *)
    let (var_instr, new_env) = List.fold_left (
        fun (var_instr_acc, map_acc) (field_name, expr_field) ->
          let (instr_init_field:typ_instr) = BinopAssign (RecordAccess (Id record_tag, field_name.contents), Standard, expr_field) in
          let new_var_instr, new_map_acc = translate_instruction var_instr_acc map_acc type_env instr_init_field in
          (
            new_var_instr,
            new_map_acc
          )
      ) (empty_cycle, new_map) record_field in

    (
      prepend var_instr assign_return_malloc,
      VARTree.Id record_tag,
      var_instr,
      new_env
    )


  |NewArray (size_expr, elt_expr) ->
    let array_type = check_expression genv type_env elt_expr in
    let (new_map, array_tag) = make_var_node array_type genv in

    (* Allocation memoire du tableau *)
    let (init_size_expr, var_size_expr, reeval_size, map_size) = translate_expression new_map type_env size_expr in
    let (call_to_malloc: var_expression) = VARTree.Call (Id malloc, [var_size_expr]) in
    let assign_return_malloc = VARTree.BinopAssign (Id array_tag, Standard, call_to_malloc) in

    (* Initialisation du tableau avec un for *)
    let i = default_node "i" in
    let init_for = VARTree.BinopAssign (Id i, Standard, Int 0) in
    let cond_for = VARTree.Binop (Id i, Lt, var_size_expr) in
    let it_for = VARTree.UnopAssign (Id i, Incr) in
    (*
      Optimisation ici : on n'a pas besoin d'évaluer elt_expr à chaque fois
      En fait il ne faut pas l'évaluer, sinon ça n'a pas le comportement attendu
      Déclare une variable locale initialisée à elt_expr
      Puis utilise-la au lieu de elt_expr dans l'affectation 
    *)
    let (block_for: typ_instr) = BinopAssign (ArrayAccess (Id array_tag, Id i), Standard, elt_expr) in
    let (block_for, map_block_for) = translate_instruction empty_cycle map_size type_env block_for in
    let array_init_instr = VARTree.For ([init_for], cond_for, [it_for], to_list block_for) in

    let tmp_cycle = Cycle.append init_size_expr assign_return_malloc in
    let tmp_cycle = Cycle.append tmp_cycle array_init_instr in
    (
      tmp_cycle,
      VARTree.Id array_tag,
      Cycle.append reeval_size array_init_instr,
      map_size
    )
  |_ -> failwith "sdlfjbiods"

and translate_instruction instr_acc genv type_env i =
  match i with
  |BinopAssign (left_expr, assign_op, expression) ->
    let (init_l_e, var_l_e, reeval_l_e, new_env_l_e) = translate_expression genv type_env left_expr in
    let (init_expr, var_e, reeval_e, new_env_e) = translate_expression new_env_l_e type_env  expression in
    let acc = extend instr_acc init_l_e in
    let acc = extend acc init_expr in
    (
      append acc (VARTree.BinopAssign (var_l_e, assign_op, var_e)),
      new_env_e
    )
  |_ -> failwith "dfjnkj"

let typ_to_tpl typ_prog =
  ()