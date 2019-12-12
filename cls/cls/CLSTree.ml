open ARTTree
open VARTree
open TPLTree
open IMPTree
open FUNTree
open TYPTree

module StringMap = Map.Make(String)

type env = _type StringMap.t
and record_env = (cls_type * int) StringMap.t

and cls_type = _type

type record_field = string node * cls_expression node
and cls_expression = typ_expression

type cls_variable = cls_type * string node * cls_expression node
type type_declaration = string node * cls_type

type cls_instr = typ_instr
type cls_instrs = cls_instr list

type cls_function = cls_instrs TYPTree.function_definition

type class_attribute = {
  name: string node;
  _type: cls_type;
  _public: bool;
  _static: bool;
  init: cls_expression;
}

type class_method = {
  _public: bool;
  _static: bool;
  _fun: cls_function;
}

type class_field =
  | Method of class_method
  | Attribute of class_attribute

type cls_class = {
  fields: class_field list;
  parent: string node option
}

type cls_declarable_type =
  | TRegular of cls_type
  | TExtended of cls_type node * (string node * cls_type) list
  | TClass of cls_class

type global_declaration =
  | Fun of cls_function
  | Var of cls_variable
  | Type of string node * cls_declarable_type

type cls_prog = global_declaration list

type cls_program = {
  genv: cls_type StringMap.t;
  types: cls_declarable_type list
  tree: 'a
}

let get_name_static _module name =
  name.contents ^ "@" ^ _module

let get_name_non_static _module _class name =
  name.contents ^ "@" ^ _class ^ "@" ^ _module

(* Pas de support des librairies pour l'instant *)
let _module = "main"

type class_env = {
  _private: record_env;
  _public: record_env;
  static_private: record_env;
  static_public: record_env;
  count: int;
  static_count: int;
}

let create_static_fun_type f =
  TFun(List.map (fun p -> p.params_type) f.params, f.return_type)

let create_fun_type f this =
  TFun((TAlias this.contents)::(List.map (fun p -> p.params_type) f.params), f.return_type)

(* Type intermédiaire des types *)
type cls_inter_type = 
  | CLSRegular of cls_type
  | CLSClass of class_env

type cls_type_env = cls_inter_type StringMap.t

let master_env = {
  _private = StringMap.empty;
  _public = StringMap.empty;
  static_private = StringMap.empty;
  static_public = StringMap.empty;
  static_count = 1; (* classe mère; aucune méthode définie par défaut (pour l'instant) *)
  count = 1; (* Pointeur vers le descripteur *)
}

let merge_record_env r1 r2 =
  StringMap.fold (fun f (t, o) acc ->
    StringMap.add f (t, o) acc 
  ) r1 r2

(* Fusionne les environnements de deux classes en supposant que child hérite de parent *)
let merge_class_env parent child = {
    _private = merge_record_env parent._private child._private;
    _public = merge_record_env parent._public child._public;
    static_private = merge_record_env parent.static_private child.static_private;
    static_public = merge_record_env parent.static_public child.static_public;
    static_count = child.static_count;
    count = child.count;
  }

(* Renvoie le nom et le type visible du descripteur de classe *)
let get_descriptor name c =
  (name, TPointer (TRecord c.static_public))

let find_field c name =
  let get r =
    StringMap.find_opt name.contents r
  in
  (* peu importe la valeur de init ici, elle n'a pas de sens *)
  let init = Null in 
  match get c._public with
  | Some _type -> {name; _type; _public = true; _static = false; init}
  | None -> match get c._private with
  | Some _type -> {name; _type; _public = false; _static = false; init}
  | None -> match get c.static_public with
  | Some _type -> {name; _type; _public = true; _static = true; init}
  | None -> match get c.static_private with
  | Some _type -> {name; _type; _public = false; _static = true; init}
  | None -> None

(* Renvoie le class_env associé à une classe *)
let get_class_env type_env name _class =
  let parent_env =
    match _class.parent with
    | None -> master_env
    | Some c -> 
      match StringMap.find_opt c.contents type_env with
      | None ->
        raise (SyntaxError(
          Printf.sprintf "Parent class '%s' does not exist." c.contents,
          c.line, c.column
        )) 
      | Some (CLSClass e) -> e
      | Some _ ->
        raise (SyntaxError(
          Printf.sprintf "Type '%s' is not a class." c.contents,
          c.line, c.column
        ))
  in

  let is_attribute_legal child_env a =
    match find_field parent_env a.name with
    | Some _ ->
      raise (SyntaxError(
        Printf.sprintf "Attribute '%s' is already defined in parent class." a.name.contents,
        a.name.line, a.name.column
      ))
    | None ->
      match find_field child_env a.name with
      | Some _ ->
        raise (SyntaxError(
          Printf.sprintf "Attribute '%s' is already defined in the same class." a.name.contents,
          a.name.line, a.name.column
        ))
      | None -> ()
  in

  let is_method_legal child_env a =
    match find_field child_env a._fun.name with
    | Some _ ->
      raise (SyntaxError(
        Printf.sprintf "Method '%s' is already defined in the same class." a._fun.name.contents,
        a._fun.name.line, a._fun.name.column
      ))
    | None ->
      match find_field parent_env a._fun.name with
      | Some t when t._public && (not a._public) ->
        raise (SyntaxError(
          Printf.sprintf "Method '%s' is private but overrides a public method." a._fun.name.contents,
          a._fun.name.line, a._fun.name.column
        ))
      | _ -> ()
  in

  let child_env =
    List.fold_left (fun acc f ->
      match f with
      | Attribute a when a._public ->
        is_attribute_legal acc a;
        if a._static then
          {acc with 
            static_public = StringMap.add a.name.contents (a._type, acc.static_count) acc.static_public;
            static_count = acc.static_count + 1; 
          }
        else
          {acc with 
            _public = StringMap.add a.name.contents (a._type, acc.count) acc._public;
            count = acc.count + 1; 
          }

      | Attribute a when a._private ->
        is_attribute_legal acc a;
        if a._static then
          {acc with 
            private_public = StringMap.add a.name.contents (a._type, acc.static_count) acc.private_public;
            static_count = acc.static_count + 1; 
          }
        else
          {acc with 
            _private = StringMap.add a.name.contents (a._type, acc.count) acc._private;
            count = acc.count + 1; 
          }

      | Method m ->
        is_method_legal acc m;
        let _type = 
          if m._static then
            create_static_fun_type m._fun 
          else
            create_fun_type m._fun name
        in
        if m._public then
          {acc with 
            static_public = StringMap.add a.name.contents (_type, acc.static_count) acc.static_public;
            static_count = acc.static_count + 1; 
          }
        else
          {acc with 
            static_private = StringMap.add a.name.contents (_type, acc.static_count) acc.static_private;
            static_count = acc.static_count + 1; 
          }
    ) StringMap.empty _class.fields
  in

  merge_class_env parent_env child_env

(* Calcule l'environnement de type intermédiaire, une map de renommage, et l'environnement global *)
let compute_type_env genv cls_prog =
  List.fold_left (fun (type_env, rename, genv) d ->
    match d with
    | Fun f ->
      let _type = create_static_fun_type f in
      (type_env, StringMap.add f.name.contents (get_name_static _module f.name) rename,
       StringMap.add f.name.contents _type genv)

    | Var(t, n, _) ->
      (type_env, StringMap.add n.contents (get_name_static _module n) rename, 
       StringMap.add n.contents t genv)

    | Type(name, TRegular t) ->
      (StringMap.add name.contents (CLSRegular t) type_env, rename, genv)

    | Type(name, TExtended(p, f)) ->
      let _type = translate_extended type_env name p f in
      (StringMap.add name.contents (CLSRegular t) type_env), rename, genv)

    | Type(name, TClass c) ->
      let class_env = get_class_env type_env name c in
      let (var_desc, type_desc) = get_descriptor name class_env in
      (StringMap.add name.contents (CLSClass class_env) type_env,
       StringMap.add name var_desc (get_name_static _module var_desc),
       StringMap.add var_desc type_desc genv)
  ) (StringMap.empty, genv) cls_prog

let translate_type_env type_env _class =
  StringMap.fold (fun alias _type acc ->
    match _type with
    | CLSRegular t ->
      StringMap.add alias t acc
    | CLSClass c ->
      match _class with
      | Some t when t = alias ->
        StringMap.add alias (TRecord (merge_record_env c._private c._public)) acc
      | _ ->
        StringMap.add alias (TRecord c._public) acc 
  ) type_env StringMap.empty

let rec translate_expression type_env rename e =
  let wrap e' =
    {e with contents = e}
  in
  let tr e =
    {e with contents = translate_expression type_env rename e}
  in
  match e.contents with
  | Null | Int _ | Bool _ -> e
  | Id id ->
    if StringMap.mem id.contents rename then
      wrap (Id(tr e))
    else
      e
  | Deref e ->
    wrap (Deref (tr e))
  | Unop(op, e) ->
    wrap (Unop(op, tr e))
  | Binop(e1, op, e2) ->
    wrap (Binop(tr e1, op, tr e2))
  | Call(f, args) ->
    wrap (Call(tr f, List.map tr args))
  | RecordAccess(e, f) ->
    (* ici, s'arranger pour savoir quand il s'agit d'un appel de méthode *)
    wrap (RecordAccess(tr e, f))
  | NewRecord(t, f) ->
    wrap (NewRecord(t, List.map (fun (s, e) -> (s, tr e)) f))
  | ArrayAccess(e1, e2) ->
    wrap (ArrayAccess(tr e1, tr e2))
  | NewArray(e1, e2) ->
    wrap (NewArray(tr e1, tr e2))
  | TupleAccess(e, i) ->
    wrap (TupleAccess(tr e, i))
  | NewTuple e ->
    wrap (NewTuple (List.map tr e))
  | InitArray e ->
    wrap (InitArray (List.map tr e))

let rec translate_instruction type_env rename acc i =
  | Nop | Exit | Break _ | Continue _ ->
    (append acc i, rename)
  | Return e ->
    (append acc (Return (translate_expression type_env rename e), rename)
  | Print e ->
    (append acc (Print (translate_expression type_env rename e), rename)
  | UnopAssign(e, op) ->
    (append acc (UnopAssign(translate_expression type_env rename e, op), rename)
  | BinopAssign of typ_expression node * assign_binop * typ_expression node
  | BinopAssign(e1, op, e2) ->
    let e1' = translate_expression type_env rename e1 in
    let e2' = translate_expression type_env rename e2 in
    (append acc (BinopAssign(e1', op, e2')), rename)
  | IfElse(c, t, e) ->
    let c' = translate_expression type_env rename c in
    let t', _ = translate_instructions type_env rename t in
    let e', _ = translate_instructions type_env rename e in
    (append acc (IfElse(c', t', e')), rename)
  | If(c, t) ->
    let c' = translate_expression type_env rename c in
    let t', _ = translate_instructions type_env rename t in
    (append acc (If(c', t')), rename)
  | While(c, b) ->
    let c' = translate_expression type_env rename c in
    let b', _ = translate_instructions type_env rename b in
    (append acc (While(c', b')), rename)
  | For(init, c, it, b) ->
    let init', rename = translate_instructions type_env rename init in
    let c' = translate_expression type_env rename c in
    let b', _ = translate_instructions type_env rename e in
    let it', _ = translate_instructions type_env rename t in
    (append acc (For(init', c', it', b')), rename)
  | Call(f, args) ->
    let f' = translate_expression type_env rename f in
    let args' = List.map (fun e -> translate_expression type_env rename e) args in
    (append acc (Call(f', args')), rename)
  | Declaration(t, n, e) ->
    let e' = translate_expression type_env rename e in
    (append acc (Declaration(t, n, e)), StringMap.remove n.contents rename)

and translate_instructions type_env rename is =
  let (acc, rename) = 
    List.fold_left (fun (acc, rename) i ->
      translate_instruction type_env rename acc i
    )
  in
  (to_list acc, rename)

let translate_function type_env rename f =
  let type_env = translate_type_env type_env None in
  {f with block = translate_instructions type_env rename f.block}

(* On ne peut pas accéder aux attributs statiques privés de la classe pour l'instant... *)
let translate_class init acc genv type_env name c =
  let type_env = translate_type_env type_env (Some name.contents) in
  let init_class, init_constructor, acc, constructor =
    List.fold_left (fun (init_class, init_constructor, acc, constructor) i ->
      match i with
      | Attribute a ->
        (* Pour l'instant, les attributs statiques ne sont pas partagés avec les classes filles. Il faudrait en faire des variables globales. Pareil pour les méthodes statiques *)
        if a._static then
          let init_class = append init_class (a.name, a.init) in
          (init_class, init_constructor, acc, constructor)
        else
          let init_constructor = append init_constructor (a.name, a.init) in
          (init_class, init_constructor, acc, constructor)

      | Method m ->
        if m._static then
          (* Même raisonnement qu'en bas sauf qu'on ne rajoute pas this *)
        else
          let real_name = get_name_non_static _module name m._fun.name in
          let init_class = append init_class (m._fun.name, Id real_name) in
          let f = {m._fun with name = real_name; params = {name = default_node "this"; reference = false; params_type = TPointer (TAlias name)}::(m._fun.params)} in
          let f' = translate_function type_env rename f in
          (init_class, init_constructor, (Fun f')::acc, (* Trouver la fonction principale ici *))

    ) (Cycle.empty_cycle, Cycle.empty_cycle, acc, None) c.fields
  in
  let init = append init (TNewRecord (to_list init_class)) in



let cls_to_typ _module cls =
