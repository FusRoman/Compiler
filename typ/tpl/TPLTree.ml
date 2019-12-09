open ARTTree
open FUNTree
open VARTree
open VARParser
open VARLexer

let tpl_lib = program token (Lexing.from_channel (open_in "typ/tpl/TPLLib.var"))

let tpl_variables = Tagset.union_duplicate tpl_lib.tag_set var_variables

let check_declarations tpl =
  List.iter (fun d ->
    let name = 
      match d with
      | Fun f -> f.name
      | Var(v, e) -> v
    in
    if Tagset.mem name.contents tpl_variables then 
      raise (SyntaxError(
        Printf.sprintf "'%s' is a reserved variable." name.contents,
        name.line, name.column
      ))
  ) tpl

let add_lib tpl =
  let rec find_main decl =
    match decl with
    | [] -> 
      raise (SyntaxError("No procedure 'main' found.", 0, 0))
    | ((Var _) as x)::s ->
      (* L'ordre des variables globales compte *)
      let (main, s') = find_main s in
      (main, x::s')
    | ((Fun f) as x)::s ->
      if f.name.contents = "main" then
        (f, s)
      else
        let (main, s') = find_main s in
        (main, x::s')
  in
  let tag_set = Tagset.union_duplicate tpl_variables tpl.tag_set in
  let (main, decl) = find_main tpl.syntax_tree in
  let memory_break = default_node "memory_break@" in
  let heap_pointer = default_node "heap_pointer@" in
  let last_arg = default_node "last_arg" in
  let argv = default_node "argv" in
  let argc = default_node "argc" in
  let init = [
    Declaration(last_arg, Deref(Binop(Deref(Id argv), Add, Binop(Deref(Id argc), Sub, Int 1))));
    BinopAssign(Id memory_break, Standard, Binop(Deref(Id last_arg), Add, Deref(Binop(Deref(Id last_arg), Sub, Int 1))));
    BinopAssign(Id heap_pointer, Standard, Deref(Id memory_break))
  ] in
  let main' = {main with block = init @ main.block} in
  let syntax_tree = tpl_lib.syntax_tree @ ((Fun main')::decl) in
  {tag_set; syntax_tree}

let tpl_to_var tpl =
  check_declarations tpl.syntax_tree;
  add_lib tpl