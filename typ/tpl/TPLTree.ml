open ARTTree
open FUNTree
open VARTree
open VARParser
open VARLexer

let tpl_variables = var_variables


let check_declaration tpl =
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

let tpl_to_var tpl =
  (*check_declaration tpl.syntax_tree;*)
  let tag_set = Tagset.union_duplicate tpl_variables tpl.tag_set in
  let tpl_lib = program token (Lexing.from_channel (open_in "typ/tpl/TPLLib.var")) in
  let syntax_tree = tpl_lib.syntax_tree @ tpl.syntax_tree in
  {syntax_tree; tag_set}
    
