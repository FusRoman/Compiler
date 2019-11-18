open ARTTree
open VARTree
open TPLTree
open VARParser
open VARLexer

let rec_variables = tpl_variables

let rec_to_tpl rec_prog =
  let tag_set = Tagset.union_duplicate rec_variables rec_prog.tag_set in
  let rec_lib = program token (Lexing.from_channel (open_in "typ/rec/RECLib.tpl")) in
  let syntax_tree = rec_lib.syntax_tree @ rec_prog.syntax_tree in
  {tag_set; syntax_tree}