open ART_SyntaxTree

(* Ouverture du fichier source et crÃ©ation du buffer d'analyse *)
let input_file = Sys.argv.(1)
let _ =
  if not (Filename.check_suffix input_file ".art") then
    failwith "expected .art extension"
let input = open_in input_file
let lexing_buffer = Lexing.from_channel input

(* Ouverture du fichier cible *)
let output_file = (Filename.chop_suffix input_file ".art") ^ ".stk"
let output = open_out output_file

(* Lecture, analyse, traduction et Ã©criture de la chaÃ®ne obtenue dans
   le fichier cible *)
let _ =
  try
    let result_parser = ARTParser.source ARTLexer.token lexing_buffer in
    let tag_set,syntax_tree = result_parser.tag_set,result_parser.syntax_tree in
    compile output tag_set syntax_tree;
    exit 0
  with
  |SyntaxError (s,l,c) ->
    Printf.printf "Syntax error at %d, %d. Message:\n%s\n" l c s;
    exit 1