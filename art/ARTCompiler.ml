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
  let t = ARTParser.source ARTLexer.token lexing_buffer in
  compile output t