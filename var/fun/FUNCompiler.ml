open ARTTree

let input_file = Sys.argv.(1)
let _ =
  if not (Filename.check_suffix input_file ".cll") then
    failwith "expected .cll extension"
let input = open_in input_file
let lexing_buffer = Lexing.from_channel input

let output_file = (Filename.chop_suffix input_file ".cll") ^ ".imp"
let output = open_out output_file

let _ =
  try
    let source = CLLParser.program CLLLexer.token lexing_buffer in
    let target = CLLTree.cll_to_imp source in
    IMPTree.write_imp output target.syntax_tree;
    exit 0
  with
  | SyntaxError(msg, l, c) -> 
    Printf.printf "[ERROR] Error at line %d, character %d. Message:\n%s\n" l c msg;
    exit 1
  | Failure msg ->
    Printf.printf "[ERROR] Syntax error. Message:\n%s\n" msg;
    exit 1
  | _ ->
    Printf.printf "[ERROR] Syntax error\n";
    exit 1