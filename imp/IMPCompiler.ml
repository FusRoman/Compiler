open ARTTree

let input_file = Sys.argv.(1)
let _ =
  if not (Filename.check_suffix input_file ".imp") then
    failwith "expected .imp extension"
let input = open_in input_file
let lexing_buffer = Lexing.from_channel input

let output_file = (Filename.chop_suffix input_file ".imp") ^ ".art"
let output = open_out output_file

let _ =
  try
    let source = IMPParser.program IMPLexer.token lexing_buffer in
    let target = IMPTree.imp_to_art source in
    write_art output target.syntax_tree;
    close_out output;
    exit 0
  with
  | SyntaxError(msg, l, c) -> 
    Printf.printf "[IMP ERROR] Error at line %d, character %d. Message:\n%s\n" l c msg;
    exit 1
  | Failure msg ->
    Printf.printf "[IMP ERROR] Syntax error. Message:\n%s\n" msg;
    exit 1
  | _ ->
    Printf.printf "[IMP ERROR] Syntax error\n";
    exit 1