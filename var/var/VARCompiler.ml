open ARTTree
open FUNTree
open VARTree

let input_file = Sys.argv.(1)
let _ =
  if not (Filename.check_suffix input_file ".var") then
    failwith "expected .var extension"
let input = open_in input_file
let lexing_buffer = Lexing.from_channel input

let output_file = (Filename.chop_suffix input_file ".var") ^ ".fun"
let output = open_out output_file

let _ =
  try
    let source = VARParser.program VARLexer.token lexing_buffer in
    let target = var_to_fun false source in
    write_fun output target;
    close_out output;
    exit 0
  with
  | SyntaxError(msg, l, c) -> 
    Printf.printf "[VAR ERROR] Error at line %d, character %d. Message:\n%s\n" l c msg;
    exit 1
  | UnboundValue(fct, var) ->
    Printf.printf "[VAR ERROR] Unbound value '%s' in function '%s' at line %d, character %d.\n" var.contents fct.contents var.line var.column;
    exit 1
  | Failure msg ->
    Printf.printf "[VAR ERROR] Syntax error. Message:\n%s\n" msg;
    exit 1
  | _ ->
    Printf.printf "[VAR ERROR] Unknown error\n";
    exit 1