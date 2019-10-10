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
    (*let target = IMPtoART.translate_program source in*)
    (*direct_print target output *)
    Printf.printf "Seems to be doing fine\n"
  with
  | SyntaxError(msg, l, c) -> 
    Printf.printf "[ERROR] Error at line %d, character %d. Message:\n%s\n" l c msg
  | _ ->
    Printf.printf "[ERROR] Syntax error\n"
