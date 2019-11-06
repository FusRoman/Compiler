let input_file = Sys.argv.(1)
let _ =
  if not (Filename.check_suffix input_file ".var") then
    failwith "expected .var extension"
let input = open_in input_file
let lexing_buffer = Lexing.from_channel input

let source = VARParser.program VARLexer.token lexing_buffer
let _ =
  VAREvals.eval_program source
