let input_file = Sys.argv.(1)
let _ =
  if not (Filename.check_suffix input_file ".imp") then
    failwith "expected .imp extension"
let input = open_in input_file
let lexing_buffer = Lexing.from_channel input

let output_file = (Filename.chop_suffix input_file ".imp") ^ ".art"
let output = open_out output_file

let source = IMPParser.program IMPLexer.token lexing_buffer
let target = IMPtoART.translate_program source

let _ = 
  Printf.fprintf output "%s" (ART.to_string target)
