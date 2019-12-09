open VARTree

let input_file = Sys.argv.(1)
let _ =
  if not (Filename.check_suffix input_file ".typ") then
    failwith "expected .var extension"
let input = open_in input_file
let lexing_buffer = Lexing.from_channel input

let output_file = (Filename.chop_suffix input_file ".tpl") ^ ".fun"
let output = open_out output_file

let _ =
  let source = TYPParser.program TYPLexer.token lexing_buffer in
  let target = TYPTree.typ_to_tpl source in
  write_var output target;
  close_out output;
  exit 0