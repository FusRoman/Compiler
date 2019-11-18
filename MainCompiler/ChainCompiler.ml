
open Arg
open Sys
open ARTTree

let output_file = ref "a"
let make_medium_file = ref false

let set_output_file file = output_file := file


let speclist = [
  ("-o", Arg.String set_output_file, "Set the output file");
  ("-p", Arg.Set make_medium_file, "Generate the medium output file")
]

let make_compilation file =
  if not (Filename.check_suffix file ".var") then
    failwith "expected .var extension"
  else
    let input = open_in file in
    let lexing_buffer = Lexing.from_channel input in
    let output = "test/" ^ !output_file ^ ".stk" in
    let output = open_out output in
    let source = VARParser.program VARLexer.token lexing_buffer in
    let target = VARTree.var_to_fun source in
    if !make_medium_file then
      begin
        let fun_output = open_out "test/fun_out.fun" in
        FUNTree.write_fun fun_output target;
        close_out fun_output
      end;
    let target = FUNTree.fun_to_cll target in 
    if !make_medium_file then
      begin
        let cll_output = open_out "test/cll_out.cll" in
        CLLTree.write_cll cll_output target;
        close_out cll_output
      end;
    let target = CLLTree.cll_to_imp target in
    if !make_medium_file then
      begin
        let imp_output = open_out "test/imp_out.imp" in
        IMPTree.write_imp imp_output target;
        close_out imp_output 
      end;
    let target = IMPTree.imp_to_art target in
    compile output target;
    flush output;
    close_out output;
    let exit_code = command ("make run_stk_inter file=" ^ !output_file) in
    match exit_code with
    |0 -> ()
    |code -> print_string ("chain_compiler failure with exit code " ^ (string_of_int code))


let _ =
  Arg.parse speclist make_compilation "compilation cll to art"
