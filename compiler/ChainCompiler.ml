
open Arg
open Sys

let make_medium_file = ref false
let language = ref "none"
let copy = ref false

let set_language l = 
  language := String.lowercase_ascii l 

let speclist = [
  ("-p", Arg.Set make_medium_file, "Generates the intermediate output files");
  ("-l", Arg.String set_language, "Specifies the source language. If not given, the program will try to guess based on the extension. Otherwise, the extension should not be given.");
  ("-c", Arg.Set copy, "Generates a copy of the source from its syntax tree (useful for debugging)")
]

let write f extension source file =
  let output_file = "test/" ^ file ^ extension in
  let output = open_out output_file in
  f output source;
  close_out output

let write_opt f extension source file =
  if !make_medium_file then
    write f extension source file

let copy f extension source file =
  if !copy then
    write f extension source (file ^ "_copy")

let read f extension file =
  let input = open_in ("test/" ^ file ^ extension) in
  let lexing_buffer = Lexing.from_channel input in
  let source = f lexing_buffer in
  close_in input;
  source

let run_btc file =
  exit (command ("./vm/VM " ^ file ^ ".btc"))

let run_asm file =
  exit (command ("make run_asm_inter file=" ^ file))
  
let run_stk file =
  exit (command ("make run_stk_inter file=" ^ file))

let run_art_inter art file =
  write ARTTree.compile ".stk" art file;
  run_stk file

let run_art file =
  let art = read (ARTParser.source ARTLexer.token) ".art" file in
  copy ARTTree.write_art ".art" art file;
  run_art_inter art file

let run_imp_inter imp file =
  let art = IMPTree.imp_to_art imp in
  write_opt ARTTree.write_art ".art" art file;
  run_art_inter art file

let run_imp file =
  let imp = read (IMPParser.program IMPLexer.token) ".imp" file in
  copy IMPTree.write_imp ".imp" imp file;
  run_imp_inter imp file

let run_cll_inter cll file =
  let imp = CLLTree.cll_to_imp cll in
  write_opt IMPTree.write_imp ".imp" imp file;
  run_imp_inter imp file

let run_cll file =
  let cll = read (CLLParser.program CLLLexer.token) ".cll" file in
  copy CLLTree.write_cll ".cll" cll file;
  run_cll_inter cll file

let run_fun_inter _fun file =
  let cll = FUNTree.fun_to_cll _fun in
  write_opt CLLTree.write_cll ".cll" cll file;
  run_cll_inter cll file

let run_fun file =
  let _fun = read (FUNParser.program FUNLexer.token) ".fun" file in
  copy FUNTree.write_fun ".fun" _fun file;
  run_fun_inter _fun file

let run_var_inter var file =
  let _fun = VARTree.var_to_fun var in
  write_opt FUNTree.write_fun ".fun" _fun file;
  run_fun_inter _fun file

let run_var file =
  let var = read (VARParser.program VARLexer.token) ".var" file in
  copy VARTree.write_var ".var" var file;
  run_var_inter var file

let rec run file =
  match !language with
  | "btc" ->
    run_btc file
  | "asm" ->
    run_asm file
  | "stk" ->
    run_stk file
  | "art" ->
    run_art file
  | "imp" ->
    run_imp file
  | "cll" ->
    run_cll file
  | "fun" ->
    run_fun file
  | "var" ->
    run_var file
  | "none" ->
    let rec find_extension acc i =
      if i < 0 then
      begin
        Printf.printf "Could not infer the source language from the file name and the option -l was not given.\n";
        exit 1
      end
      else if file.[i] = '.' then
      begin
        set_language String.(sub file (i+1) ((length file) - i - 1));
        run String.(sub file 0 i)
      end
      else
        find_extension ((String.make 1 file.[i]) ^ acc) (i - 1)
    in
    find_extension "" ((String.length file) - 1)
  | _ ->
    Printf.printf "Unknown language '%s'\n" !language;
    exit 1

let _ =
  Arg.parse speclist run "Usage: ./ChainCompiler -p -l <source language> <source file without extension>\nOr: ./ChainCompiler -p <source file WITH extension>"