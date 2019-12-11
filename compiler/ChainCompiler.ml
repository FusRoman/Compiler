
open Arg
open Sys

let make_medium_file = ref false
let language = ref "none"
let copy = ref false
let library = ref false

let set_language l = 
  language := String.lowercase_ascii l 

let speclist = [
  ("-p", Arg.Set make_medium_file, "Generates the intermediate output files");
  ("--print", Arg.Set make_medium_file, "Same as -p");
  ("-l", Arg.String set_language, "Specifies the source language. If not given, the program will try to guess based on the extension. Otherwise, the extension should not be given.");
  ("--language", Arg.String set_language, "Same as -l");
  ("-c", Arg.Set copy, "Generates a copy of the source from its abstract syntax tree (useful for debugging)");
  ("--copy", Arg.Set copy, "Same as -c");
  ("--lib", Arg.Set library, "Compiles the source file as a library")
]

(* Début des arguments du main *)
let argv = ref (-1)

let write f extension source file =
  let output_file = "test/" ^ file ^ "." ^ extension in
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
  let upper_ext = String.uppercase_ascii extension in
  try
    let input = open_in ("test/" ^ file ^ "." ^ extension) in
    let lexing_buffer = Lexing.from_channel input in
    let source = f lexing_buffer in
    close_in input;
    source
  with
  | ARTTree.SyntaxError(msg, l, c) -> 
    Printf.printf "[%s ERROR] Error at line %d, character %d. Message:\n%s\n" upper_ext l c msg;
    exit 1
  | FUNTree.UnboundValue(fct, var) ->
    Printf.printf "[%s ERROR] Unbound value '%s' in function '%s' at line %d, character %d.\n" upper_ext var.contents fct.contents var.line var.column;
    exit 1
  | TYPTree.TypeError(msg, line, column) ->
    Printf.printf "[%s ERROR] Type error at line %d, column %d. Message:\n%s\n" upper_ext line column msg;
    exit 1
  | Failure msg ->
    Printf.printf "[%s ERROR] Syntax error. Message:\n%s\n" upper_ext msg;
    exit 1
  | e ->
    Printf.printf "[%s ERROR] Unknown error\n" upper_ext;
    raise e

let translate t extension source =
  let upper_ext = String.uppercase_ascii extension in
  try
    t source
  with
  | ARTTree.SyntaxError(msg, l, c) ->
    Printf.printf "[%s ERROR] Error at line %d, character %d. Message:\n%s\n" upper_ext l c msg;
    exit 1
  | FUNTree.UnboundValue(fct, var) ->
    Printf.printf "[%s ERROR] Unbound value '%s' in function '%s' at line %d, character %d.\n" upper_ext var.contents fct.contents var.line var.column;
    exit 1
  | TYPTree.TypeError(msg, line, column) ->
    Printf.printf "[%s ERROR] Type error at line %d, column %d. Message:\n%s\n" upper_ext line column msg;
    exit 1
  | Failure msg ->
    Printf.printf "[%s ERROR] Syntax error. Message:\n%s\n" upper_ext msg;
    exit 1
  | e ->
    Printf.printf "[%s ERROR] Unknown error\n" upper_ext;
    raise e

let translate_lib t extension source =
  translate (t !library) extension source

let run_btc file =
  if !library then
    Printf.printf "cannot run a library\n";
  let line = ref ("./vm/VM test/" ^ file ^ ".btc ") in
  for i = !argv to (Array.length Sys.argv) - 1 do
    line := !line ^ Sys.argv.(i) ^ " ";
  done;
  exit (command !line)

let run_asm file =
  if !library then
    Printf.printf "cannot run a library\n";
  let code = command ("./assembler/Assembler test/" ^ file ^ ".asm") in
  if code = 0 then
    run_btc file;
  exit code
  
let run_stk file =
  let base = "./stk/STKCompilerAlloc test/" ^ file ^ ".stk" in
  let code = 
    if !library then
      command (base ^ " --lib") 
    else
      command base
  in
  if code = 0 && not !library then
    run_asm file;
  exit code

let run_art_inter art file =
  write ARTTree.compile "stk" art file;
  run_stk file

let run_art file =
  let art = read (ARTParser.source ARTLexer.token) "art" file in
  copy ARTTree.write_art "art" art file;
  run_art_inter art file

let run_imp_inter imp file =
  let art = translate IMPTree.imp_to_art "imp" imp in
  write_opt ARTTree.write_art "art" art file;
  run_art_inter art file

let run_imp file =
  let imp = read (IMPParser.program IMPLexer.token) "imp" file in
  copy IMPTree.write_imp "imp" imp file;
  run_imp_inter imp file

let run_cll_inter cll file =
  let imp = translate_lib CLLTree.cll_to_imp "cll" cll in
  write_opt IMPTree.write_imp "imp" imp file;
  run_imp_inter imp file

let run_cll file =
  let cll = read (CLLParser.program CLLLexer.token) "cll" file in
  copy CLLTree.write_cll "cll" cll file;
  run_cll_inter cll file

let run_fun_inter _fun file =
  let cll = translate_lib FUNTree.fun_to_cll "fun" _fun in
  write_opt CLLTree.write_cll "cll" cll file;
  run_cll_inter cll file

let run_fun file =
  let _fun = read (FUNParser.program FUNLexer.token) "fun" file in
  copy FUNTree.write_fun "fun" _fun file;
  run_fun_inter _fun file

let run_var_inter var file =
  let _fun = translate_lib VARTree.var_to_fun "var" var in
  write_opt FUNTree.write_fun "fun" _fun file;
  run_fun_inter _fun file

let run_var file =
  let var = read (VARParser.program VARLexer.token) "var" file in
  copy VARTree.write_var "var" var file;
  run_var_inter var file

let run_tpl_inter tpl file =
  let var = translate_lib TPLTree.tpl_to_var "tpl" tpl in 
  write_opt VARTree.write_var "var" var file;
  run_var_inter var file

let run_tpl file =
  let tpl = read (VARParser.program VARLexer.token) "tpl" file in
  copy VARTree.write_var "tpl" tpl file;
  run_tpl_inter tpl file

let run_typ_inter typ file =
  let tpl = translate TYPTree.typ_to_tpl "typ" typ in 
  write_opt VARTree.write_var "tpl" tpl file;
  run_tpl_inter tpl file

let run_typ file =
  let typ = read (TYPParser.program TYPLexer.token) "typ" file in
  (*copy TYPTree.write_typ "typ" typ file;*)
  run_typ_inter typ file

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
  | "tpl" ->
    run_tpl file
  | "typ" ->
    run_typ file
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

let start file =
  let i = ref 0 in
  let argc = Array.length Sys.argv in
  while !i < argc && !argv = -1 do
    if Sys.argv.(!i) = file then
      argv := !i + 1;
    incr i
  done;
  run file

let _ =
  Arg.parse speclist start ("Usage: ./compiler/ChainCompiler -p -l <source language> <source file WITHOUT extension> [<parameter> ...]\n"
    ^ "Or: ./compiler/ChainCompiler -p <source file WITH extension> [<parameter> ...]")