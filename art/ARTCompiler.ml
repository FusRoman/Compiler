open ARTTree

let input_file = Sys.argv.(1)
let _ =
  if not (Filename.check_suffix input_file ".art") then
    failwith "expected .art extension"
let input = open_in input_file
let lexing_buffer = Lexing.from_channel input

(* Ouverture du fichier cible *)
let output_file = (Filename.chop_suffix input_file ".art") ^ ".stk"
let output = open_out output_file

let _ =
  try
    (* La lecture du fichier cible renvoie un type 'a compiler_type avec 'a = art_prog. C'est un enregistrement 
    contenant l'arbre de syntaxe abstraite du fichier art analysé et un ensemble de tag 
    permettant de faire les vérifications sur les tags. La fonction compile se charge de parcourir l'arbre de syntaxe 
    abstraite d'art et d'écrire dans le fichier de sortie. *)
    let source = ARTParser.source ARTLexer.token lexing_buffer in
    compile output source;
    close_out output;
    exit 0
  with
  (* Cette exception est levée à la fois dans ArtParser.source et dans compile. *)
  | SyntaxError (s,l,c) ->
    Printf.printf "[ART ERROR] Syntax error at %d, %d. Message:\n%s\n" l c s;
    exit 1