let new_return_label =
  let cpt = ref 0 in
  fun () -> incr cpt; Printf.sprintf "_return_label_%i" !cpt



let translate_function_definition fdef =
  failwith "not implemented"
    
let translate_program prog = {
  IMP.text =
    List.flatten (List.map translate_function_definition CLL.(prog.text));
  IMP.data =
    failwith "not implemented"
}