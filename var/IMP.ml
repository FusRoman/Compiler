type program = { text: IMPInstr.sequence; data: (string * int) list }

let to_string prog =
  ".text\n" ^ (IMPInstr.sequence_to_string prog.text)
  ^ ".data\n" ^ (ART.data_to_string prog.data)
