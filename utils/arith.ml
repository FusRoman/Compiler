let true_int = 1
let false_int = 0
let bool_of_int b = b <> false_int
let int_of_bool b = if b then true_int else false_int
let minus = (~-)
let cpl = lnot
let anot x = if bool_of_int x then false_int else true_int
let add = (+)
let sub = (-)
let multiply = ( * )
let divise = (/)
let modulo = (mod)
let bti f a b = if f a b then true_int else false_int
let equal = bti (=)
let different = bti (<>)
let lt = bti (<)
let le = bti (<=)
let gt = bti (>)
let ge = bti (>=)
let bit_and = (land)
let bit_or = (lor)