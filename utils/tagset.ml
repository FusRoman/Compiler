exception DuplicateElement of string

module T = Set.Make (String)

type elt = T.elt
type t = T.t

let empty = T.empty

let add x l =
  if T.mem x l then
    raise (DuplicateElement x)
  else
    T.add x l

let mem = T.mem
let singleton = T.singleton

let union l1 l2 =
  T.fold (fun x acc -> add x acc) l1 l2

let fold = T.fold

let make_tag_maker l =
  let cpt = ref 0 in
  let max_length = fold (fun t acc -> max acc (String.length t)) l 1 in
  let base = String.make max_length 'a' in
  (fun () -> 
    let t = base ^ (string_of_int !cpt) in
    incr cpt; t
  )