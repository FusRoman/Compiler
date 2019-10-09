exception DuplicateElement of string

(*module TagSet = Set.Make (String)*)

type elt = string
type t = string list

let empty = []

let add x l =
  if List.mem x l then
    raise (DuplicateElement x)
  else
    x::l

let mem = List.mem
let singleton x = [x]

let rec union l1 l2 =
  match l1 with
  | [] -> l2
  | x::s ->
    union s (add x l2)