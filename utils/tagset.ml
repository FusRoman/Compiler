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
let of_list = T.of_list

let add_duplicate = T.add
let union_duplicate = T.union

type tag_maker = {
  mutable set: t;
  mutable cpt: int;
  base: string;
}

let make_tag_maker l =
  let max_length = fold (fun t acc -> max acc (String.length t)) l 1 in
  {
    set = l;
    cpt = 0;
    base = String.make max_length 'a'
  }

let make_tag maker =
  let t = maker.base ^ (string_of_int maker.cpt) in
  maker.cpt <- maker.cpt + 1;
  maker.set <- add_duplicate t maker.set;
  t

let get_updated_set maker =
  maker.set