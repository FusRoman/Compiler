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
let choose = T.choose

let union l1 l2 =
  T.fold (fun x acc -> add x acc) l1 l2

let fold = T.fold
let iter = T.iter
let of_list = T.of_list

let add_duplicate = T.add
let union_duplicate = T.union

type tag_maker = {
  mutable set: t;
  mutable cpt: int;
  base: string;
}

let transform_file file =
  let is_alpha c =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' || c = '@'
  in
  let is_alphanumeric c =
    is_alpha c || (c >= '0' && c <= '9')
  in

  let result = ref "" in
  let add c =
    result := !result ^ (Char.escaped c)
  in

  let length = String.length file in
  if length = 0 then
    raise (Invalid_argument "Tagset.make_tag_maker: empty file name, can not guarantee the uniqueness of automatically generated tags");

  let fst = file.[0] in
  if is_alpha fst then
    add fst
  else
    add '@';  

  for i = 1 to length - 1 do
    let c = file.[i] in
    if is_alphanumeric c then
      add c
    else
      add '@'
  done;
  add '@';
  !result

let make_tag_maker l file =
  (* on suppose que id est valide (i.e non vide, respecte la syntaxe du langage, etc.) *)
  let file' = transform_file file in
  let file_length = String.length file' in
  let max_length = fold (fun t acc -> max acc (String.length t)) l file_length in
  {
    set = l;
    cpt = 0;
    base = file' ^ (String.make (max_length - file_length) 'a')
  }

let make_tag maker =
  let t = maker.base ^ (string_of_int maker.cpt) in
  maker.cpt <- maker.cpt + 1;
  maker.set <- add_duplicate t maker.set;
  t

let get_updated_set maker =
  maker.set