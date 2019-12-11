type 'a cycle = ('a list) * ('a list)

let empty_cycle = ([], [])

let from_elt x =
  ([], [x])

(* En supposant que la première liste soit vide, remet tout dans la première dans l'ordre *)
let rec clean q =
  match q with
  | (l, []) -> q
  | (l, x::s) -> clean (x::l, s)

(* Renvoie le nombre d'éléments dans le cycle *)
let count q =
  let rec count_tr q acc =
    match q with
    | ([], []) -> acc
    | (x::s, []) -> count_tr (s, []) (acc + 1)
    | ([], x::s) -> count_tr ([], s) (acc + 1)
    | (x::s, y::t) -> count_tr (s, t) (acc + 2)
  in
  count_tr q 0

(* Renvoie vrai si le cycle a au moins k éléments *)
let count_k q k =
  let rec count_tr q acc =
    if acc >= k then
      true
    else
      match q with
      | ([], []) -> false
      | (x::s, []) -> count_tr (s, []) (acc + 1)
      | ([], x::s) -> count_tr ([], s) (acc + 1)
      | (x::s, y::t) -> count_tr (s, t) (acc + 2)
  in
  count_tr q 0

let count_1 q =
  match q with
  | ([], []) -> false
  | _ -> true

(* Récupère le premier élément du cycle sans affecter son placement. 
   La queue étant potentiellement mise à jour, on la renvoit en même temps. *)
let rec peek q =
  match q with
  | ([], []) -> raise Not_found
  | (x::s, _) -> (x, q)
  | ([], _) -> peek (clean q)

(* Renvoie le premier élément et le met à la fin *)
let rec turn q =
  match q with
  | ([], []) -> raise Not_found
  | (x::s, l) -> (x, (s, x::l))
  | ([], _) -> turn (clean q)

(* Renvoie le premier élément et le retire du cycle *)
let rec take q =
  match q with
  | ([], []) -> raise Not_found
  | (x::s, l) -> (x, (s, l))
  | ([], _) -> take (clean q)

(* Rajoute un élément à la fin *)
let append (l1, l2) x =
  (l1, x::l2)

(* Rajoute tous les éléments d'une liste à la fin du cycle *)
let append_list q l =
  let rec from_rec l (l1, l2) =
    match l with
    | [] -> (l1, l2)
    | x::s ->
      from_rec s (l1, x::l2)
  in
  from_rec l q

(* Crée un cycle à partir d'une liste *)
let from_list l = append_list empty_cycle l

(** Rajoute un élément au début *)
let prepend (l1, l2) x =
  (x::l1, l2)

let rec extend l1 l2 =
  if l2 = empty_cycle then
    l1
  else
    let (x, s) = take l2 in
    extend (append l1 x) s

let rec pop_last_list l =
  match l with
  | [] -> raise Not_found
  | x::[] -> (x, [])
  | x::s ->
    let (last, l') = pop_last_list s in
    (last, x::l')

(** Renvoie le dernier élément et le retire du cycle *)
let take_last q =
  match q with
  | ([], []) -> raise Not_found
  | (l, x::s) -> (x, (l, s))
  | (l, []) ->
    let (last, l') = pop_last_list l in
    (last, (l', []))

(** Renvoie le dernier élément et le place au début *)
let turn_last q =
  match q with
  | ([], []) -> raise Not_found
  | (l, x::s) -> (x, (x::l, s))
  | (l, []) ->
    let (last, l') = pop_last_list l in
    (last, (last::l', []))

(* Itère sur un cycle, dans l'ordre *)
let rec iter q f acc =
  if count_1 q then
    let (x, q') = take q in
    iter q' f (f x acc)
  else
    acc

let rec iter2 q1 q2 f acc =
  let ok1 = count_1 q1 in
  let ok2 = count_1 q2 in
  match (ok1, ok2) with
  | (true, true) ->
    let (x1, q1') = take q1 in
    let (x2, q2') = take q2 in
    iter2 q1' q2' f (f x1 x2 acc)
  | (false, true) | (true, false) ->
    raise (Invalid_argument "Cycle.iter2: both arguments must have the same size")
  | (false, false) ->
    acc

let rec to_list q =
  if count_1 q then
    let (x, q') = take q in
    x::(to_list q')
  else
    []

let map q f =
  let rec map_rec q f acc =
    if count_1 q then
      let (x, q') = take q in
      map_rec q' f (append acc (f x))
    else
      acc
  in
  map_rec q f empty_cycle