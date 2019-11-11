exception DuplicateElement of string

(** Type manipulé *)
type elt = string

(** Type des ensembles de strings *)
type t

(** Ensemble vide *)
val empty : t

(** Ajoute un élément à un ensemble ; lance DuplicateElement si déjà présent *)
val add : elt -> t -> t

(** Renvoie true si l'élément est dans l'ensemble *)
val mem : elt -> t -> bool

(** Réalise l'union des deux ensembles. Peut lancer DuplicateElement. *)
val union : t -> t -> t

(** L'ensemble formé d'un seul élément *)
val singleton : elt -> t

(** *)
val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a

(** Prend un ensemble de tags et renvoie une fonction unit -> string qui renvoie à chaque appel un tag différent, jamais déclaré *)
val make_tag_maker : t -> unit -> string

(** Réalise une union mais sans lancer DuplicateElement *)
val union_duplicate : t -> t -> t
