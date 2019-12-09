exception DuplicateElement of string

(** Type manipulé *)
type elt = string

(** Type des ensembles de strings *)
type t

(** Ensemble vide *)
val empty: t

(** Ajoute un élément à un ensemble ; lance DuplicateElement si déjà présent *)
val add: elt -> t -> t

(** Renvoie true si l'élément est dans l'ensemble *)
val mem: elt -> t -> bool

(** Réalise l'union des deux ensembles. Peut lancer DuplicateElement. *)
val union: t -> t -> t

(** L'ensemble formé d'un seul élément *)
val singleton: elt -> t

(** *)
val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a

val iter: (elt -> unit) -> t -> unit

(** Crée un ensemble depuis une liste. Ne lance pas d'exception. *)
val of_list: elt list -> t

(** Ajoute un élément à un ensemble, sans lancer DuplicateElement *)
val add_duplicate: elt -> t -> t

(** Réalise une union mais sans lancer DuplicateElement *)
val union_duplicate: t -> t -> t

(** Type des tags maker *)
type tag_maker

(** Prend un ensemble de tags et renvoie un tag_maker qui renvoie à chaque appel de make_tag un tag différent, jamais déclaré *)
val make_tag_maker: t -> tag_maker

(** Renvoie un nouveau tag jamais déclaré *)
val make_tag: tag_maker -> string

(** Renvoie l'ensemble des tags avec lequel le tag_maker a été initialisé, uni avec les tags créés après *)
val get_updated_set: tag_maker -> t
