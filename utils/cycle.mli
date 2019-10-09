(**
  Un module implémentant des cycles, c'est-à-dire une queue circulaire.
*)

(** Le type des queues circulaires *)
type 'a cycle

(** Le cycle vide *)
val empty_cycle : 'a cycle

(** Crée un cycle à partir d'une liste *)
val from_list : 'a list -> 'a cycle

(** Renvoie le nombre d'éléments dans le cycle *)
val count : 'a cycle -> int

(** Renvoie vrai si le cycle a au moins k éléments *)
val count_k : 'a cycle -> int -> bool

(** Raccourci pour count_k avec k = 1 *)
val count_1 : 'a cycle -> bool

(** 
  Récupère le premier élément du cycle sans affecter son placement. 
  La queue étant potentiellement mise à jour, on la renvoit en même temps. 
*)
val peek : 'a cycle -> 'a * ('a cycle)

(** Renvoie le premier élément et le met à la fin *)
val turn : 'a cycle -> 'a * ('a cycle) 

(** Renvoie le premier élément et le retire du cycle *)
val take : 'a cycle -> 'a * ('a cycle) 

(** Rajoute un élément à la fin *)
val append : 'a cycle -> 'a -> 'a cycle

(** Rajoute un élément au début *)
val prepend : 'a cycle -> 'a -> 'a cycle

(** Renvoie le dernier élément et le retire du cycle *)
val take_last : 'a cycle -> 'a * ('a cycle)

(** Renvoie le dernier élément et le place au début *)
val turn_last : 'a cycle -> 'a * ('a cycle)

(* Itère sur un cycle, dans l'ordre *)
val iter : 'a cycle -> ('a -> 'b -> 'b) -> 'b -> 'b