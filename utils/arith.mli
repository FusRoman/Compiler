(* 
  Module contenant toutes les fonctions de base utiles à la VM.
  On pourra donc utiliser ces fonctions dans d'autres compilateurs,
  par exemple pour précalculer les instructions sur des constantes,
  sans crainte de faire une erreur dans la réimplémentation de ces fonctions.
*)
(** La valeur entière correspondante à true *)
val true_int : int

(** La valeur entière correspondante à false *)
val false_int : int

(** Renvoie true si l'entier donné code true, faux autrement *)
val bool_of_int : int -> bool

(** Renvoie la représentation du booléen donné en entier *)
val int_of_bool : bool -> int

val minus : int -> int

val cpl : int -> int

val anot : int -> int

val add : int -> int -> int

val sub : int -> int -> int

val multiply : int -> int -> int

val divise : int -> int -> int

val modulo : int -> int -> int

val equal : int -> int -> int

val different : int -> int -> int

val lt : int -> int -> int

val le : int -> int -> int

val gt : int -> int -> int

val ge : int -> int -> int

val bit_and : int -> int -> int

val bit_or : int -> int -> int