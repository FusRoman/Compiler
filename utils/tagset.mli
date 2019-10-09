exception DuplicateElement of string

type elt = string
type t = string list

val empty : t
val add : elt -> t -> t
val mem : elt -> t -> bool
val union : t -> t -> t
val singleton : elt -> t

