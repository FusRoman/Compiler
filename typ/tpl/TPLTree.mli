open ARTTree
open VARTree

(**
  TPL est VAR avec quelques variables et fonctions supplémentaires, rajoutées ici. 
  make_tuple et tuple_access étaient originellement définis dans TPLLib.var,
  mais puisque TYP se charge de les inliner dans les expressions, 
  ces définitions sont devenues inutiles et ont donc été supprimées.
  Donc le langage TPL n'a plus rien à voir avec les tuples
  et est juste une extension de VAR.
*)

(** 
  Variables de VAR, en rajoutant en plus essentiellement malloc.
  D'autres noms sont éventuellement rajoutés en fonction des déclarations globales de TPLLib,
  mais comme ces variables et fonctions ne sont pas censées être utilisées autrement que via malloc,
  elles finissent toutes par @ et seront donc inaccessibles depuis TYP.
*)
val tpl_variables : Tagset.t

val tpl_to_var : bool -> var_prog compiler_type -> var_prog compiler_type 