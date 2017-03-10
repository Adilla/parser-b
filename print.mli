open Expression
open Substitution
open Component

val ef_expr : expression -> Easy_format.t
val ef_pred : predicate -> Easy_format.t
val add_par : expression -> expression

val ef_subst : substitution -> Easy_format.t
val add_begin_end_ifn : substitution -> substitution

val ef_machine : abstract_machine -> Easy_format.t 
val ef_refinement : refinement -> Easy_format.t 
val ef_implementation : implementation -> Easy_format.t 
val ef_component : component -> Easy_format.t 
