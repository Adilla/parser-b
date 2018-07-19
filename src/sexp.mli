(** S-expression generation*)
open Syntax.R

type t

val sexp_to_string : t -> string

val sexp_to_channel : out_channel -> t -> unit

val sexp_of_expr : expression -> t

val sexp_of_pred : predicate -> t

val sexp_of_subst : substitution -> t

val sexp_of_component : component -> t
