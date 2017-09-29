open Utils
open Expression
open Substitution
open Component

type t (* =
  | Atom of string
  | List of t list *)

val to_string : t -> string
val to_channel : out_channel -> t -> unit

val sexp_of_ident : 'lc ident -> t

val sexp_of_expr : ('lc,'ty) expression -> t

val sexp_of_pred : ('lc,'ty) predicate -> t

val sexp_of_subst : ('lc,'ty) substitution -> t

val sexp_of_mch : ('lc,'ty) abstract_machine -> t

val sexp_of_ref : ('lc,'ty) refinement -> t

val sexp_of_imp : ('lc,'ty) implementation -> t

val sexp_of_component : ('lc,'ty) component -> t
