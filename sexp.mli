type t =
  | Atom of string
  | List of t list

val to_string : t -> string
val to_channel : out_channel -> t -> unit

val sexp_of_ident : Utils.ident -> t

val sexp_of_expr : Expression.expression -> t

val sexp_of_pred : Expression.predicate -> t

val sexp_of_subst : Substitution.substitution -> t

val sexp_of_mch : Component.abstract_machine -> t

val sexp_of_ref : Component.refinement -> t

val sexp_of_imp : Component.implementation -> t

val sexp_of_component : Component.component -> t
