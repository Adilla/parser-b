(** Random generator for expressions, predicates, substitution and components*)
open QCheck
open Blib.PSyntax

val sized_expr : expression Gen.sized
val sized_pred : predicate Gen.sized

val gen_expr : expression Gen.t
val gen_pred : predicate Gen.t
val gen_subst : substitution Gen.t
val gen_machine : component Gen.t
val gen_refinement : component Gen.t
val gen_implementation : component Gen.t
val gen_component : component Gen.t
