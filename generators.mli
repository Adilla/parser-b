open QCheck

type ident = unit Utils.ident
type expr = (unit,unit) Expression.expression
type pred = (unit,unit) Expression.predicate
type subst = (unit,unit) Substitution.substitution
type machine = (unit,unit) Component.abstract_machine
type refinement = (unit,unit) Component.refinement
type implementation = (unit,unit) Component.implementation
type component = (unit,unit) Component.component

val sized_expr : expr Gen.sized
val sized_pred : pred Gen.sized

val gen_expr : expr Gen.t
val gen_pred : pred Gen.t
val gen_subst : subst Gen.t
val gen_machine : machine Gen.t
val gen_refinement : refinement Gen.t
val gen_implementation : implementation Gen.t
val gen_component : component Gen.t
