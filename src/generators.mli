open QCheck

type expression = (unit,unit) Syntax.expression
type predicate = (unit,unit) Syntax.predicate
type substitution = (unit,unit) Syntax.substitution
type component = (unit,unit) Syntax.component

val sized_expr : expression Gen.sized
val sized_pred : predicate Gen.sized

val gen_expr : expression Gen.t
val gen_pred : predicate Gen.t
val gen_subst : substitution Gen.t
val gen_machine : component Gen.t
val gen_refinement : component Gen.t
val gen_implementation : component Gen.t
val gen_component : component Gen.t
