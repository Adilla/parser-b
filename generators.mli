open QCheck

val sized_expr : Expression.expression Gen.sized
val sized_pred : Expression.predicate Gen.sized

val gen_expr : Expression.expression Gen.t
val gen_pred : Expression.predicate Gen.t
val gen_subst : Substitution.substitution Gen.t
val gen_component : Component.component Gen.t
