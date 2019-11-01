(** Type inference for expressions, predicate and substitutions *)

val type_expression_exn :
  'mr Visibility.clause ->
  'mr Global.t -> Local.t -> PSyntax.expression ->
  ('mr,Btype.Open.t) TSyntax.expression

val type_predicate_exn :
  'mr Visibility.clause ->
  'mr Global.t -> Local.t -> PSyntax.predicate ->
  ('mr,Btype.Open.t) TSyntax.predicate

val type_substitution_exn:
  'mr Visibility.mclause ->
  'mr Global.t -> Local.t -> PSyntax.substitution ->
  ('mr,Btype.Open.t) TSyntax.substitution
