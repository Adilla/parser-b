(** Type inference for expressions, predicate and substitutions *)

val type_expression_exn :
  ('mr,'cl) Visibility.clause ->
  'mr Global.t -> Local.t -> PSyntax.expression ->
  ('mr,'cl,Btype.Open.t) TSyntax.expression

val type_predicate_exn :
  ('mr,'cl) Visibility.clause ->
  'mr Global.t -> Local.t -> PSyntax.predicate ->
  ('mr,'cl,Btype.Open.t) TSyntax.predicate

val type_substitution_exn:
  ('mr,'cl) Visibility.clause ->
  'mr Global.t -> Local.t -> PSyntax.substitution ->
  ('mr,'cl,Btype.Open.t) TSyntax.substitution
