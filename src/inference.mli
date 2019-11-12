(** Type inference for expressions, predicate and substitutions *)

val type_expression_exn :
  ('env_ki,'id_ki) Visibility.clause ->
  ('env_ki,_) Global.env -> Local.t -> PSyntax.expression ->
  ('id_ki,Btype.Open.t) TSyntax.expression

val type_predicate_exn :
  ('env_ki,'id_ki) Visibility.clause ->
  ('env_ki,_) Global.env -> Local.t -> PSyntax.predicate ->
  ('id_ki,Btype.Open.t) TSyntax.predicate

val type_substitution_exn:
  ('env_ki,'id_ki,'mut_ki,'assert_ki,'env_op_ki,'op_ki) Visibility.sclause ->
  ('env_ki,'env_op_ki) Global.env -> Local.t -> PSyntax.substitution ->
  ('id_ki,'mut_ki,'assert_ki,'op_ki,Btype.Open.t) TSyntax.substitution
