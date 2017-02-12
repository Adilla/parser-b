(** Parsing a component from an input channel *)
val parse_component : string(*filename*) -> in_channel -> (Component.component,Utils.loc*string) result
