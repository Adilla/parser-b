(** Parsing a component from an input channel *)
val parse_component : string(*filename*) -> in_channel -> (Component.component,Utils.loc*string) result
val parse_component_from_string : string -> (Component.component,Utils.loc*string) result
