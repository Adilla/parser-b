(** Parsing a component from an input channel *)
val parse_component : string(*filename*) -> in_channel -> Component.u_comp Error.t_error
val parse_component_from_string : string -> Component.u_comp Error.t_error

val parse_component_exn : string(*filename*) -> in_channel -> Component.u_comp
val parse_component_from_string_exn : string -> Component.u_comp
