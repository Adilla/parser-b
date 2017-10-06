open Syntax

val parse_component : string(*filename*) -> in_channel -> p_component Error.t_result
val parse_component_from_string : string -> p_component Error.t_result
