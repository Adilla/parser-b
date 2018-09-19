(** Parsing functions *)

val parse_component_from_channel : filename:string -> in_channel -> PSyntax.component Error.t_result
(** [parse_component_from_channel ~filename input] reads a component from input. [~filename] is used for error localization. *)
val parse_component_from_string : string -> PSyntax.component Error.t_result
(** [parse_component_from_string input] reads a component from input. *)
