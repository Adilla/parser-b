(** Parsing functions *)

val parse_component_from_channel : filename:string -> in_channel -> Syntax.p_component Error.t_result
(** [parse_component_from_channel ~filename input] reads a component from input. [~filename] is used for error localization. *)
val parse_component_from_string : string -> Syntax.p_component Error.t_result
(** [parse_component_from_string input] reads a component from input. *)
