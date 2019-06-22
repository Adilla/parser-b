(** Printing functions *)

(*
val expression_to_format : PSyntax.expression -> Easy_format.t
val predicate_to_format : PSyntax.predicate -> Easy_format.t
val substitution_to_format : PSyntax.substitution -> Easy_format.t
val component_to_format : PSyntax.component -> Easy_format.t 

*)
val print_expression : out_channel -> PSyntax.expression -> unit
val print_predicate : out_channel -> PSyntax.predicate -> unit
val print_substitution : out_channel -> PSyntax.substitution -> unit
val print_component : out_channel -> PSyntax.component -> unit

val expression_to_string : PSyntax.expression -> string
val predicate_to_string : PSyntax.predicate -> string
val substitution_to_string : PSyntax.substitution -> string
val component_to_string : PSyntax.component -> string
