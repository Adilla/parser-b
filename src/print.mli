(** Printing functions *)
open Syntax.R

val expression_to_format : expression -> Easy_format.t
val predicate_to_format : predicate -> Easy_format.t
val substitution_to_format : substitution -> Easy_format.t
val component_to_format : component -> Easy_format.t 

val print_expression : out_channel -> expression -> unit
val print_predicate : out_channel -> predicate -> unit
val print_substitution : out_channel -> substitution -> unit
val print_component : out_channel -> component -> unit
