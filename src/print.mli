open Syntax

val builtin_to_string : e_builtin -> string
val pred_bop_to_string : pred_bop -> string
val prop_bop_to_string : prop_bop -> string
val binder_to_string : expr_binder -> string

val expression_to_format : ('lc,'ty) expression -> Easy_format.t
val predicate_to_format : ('lc,'ty) predicate -> Easy_format.t
val substitution_to_format : ('lc,'ty) substitution -> Easy_format.t
val component_to_format : ('lc,'ty) component -> Easy_format.t 

val print_expression : out_channel -> ('lc,'ty) expression -> unit
val print_predicate : out_channel -> ('lc,'ty) predicate -> unit
val print_substitution : out_channel -> ('lc,'ty) substitution -> unit
val print_component : out_channel -> ('lc,'ty) component -> unit
