open Expression
open Substitution
open Component

val expression_to_format : ('lc,'ty) expression -> Easy_format.t
val predicate_to_format : ('lc,'ty) predicate -> Easy_format.t
val substitution_to_format : ('lc,'ty) substitution -> Easy_format.t
val machine_to_format : ('lc,'ty) abstract_machine -> Easy_format.t 
val refinement_to_format : ('lc,'ty) refinement -> Easy_format.t 
val implementation_to_format: ('lc,'ty) implementation -> Easy_format.t 
val component_to_format : ('lc,'ty) component -> Easy_format.t 

val print_expression : out_channel -> ('lc,'ty) expression -> unit
val print_predicate : out_channel -> ('lc,'ty) predicate -> unit
val print_substitution : out_channel -> ('lc,'ty) substitution -> unit
val print_machine : out_channel -> ('lc,'ty) abstract_machine -> unit
val print_refinement : out_channel -> ('lc,'ty) refinement -> unit
val print_implementation: out_channel -> ('lc,'ty) implementation -> unit
val print_component : out_channel -> ('lc,'ty) component -> unit
