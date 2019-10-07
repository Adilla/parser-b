(** Ada code generation *)
open Blib
open Codegen.Ada

val print_package_spec : out_channel -> t_package -> unit Error.t_result
val print_package_body : out_channel -> t_package -> unit Error.t_result
val is_package_body_empty : t_package -> bool
