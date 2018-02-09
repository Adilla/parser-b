open Codegen.Ada
(*
val package_to_format_spec : t_package -> Easy_format.t
val package_to_format_body : t_package -> Easy_format.t option
*)
val print_package_spec : out_channel -> t_package -> unit Error.t_result
val print_package_body : out_channel -> t_package -> unit Error.t_result
val is_package_body_empty : t_package -> bool
