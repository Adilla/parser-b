(** Rust code generation *)

val print_package : out_channel -> string -> Codegen.t_package -> unit Error.t_result
val print_state : out_channel -> (string*Codegen.t_package) list -> unit
