(** Rust code generation *)
open Codegen.Rust

val print_package : out_channel -> t_package -> unit Error.t_result
