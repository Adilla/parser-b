(** Functor for code generation parametrized by the target language*)
module Make (
    Ident:
    sig
      type t
      val make:string -> t option
      val to_string:t -> string
      type t_pkg_id
      val make_pkg_id:string -> t_pkg_id option
      val pkg_to_string:t_pkg_id -> string
    end)
  : Codegen_sig.S
    with type t_id=Ident.t
    with type t_pkg_id=Ident.t_pkg_id

module Ada_ident :
  sig
    type t
    val make:string -> t option
    val to_string:t -> string
    type t_pkg_id
    val make_pkg_id:string -> t_pkg_id option
    val pkg_to_string:t_pkg_id -> string
  end

module Ada : Codegen_sig.S
  with type t_id=Ada_ident.t
  with type t_pkg_id=Ada_ident.t_pkg_id

module Rust_ident :
  sig
    type t
    val make:string -> t option
    val to_string:t -> string
    type t_pkg_id
    val make_pkg_id:string -> t_pkg_id option
    val pkg_to_string:t_pkg_id -> string
  end

module Rust : Codegen_sig.S
  with type t_id=Rust_ident.t
  with type t_pkg_id=Rust_ident.t_pkg_id
