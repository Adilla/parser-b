type t

type t_local_kind =
  | L_Expr_Binder
  | L_Subst_Binder
  | L_Param_In
  | L_Param_Out

val empty : t
val declare : t -> string -> t_local_kind -> t
val declare_with_type : t -> string -> Btype.t -> t_local_kind -> t
val set_type : t -> string -> Btype.t -> unit
val get : t -> string -> (Btype.t option*t_local_kind) option
val get_vars : t -> string list
