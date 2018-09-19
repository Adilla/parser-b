type t

type t_local_kind =
  | L_Expr_Binder
  | L_Subst_Binder
  | L_Param_In
  | L_Param_Out

val empty : t
val add : t -> string -> Btype.Open.t -> t_local_kind -> t
val get : t -> string -> (Btype.Open.t*t_local_kind) option
val get_vars : t -> string list
