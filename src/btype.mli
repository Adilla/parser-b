type t

val mk_Atomic : string -> t
val mk_Power : t -> t
val mk_Product : t -> t -> t
val mk_Record : (string*t) list -> t

val t_int : t
val t_bool : t
val t_string : t

val type_of_unary_fun : t -> t -> t 
val type_of_binary_fun : t -> t -> t -> t 
val type_of_sequence : t -> t 

val to_string : t -> string
val equal : t -> t -> bool

type t_view =
| T_Atomic of string
| T_Power of t
| T_Product of t * t
| T_Record of (string*t) list

val view: t -> t_view

val to_btype_mt : t -> Btype_mt.t
val from_btype_mt : Btype_mt.t -> t option
