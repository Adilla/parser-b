type t =
| T_Atomic : string -> t
| T_Power : t -> t
| T_Product : t * t -> t
| T_Record : (string*t) list -> t
| T_Meta : int -> t

val t_int : t
val t_bool : t
val t_string : t

val type_of_unary_fun : t -> t -> t 
val type_of_binary_fun : t -> t -> t -> t 
val type_of_sequence : t -> t 

val to_string : t -> string
val equal : t -> t -> bool

val occurs : int -> t -> bool
val occurs2 : int -> t -> bool*bool
val subst : int -> t -> t -> t
val subst2 : int -> t -> t -> t*bool
