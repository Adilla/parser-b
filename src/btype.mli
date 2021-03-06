(** Types for B expressions*)
type t_alias
val no_alias: t_alias

module Open :
sig
  type t = private
    | T_Int
    | T_Bool
    | T_String
    | T_Abstract_Set of string
    | T_Concrete_Set of string
    | T_Power of t
    | T_Product of t * t
    | T_Record of (string*t) list
    | T_UVar of uv ref
  and uv = Unbound of int | Bound of t

  val t_int : t
  val t_bool : t
  val t_string : t

  val mk_Abstract_Set : string -> t
  val mk_Concrete_Set : string -> t
  val mk_Power : t -> t
  val mk_Product : t -> t -> t
  val mk_Record : (string*t) list -> t
  val new_meta : unit -> t

  val type_of_unary_fun : t -> t -> t 
  val type_of_binary_fun : t -> t -> t -> t 
  val type_of_sequence : t -> t 

  val to_string : t -> string
  val equal : t -> t -> bool

  val get_stype : t_alias -> t -> t -> t option
  val weak_norm : t_alias -> t -> t
end

type t = private Open.t

val t_int : t
val t_bool : t
val t_string : t

val to_string : t -> string
val equal : t -> t -> bool

val close : Open.t -> t option

val mk_Abstract_Set : string -> t
val mk_Concrete_Set : string -> t
val mk_Power : t -> t
val mk_Product : t -> t -> t
val mk_Record : (string*t) list -> t

type t_view =
  | T_Int
  | T_Bool
  | T_String
  | T_Abstract_Set of string
  | T_Concrete_Set of string
  | T_Power of t
  | T_Product of t * t
  | T_Record of (string*t) list

val view : t -> t_view

val is_equal : t_alias -> t -> t -> bool
val add_alias : t_alias -> string -> t -> t_alias option
(* val change_current : t_atomic_src -> t -> t *)

val subst : t Utils.SMap.t -> t -> t
