type opn
type cls

type 'a typ =
| T_Atomic : string -> 'a typ
| T_Power : 'a typ -> 'a typ
| T_Product : 'a typ * 'a typ -> 'a typ
| T_Record : (string*'a typ) list -> 'a typ
| T_Meta : int -> opn typ

type btype = cls typ
type opn_btype = opn typ

val t_int : 'a typ
val t_bool : 'a typ
val t_string : 'a typ

val type_of_unary_fun : 'a typ -> 'a typ -> 'a typ 
val type_of_binary_fun : 'a typ -> 'a typ -> 'a typ -> 'a typ 
val type_of_sequence : 'a typ -> 'a typ 

val to_string : 'a typ -> string
val is_equal : 'a typ -> 'a typ -> bool

val occurs : int -> 'a typ -> bool
val subst : int -> 'a typ -> 'a typ -> 'a typ
val close : 'a typ -> btype option
val to_open : btype -> 'a typ

module Unif : sig
  type t
  val create : unit -> t
  val new_meta : t -> opn typ
  val get_stype : t -> opn typ -> opn typ -> (opn typ) option
  val normalize : t -> opn typ -> opn typ
end
