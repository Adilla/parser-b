type t
val create : unit -> t
val new_meta : t -> Btype_mt.t
val get_stype : t -> Btype_mt.t -> Btype_mt.t -> Btype_mt.t option
val normalize : t -> Btype_mt.t -> Btype_mt.t
val add_alias : t -> string -> Btype_mt.t -> bool
val is_equal_modulo_alias : t -> Btype_mt.t -> Btype_mt.t -> bool
