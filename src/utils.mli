(** General utility functions*)
type loc = Lexing.position
val dloc : loc
type 'a non_empty_list = 'a*'a list
val list_eq : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
val nelist_eq : ('a -> 'b -> bool) -> 'a non_empty_list -> 'b non_empty_list -> bool
val map_opt : ('a -> 'b) -> 'a option -> 'b option
val filter_map : ('a -> 'b option) -> 'a list -> 'b list
