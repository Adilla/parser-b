val bind: ('a,'e) result -> ('a -> ('c,'e) result) -> ('c,'e) result
val (>>=): ('a,'e) result -> ('a -> ('c,'e) result) -> ('c,'e) result

type loc = Lexing.position
val dloc : loc
type ident = loc*string
val ident_eq : ident -> ident -> bool
val ident_list_eq : ident list -> ident list -> bool

type 'a non_empty_list = 'a*'a list

exception Error of loc*string
