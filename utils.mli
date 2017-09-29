type loc = Lexing.position
val dloc : loc

type 'lc ident = 'lc * string
type u_ident = loc ident

type 'a non_empty_list = 'a*'a list
