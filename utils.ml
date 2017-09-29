let bind u f =
  match u with
  | Ok a -> f a
  | Error a -> Error a

let (>>=) = bind

type loc = Lexing.position
let dloc = Lexing.dummy_pos

type 'lc ident = 'lc * string
type u_ident = loc ident

type 'a non_empty_list = 'a*'a list

let ident_eq (_,s1) (_,s2) = ( String.compare s1 s2 = 0 )

let ident_list_eq l1 l2 =
  try List.for_all2 ident_eq l1 l2
  with Invalid_argument _ -> false

exception Error of loc*string
