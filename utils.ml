let bind u f =
  match u with
  | Ok a -> f a
  | Error a -> Error a

let (>>=) = bind

type loc = Lexing.position
let dloc = Lexing.dummy_pos
type ident = loc*string
type 'a non_empty_list = 'a*'a list

let ident_eq (_,s1) (_,s2) = ( String.compare s1 s2 = 0 )
