type loc = Lexing.position
let dloc = Lexing.dummy_pos
type 'a non_empty_list = 'a*'a list

let list_eq f a b =
  try List.for_all2 f a b
  with Invalid_argument _ -> false

let nelist_eq f (hd1,tl1) (hd2,tl2) = list_eq f (hd1::tl1) (hd2::tl2)
