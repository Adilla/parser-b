type loc = Lexing.position
let dloc = Lexing.dummy_pos
type 'a non_empty_list = 'a*'a list

let list_eq f a b =
  try List.for_all2 f a b
  with Invalid_argument _ -> false

let nelist_eq f (hd1,tl1) (hd2,tl2) = list_eq f (hd1::tl1) (hd2::tl2)

let map_opt f = function
  | None -> None
  | Some x -> Some (f x)

let rev_filter_map f l =
  let rec loop l accum =
    match l with
    | [] -> accum
    | hd :: tl ->
      match f hd with
      | Some x -> loop tl (x :: accum)
      | None   -> loop tl accum
  in
  loop l []

let filter_map f l = List.rev (rev_filter_map f l)

let fold_map f acc l =
  let rec aux f acc map_acc l = match l with
    | [] -> acc, List.rev map_acc
    | x :: l' ->
      let acc, y = f acc x in
      aux f acc (y :: map_acc) l'
  in
aux f acc [] l
