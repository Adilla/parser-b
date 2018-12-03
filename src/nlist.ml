type 'a t = 'a*'a list

let lb_equal ~p (hd1,tl1) (hd2,tl2) =
  Utils.list_eq p (hd1::tl1) (hd2::tl2)

let equal p (hd1,tl1) (hd2,tl2) =
  Utils.list_eq p (hd1::tl1) (hd2::tl2)

let make hd tl = (hd,tl)

let make1 hd = (hd,[])

let to_list (hd,tl) = hd::tl

let cons x (hd,tl) = (x,hd::tl)

let from_list = function
  | [] -> None
  | hd::tl -> Some (hd,tl)

let from_list_exn = function
  | [] -> raise (Failure "from_list_exn")
  | hd::tl -> (hd,tl)

let hd = fst
let tl = snd

let lb_map ~f (hd,tl) =
  let hd = f hd in
  let tl = List.map f tl in
  (hd,tl)

let map f (hd,tl) =
  let hd = f hd in
  let tl = List.map f tl in
  (hd,tl)

let rev (hd,tl) = from_list_exn (List.rev (hd::tl))
