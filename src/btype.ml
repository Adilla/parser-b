type t = Btype_mt.t

type t_view =
  | T_Atomic of string
  | T_Power of t
  | T_Product of t * t
  | T_Record of (string*t) list

let mk_Atomic s = Btype_mt.T_Atomic s
let mk_Power ty = Btype_mt.T_Power ty
let mk_Product ty1 ty2 = Btype_mt.T_Product (ty1,ty2)
let mk_Record lst = Btype_mt.T_Record lst

let t_int = Btype_mt.t_int
let t_bool = Btype_mt.t_bool
let t_string = Btype_mt.t_string

let to_btype_mt x = x

let type_of_unary_fun = Btype_mt.type_of_unary_fun
let type_of_binary_fun = Btype_mt.type_of_binary_fun
let type_of_sequence = Btype_mt.type_of_sequence
let to_string = Btype_mt.to_string
let equal = Btype_mt.equal

let rec close_exn = function
  | Btype_mt.T_Atomic _ as ty -> ty
  | Btype_mt.T_Power ty -> Btype_mt.T_Power (close_exn ty)
  | Btype_mt.T_Product (ty1,ty2) -> Btype_mt.T_Product (close_exn ty1, close_exn ty2)
  | Btype_mt.T_Record lst -> Btype_mt.T_Record (List.map (fun (s,t) -> (s,close_exn t)) lst)
  | Btype_mt.T_Meta _ -> raise (Failure "close_exn")

let from_btype_mt ty = try Some (close_exn ty) with Failure _ -> None

let view = function
  | Btype_mt.T_Atomic x -> T_Atomic x
  | Btype_mt.T_Power ty -> T_Power ty
  | Btype_mt.T_Product (ty1,ty2) -> T_Product (ty1,ty2)
  | Btype_mt.T_Record lst -> T_Record lst
  | Btype_mt.T_Meta _ -> assert false
