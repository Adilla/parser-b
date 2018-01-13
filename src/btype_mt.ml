type t =
| T_Atomic : string -> t
| T_Power : t -> t
| T_Product : t * t -> t
| T_Record : (string*t) list -> t
| T_Meta : int -> t

let t_int = T_Atomic "INTEGER"
let t_bool = T_Atomic "BOOLEAN"
let t_string = T_Atomic "STRING"

let type_of_unary_fun (t_arg:t) (t_res:t) : t =
  T_Power (T_Product(t_arg,t_res))

let type_of_binary_fun (t_arg1:t) (t_arg2:t) (t_res:t) : t =
  T_Power (T_Product(T_Product (t_arg1,t_arg2),t_res))

let type_of_sequence (ty:t) : t = type_of_unary_fun t_int ty

let rec to_string_np () ty =
  match ty with
  | T_Atomic s -> s
  | T_Power t -> Printf.sprintf "POW(%a)" to_string_np t
  | T_Product (t1,t2) -> Printf.sprintf "%a*%a" to_string_wp t1 to_string_wp t2
  | T_Meta i -> "?" ^ string_of_int i
  | T_Record lst -> Printf.sprintf "Struct(%a)" to_string_list lst

and to_string_wp () ty =
  match ty with
  | (T_Product _) as t -> Printf.sprintf "( %a )" to_string_np t
  | t ->  to_string_np () t

and to_string_list () lst =
  match lst with
  | [] -> ""
  | [(x,ty)] -> Printf.sprintf "%s: %a" x to_string_np ty
  | (x,ty)::tl -> Printf.sprintf "%s: %a, %a" x to_string_np ty to_string_list tl

let to_string x = to_string_np () x

let rec equal ty1 ty2 =
  match ty1, ty2 with
  | T_Atomic s1, T_Atomic s2 -> String.equal s1 s2
  | T_Power a, T_Power b -> equal a b
  | T_Product (a1,b1), T_Product (a2,b2) -> equal a1 a2 && equal b1 b2
  | T_Meta n1, T_Meta n2 -> n1 == n2
  | T_Record lst1, T_Record lst2 ->
    let aux (s1,ty1) (s2,ty2) = String.equal s1 s2 && equal ty1 ty2 in
    ( try List.for_all2 aux lst1 lst2
      with Invalid_argument _ -> false )
  | _, _ -> false

let rec occurs n t =
  match t with
  | T_Atomic _ -> false
  | T_Power ty -> occurs n ty
  | T_Product (ty1,ty2) -> ( occurs n ty1 || occurs n ty2 )
  | T_Meta m -> n=m
  | T_Record lst ->  List.exists (fun (_,t) -> occurs n t) lst

let rec subst n rep ty =
  match ty with
  | T_Atomic _ as ty -> ty
  | T_Power ty -> T_Power (subst n rep ty)
  | T_Product (ty1,ty2) -> T_Product (subst n rep ty1,subst n rep ty2)
  | T_Record lst -> T_Record (List.map (fun (s,ty) -> (s,subst n rep ty)) lst)
  | T_Meta m as ty -> if n=m then rep else ty
