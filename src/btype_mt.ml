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

let rec occurs2 (n:int) (t:t) : bool*bool =
  match t with
  | T_Atomic _ -> (false,true)
  | T_Power ty -> occurs2 n ty
  | T_Product (ty1,ty2) ->
    let occ1,isc1 = occurs2 n ty1 in
    if occ1 then (true,false)
    else
      let occ2,isc2 = occurs2 n ty2 in
      (occ2, isc1&&isc2)
  | T_Meta m -> (n=m,false)
  | T_Record lst -> 
    let rec aux (isc0:bool) : (_*t) list -> bool*bool = function
      | [] -> (false,isc0)
      | (_,hd)::tl ->
        let occ,isc = occurs2 n hd in
        if occ then (true,false)
        else aux (isc&&isc0) tl
    in
    aux true lst

let rec occurs n t =
  match t with
  | T_Atomic _ -> false
  | T_Power ty -> occurs n ty
  | T_Product (ty1,ty2) -> ( occurs n ty1 || occurs n ty2 )
  | T_Meta m -> n=m
  | T_Record lst ->  List.exists (fun (_,t) -> occurs n t) lst

let rec subst2 n rep ty =
  match ty with
  | T_Atomic _ as ty -> ty,true
  | T_Power ty -> let nty,is_cl = subst2 n rep ty in T_Power nty, is_cl
  | T_Product (ty1,ty2) ->
    let nty1, is_cl1 = subst2 n rep ty1 in
    let nty2, is_cl2 = subst2 n rep ty2 in
    T_Product (nty1,nty2), (is_cl1&&is_cl2)
  | T_Record lst ->
    let rec aux is_cl0 lst_rev = function
      | [] -> (List.rev lst_rev,is_cl0)
      | (id,hd)::tl ->
        let nty,is_cl = subst2 n rep hd in
        aux (is_cl0&&is_cl) ((id,nty)::lst_rev) tl
    in
    let lst,is_cl = aux true [] lst in
    (T_Record lst), is_cl
  | T_Meta m as ty -> if n=m then rep,true else ty,false

let rec subst n rep ty =
  match ty with
  | T_Atomic _ as ty -> ty
  | T_Power ty -> T_Power (subst n rep ty)
  | T_Product (ty1,ty2) -> T_Product (subst n rep ty1,subst n rep ty2)
  | T_Record lst -> T_Record (List.map (fun (s,ty) -> (s,subst n rep ty)) lst)
  | T_Meta m as ty -> if n=m then rep else ty

