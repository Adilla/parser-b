module SMap = Map.Make(
  struct
    type t = string
    let compare = compare
  end )

type t_atomic_src =
  | T_Current
  | T_Seen of string

module Open =
struct
  type t =
    | T_Int
    | T_Bool
    | T_String
    | T_Abstract_Set of t_atomic_src*string
    | T_Concrete_Set of t_atomic_src*string
    | T_Power of t
    | T_Product of t * t
    | T_Record of (string*t) list
    | T_UVar of uv ref
  and uv = Unbound of int | Bound of t

  type t_alias = t SMap.t

  let t_int = T_Int
  let t_bool = T_Bool
  let t_string = T_String

  let type_of_unary_fun t_arg t_res =
    T_Power (T_Product(t_arg,t_res))

  let type_of_binary_fun t_arg1 t_arg2 t_res =
    T_Power (T_Product(T_Product (t_arg1,t_arg2),t_res))

  let type_of_sequence ty = type_of_unary_fun t_int ty

  let rec to_string_np () ty =
    match ty with
    | T_Int -> "INTEGER"
    | T_Bool -> "BOOLEAN"
    | T_String -> "STRING"
    | T_Abstract_Set (T_Current,s) -> s
    | T_Abstract_Set (T_Seen mch,s) -> mch^"."^s
    | T_Concrete_Set (T_Current,s) -> s
    | T_Concrete_Set (T_Seen mch,s) -> mch^"."^s
    | T_Power t -> Printf.sprintf "POW(%a)" to_string_np t
    | T_Product (t1,t2) -> Printf.sprintf "%a*%a" to_string_wp t1 to_string_wp t2
    | T_UVar { contents=Unbound i } -> "?" ^ string_of_int i
    | T_UVar { contents=Bound t } -> to_string_np () t
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

  let to_string = to_string_np ()

  let rec equal ty1 ty2 =
    if ty1 == ty2 then true
    else
      match ty1, ty2 with
      | T_Abstract_Set (T_Current,s1), T_Abstract_Set (T_Current,s2)
      | T_Concrete_Set (T_Current,s1), T_Concrete_Set (T_Current,s2) -> String.equal s1 s2
      | T_Abstract_Set (T_Seen m1,s1), T_Abstract_Set (T_Seen m2,s2)
      | T_Concrete_Set (T_Seen m1,s1), T_Concrete_Set (T_Seen m2,s2) ->
        String.equal s1 s2 && String.equal m1 m2
      | T_Power a, T_Power b -> equal a b
      | T_Product (a1,b1), T_Product (a2,b2) -> equal a1 a2 && equal b1 b2
      | T_UVar { contents=Unbound n1 }, T_UVar {contents=Unbound n2} -> n1 == n2
      | T_Record lst1, T_Record lst2 ->
        let aux (s1,ty1) (s2,ty2) = String.equal s1 s2 && equal ty1 ty2 in
        ( try List.for_all2 aux lst1 lst2
          with Invalid_argument _ -> false )
      | T_UVar { contents=Bound ty1}, ty2 -> equal ty1 ty2
      | ty1, T_UVar { contents=Bound ty2} -> equal ty1 ty2
      | _, _ -> false

  let uv_count = ref 0

  let mk_Abstract_Set m s = T_Abstract_Set (m,s)
  let mk_Concrete_Set m s = T_Concrete_Set (m,s)
  let mk_Power ty = T_Power ty
  let mk_Product ty1 ty2 = T_Product (ty1,ty2)
  let mk_Record lst = T_Record lst

  let new_meta () =
    incr uv_count;
    T_UVar (ref (Unbound !uv_count))

  exception Not_Unifiable

  let rec occurs uv = function
    | T_Int
    | T_Bool
    | T_String
    | T_Abstract_Set _ -> false
    | T_Concrete_Set _ -> false
    | T_Power ty -> occurs uv ty
    | T_Product (ty1,ty2) -> (occurs uv ty1 || occurs uv ty2)
    | T_Record lst -> List.exists (fun (_,ty) -> occurs uv ty) lst
    | T_UVar uv' -> ( uv == uv' )

  let atm_src_eq m1 m2 =
    match m1, m2 with
    | T_Current, T_Current -> true
    | T_Seen s1, T_Seen s2 -> String.equal s1 s2
    | _, _ -> false

  let rec unify_exn (alias:t_alias) t1 t2 =
    if t1 == t2 then ()
    else
      match t1, t2 with
      | T_Abstract_Set (m1,s1), T_Abstract_Set (m2,s2) when String.equal s1 s2 && atm_src_eq m1 m2 -> ()
      | T_Concrete_Set (m1,s1), T_Concrete_Set (m2,s2) when String.equal s1 s2 && atm_src_eq m1 m2 -> ()
      | T_Power t1, T_Power t2 -> unify_exn alias t1 t2
      | T_Product (a1,b1), T_Product (a2,b2) -> ( unify_exn alias a1 a2; unify_exn alias b1 b2 )
      | T_Record lst1, T_Record lst2 ->
        let aux (s1,t1) (s2,t2) =
          if String.equal s1 s2 then unify_exn alias t1 t2
          else raise Not_Unifiable
        in
        ( try List.iter2 aux lst1 lst2
          with Invalid_argument _ -> raise Not_Unifiable)
      | T_UVar { contents=Bound t1 }, t2 | t1, T_UVar { contents=Bound t2 } ->
        unify_exn alias t1 t2
      | T_UVar ({ contents=Unbound _} as uv), ty | ty, T_UVar ({ contents=Unbound _} as uv) ->
        if occurs uv ty then raise Not_Unifiable
        else uv := Bound ty
      | T_Concrete_Set (T_Current,s1), T_Concrete_Set (T_Current,s2)
      | T_Abstract_Set (T_Current,s1), T_Abstract_Set (T_Current,s2) ->
        begin match SMap.find_opt s1 alias, SMap.find_opt s2 alias with
          | None, None -> raise Not_Unifiable
          | None, Some t2 -> unify_exn alias t1 t2
          | Some t1, None -> unify_exn alias t1 t2
          | Some t1, Some t2 -> unify_exn alias t1 t2
        end
      | T_Concrete_Set (T_Current,s) , ty | ty, T_Concrete_Set (T_Current,s)
      | T_Abstract_Set (T_Current,s) , ty | ty, T_Abstract_Set (T_Current,s) ->
        begin match SMap.find_opt s alias with
          | None -> raise Not_Unifiable
          | Some ty' -> unify_exn alias ty ty'
        end
      | _, _ -> raise Not_Unifiable

  let rec normalize ty =
    match ty with
    | T_Int | T_Bool | T_String | T_Abstract_Set _ | T_Concrete_Set _
    | T_UVar { contents=Unbound _ } -> ty
    | T_Power ty -> T_Power (normalize ty)
    | T_Product (ty1,ty2) -> T_Product (normalize ty1,normalize ty2)
    | T_Record lst -> T_Record (List.map (fun (s,ty) -> (s,normalize ty)) lst)
    | T_UVar { contents=Bound ty } -> normalize ty

  let get_stype alias t1 t2 =
    try unify_exn alias t1 t2; Some (normalize t1) with
    | Not_Unifiable -> None

end

type t = Open.t

let equal = Open.equal
let to_string = Open.to_string

let t_int = Open.t_int
let t_bool = Open.t_bool
let t_string = Open.t_string

let mk_Abstract_Set m s = Open.T_Abstract_Set (m,s)
let mk_Concrete_Set m s = Open.T_Concrete_Set (m,s)
let mk_Power ty = Open.T_Power ty
let mk_Product ty1 ty2 = Open.T_Product (ty1,ty2)
let mk_Record lst = Open.T_Record lst

let rec close_exn : Open.t -> t = function
  | Open.T_Int | Open.T_Bool | Open.T_String | Open.T_Abstract_Set _
  | Open.T_Concrete_Set _ as ty -> ty
  | Open.T_Power ty -> mk_Power (close_exn ty)
  | Open.T_Product (ty1,ty2) -> mk_Product (close_exn ty1) (close_exn ty2)
  | Open.T_Record lst -> mk_Record (List.map (fun (s,t) -> (s,close_exn t)) lst)
  | Open.T_UVar {contents=Open.Bound ty} -> close_exn ty
  | Open.T_UVar _ -> raise (Failure "close_exn")

let close (ty:Open.t) : t option = try Some (close_exn ty) with Failure _ -> None

type t_view =
  | T_Int
  | T_Bool
  | T_String
  | T_Abstract_Set of t_atomic_src*string
  | T_Concrete_Set of t_atomic_src*string
  | T_Power of t
  | T_Product of t * t
  | T_Record of (string*t) list

let view = function 
  | Open.T_Int -> T_Int
  | Open.T_Bool -> T_Bool
  | Open.T_String -> T_String
  | Open.T_Abstract_Set (m,s) -> T_Abstract_Set (m,s)
  | Open.T_Concrete_Set (m,s) -> T_Concrete_Set (m,s)
  | Open.T_Power ty -> T_Power ty
  | Open.T_Product (ty1,ty2) -> T_Product (ty1,ty2)
  | Open.T_Record lst -> T_Record lst
  | Open.T_UVar _ -> assert false

type t_alias = Open.t_alias
let no_alias = SMap.empty

let normalize_alias (alias:t_alias) (ty:t) : t =
  let rec loop : t -> t = fun ty ->
    match ty with
    | Open.T_Int | Open.T_Bool | Open.T_String
    | Open.T_Abstract_Set (T_Seen _,_)
    | Open.T_Concrete_Set (T_Seen _,_) -> ty
    | Open.T_Concrete_Set (T_Current,s)
    | Open.T_Abstract_Set (T_Current,s) ->
      begin match SMap.find_opt s alias with
        | None -> ty
        | Some ty' -> ty'
      end
    | Open.T_Power ty -> mk_Power (loop ty)
    | Open.T_Product (ty1,ty2) -> mk_Product (loop ty1) (loop ty2)
    | Open.T_Record lst -> mk_Record (List.map (fun (x,ty) -> (x,loop ty)) lst)
    | Open.T_UVar _ -> assert false
  in
  loop ty

let rec occurs_atm (str:string) (ty:t) : bool =
  match ty with
  | Open.T_Int | Open.T_Bool | Open.T_String | Open.T_Concrete_Set _
  | Open.T_Abstract_Set (T_Seen _,_) -> false
  | Open.T_Abstract_Set (T_Current,str2) -> String.equal str str2
  | Open.T_Power ty -> occurs_atm str ty
  | Open.T_Product (ty1,ty2) -> ( occurs_atm str ty1 || occurs_atm str ty2 )
  | Open.T_Record lst ->  List.exists (fun (_,t) -> occurs_atm str t) lst
  | Open.T_UVar _ -> assert false

let is_equal_modulo_alias (alias:t_alias) (t1:t) (t2:t) : bool =
  equal (normalize_alias alias t1) (normalize_alias alias t2)

let add_alias (s:t_alias) (alias:string) (ty:t) : t_alias option =
  let ty = normalize_alias s ty in
  if occurs_atm alias ty || SMap.mem alias s then None
  else Some (SMap.add alias ty s)

let change_current (src:t_atomic_src) (ty:Open.t) : Open.t =
  let rec aux = function
  | Open.T_Abstract_Set (T_Current,s) -> Open.T_Abstract_Set(src,s)
  | Open.T_Concrete_Set (T_Current,s) -> Open.T_Concrete_Set(src,s)
  | Open.T_Int | Open.T_Bool | Open.T_String
  | Open.T_Abstract_Set (T_Seen _,_)
  | Open.T_Concrete_Set (T_Seen _,_) as ty -> ty
  | Open.T_Power ty -> Open.T_Power (aux ty)
  | Open.T_Product (ty1,ty2) -> Open.T_Product (aux ty1,aux ty2)
  | Open.T_Record lst -> Open.T_Record (List.map (fun (s,ty) -> (s,aux ty)) lst)
  | Open.T_UVar _ -> assert false
  in
  aux ty
