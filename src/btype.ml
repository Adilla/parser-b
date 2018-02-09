module SMap = Map.Make(
  struct
    type t = string
    let compare = compare
  end )

module Unif =
struct
  type t =
    | T_Atomic of string
    | T_Power of t
    | T_Product of t * t
    | T_Record of (string*t) list
    | T_UVar of uv ref
  and uv = Unbound of int | Bound of t

  type t_alias = t SMap.t

  let t_int = T_Atomic "INTEGER"
  let t_bool = T_Atomic "BOOLEAN"
  let t_string = T_Atomic "STRING"

  let type_of_unary_fun t_arg t_res =
    T_Power (T_Product(t_arg,t_res))

  let type_of_binary_fun t_arg1 t_arg2 t_res =
    T_Power (T_Product(T_Product (t_arg1,t_arg2),t_res))

  let type_of_sequence ty = type_of_unary_fun t_int ty

  let rec to_string_np () ty =
    match ty with
    | T_Atomic s -> s
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
      | T_Atomic s1, T_Atomic s2 -> String.equal s1 s2
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

  let new_meta () =
    incr uv_count;
    T_UVar (ref (Unbound !uv_count))

  exception Not_Unifiable

  let rec occurs uv = function
    | T_Atomic _ -> false
    | T_Power ty -> occurs uv ty
    | T_Product (ty1,ty2) -> (occurs uv ty1 || occurs uv ty2)
    | T_Record lst -> List.exists (fun (_,ty) -> occurs uv ty) lst
    | T_UVar uv' -> ( uv == uv' )

  let rec unify_exn (alias:t_alias) t1 t2 =
    if t1 == t2 then ()
    else
      match t1, t2 with
      | T_Atomic s1, T_Atomic s2 when String.equal s1 s2 -> ()
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
      | T_Atomic s1, T_Atomic s2 ->
        begin match SMap.find_opt s1 alias, SMap.find_opt s2 alias with
          | None, None -> raise Not_Unifiable
          | None, Some t2 -> unify_exn alias t1 t2
          | Some t1, None -> unify_exn alias t1 t2
          | Some t1, Some t2 -> unify_exn alias t1 t2
        end
      | T_Atomic s , ty | ty, T_Atomic s ->
        begin match SMap.find_opt s alias with
          | None -> raise Not_Unifiable
          | Some ty' -> unify_exn alias ty ty'
        end
      | _, _ -> raise Not_Unifiable

  let rec normalize ty =
    match ty with
    | T_Atomic _ | T_UVar { contents=Unbound _ } -> ty
    | T_Power ty -> T_Power (normalize ty)
    | T_Product (ty1,ty2) -> T_Product (normalize ty1,normalize ty2)
    | T_Record lst -> T_Record (List.map (fun (s,ty) -> (s,normalize ty)) lst)
    | T_UVar { contents=Bound ty } -> normalize ty

  let get_stype alias t1 t2 =
    try unify_exn alias t1 t2; Some (normalize t1) with
    | Not_Unifiable -> None

end

type t = Unif.t
let to_unif x = x

let equal = Unif.equal
let to_string = Unif.to_string

let t_int = Unif.t_int
let t_bool = Unif.t_bool
let t_string = Unif.t_string

let mk_Atomic s = Unif.T_Atomic s
let mk_Power ty = Unif.T_Power ty
let mk_Product ty1 ty2 = Unif.T_Product (ty1,ty2)
let mk_Record lst = Unif.T_Record lst

let rec close_exn : Unif.t -> t = function
  | Unif.T_Atomic _ as ty -> ty
  | Unif.T_Power ty -> mk_Power (close_exn ty)
  | Unif.T_Product (ty1,ty2) -> mk_Product (close_exn ty1) (close_exn ty2)
  | Unif.T_Record lst -> mk_Record (List.map (fun (s,t) -> (s,close_exn t)) lst)
  | Unif.T_UVar {contents=Unif.Bound ty} -> close_exn ty
  | Unif.T_UVar _ -> raise (Failure "close_exn")

let close (ty:Unif.t) : t option = try Some (close_exn ty) with Failure _ -> None

type t_view =
  | T_Atomic of string
  | T_Power of t
  | T_Product of t * t
  | T_Record of (string*t) list

let view = function 
  | Unif.T_Atomic s -> T_Atomic s
  | Unif.T_Power ty -> T_Power ty
  | Unif.T_Product (ty1,ty2) -> T_Product (ty1,ty2)
  | Unif.T_Record lst -> T_Record lst
  | Unif.T_UVar _ -> assert false

type t_alias = Unif.t_alias
let no_alias = SMap.empty

let normalize_alias (alias:t_alias) (ty:t) : t =
  let rec loop : t -> t = fun ty ->
    match ty with
    | Unif.T_Atomic s ->
      begin match SMap.find_opt s alias with
        | None -> ty
        | Some ty' -> ty'
      end
    | Unif.T_Power ty -> mk_Power (loop ty)
    | Unif.T_Product (ty1,ty2) -> mk_Product (loop ty1) (loop ty2)
    | Unif.T_Record lst -> mk_Record (List.map (fun (x,ty) -> (x,loop ty)) lst)
    | Unif.T_UVar _ -> assert false
  in
  loop ty

let rec occurs_atm (str:string) (ty:t) : bool =
  match ty with
  | Unif.T_Atomic str2 -> String.equal str str2
  | Unif.T_Power ty -> occurs_atm str ty
  | Unif.T_Product (ty1,ty2) -> ( occurs_atm str ty1 || occurs_atm str ty2 )
  | Unif.T_Record lst ->  List.exists (fun (_,t) -> occurs_atm str t) lst
  | Unif.T_UVar _ -> assert false

let is_equal_modulo_alias (alias:t_alias) (t1:t) (t2:t) : bool =
  equal (normalize_alias alias t1) (normalize_alias alias t2)

let add_alias (s:t_alias) (alias:string) (ty:t) : t_alias option =
  let ty = normalize_alias s ty in
  if occurs_atm alias ty || SMap.mem alias s then None
  else Some (SMap.add alias ty s)
