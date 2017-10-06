type opn = Opn
type cls = Cls

type 'a typ =
  | T_Atomic : string -> 'a typ
  | T_Power : 'a typ -> 'a typ
  | T_Product : 'a typ * 'a typ -> 'a typ
  | T_Record : (string*'a typ) list -> 'a typ
  | T_Meta : int -> opn typ

type btype = cls typ
type opn_btype = opn typ

let t_int = T_Atomic "INTEGER"
let t_bool = T_Atomic "BOOLEAN"
let t_string = T_Atomic "STRING"

let type_of_unary_fun (t_arg:'a typ) (t_res:'a typ) : 'a typ =
  T_Power (T_Product(t_arg,t_res))

let type_of_binary_fun (t_arg1:'a typ) (t_arg2:'a typ) (t_res:'a typ) : 'a typ =
  T_Power (T_Product(T_Product (t_arg1,t_arg2),t_res))

let type_of_sequence (ty:'a typ) : 'a typ =
  type_of_unary_fun t_int ty

let rec to_string_np : type a. unit -> a typ -> string = fun () ty ->
  match ty with
  | T_Atomic s -> s
  | T_Power t -> Printf.sprintf "POW(%a)" to_string_np t
  | T_Product (t1,t2) -> Printf.sprintf "%a*%a" to_string_wp t1 to_string_wp t2
  | T_Meta i -> "?" ^ string_of_int i
  | T_Record lst -> Printf.sprintf "Struct(%a)" to_string_list lst

and to_string_wp : type a. unit -> a typ -> string = fun () ty ->
  match ty with
  | (T_Product _) as t -> Printf.sprintf "( %a )" to_string_np t
  | t ->  to_string_np () t

and to_string_list : type a. unit -> (string*a typ) list -> string = fun () lst ->
  match lst with
  | [] -> ""
  | [(x,ty)] -> Printf.sprintf "%s: %a" x to_string_np ty
  | (x,ty)::tl -> Printf.sprintf "%s: %a, %a" x to_string_np ty to_string_list tl

let to_string : type a. a typ -> string = fun x ->
  to_string_np () x

let rec is_equal : type a. a typ -> a typ -> bool = fun ty1 ty2 ->
  match ty1, ty2 with
  | T_Atomic s1, T_Atomic s2 -> String.equal s1 s2
  | T_Power a, T_Power b -> is_equal a b
  | T_Product (a1,b1), T_Product (a2,b2) -> is_equal a1 a2 && is_equal b1 b2
  | T_Meta n1, T_Meta n2 -> n1 == n2
  | T_Record lst1, T_Record lst2 ->
    let aux (s1,ty1) (s2,ty2) = String.equal s1 s2 && is_equal ty1 ty2 in
    ( try List.for_all2 aux lst1 lst2
      with Invalid_argument _ -> false )
  | _, _ -> false

let rec occurs : type a. int -> a typ -> bool = fun n t ->
  match t with
  | T_Atomic _ -> false
  | T_Power ty -> occurs n ty
  | T_Product (ty1,ty2) -> ( occurs n ty1 || occurs n ty2 )
  | T_Meta m -> n=m
  | T_Record lst ->  List.exists (fun (_,t) -> occurs n t) lst

let rec subst : type a. int -> a typ -> a typ -> a typ = fun n rep ty ->
  match ty with
  | T_Atomic _ as ty -> ty
  | T_Power ty -> T_Power (subst n rep ty)
  | T_Product (ty1,ty2) -> T_Product (subst n rep ty1,subst n rep ty2)
  | T_Record lst -> T_Record (List.map (fun (s,ty) -> (s,subst n rep ty)) lst)
  | T_Meta m as ty -> if n=m then rep else ty

let rec close_exn : type a. a typ -> btype = function
  | T_Atomic _ as ty -> ty
  | T_Power ty -> T_Power (close_exn ty)
  | T_Product (ty1,ty2) -> T_Product (close_exn ty1, close_exn ty2)
  | T_Record lst -> T_Record (List.map (fun (s,t) -> (s,close_exn t)) lst)
  | T_Meta _ -> raise (Failure "close_exn")

let close ty = try Some (close_exn ty) with Failure _ -> None

let rec to_open : type a. btype -> a typ = function
  | T_Atomic _ as ty -> ty
  | T_Power ty -> T_Power (to_open ty)
  | T_Product (ty1,ty2) -> T_Product (to_open ty1, to_open ty2)
  | T_Record lst -> T_Record (List.map (fun (s,ty) -> (s,to_open ty)) lst)

module Unif : sig
  type t
  val create : unit -> t
  val new_meta : t -> opn typ
  val get_stype : t -> opn typ -> opn typ -> (opn typ) option
  val normalize : t -> opn typ -> opn typ
end = struct

  module I = Map.Make(
    struct
      type t = int
      let compare = compare
    end )

  type t = { mutable fvar: int; mutable subst: (opn typ) I.t }

  let create () : t = { fvar=0; subst=I.empty }

  let new_meta (env:t) : opn typ =
    env.fvar <- env.fvar+1;
    T_Meta env.fvar

  let rec normalize (s:t) (ty:opn typ) : opn typ =
    match ty with
    | T_Atomic _ as ty -> ty
    | T_Power ty -> T_Power (normalize s ty)
    | T_Product (ty1,ty2) -> T_Product (normalize s ty1,normalize s ty2)
    | T_Meta m when I.mem m s.subst -> I.find m s.subst
    | T_Meta _ as ty -> ty
    | T_Record lst -> T_Record (List.map (fun (x,ty) -> (x,normalize s ty)) lst)

  let update_subst (env:t) (n:int) (rep:opn typ) : unit =
    env.subst <- I.map (subst n rep) env.subst;
    env.subst <- I.add n rep env.subst

  let rec get_stype (env:t) (t1:opn typ) (t2:opn typ) : (opn typ) option =
    match normalize env t1, normalize env t2 with
    | T_Atomic s1, T_Atomic s2 when String.equal s1 s2 -> Some (T_Atomic s1)
    | T_Power p1, T_Power p2 ->
      begin
        match get_stype env p1 p2 with
        | None -> None
        | Some p -> Some (T_Power p)
      end
    | T_Product (a1,b1), T_Product (a2,b2) ->
      begin
        match get_stype env a1 a2, get_stype env b1 b2 with
        | Some a, Some b -> Some (T_Product (a,b))
        | _, _ -> None
      end
    | T_Record lst1, T_Record lst2 ->
      begin
        let aux (s1,ty1) (s2,ty2) : string*opn typ =
          if String.equal s1 s2 then
            match get_stype env ty1 ty2 with
            | Some ty -> (s1,ty)
            | None -> raise (Failure "")
          else
            raise (Failure "")
        in
        ( try Some (T_Record (List.map2 aux lst1 lst2))
          with Failure _ | Invalid_argument _ -> None )
      end
    | T_Meta n, (T_Meta m as meta) when n=m -> Some meta
    | T_Meta n, ty | ty, T_Meta n ->
      if occurs n ty then None
      else (update_subst env n ty; Some ty)
    | _, _ -> None

end
