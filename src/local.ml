module M = Map.Make(String)

type t_local_kind =
  | L_Expr_Binder
  | L_Subst_Binder
  | L_Param_In
  | L_Param_Out

type t_type =
  { kind:t_local_kind;
    mutable typ: Btype.t option }

type t = t_type M.t

let empty = M.empty

let declare (ctx:t) (id:string) (kind:t_local_kind) : t =
  M.add id { typ=None; kind } ctx

let declare_with_type (ctx:t) (id:string) (ty:Btype.t) (kind:t_local_kind) : t =
  M.add id { typ=Some ty; kind } ctx

let set_type (ctx:t) (id:string) (ty:Btype.t) : unit =
  match M.find_opt id ctx with
  | None -> ()
  | Some rc -> ( rc.typ <- Some ty )

let get (ctx:t) (id:string) : (Btype.t option*t_local_kind) option =
  match M.find_opt id ctx with
  | None -> None
  | Some rc -> Some (rc.typ,rc.kind)

let get_vars ctx = List.map fst (M.bindings ctx)
