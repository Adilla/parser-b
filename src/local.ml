module M = Map.Make(String)

type t_local_kind =
  | L_Expr_Binder
  | L_Subst_Binder
  | L_Param_In
  | L_Param_Out

type t = (Btype.Open.t*t_local_kind) M.t

let empty = M.empty

let add (ctx:t) (id:string) (ty:Btype.Open.t) (ro:t_local_kind) : t =
  M.add id (ty,ro) ctx

let get (ctx:t) (id:string) : (Btype.Open.t*t_local_kind) option =
  try Some (M.find id ctx)
  with Not_found -> None

let get_vars ctx = List.map fst (M.bindings ctx)
