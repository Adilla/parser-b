type lident = SyntaxCore.lident
type ren_ident = SyntaxCore.ren_ident
type loc = Utils.loc

type t_param_kind = Set | Scalar

type 'a t_symbol_infos = {
  sy_typ:Btype.t;
  sy_kind:'a
}

type 'a t_operation_infos =
  { op_args_in: (string*Btype.t) list;
    op_args_out: (string*Btype.t) list;
    op_readonly:bool;
    op_src: 'a; }

type t_global_kind =
  | K_Parameter  of t_param_kind
  | K_Abstract_Variable
  | K_Abstract_Constant
  | K_Concrete_Variable
  | K_Concrete_Constant
  | K_Abstract_Set
  | K_Concrete_Set of string list
  | K_Enumerate

module MachineInterface :
sig
  type t
  type t_symb = { id:string; typ:Btype.t; kind:t_global_kind }
  type t_op = { id:string; args_in: (string*Btype.t) list; args_out: (string*Btype.t) list; readonly:bool }
  type t_param = { id:string; typ:Btype.t; kind:t_param_kind }
  val make : t_param list -> t_symb list -> t_op list -> t
  val get_symbols : t -> t_symb list
  val get_operations : t -> t_op list
  val get_params : t -> t_param list
end = struct
  type t_symb = { id:string; typ:Btype.t; kind:t_global_kind }
  type t_op = { id:string; args_in: (string*Btype.t) list; args_out: (string*Btype.t) list; readonly:bool }
  type t_param = { id:string; typ:Btype.t; kind:t_param_kind }
  type t = t_param list * t_symb list * t_op list

  let make l0 l1 l2 = (l0,l1,l2)
  let get_symbols (_,l,_) = l
  let get_operations (_,_,l) = l
  let get_params (l,_,_) = l
end

type t_interface = MachineInterface.t

let check_args_type alias (err_loc:loc) (args_old:(string*Btype.t)list) (args_new:(string*Btype.t)list) : unit =
  let aux (x1,ty1) (x2,ty2) =
    if String.equal x1 x2 then
      if Btype.is_equal_modulo_alias alias ty1 ty2 then ()
      else Error.error err_loc
          ("The parameter '"^x1^"' has type '"^Btype.to_string ty1^
           "' but parameter of type '"^Btype.to_string ty2^"' was expected.")
    else Error.error err_loc ("Parameter '"^x2^"' expected but '"^x1^"' was found.")
  in
  try List.iter2 aux args_old args_new;
  with Invalid_argument _ -> Error.error err_loc "Unexpected number of parameters."

let is_in_deps (deps:string list) (mch:string) : bool =
  List.exists (String.equal mch) deps

let rec find_duplicate : (string*'a) list -> string option = function
  | [] -> None
  | (x,_)::tl ->
    let aux (y,_) = String.equal x y in
    if List.exists aux tl then Some x
    else find_duplicate tl

type ('sy_ki,'op_ki) env = {
  mutable deps: string list;
  params: lident list;
  mutable alias:Btype.t_alias;
  symb:(string,'sy_ki t_symbol_infos) Hashtbl.t;
  ops:(string,'op_ki t_operation_infos) Hashtbl.t
}

let get_alias env = env.alias
let get_symbol (env:_ env) (id:string) : _ t_symbol_infos option =
  Hashtbl.find_opt env.symb id
let get_operation (env:_ env) (id:string) : _ t_operation_infos option =
  Hashtbl.find_opt env.ops id

module Mch = struct

  type t_source =
    | Machine of loc
    | Seen of ren_ident
    | Used of ren_ident
    | Included of ren_ident

  type t_kind =
    | Parameter of t_param_kind*loc
    | Abstract_Variable of t_source
    | Abstract_Constant of t_source
    | Concrete_Variable of t_source
    | Concrete_Constant of t_source
    | Abstract_Set of t_source
    | Concrete_Set of string list * t_source
    | Enumerate of t_source

  type t_op_decl =
    | O_Machine of loc
    | O_Seen of ren_ident
    | O_Used of ren_ident
    | O_Included of ren_ident
    | O_Included_And_Promoted of ren_ident

  type t = (t_kind,t_op_decl) env

  let create params : t =
    { deps=[];
      params;
      alias=Btype.no_alias;
      symb=Hashtbl.create 47;
      ops=Hashtbl.create 47 }

  
  let _add_symbol (env:t) (l:loc) (id:string) (sy_typ:Btype.t) (ki: t_global_kind) (src:t_source) : unit = (*FIXME*)
    match Hashtbl.find_opt env.symb id with
    | Some _ ->
      Error.error l ("The identifier '" ^ id ^ "' clashes with previous declaration.")
    | None ->
      let sy_kind = match ki with
        | K_Parameter k -> Parameter (k,l)
        | K_Abstract_Variable -> Abstract_Variable src
        | K_Abstract_Constant -> Abstract_Constant src
        | K_Concrete_Variable -> Concrete_Variable src
        | K_Concrete_Constant -> Concrete_Constant src
        | K_Abstract_Set -> Abstract_Set src
        | K_Concrete_Set elts -> Concrete_Set (elts,Machine l)
        | K_Enumerate -> Enumerate (Machine l)
      in
      Hashtbl.add env.symb id { sy_typ; sy_kind}

  let add_symbol (env:t) (l:loc) (id:string) (sy_typ:Btype.t) (ki: t_global_kind) : unit = (*FIXME*)

  let _add_operation (env:t) (err_loc:loc) (id:string) (op_args_in:(string*Btype.t)list)
      (op_args_out:(string*Btype.t)list) (op_readonly:bool) (src:t_source) : unit =
    match Hashtbl.find_opt env.ops id with
    | Some _ ->
      Error.error err_loc ("The operation '" ^ id ^ "' clashes with previous declaration.") 
    | None ->
      let op_src = match src with
        | Machine l -> O_Machine l
        | Seen mch -> O_Seen mch
        | Used mch -> O_Used mch
        | Included mch -> O_Included mch
      in
      Hashtbl.add env.ops id { op_args_in; op_args_out; op_readonly; op_src }

  let add_operation (env:t) (loc:loc) (id:string) (args_in) (args_out) ~is_readonly : unit =
    match find_duplicate (args_in@args_out) with
    | None -> _add_operation env loc id args_in args_out is_readonly (Machine loc)
    | Some arg ->
      Error.error loc ("The argument '"^arg^"' appears twice in this operation declaration.")

  let promote_operation (env:t) (lc:loc) (id:string) : unit =
    match get_operation env id with
    | None -> Error.error lc ("Unknown operation '"^id^"'.")
    | Some ({ op_src=O_Included mch; _ } as op) ->
      Hashtbl.replace env.ops id { op with op_src=O_Included_And_Promoted mch}
    | Some { op_src=O_Included_And_Promoted _; _ } -> assert false (*FIXME*)
    | Some _ ->
      Error.error lc ("The operation '"^id^"' is not an operation of an included machine.")

(*
  let mk_kind (kind:MachineInterface.t_kind) src = match kind with
    | MachineInterface.Abstract_Variable -> Abstract_Variable src
    | MachineInterface.Abstract_Constant -> Abstract_Constant src
    | MachineInterface.Concrete_Variable -> Concrete_Variable src
    | MachineInterface.Concrete_Constant -> Concrete_Constant src
    | MachineInterface.Abstract_Set -> Abstract_Set src
    | MachineInterface.Concrete_Set elts -> Concrete_Set (elts,src)
    | MachineInterface.Enumerate -> Enumerate src
*)

  let load_external_symbol (env:t) (mch:ren_ident) (src:t_source)
      ({id;typ;kind}:MachineInterface.t_symb) : unit =
    let mk_id id = match mch.r_prefix with
      | None -> id
      | Some p -> p ^ "." ^ id 
    in
    match kind with
    | MachineInterface.Abstract_Variable
    | MachineInterface.Concrete_Variable ->
      add_symbol env mch.SyntaxCore.r_loc (mk_id id)
        (Btype.change_current (Btype.T_Ext mch.SyntaxCore.r_str) typ)
        kind src
    | _ ->
      if (is_in_deps env.deps mch.SyntaxCore.r_str) then ()
      else
        add_symbol env mch.SyntaxCore.r_loc id
          (Btype.change_current (Btype.T_Ext mch.SyntaxCore.r_str) typ)
          (mk_kind kind src)

  let load_external_op (env:t) (mch:ren_ident) (src:t_source)
      (op:MachineInterface.t_op) : unit =
    let change_current = List.map (fun (s,ty) ->
        (s,Btype.change_current (Btype.T_Ext mch.SyntaxCore.r_str) ty)
      ) in
    let op_name = match mch.SyntaxCore.r_prefix with
      | None -> op.id
      | Some p -> p ^ "." ^ op.id
    in
    _add_operation env mch.SyntaxCore.r_loc op_name (change_current op.args_in)
      (change_current op.args_out) op.readonly src

  let load_interface_for_seen_machine (env:t) (itf:t_interface) (mch:ren_ident) : unit =
    (*FIXME check not already in deps*)
    let open MachineInterface in
    List.iter (load_external_symbol env mch (Seen mch)) (get_symbols itf);
    List.iter (load_external_op env mch (Seen mch)) (get_operations itf);
    env.deps <- mch.r_str::env.deps

  let load_interface_for_used_machine (env:t) (itf:t_interface) (mch:ren_ident) : unit =
    (*FIXME check not already in deps*)
    let open MachineInterface in
    List.iter (load_external_symbol env mch (Used mch)) (get_symbols itf);
    env.deps <- mch.r_str::env.deps

  let load_interface_for_included_machine (env:t) (itf:t_interface) (mch:ren_ident) : unit =
    (*FIXME check not already in deps*)
    let open MachineInterface in
    List.iter (load_external_symbol env mch (Included mch)) (get_symbols itf);
    List.iter (load_external_op env mch (Included mch)) (get_operations itf);
    env.deps <- mch.r_str::env.deps

  let load_interface_for_extended_machine (env:t) (itf:t_interface) (mch:ren_ident) : unit =
    let open MachineInterface in
    load_interface_for_included_machine env itf mch;
    List.iter (fun (r:t_op) ->
        promote_operation env mch.SyntaxCore.r_loc r.id
      ) (get_operations itf)

  let to_interface (env:t) : t_interface =
    let get_symbols (x:string) (symb:t_kind t_symbol_infos) (lst:MachineInterface.t_symb list) =
      match symb.sy_kind with
      | Parameter (_, _) -> lst
      | Abstract_Variable (Machine _|Included _)->
        { id=x; typ=symb.sy_typ; kind=Abstract_Variable }::lst
      | Abstract_Constant (Machine _|Included _) ->
        { id=x; typ=symb.sy_typ; kind=Abstract_Constant }::lst
      | Concrete_Variable (Machine _|Included _) ->
        { id=x; typ=symb.sy_typ; kind=Concrete_Variable }::lst
      | Concrete_Constant (Machine _|Included _) ->
        { id=x; typ=symb.sy_typ; kind=Concrete_Constant }::lst
      | Abstract_Set (Machine _|Included _) ->
        { id=x; typ=symb.sy_typ; kind=Abstract_Set }::lst
      | Concrete_Set (elts, (Machine _|Included _)) ->
        { id=x; typ=symb.sy_typ; kind=Concrete_Set elts }::lst
      | Enumerate (Machine _|Included _) ->
        { id=x; typ=symb.sy_typ; kind=Enumerate }::lst
      | _ -> lst
    in
    let get_operations (id:string) (op:t_op_decl t_operation_infos) lst =
      match op.op_src with
      | O_Machine _ | O_Included_And_Promoted _ ->
        { MachineInterface.id; args_in=op.op_args_in;
          args_out=op.op_args_out; readonly=op.op_readonly }::lst
      | _ -> lst
    in
    let get_params (lid:lident) : MachineInterface.t_param =
      let id = lid.SyntaxCore.lid_str in
      match Hashtbl.find_opt env.symb id with
      | None -> assert false
      | Some infos ->
        begin match infos.sy_kind with
          | Parameter (kind,_) ->
            { MachineInterface.id; typ=infos.sy_typ; kind }
          | _ -> assert false
        end
    in
    let params:MachineInterface.t_param list = List.map get_params env.params in
    let symbs = Hashtbl.fold get_symbols env.symb [] in
    let ops = Hashtbl.fold get_operations env.ops [] in
    MachineInterface.make params symbs ops
end

module Ref = struct

  type t_source =
    | Machine of loc
    | Seen of ren_ident
    | Refined
    | Included of ren_ident

  type t_source_2 =
    | A_Machine of loc
    | A_Seen of ren_ident
    | A_Refined
    | A_Included of ren_ident
    | A_Redeclared_In_Machine of loc
    | A_Redeclared_In_Included of ren_ident

  type t_kind =
    | Parameter of t_param_kind*loc
    | Abstract_Variable of t_source_2
    | Abstract_Constant of t_source_2
    | Concrete_Variable of t_source_2
    | Concrete_Constant of t_source_2
    | Abstract_Set of t_source
    | Concrete_Set of string list * t_source
    | Enumerate of t_source

  type t_op_decl =
    | O_Refined
    | O_Refined_And_Machine of loc
    | O_Seen of ren_ident
    | O_Included of ren_ident
    | O_Refined_And_Included of ren_ident
    | O_Refined_Included_And_Promoted of ren_ident

  type t = (t_kind,t_op_decl) env

  let create params : t =
    { deps=[];
      params;
      alias=Btype.no_alias;
      symb=Hashtbl.create 47;
      ops=Hashtbl.create 47 }

(*
  let update_kind (decl:t_kind) (ki:t_global_kind) (src:t_source) : t_kind option =
    match decl, ki with
    | Abstract_Variable src2, K_Abstract_Variable ->
      begin match src2, src with
        | A_Refined, Machine l ->
          Some (Abstract_Variable (A_Redeclared_In_Machine l))
        | A_Refined, Included mch ->
          Some (Abstract_Variable (A_Redeclared_In_Included mch))
        | _, _ -> None
      end
    | Abstract_Variable src2, K_Concrete_Variable ->
      begin match src2, src with
        | A_Refined, Machine l ->
          Some (Concrete_Variable (A_Redeclared_In_Machine l))
        | A_Refined, Included mch->
          Some (Concrete_Variable (A_Redeclared_In_Included mch))
        | _, _ -> None
      end
    | Abstract_Constant src2, K_Abstract_Constant ->
      begin match src2, src with
        | A_Refined, Machine l ->
          Some (Abstract_Constant (A_Redeclared_In_Machine l))
        | A_Refined, Included mch ->
          Some (Abstract_Constant (A_Redeclared_In_Included mch))
        | _, _ -> None
      end
    | Abstract_Constant src2, K_Concrete_Constant ->
      begin match src2, src with
        | A_Refined, Machine l ->
          Some (Concrete_Constant (A_Redeclared_In_Machine l))
        | A_Refined, Included mch->
          Some (Concrete_Constant (A_Redeclared_In_Included mch))
        | _, _ -> None
      end
    | _ -> None

  let to_source_2 = function
    | Machine l -> A_Machine l
    | Seen mch -> A_Seen mch
    | Refined -> A_Refined
    | Included mch -> A_Included mch

  let mk_kind (kind:t_global_kind) (src:t_source) : t_kind =
    match kind with
    | K_Abstract_Variable -> Abstract_Variable (to_source_2 src)
    | K_Abstract_Constant -> Abstract_Constant (to_source_2 src)
    | K_Concrete_Variable -> Concrete_Variable (to_source_2 src)
    | K_Concrete_Constant -> Concrete_Constant (to_source_2 src)
    | K_Abstract_Set -> Abstract_Set src
    | K_Concrete_Set elts -> Concrete_Set (elts,src)
    | K_Enumerate -> Enumerate src
*)

  let add_symbol (env:t) (err_loc:loc) (id:string) (sy_typ:Btype.t)
      (ki:t_global_kind) (src:t_source) : unit =
    match Hashtbl.find_opt env.symb id with
    | Some infos ->
      let sy_kind = match update_kind infos.sy_kind ki src with
        | Some x -> x
        | None ->
          Error.error err_loc ("The identifier '" ^ id ^ "' clashes with previous declaration.")
      in
      if not (Btype.is_equal_modulo_alias Btype.no_alias infos.sy_typ sy_typ) then
        Error.error  err_loc
          ("The identifier '" ^ id ^ "' has type " 
           ^ Btype.to_string sy_typ
           ^ " but was previously declared with type "
           ^ Btype.to_string infos.sy_typ ^ ".")
      else
        Hashtbl.replace env.symb id { sy_typ; sy_kind }
    | None ->
      Hashtbl.add env.symb id { sy_typ; sy_kind=mk_kind ki src }

  let get_operation (env:t) (id:string) : t_op_decl t_operation_infos option =
    Hashtbl.find_opt env.ops id

  let update_op_source (old_src:t_op_decl) (new_src:t_source) : t_op_decl option =
    match old_src, new_src with
    | O_Refined, Machine lc -> Some (O_Refined_And_Machine lc)
    | O_Included mch, Refined
    | O_Refined, Included mch -> Some (O_Refined_And_Included mch)
    | _, _ -> None

  let _add_operation (env:t) (err_loc:loc) (id:string) (op_args_in:(string*Btype.t)list)
      (op_args_out:(string*Btype.t)list) (src:t_source) : unit =
    match Hashtbl.find_opt env.ops id with
    | Some infos ->
      begin match update_op_source infos.op_src src with
        | None ->
          Error.error err_loc ("The operation '" ^ id ^ "' clashes with previous declaration.") 
        | Some op_src ->
          ( check_args_type Btype.no_alias err_loc infos.op_args_in op_args_in;
            check_args_type Btype.no_alias err_loc infos.op_args_out op_args_out;
            Hashtbl.replace env.ops id { infos with op_src } )
      end
    | None ->
      let op_src = match src with
        | Refined -> O_Refined
        | Seen mch -> O_Seen mch
        | Included mch -> O_Included mch
        | Machine _ -> assert false (*FIXME*)
      in
      Hashtbl.add env.ops id { op_args_in; op_args_out; op_readonly=false; op_src }

  let add_operation (env:t) (loc:loc) (id:string) (args_in:(string*Btype.t) list)
      (args_out:(string*Btype.t) list) : unit
    =
    match find_duplicate (args_in@args_out) with
    | None -> _add_operation env loc id args_in args_out (Machine loc)
    | Some arg ->
      Error.error loc ("The argument '"^arg^"' appears twice in this operation declaration.")

  let promote_operation (env:t) (loc:loc) (id:string) : unit =
    match get_operation env id with
    | None -> Error.error loc ("Unknown operation '"^id^"'.")
    | Some ({ op_src=O_Refined_And_Included mch; _ } as op) ->
      Hashtbl.replace env.ops id { op with op_src=O_Refined_Included_And_Promoted mch}
    | Some { op_src=O_Included _; _ } -> assert false (*FIXME*)
    | Some { op_src=O_Refined_Included_And_Promoted _; _ } -> assert false (*FIXME*)
    | Some _ ->
      Error.error loc ("The operation '"^id^"' is not an operation of an included machine.")

  let load_external_symbol (env:t) (mch:ren_ident) (src:t_source)
      ({id;typ;kind}:MachineInterface.t_symb) : unit =
    let open MachineInterface in
    let mk_id id = match mch.r_prefix with
      | None -> id
      | Some p -> p ^ "." ^ id 
    in
    match kind with
    | K_Abstract_Variable | K_Concrete_Variable ->
      add_symbol env mch.SyntaxCore.r_loc (mk_id id)
        (Btype.change_current (Btype.T_Ext mch.SyntaxCore.r_str) typ) kind src
    | _ ->
      if (is_in_deps env.deps mch.SyntaxCore.r_str) then ()
      else
        add_symbol env mch.SyntaxCore.r_loc id
          (Btype.change_current (Btype.T_Ext mch.SyntaxCore.r_str) typ) kind src

  let load_external_op (env:t) (mch:ren_ident) (src:t_source)
      (op:MachineInterface.t_op) : unit =
    let change_current = List.map (fun (s,ty) ->
        (s,Btype.change_current (Btype.T_Ext mch.SyntaxCore.r_str) ty)
      ) in
    let op_name = match mch.SyntaxCore.r_prefix with
      | None -> op.id
      | Some p -> p ^ "." ^ op.id
    in
    _add_operation env mch.SyntaxCore.r_loc op_name (change_current op.args_in)
      (change_current op.args_out) src

  let load_interface_for_seen_machine (env:t) (itf:t_interface) (mch:ren_ident) : unit =
    (*FIXME check not already in deps*)
    let open MachineInterface in
    List.iter (load_external_symbol env mch (Seen mch)) (get_symbols itf);
    List.iter (load_external_op env mch (Seen mch)) (get_operations itf);
    env.deps <- mch.r_str::env.deps

  let load_interface_for_refined_machine (env:t) (itf:t_interface) (mch:lident) : unit =
    (*FIXME check not already in deps*)
    let open MachineInterface in
    let ren_mch = { SyntaxCore.r_prefix=None; r_str=mch.lid_str; r_loc=mch.lid_loc } in
    List.iter (load_external_symbol env ren_mch Refined) (get_symbols itf);
    List.iter (load_external_op env ren_mch Refined) (get_operations itf);
    env.deps <- mch.lid_str::env.deps

  let load_interface_for_included_machine (env:t) (itf:t_interface) (mch:ren_ident) : unit =
    (*FIXME check not already in deps*)
    let open MachineInterface in
    List.iter (load_external_symbol env mch (Included mch)) (get_symbols itf);
    List.iter (load_external_op env mch (Included mch)) (get_operations itf);
    env.deps <- mch.r_str::env.deps

  let load_interface_for_extended_machine (env:t) (itf:t_interface) (mch:ren_ident) : unit =
    let open MachineInterface in
    load_interface_for_included_machine env itf mch;
    List.iter (fun (r:t_op) ->
        promote_operation env mch.SyntaxCore.r_loc r.id
      ) (get_operations itf)

  let to_interface (env:t) : t_interface =
    let get_symbols (x:string) (symb:t_kind t_symbol_infos) (lst:MachineInterface.t_symb list) =
      match symb.sy_kind with
      | Parameter (_, _) -> lst
      | Abstract_Variable (A_Machine _|A_Included _
                          |A_Redeclared_In_Machine _|A_Redeclared_In_Included _)->
        { id=x; typ=symb.sy_typ; kind=K_Abstract_Variable }::lst
      | Abstract_Constant (A_Machine _|A_Included _
                          |A_Redeclared_In_Machine _|A_Redeclared_In_Included _) ->
        { id=x; typ=symb.sy_typ; kind=K_Abstract_Constant }::lst
      | Concrete_Variable (A_Machine _|A_Included _|A_Refined
                          |A_Redeclared_In_Machine _|A_Redeclared_In_Included _) ->
        { id=x; typ=symb.sy_typ; kind=K_Concrete_Variable }::lst
      | Concrete_Constant (A_Machine _|A_Included _|A_Refined
                          |A_Redeclared_In_Machine _|A_Redeclared_In_Included _) ->
        { id=x; typ=symb.sy_typ; kind=K_Concrete_Constant }::lst
      | Abstract_Set (Machine _|Included _|Refined) ->
        { id=x; typ=symb.sy_typ; kind=K_Abstract_Set }::lst
      | Concrete_Set (elts, (Machine _|Included _|Refined)) ->
        { id=x; typ=symb.sy_typ; kind=K_Concrete_Set elts }::lst
      | Enumerate (Machine _|Included _|Refined) ->
        { id=x; typ=symb.sy_typ; kind=K_Enumerate }::lst
      | _ -> lst
    in
    let get_operations (id:string) (op:t_op_decl t_operation_infos) lst =
      match op.op_src with
      | O_Refined_And_Machine _ | O_Refined | O_Refined_Included_And_Promoted _ ->
        { MachineInterface.id; args_in=op.op_args_in;
          args_out=op.op_args_out; readonly=op.op_readonly }::lst
      | O_Seen _ | O_Included _ | O_Refined_And_Included _ -> lst
    in
    let get_params (lid:lident) : MachineInterface.t_param =
      let id = lid.SyntaxCore.lid_str in
      match Hashtbl.find_opt env.symb id with
      | None -> assert false
      | Some infos ->
        begin match infos.sy_kind with
          | Parameter (kind,_) ->
            { MachineInterface.id; typ=infos.sy_typ; kind }
          | _ -> assert false
        end
    in
    let params:MachineInterface.t_param list = List.map get_params env.params in
    let symbs = Hashtbl.fold get_symbols env.symb [] in
    let ops = Hashtbl.fold get_operations env.ops [] in
    MachineInterface.make params symbs ops
end

module Imp = struct

  type t_source =
    | Machine of loc
    | Seen of ren_ident
    | Refined
    | Imported of ren_ident

  type t_abstract_decl =
    | A_Seen of ren_ident
    | A_Refined
    | A_Imported of ren_ident
    | A_Redeclared_In_Imported of ren_ident

  type t_concrete_var_decl =
    | V_Machine of loc
    | V_Seen of ren_ident
    | V_Refined
    | V_Imported of ren_ident
    | V_Redeclared_In_Imported of ren_ident

  type t_concrete_const_decl =
    | C_Machine of loc
    | C_Seen of ren_ident
    | C_Refined
    | C_Imported of ren_ident
    | C_Redeclared_In_Seen of ren_ident
    | C_Redeclared_In_Imported of ren_ident

  type t_kind =
    | Parameter of t_param_kind*loc
    | Abstract_Variable of t_abstract_decl
    | Abstract_Constant of t_abstract_decl
    | Concrete_Variable of t_concrete_var_decl
    | Concrete_Constant of t_concrete_const_decl
    | Abstract_Set of t_concrete_const_decl
    | Concrete_Set of string list * t_concrete_const_decl
    | Enumerate of t_concrete_const_decl

  type t_op_decl =
    | O_Current of loc
    | O_Seen of ren_ident
    | O_Imported of ren_ident
    | O_Imported_And_Promoted of ren_ident*loc
    | O_Refined
    | O_Current_And_Refined of loc
    | O_Imported_And_Refined of ren_ident
    | O_Imported_Promoted_And_Refined of ren_ident*loc
    | O_Local_Spec of loc
    | O_Local_Spec_And_Implem of loc*loc

  type t = (t_kind,t_op_decl) env

  let create params : t =
    { params;
      deps=[];
      alias=Btype.no_alias;
      symb=Hashtbl.create 47;
      ops=Hashtbl.create 47 }


  let add_alias (s:t) (alias:string) (ty:Btype.t) : bool =
    match Btype.add_alias s.alias alias ty with
    | None -> false
    | Some alias -> (s.alias <- alias; true)

  let to_abstract_decl (src:t_source) : t_abstract_decl =
    match src with
     | Machine _ -> assert false (*FIXME*)
     | Seen mch -> A_Seen mch
     | Refined -> A_Refined
     | Imported mch -> A_Imported mch

  let to_concrete_var_decl (src:t_source) : t_concrete_var_decl =
    match src with
     | Machine l -> V_Machine l 
     | Seen mch -> V_Seen mch
     | Refined -> V_Refined
     | Imported mch -> V_Imported mch 

  let to_concrete_const_decl (src:t_source) : t_concrete_const_decl =
    match src with
     | Machine l -> C_Machine l
     | Seen mch -> C_Seen mch
     | Refined -> C_Refined
     | Imported mch -> C_Imported mch

  let mk_kind (kind:t_global_kind) (src:t_source) : t_kind =
    match kind with
    | K_Abstract_Variable -> Abstract_Variable (to_abstract_decl src)
    | K_Abstract_Constant -> Abstract_Constant (to_abstract_decl src)
    | K_Concrete_Variable -> Concrete_Variable (to_concrete_var_decl src)
    | K_Concrete_Constant -> Concrete_Constant (to_concrete_const_decl src)
    | K_Abstract_Set -> Abstract_Set (to_concrete_const_decl src)
    | K_Concrete_Set elts -> Concrete_Set (elts,to_concrete_const_decl src)
    | K_Enumerate -> Enumerate (to_concrete_const_decl src)

  let update_kind (decl:t_kind) (ki:t_global_kind) (src:t_source) : t_kind option =
    match decl, ki with
    | Abstract_Variable src2, K_Abstract_Variable ->
      begin match src2, src with
        | A_Refined, Machine _ -> assert false (*FIXME*)
        | A_Refined, Imported mch ->
          Some (Abstract_Variable (A_Redeclared_In_Imported mch))
        | _, _ -> None
      end
    | Abstract_Variable src2, K_Concrete_Variable ->
      begin match src2, src with
        | A_Refined, Machine _ -> assert false (*FIXME*)
(*           Some (Concrete_Variable (A_Redeclared_In_Machine l)) *)
        | A_Refined, Imported mch->
          Some (Concrete_Variable (V_Redeclared_In_Imported mch))
        | _, _ -> None
      end
    | Abstract_Constant src2, K_Abstract_Constant ->
      begin match src2, src with
        | A_Refined, Machine _ -> assert false (*FIXME*)
        | A_Refined, Imported mch ->
          Some (Abstract_Constant (A_Redeclared_In_Imported mch))
        | _, _ -> None
      end
    | Abstract_Constant src2, K_Concrete_Constant ->
      begin match src2, src with
        | A_Refined, Machine _ -> assert false (*FIXME*)
(*           Some (Concrete_Constant (A_Redeclared_In_Machine l)) *)
        | A_Refined, Imported mch->
          Some (Concrete_Constant (C_Redeclared_In_Imported mch))
        | _, _ -> None
      end
      (*FIXME*)
    | _ -> None

  let add_symbol (env:t) (err_loc:loc) (id:string) (sy_typ:Btype.t)
      (ki:t_global_kind) (src:t_source) : unit =
    match Hashtbl.find_opt env.symb id with
    | Some infos ->
      let sy_kind = match update_kind infos.sy_kind ki src with
        | Some x -> x
        | None ->
          Error.error err_loc ("The identifier '" ^ id ^ "' clashes with previous declaration.")
      in
      if not (Btype.is_equal_modulo_alias Btype.no_alias infos.sy_typ sy_typ) then
        Error.error  err_loc
          ("The identifier '" ^ id ^ "' has type " 
           ^ Btype.to_string sy_typ
           ^ " but was previously declared with type "
           ^ Btype.to_string infos.sy_typ ^ ".")
      else
        Hashtbl.replace env.symb id { sy_typ; sy_kind }
    | None ->
      Hashtbl.add env.symb id { sy_typ; sy_kind=mk_kind ki src }

  let get_operation (env:t) (id:string) : t_op_decl t_operation_infos option =
    Hashtbl.find_opt env.ops id

  let update_op_source (current_source:t_op_decl) (new_source:t_source) : t_op_decl option =
    match current_source, new_source with
    | O_Refined, Machine l -> Some (O_Current_And_Refined l)
    | O_Refined, Imported inc -> Some (O_Imported_And_Refined inc)
    | O_Imported inc, Refined -> Some (O_Imported_And_Refined inc)
    | O_Local_Spec l1, Machine l2 -> Some (O_Local_Spec_And_Implem (l1,l2))
    | _, _ -> None

  let _add_operation (env:t) (err_loc:loc) (id:string) (op_args_in:(string*Btype.t)list)
      (op_args_out:(string*Btype.t)list) (src:t_source) : unit =
    match Hashtbl.find_opt env.ops id with
    | Some infos ->
      begin match update_op_source infos.op_src src with
        | None ->
          Error.error err_loc ("The operation '" ^ id ^ "' clashes with previous declaration.") 
        | Some op_src ->
          ( check_args_type Btype.no_alias err_loc infos.op_args_in op_args_in;
            check_args_type Btype.no_alias err_loc infos.op_args_out op_args_out;
            Hashtbl.replace env.ops id { infos with op_src } )
      end
    | None ->
      let op_src = match src with
        | Refined -> O_Refined
        | Seen mch -> O_Seen mch
        | Imported mch -> O_Imported mch
        | Machine _ -> assert false (*FIXME*)
      in
      Hashtbl.add env.ops id { op_args_in; op_args_out; op_readonly=false; op_src }

  let add_operation (env:t) (loc:loc) (id:string) (args_in:(string*Btype.t) list)
      (args_out:(string*Btype.t) list) : unit
    =
    match find_duplicate (args_in@args_out) with
    | None -> _add_operation env loc id args_in args_out (Machine loc)
    | Some arg ->
      Error.error loc ("The argument '"^arg^"' appears twice in this operation declaration.")

  let promote_operation (env:t) (lc:loc) (id:string) : unit =
    match get_operation env id with
    | None -> Error.error lc ("Unknown operation '"^id^"'.")
    | Some ({ op_src=O_Imported_And_Refined mch; _ } as op) ->
      Hashtbl.replace env.ops id { op with op_src=O_Imported_Promoted_And_Refined (mch,lc)}
    | Some { op_src=O_Imported _; _ } -> assert false (*FIXME*)
    | Some { op_src=O_Imported_Promoted_And_Refined _; _ } -> assert false (*FIXME*)
    | Some _ ->
      Error.error lc ("The operation '"^id^"' is not an operation of an imported machine.")

  let load_external_symbol (env:t) (mch:ren_ident) (src:t_source)
      ({id;typ;kind}:MachineInterface.t_symb) : unit =
    let open MachineInterface in
    let mk_id id = match mch.r_prefix with
      | None -> id
      | Some p -> p ^ "." ^ id 
    in
    match kind with
    | K_Abstract_Variable | K_Concrete_Variable ->
      add_symbol env mch.SyntaxCore.r_loc (mk_id id)
        (Btype.change_current (Btype.T_Ext mch.SyntaxCore.r_str) typ) kind src
    | _ ->
      if (is_in_deps env.deps mch.SyntaxCore.r_str) then ()
      else
        add_symbol env mch.SyntaxCore.r_loc id
          (Btype.change_current (Btype.T_Ext mch.SyntaxCore.r_str) typ) kind src

  let load_external_op (env:t) (mch:ren_ident) (src:t_source)
      (op:MachineInterface.t_op) : unit =
    let change_current = List.map (fun (s,ty) ->
        (s,Btype.change_current (Btype.T_Ext mch.SyntaxCore.r_str) ty)
      ) in
    let op_name = match mch.SyntaxCore.r_prefix with
      | None -> op.id
      | Some p -> p ^ "." ^ op.id
    in
    _add_operation env mch.SyntaxCore.r_loc op_name (change_current op.args_in)
      (change_current op.args_out) src

  let load_interface_for_seen_machine (env:t) (itf:t_interface) (mch:ren_ident) : unit =
    (*FIXME check not already in deps*)
    let open MachineInterface in
    List.iter (load_external_symbol env mch (Seen mch)) (get_symbols itf);
    List.iter (load_external_op env mch (Seen mch)) (get_operations itf);
    env.deps <- mch.r_str::env.deps

  let load_interface_for_refined_machine (env:t) (itf:t_interface) (mch:lident) : unit =
    (*FIXME check not already in deps*)
    let open MachineInterface in
    let ren_mch = { SyntaxCore.r_prefix=None; r_str=mch.lid_str; r_loc=mch.lid_loc } in
    List.iter (load_external_symbol env ren_mch Refined) (get_symbols itf);
    List.iter (load_external_op env ren_mch Refined) (get_operations itf);
    env.deps <- mch.lid_str::env.deps

  let load_interface_for_imported_machine (env:t) (itf:t_interface) (mch:ren_ident) (_:(loc*Btype.t) list): unit =
    (*FIXME check not already in deps*)
    let open MachineInterface in
    List.iter (load_external_symbol env mch (Imported mch)) (get_symbols itf);
    List.iter (load_external_op env mch (Imported mch)) (get_operations itf);
    env.deps <- mch.r_str::env.deps

  let load_interface_for_extended_machine (env:t) (itf:t_interface) (mch:ren_ident) (params:(loc*Btype.t) list): unit =
    let open MachineInterface in
    load_interface_for_imported_machine env itf mch params;
    List.iter (fun (r:t_op) ->
        promote_operation env mch.SyntaxCore.r_loc r.id
      ) (get_operations itf)

  let check_operation_coherence (_:t) (_:loc) : unit = () (*FIXME*)
end

(*



type 'a t_op_source =
  | SO_Current : loc -> 'a t_op_source
  | SO_Seen : ren_ident -> 'a t_op_source
  | SO_Refined : lident -> t_ref t_op_source
  | SO_Included_Or_Imported : ren_ident -> 'a t_op_source
  | SO_Local : loc -> t_ref t_op_source

let rec string_list_eq l1 l2 = match l1, l2 with
  | [], [] -> true
  | h1::t1, h2::t2 -> String.equal h1 h2 && string_list_eq t1 t2
  | _, _ -> false

let update_kind (type mr h) (old_kind:(mr,h) t_kind) (new_kind:(mr,t_simple) t_kind)
  : (mr,_) t_kind option =
  match old_kind with
  (* Parameters *)
    | K_Parameter (_, _) -> None
    (* Machine *)
    | M_Abstract_Variable_Machine _ -> None
    | M_Abstract_Variable_Seen _ -> None
    | M_Abstract_Variable_Used _ -> None
    | M_Abstract_Variable_Included _ -> None
    | M_Abstract_Constant_Machine _ -> None
    | M_Abstract_Constant_Seen _ -> None
    | M_Abstract_Constant_Used _ -> None
    | M_Abstract_Constant_Included _ -> None
    | M_Concrete_Variable_Machine _ -> None
    | M_Concrete_Variable_Seen _ -> None
    | M_Concrete_Variable_Used _ -> None
    | M_Concrete_Variable_Included _ -> None
    | M_Concrete_Constant_Machine _ -> None
    | M_Concrete_Constant_Seen _ -> None
    | M_Concrete_Constant_Used _ -> None
    | M_Concrete_Constant_Included _ -> None
    | M_Set_Or_Enum_Machine _ -> None
    | M_Set_Or_Enum_Seen _ -> None
    | M_Set_Or_Enum_Used _ -> None
    | M_Set_Or_Enum_Included _ -> None
    (* Abstract Variables in Refinement *)
    | R_Abstract_Variable_Machine lc ->
      begin match new_kind with
        | R_Abstract_Variable_Refined -> Some (R_Abstract_Variable_Redeclared_In_Machine lc)
        | _ -> None
      end
    | R_Abstract_Variable_Refined ->
      begin match new_kind with
        | R_Abstract_Variable_Machine lc -> Some (R_Abstract_Variable_Redeclared_In_Machine lc)
        | R_Abstract_Variable_Included mch -> Some (R_Abstract_Variable_Redeclared_In_Included mch)
        | _ -> None
      end
    | R_Abstract_Variable_Seen _ -> None
    | R_Abstract_Variable_Included mch ->
      begin match new_kind with
        | R_Abstract_Variable_Refined -> Some (R_Abstract_Variable_Redeclared_In_Included mch)
        | _ -> None
      end
    | R_Abstract_Variable_Redeclared_In_Machine _ -> None
    | R_Abstract_Variable_Redeclared_In_Included _ -> None
    (* Abstract Constant in Refinement *)
    | R_Abstract_Constant_Machine lc ->
      begin match new_kind with
        | R_Abstract_Constant_Refined ->
          Some (R_Abstract_Constant_Redeclared_In_Machine lc)
        | _ -> None
      end
    | R_Abstract_Constant_Refined ->
      begin match new_kind with
        | R_Abstract_Constant_Machine lc ->
          Some (R_Abstract_Constant_Redeclared_In_Machine lc)
        | R_Abstract_Constant_Included mch ->
          Some (R_Abstract_Constant_Redeclared_In_Included mch)
        | _ -> None
      end
    | R_Abstract_Constant_Seen _ -> None
    | R_Abstract_Constant_Included _ -> assert false (*FIXME*)
    | R_Abstract_Constant_Redeclared_In_Machine _ -> None
    | R_Abstract_Constant_Redeclared_In_Included _ -> None
    | R_Concrete_Variable_Machine _ -> None
    | R_Concrete_Variable_Seen _ -> None
    | R_Concrete_Variable_Refined -> None
    | R_Concrete_Variable_Included _ -> None
    | R_Concrete_Constant_Machine _ -> None
    | R_Concrete_Constant_Seen _ -> None
    | R_Concrete_Constant_Refined -> None
    | R_Concrete_Constant_Included _ -> None
    | R_Set_Or_Enum_Machine _ -> None
    | R_Set_Or_Enum_Seen _ -> None
    | R_Set_Or_Enum_Refined _ -> None
    | R_Set_Or_Enum_Included _ -> None
    | I_Abstract_Variable_Refined -> assert false (*FIXME*)
    | I_Abstract_Variable_Seen _ -> None
    | I_Abstract_Variable_Imported _ -> assert false (*FIXME*)
    | I_Abstract_Variable_Redeclared_In_Imported _ -> None
    | I_Abstract_Constant_Refined -> assert false (*FIXME*)
    | I_Abstract_Constant_Seen _ -> None
    | I_Abstract_Constant_Imported _ -> assert false (*FIXME*)
    | I_Abstract_Constant_Redeclared_In_Imported _ -> None
    | I_Concrete_Variable_Machine -> None
    | I_Concrete_Variable_Refined -> assert false (*FIXME*)
    | I_Concrete_Variable_Seen _ -> None
    | I_Concrete_Variable_Imported _ -> assert false (*FIXME*)
    | I_Concrete_Variable_Redeclared_In_Imported _ -> None
    | I_Concrete_Constant_Machine -> None
    | I_Concrete_Constant_Refined -> assert false (*FIXME*)
    | I_Concrete_Constant_Seen _ -> assert false (*FIXME*)
    | I_Concrete_Constant_Imported _ -> assert false (*FIXME*)
    | I_Concrete_Constant_Redeclared_In_Imported _ -> None
    | I_Concrete_Constant_Redeclared_In_Seen _ -> None
    | I_Set_Or_Enum_Machine _ -> None
    | I_Set_Or_Enum_Refined _ -> assert false (*FIXME*)
    | I_Set_Or_Enum_Seen _ -> assert false (*FIXME*)
    | I_Set_Or_Enum_Imported _ -> assert false (*FIXME*)
    | I_Set_Or_Enum_Redeclared_In_Imported _ -> None
    | I_Set_Or_Enum_Redeclared_In_Seen _ -> None
    (*
  (*Machine*)

  (*Abstract Variables*)
  | K_Abstract_Variable R_Refined, S_Current_Ref l, G_Abstract_Variable ->
    Some (K_Abstract_Variable (R_Redeclared_In_Machine l))
  | K_Abstract_Variable R_Refined, S_Included_Ref l, G_Abstract_Variable ->
    Some (K_Abstract_Variable (R_Redeclared_In_Included l))
  | K_Abstract_Variable R_Refined, S_Current_Ref l, G_Concrete_Variable ->
    Some (K_Concrete_Variable (R_Redeclared_In_Machine l))
  | K_Abstract_Variable R_Disappearing, S_Included_Or_Imported l, G_Concrete_Variable ->
    Some (K_Concrete_Variable (R_Redeclared (By_Included_Or_Imported l)))
  | K_Abstract_Variable _, _, _ -> None

  | K_Abstract_Constant D_Disappearing, S_Current l, G_Abstract_Constant ->
    Some (K_Abstract_Constant (D_Redeclared (By_Machine l)))
  | K_Abstract_Constant D_Disappearing, S_Current l, G_Concrete_Constant ->
    Some (K_Abstract_Constant (D_Redeclared (By_Machine l)))
  | K_Abstract_Constant D_Disappearing, S_Included_Or_Imported l, G_Abstract_Constant ->
    Some (K_Abstract_Constant (D_Redeclared (By_Included_Or_Imported l)))
  | K_Abstract_Constant D_Disappearing, S_Included_Or_Imported l, G_Concrete_Constant ->
    Some (K_Concrete_Constant (D_Redeclared (By_Included_Or_Imported l)))
  | K_Abstract_Constant D_Disappearing, S_Seen l, G_Abstract_Constant ->
    Some (K_Abstract_Constant (D_Redeclared (By_Seen l)))
  | K_Abstract_Constant D_Disappearing, S_Seen l, G_Concrete_Constant ->
    Some (K_Concrete_Constant (D_Redeclared (By_Seen l)))
  | K_Abstract_Constant _, _, _ -> None

  | K_Concrete_Variable (D_Redeclared Implicitely), S_Included_Or_Imported l, G_Concrete_Variable ->
    Some (K_Concrete_Variable (D_Redeclared (By_Included_Or_Imported l)))
  | K_Concrete_Variable (D_Redeclared Implicitely), S_Seen l, G_Concrete_Variable ->
    Some (K_Concrete_Variable (D_Redeclared (By_Seen l)))
  | K_Concrete_Variable _, _, _ -> None

  | K_Concrete_Constant (D_Redeclared Implicitely), S_Included_Or_Imported l, G_Concrete_Constant ->
    Some (K_Concrete_Constant (D_Redeclared (By_Included_Or_Imported l)))
  | K_Concrete_Constant (D_Redeclared Implicitely), S_Seen l, G_Concrete_Constant ->
    Some (K_Concrete_Constant (D_Redeclared (By_Seen l)))
  | K_Concrete_Constant _, _, _ -> None

  | K_Abstract_Set (D_Redeclared Implicitely), S_Included_Or_Imported l, G_Abstract_Set ->
    Some (K_Abstract_Set (D_Redeclared (By_Included_Or_Imported l)))
  | K_Abstract_Set (D_Redeclared Implicitely), S_Seen l, G_Abstract_Set ->
    Some (K_Abstract_Set (D_Redeclared (By_Seen l)))
  | K_Abstract_Set _, _, _ -> None

  | K_Concrete_Set (elts,D_Redeclared Implicitely), S_Included_Or_Imported l, G_Concrete_Set elts2 ->
    if string_list_eq elts elts2 then
      Some (K_Concrete_Set (elts,D_Redeclared (By_Included_Or_Imported l)))
    else
      None
  | K_Concrete_Set (elts,D_Redeclared Implicitely), S_Seen l, G_Concrete_Set elts2 ->
    if string_list_eq elts elts2 then
      Some (K_Concrete_Set (elts,D_Redeclared (By_Seen l)))
    else
      None
  | K_Concrete_Set _, _, _ -> None

  | K_Enumerate (D_Redeclared Implicitely), S_Included_Or_Imported l, G_Enumerate ->
    Some (K_Enumerate (D_Redeclared (By_Included_Or_Imported l)))
  | K_Enumerate (D_Redeclared Implicitely), S_Seen l, G_Enumerate ->
    Some (K_Enumerate (D_Redeclared (By_Seen l)))
  | K_Enumerate _, _, _ -> None

  | K_Parameter (_, _), _, _ -> None
*)

let add_alias (s:'a t) (alias:string) (ty:Btype.t) : bool =
  match Btype.add_alias s.alias alias ty with
  | None -> false
  | Some alias -> (s.alias <- alias; true)

let _add_symbol (type mr ac) (env:mr t) (err_loc:loc) (id:string) (sy_typ:Btype.t)
    (sy_kind:(mr,t_simple) t_kind) : unit
  =
(*
  (match src, ki with
   | S_Included_Or_Imported inc, G_Abstract_Set ->
     let success = add_alias env id (Btype.mk_Abstract_Set (Btype.T_Ext inc.SyntaxCore.r_str) id) in
     assert success
   | S_Included_Or_Imported inc, G_Concrete_Set _ ->
     let success = add_alias env id (Btype.mk_Concrete_Set (Btype.T_Ext inc.SyntaxCore.r_str) id) in
     assert success
   | _, _ -> () );
*)
  match Hashtbl.find_opt env.symb id with
  | Some (I infos) ->
    let sy_kind = match update_kind infos.sy_kind sy_kind with
      | Some x -> x
      | None ->
        Error.error err_loc ("The identifier '" ^ id ^ "' clashes with previous declaration.")
    in
    if not (Btype.is_equal_modulo_alias env.alias infos.sy_typ sy_typ) then
      Error.error  err_loc
        ("The identifier '" ^ id ^ "' has type " 
         ^ Btype.to_string sy_typ
         ^ " but was previously declared with type "
         ^ Btype.to_string infos.sy_typ ^ ".")
    else
      Hashtbl.replace env.symb id (I{ sy_typ; sy_kind })
  | None ->
    Hashtbl.add env.symb id (I { sy_typ; sy_kind })

let add_symbol (type mr) (env:mr t) (loc:loc) (id:string) (typ:Btype.t) (ki:t_global_kind) : unit =
  match env.kind, ki with
  | Mch, G_Abstract_Variable -> _add_symbol env loc id typ (M_Abstract_Variable_Machine loc)
  | Mch, G_Abstract_Constant -> _add_symbol env loc id typ (M_Abstract_Constant_Machine loc)
  | Mch, G_Concrete_Variable -> _add_symbol env loc id typ (M_Concrete_Variable_Machine loc)
  | Mch, G_Concrete_Constant -> _add_symbol env loc id typ (M_Concrete_Constant_Machine loc)
  | Mch, G_Abstract_Set -> _add_symbol env loc id typ (M_Concrete_Constant_Machine loc)
  | Mch, G_Concrete_Set _
  | Mch, G_Enumerate -> assert false (*FIXME*)
  | _, _ -> assert false (*FIXME*)

let update_op_source (type a) (current_source:a t_op_decl) (new_source:a t_op_source) : a t_op_decl option =
  match current_source, new_source with
  | OD_Refined ref, SO_Current l -> Some (OD_Current_And_Refined (l,ref))
  | OD_Refined ref, SO_Included_Or_Imported inc -> Some (OD_Included_Or_Imported_And_Refined (inc,ref))
  | OD_Included_Or_Imported inc, SO_Refined ref -> Some (OD_Included_Or_Imported_And_Refined (inc,ref))
  | OD_Local_Spec spe, SO_Current imp -> Some (OD_Local_Spec_And_Implem (spe,imp))
  | _, _ -> None


let _add_operation (type a) (env:a t) (err_loc:loc) (id:string) (op_args_in:(string*Btype.t)list)
    (op_args_out:(string*Btype.t)list) (op_readonly:bool) (op_src:a t_op_source) : unit =
  match Hashtbl.find_opt env.ops id with
  | Some infos ->
    begin match update_op_source infos.op_src op_src with
      | None ->
        Error.error err_loc ("The operation '" ^ id ^ "' clashes with previous declaration.") 
      | Some op_src ->
        ( check_args_type env err_loc infos.op_args_in op_args_in;
          check_args_type env err_loc infos.op_args_out op_args_out;
          Hashtbl.replace env.ops id { infos with op_src } )
    end
  | None ->
    let ret (op_src:a t_op_decl) =
      Hashtbl.add env.ops id { op_args_in; op_args_out; op_readonly; op_src }
    in
    begin match op_src with
      | SO_Current lc ->
        begin match env.kind with
          | Mch -> ret (OD_Current lc)
          | Ref -> Error.error lc "This operation is not declared in the refined machine."
        end
      | SO_Seen mch ->  ret (OD_Seen mch)
      | SO_Refined mch -> ret (OD_Refined mch)
      | SO_Included_Or_Imported mch -> ret (OD_Included_Or_Imported mch)
      | SO_Local lc -> ret (OD_Local_Spec lc)
    end

let get_operation (env:'a t) (id:string) : 'a t_operation_infos option =
    Hashtbl.find_opt env.ops id

let rec find_duplicate : (string*'a) list -> string option = function
  | [] -> None
  | (x,_)::tl ->
    let aux (y,_) = String.equal x y in
    if List.exists aux tl then Some x
    else find_duplicate tl

let add_mch_operation (env:t_mch t) (loc:loc) (id:string) (args_in) (args_out)
    ~is_readonly : unit =
  match find_duplicate (args_in@args_out) with
  | None -> _add_operation env loc id args_in args_out is_readonly (SO_Current loc)
  | Some arg ->
    Error.error loc ("The argument '"^arg^"' appears twice in this operation declaration.")

let add_ref_operation (env:t_ref t) (loc:loc) (id:string) (args_in) (args_out)
    ~is_local : unit =
  let src = if is_local then SO_Local loc else SO_Current loc in
  match find_duplicate (args_in@args_out) with
  | None -> _add_operation env loc id args_in args_out false(*this is not relevant for refinements*) src
  | Some arg ->
    Error.error loc ("The argument '"^arg^"' appears twice in this operation declaration.")

let promote_operation (type a) (env:a t) (loc:loc) (id:string) : unit =
  match Hashtbl.find_opt env.ops id with
  | None -> Error.error loc ("Unknown operation '"^id^"'.")
  | Some infos ->
    begin match infos.op_src with
      | OD_Included_Or_Imported mch ->
        begin match env.kind with
          | Mch ->
            Hashtbl.replace env.ops id { infos with op_src=OD_Included_Or_Imported_And_Promoted (mch,loc) }
          | Ref ->
            Error.error loc ("The operation '"^id^"' does not exist in the refined machine.")
        end
      | OD_Included_Or_Imported_And_Refined (inc,ref) ->
        Hashtbl.replace env.ops id { infos with op_src=OD_Included_Or_Imported_Promoted_And_Refined (inc,loc,ref) }
      | OD_Included_Or_Imported_Promoted_And_Refined _ ->
        Error.error loc ("The operation '"^id^"' is already promoted.")
      | OD_Included_Or_Imported_And_Promoted _ ->
        Error.error loc ("The operation '"^id^"' is already promoted.")
      | OD_Seen _ ->
        Error.error loc ("The operation '"^id^"' is not an operation of an imported or included machine.")
      | OD_Current_And_Refined _ ->
        Error.error loc ("The operation '"^id^"' is not an operation of an imported or included machine.")
      | OD_Local_Spec_And_Implem _ ->
        Error.error loc ("The operation '"^id^"' is not an operation of an imported or included machine.")
      | OD_Refined _ ->
        Error.error loc ("The operation '"^id^"' is not an operation of an imported or included machine.")
      | OD_Current _ ->
        Error.error loc ("The operation '"^id^"' is not an operation of an imported or included machine.")
      | OD_Local_Spec _ ->
        Error.error loc ("The operation '"^id^"' is not an operation of an imported or included machine.")
    end

let is_in_deps (env:'a t) (mch:string) : bool =
  List.exists (String.equal mch) env.deps

let load_interface_for_seen_machine (env:'a t) (itf:MachineInterface.t) (mch:ren_ident) : unit =
  let open MachineInterface in
  List.iter (fun (S { id;typ;kind}:t_symb) ->
      match kind with
      | G_Parameter _ -> ()
      | G_Abstract_Variable ->
        let ren_id = match mch.r_prefix with
          | None -> id
          | Some p -> p ^ "." ^ id 
        in
        _add_symbol env mch.SyntaxCore.r_loc ren_id
          (Btype.change_current (Btype.T_Ext mch.SyntaxCore.r_str) typ)
          kind (S_Seen mch)
      | G_Concrete_Variable ->
        let ren_id = match mch.r_prefix with
          | None -> id
          | Some p -> p ^ "." ^ id 
        in
        _add_symbol env mch.SyntaxCore.r_loc ren_id
          (Btype.change_current (Btype.T_Ext mch.SyntaxCore.r_str) typ)
          kind (S_Seen mch)
      | _ ->
        begin match mch.r_prefix with
          | None ->
            _add_symbol env mch.SyntaxCore.r_loc id
              (Btype.change_current (Btype.T_Ext mch.SyntaxCore.r_str) typ)
              kind (S_Seen mch)
          | Some _ ->
            if is_in_deps env mch.SyntaxCore.r_str then ()
            else
              _add_symbol env mch.SyntaxCore.r_loc id
                (Btype.change_current (Btype.T_Ext mch.SyntaxCore.r_str) typ)
                kind (S_Seen mch)
        end
    ) (get_symbols itf);
  let change_current = List.map (fun (s,ty) -> (s,Btype.change_current (Btype.T_Ext mch.SyntaxCore.r_str) ty)) in
  List.iter (fun (r:t_op) ->
      let op_name = match mch.SyntaxCore.r_prefix with
        | None -> r.id
        | Some p -> p ^ "." ^ r.id
      in
      _add_operation env mch.SyntaxCore.r_loc op_name (change_current r.args_in) (change_current r.args_out) r.readonly (SO_Seen mch)
    ) (get_operations itf);
  env.deps <- mch.r_str::env.deps

let load_interface_for_used_machine (env:'a t) (itf:MachineInterface.t) (mch:ren_ident) : unit =
  let open MachineInterface in
  List.iter (fun (S { id;typ;kind}:t_symb) ->
      match kind with
      | G_Parameter _ -> ()
      | G_Abstract_Variable ->
        let ren_id = match mch.r_prefix with
          | None -> id
          | Some p -> p ^ "." ^ id 
        in
        _add_symbol env mch.SyntaxCore.r_loc ren_id
          (Btype.change_current (Btype.T_Ext mch.SyntaxCore.r_str) typ)
          kind (S_Used mch)
      | G_Concrete_Variable ->
        let ren_id = match mch.r_prefix with
          | None -> id
          | Some p -> p ^ "." ^ id 
        in
        _add_symbol env mch.SyntaxCore.r_loc ren_id
          (Btype.change_current (Btype.T_Ext mch.SyntaxCore.r_str) typ)
          kind (S_Used mch)
      | _ ->
        begin match mch.r_prefix with
          | None ->
            _add_symbol env mch.SyntaxCore.r_loc id
              (Btype.change_current (Btype.T_Ext mch.SyntaxCore.r_str) typ)
              kind (S_Used mch)
          | Some _ ->
            if is_in_deps env mch.SyntaxCore.r_str then ()
            else
              _add_symbol env mch.SyntaxCore.r_loc id
                (Btype.change_current (Btype.T_Ext mch.SyntaxCore.r_str) typ)
                kind (S_Used mch)
        end
    ) (get_symbols itf);
  env.deps <- mch.r_str::env.deps

let load_interface_for_included_or_imported_machine (env:'a t) (itf:MachineInterface.t)
    (mch:ren_ident) (params:(loc*Btype.t) list) : unit =
  let open MachineInterface in
  let rec check_set_params lst1 lst2 alias =
    match lst1, lst2 with
    | [], [] -> alias
    | hd1::tl1, (lc,hd2)::tl2 ->
      begin match hd1.kind with
        | Scalar -> check_set_params tl1 tl2 alias
        | Set ->
          begin match Btype.view hd2 with
            | Btype.T_Power ty -> check_set_params tl1 tl2 (Btype.SMap.add hd1.id ty alias)
            | _ ->
              let err_txt = Printf.sprintf
                  "This expression has type '%s' but an expression of type '%s' was expected."
                  (Btype.to_string hd2)
                  (Btype.Open.to_string (Btype.Open.mk_Power (Btype.Open.new_meta ())))
              in
              Error.error lc err_txt
          end
      end
    | _, _ -> Error.error mch.r_loc "Wrong number of parameters."
  in
  let alias = check_set_params (get_params itf) params Btype.SMap.empty in
  let rec check_scalar_params lst1 lst2 =
    match lst1, lst2 with
    | [], [] -> ()
    | hd1::tl1, (lc,hd2)::tl2 ->
      begin match hd1.kind with
        | Scalar ->
          if Btype.equal (Btype.subst alias hd1.typ) hd2 then check_scalar_params tl1 tl2
          else
            let err_txt = Printf.sprintf
                "This expression has type '%s' but an expression of type '%s' was expected."
                (Btype.to_string hd2)
                (Btype.Open.to_string (Btype.Open.mk_Power (Btype.Open.new_meta ())))
            in
            Error.error lc err_txt
        | Set -> check_scalar_params tl1 tl2
      end
    | _, _ -> assert false
  in
  check_scalar_params (get_params itf) params;
  List.iter (fun (S { id;typ;kind}:t_symb) ->
      match kind with
      | G_Parameter _ -> ()
      | G_Abstract_Variable ->
        let ren_id = match mch.r_prefix with
          | None -> id
          | Some p -> p ^ "." ^ id 
        in
        _add_symbol env mch.SyntaxCore.r_loc ren_id (Btype.subst alias typ) kind (S_Included_Or_Imported mch)
      | G_Concrete_Variable ->
        let ren_id = match mch.r_prefix with
          | None -> id
          | Some p -> p ^ "." ^ id 
        in
        _add_symbol env mch.SyntaxCore.r_loc ren_id (Btype.subst alias typ) kind (S_Included_Or_Imported mch)
      | _ ->
        begin match mch.r_prefix with
          | None ->
            _add_symbol env mch.SyntaxCore.r_loc id typ kind (S_Included_Or_Imported mch)
          | Some _ ->
            if is_in_deps env mch.SyntaxCore.r_str then ()
            else
              _add_symbol env mch.SyntaxCore.r_loc id typ kind (S_Included_Or_Imported mch)
        end
    ) (get_symbols itf);
  List.iter (fun (r:t_op) ->
      let op_name = match mch.SyntaxCore.r_prefix with
        | None -> r.id
        | Some p -> p ^ "." ^ r.id
      in
      _add_operation env mch.SyntaxCore.r_loc op_name r.args_in r.args_out r.readonly (SO_Included_Or_Imported mch)
    ) (get_operations itf);
  env.deps <- mch.r_str::env.deps


let load_interface_for_refined_machine (env:t_ref t) (itf:MachineInterface.t) (mch:lident) (params:lident list): unit =
  let open MachineInterface in
  let rec check_param (lst1:lident list) (lst2:t_param list) =
    match lst1, lst2 with
    | [], [] -> ()
    | hd1::tl1, hd2::tl2 ->
      if hd1.lid_str = hd2.id then
        ( _add_symbol env hd1.lid_loc hd2.id hd2.typ (G_Parameter hd2.kind) (S_Current hd1.lid_loc);
          check_param tl1 tl2 )
      else Error.error hd1.lid_loc "Parameter mismatch."
    | hd1::_, [] -> Error.error hd1.lid_loc "Parameter mismatch."
    | [], hd2::_ -> Error.error mch.lid_loc ("Missing parameter '"^hd2.id^"'.")
  in
  check_param params (get_params itf);
  List.iter (fun (S { id;typ;kind}:t_symb) ->
      _add_symbol env mch.SyntaxCore.lid_loc id typ kind (S_Refined mch)
    ) (get_symbols itf);
  List.iter (fun (r:t_op) ->
      _add_operation env mch.SyntaxCore.lid_loc r.id r.args_in r.args_out r.readonly (SO_Refined mch)
    ) (get_operations itf);
  env.deps <- mch.lid_str::env.deps

let load_interface_for_extended_machine (env:'mr t) (itf:MachineInterface.t)
    (mch:ren_ident) (params:(loc*Btype.t) list) : unit
  =
  let open MachineInterface in
  load_interface_for_included_or_imported_machine env itf mch params;
  List.iter (fun (r:t_op) ->
      promote_operation env mch.SyntaxCore.r_loc r.id
    ) (get_operations itf)

let is_exported_symbol (type mr ac) : (mr,ac) t_decl -> bool = function
  | D_Machine _ -> true
  | D_Included_Or_Imported _ -> true
  | D_Redeclared _ -> true
  | D_Seen _ -> false
  | D_Used _ -> false
  | D_Disappearing -> false

let to_interface (type mr) (env:mr t) : MachineInterface.t =
  let aux1 (x:string) (symb:mr t_symbol_infos) (lst:MachineInterface.t_symb list) =
    match symb.sy_kind with
    | K_Parameter (_, _) -> lst
    | K_Abstract_Variable d when is_exported_symbol d ->
      (MachineInterface.S { id=x; typ=symb.sy_typ; kind=G_Abstract_Variable })::lst
    | K_Abstract_Constant d when is_exported_symbol d ->
      (MachineInterface.S { id=x; typ=symb.sy_typ; kind=G_Abstract_Constant })::lst
    | K_Concrete_Variable d when is_exported_symbol d ->
      (MachineInterface.S { id=x; typ=symb.sy_typ; kind=G_Concrete_Variable })::lst
    | K_Concrete_Constant d when is_exported_symbol d ->
      (MachineInterface.S { id=x; typ=symb.sy_typ; kind=G_Concrete_Constant })::lst
    | K_Abstract_Set d when is_exported_symbol d ->
      (MachineInterface.S { id=x; typ=symb.sy_typ; kind=G_Abstract_Set })::lst
    | K_Concrete_Set (elts, d) when is_exported_symbol d ->
      (MachineInterface.S { id=x; typ=symb.sy_typ; kind=G_Concrete_Set elts })::lst
    | K_Enumerate d when is_exported_symbol d ->
      (MachineInterface.S { id=x; typ=symb.sy_typ; kind=G_Enumerate })::lst
    | _ -> lst
  in
  let aux2 (id:string) (op:mr t_operation_infos) lst =
    let x = { MachineInterface.id; args_in=op.op_args_in;
              args_out=op.op_args_out; readonly=op.op_readonly }
    in
    match op.op_src with
    | OD_Current _ -> x::lst
    | OD_Refined _ -> x::lst
    | OD_Current_And_Refined _ -> x::lst
    | OD_Included_Or_Imported_Promoted_And_Refined _ -> x::lst
    | OD_Included_Or_Imported_And_Refined _ -> x::lst
    | OD_Included_Or_Imported_And_Promoted _ -> x::lst
    | OD_Local_Spec _ -> lst
    | OD_Local_Spec_And_Implem _ -> lst
    | OD_Seen _ | OD_Included_Or_Imported _ -> lst
  in
  let aux (lid:lident) : MachineInterface.t_param =
    let id = lid.SyntaxCore.lid_str in
    match Hashtbl.find_opt env.symb id with
    | None -> assert false
    | Some infos ->
      begin match infos.sy_kind with
        | K_Parameter (kind,_) ->
          { MachineInterface.id; typ=infos.sy_typ; kind }
        | _ -> assert false
      end
  in
  let params:MachineInterface.t_param list = List.map aux env.params in
  let symbs = Hashtbl.fold aux1 env.symb [] in
  let ops = Hashtbl.fold aux2 env.ops [] in
  MachineInterface.make params symbs ops

let check_operation_coherence_ref (env:t_ref t) (_:loc) : unit =
  Hashtbl.iter (
    fun x op ->
      match op.op_src with
      | OD_Seen _ -> ()
      | OD_Included_Or_Imported _ -> ()
      | OD_Local_Spec_And_Implem _ -> ()
      | OD_Current_And_Refined _ -> ()
      | OD_Included_Or_Imported_Promoted_And_Refined _ -> ()
      | OD_Refined _ -> ()
      | OD_Local_Spec lc ->
        Error.error lc ("The operation '"^x^"' is not implemented.")
      | OD_Included_Or_Imported_And_Refined _ -> ()
  ) env.ops

let check_operation_coherence_imp (env:t_ref t) (err_loc:loc) : unit =
  Hashtbl.iter (
    fun x op ->
      match op.op_src with
      | OD_Seen _ -> ()
      | OD_Included_Or_Imported _ -> ()
      | OD_Local_Spec_And_Implem _ -> ()
      | OD_Current_And_Refined _ -> ()
      | OD_Included_Or_Imported_Promoted_And_Refined _ -> ()
      | OD_Refined _ ->
        Error.error err_loc ("The operation '"^x^"' is not refined.")
      | OD_Local_Spec lc ->
        Error.error lc ("The operation '"^x^"' is not implemented.")
      | OD_Included_Or_Imported_And_Refined _ ->
        Error.error err_loc ("The operation '"^x^"' is not refined (missing promotion?).")
  ) env.ops

let fold_symbols (f:string -> 'mr t_symbol_infos -> 'a -> 'a) (env:'mr t) : 'a -> 'a =
  Hashtbl.fold f env.symb

let fold_operations (f:string -> 'mr t_operation_infos -> 'a -> 'a) (env:'mr t) : 'a -> 'a =
  Hashtbl.fold f env.ops

let add_abstract_sets (src:Btype.t_atomic_src) (accu:(Btype.t_atomic_src*string) list)
    (itf:t_interface) : (Btype.t_atomic_src*string) list =
  let open MachineInterface in
  let aux accu (x:t_symb) = match x.kind with
    | G_Abstract_Set -> (src,x.id)::accu
    | _ -> accu
  in
  List.fold_left aux accu (get_symbols itf)
   *)
