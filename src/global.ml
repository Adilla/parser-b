type lident = SyntaxCore.lident
type ren_ident = SyntaxCore.ren_ident
type loc = Utils.loc

type t_abstract = private T_Abs
type t_concrete = private T_Conc
type t_mch = private T_Mch
type t_ref = private T_Ref

type 'ac t_redeclared =
  | Implicitely : t_concrete t_redeclared
  | By_Machine : loc -> 'ac t_redeclared
  | By_Included_Or_Imported : ren_ident -> 'ac t_redeclared

type ('mr,'ac) t_decl =
  | D_Machine : loc -> ('mr,'ac) t_decl
  | D_Seen : ren_ident -> ('mr,'ac) t_decl
  | D_Used : ren_ident -> (t_mch,'ac) t_decl
  | D_Included_Or_Imported : ren_ident -> ('mr,'ac) t_decl
  | D_Disappearing : (t_ref,t_abstract) t_decl
  | D_Redeclared : 'ac t_redeclared -> (t_ref,'ac) t_decl

type t_variable = private T_Var
type t_constant = private T_Const

type t_param_kind = Set | Scalar

type _ t_global_kind = 
  | K_Parameter : t_param_kind -> t_concrete t_global_kind
  | K_Abstract_Variable : t_abstract t_global_kind
  | K_Abstract_Constant : t_abstract t_global_kind
  | K_Concrete_Variable : t_concrete t_global_kind
  | K_Concrete_Constant : t_concrete t_global_kind
  | K_Abstract_Set : t_concrete t_global_kind
  | K_Concrete_Set : string list  -> t_concrete t_global_kind
  | K_Enumerate : t_concrete t_global_kind

type 'mr t_kind = Pack : 'ac t_global_kind*('mr,'ac) t_decl -> 'mr t_kind

type 'a t_symbol_infos = {
  sy_typ:Btype.t;
  sy_kind:'a t_kind
}

type 'a t_op_decl =
  | OD_Current : loc -> t_mch t_op_decl
  | OD_Seen : ren_ident -> 'a t_op_decl
  | OD_Included_Or_Imported : ren_ident -> 'a t_op_decl
  | OD_Included_Or_Imported_And_Promoted : ren_ident*loc -> t_mch t_op_decl
  | OD_Refined : lident -> t_ref t_op_decl
  | OD_Current_And_Refined : loc*lident -> t_ref t_op_decl
  | OD_Included_Or_Imported_And_Refined : ren_ident*lident -> t_ref t_op_decl
  | OD_Included_Or_Imported_Promoted_And_Refined : ren_ident*loc*lident -> t_ref t_op_decl
  | OD_Local_Spec : loc -> t_ref t_op_decl
  | OD_Local_Spec_And_Implem : loc*loc -> t_ref t_op_decl

type 'a t_operation_infos =
  { op_args_in: (string*Btype.t) list;
    op_args_out: (string*Btype.t) list;
    op_readonly:bool;
    op_src: 'a t_op_decl; }

module MachineInterface :
sig
  type t
  type t_symb = S : { id:string; typ:Btype.t; kind:'ac t_global_kind } -> t_symb
  type t_op = { id:string; args_in: (string*Btype.t) list; args_out: (string*Btype.t) list; readonly:bool }
  type t_param = { id:string; typ:Btype.t; kind:t_param_kind }
  val make : t_param list -> t_symb list -> t_op list -> t
  val get_symbols : t -> t_symb list
  val get_operations : t -> t_op list
  val get_params : t -> t_param list
end = struct
  type t_symb = S : { id:string; typ:Btype.t; kind:'ac t_global_kind } -> t_symb
  type t_op = { id:string; args_in: (string*Btype.t) list; args_out: (string*Btype.t) list; readonly:bool }
  type t_param = { id:string; typ:Btype.t; kind:t_param_kind }
  type t = t_param list * t_symb list * t_op list

  let make l0 l1 l2 = (l0,l1,l2)
  let get_symbols (_,l,_) = l
  let get_operations (_,_,l) = l
  let get_params (l,_,_) = l
end

type t_interface = MachineInterface.t

type _ env_kind = Mch : t_mch env_kind | Ref : t_ref env_kind

type 'a t = {
  kind:'a env_kind;
  mutable alias: Btype.t_alias;
  mutable deps: string list;
  params: lident list;
  symb:(string,'a t_symbol_infos) Hashtbl.t;
  ops:(string,'a t_operation_infos) Hashtbl.t
}

let create_mch params : t_mch t =
  { kind=Mch;
    alias=Btype.no_alias;
    deps=[];
    params;
    symb=Hashtbl.create 47;
    ops=Hashtbl.create 47 }

let create_ref params : t_ref t =
  { kind=Ref;
    params;
    deps=[];
    alias=Btype.no_alias;
    symb=Hashtbl.create 47;
    ops=Hashtbl.create 47 }

let get_alias env = env.alias

let get_symbol (env:'a t) (id:string) : 'a t_symbol_infos option =
  Hashtbl.find_opt env.symb id

type 'a t_source =
  | S_Current : loc -> 'a t_source
  | S_Seen : ren_ident -> 'a t_source
  | S_Used : ren_ident -> t_mch t_source
  | S_Refined : lident -> t_ref t_source
  | S_Included_Or_Imported : ren_ident -> 'a t_source

type 'a t_op_source =
  | SO_Current : loc -> 'a t_op_source
  | SO_Seen : ren_ident -> 'a t_op_source
  | SO_Refined : lident -> t_ref t_op_source
  | SO_Included_Or_Imported : ren_ident -> 'a t_op_source
  | SO_Local : loc -> t_ref t_op_source

let update_decl (type mr ac1 ac2) (ki:ac2 t_global_kind)
    (decl:(mr,ac1) t_decl) (src:mr t_source) : mr t_kind option =
  match decl, src with
  | D_Machine l, S_Refined _ -> Some (Pack(ki,D_Redeclared (By_Machine l)))
  | D_Disappearing, S_Current l -> Some (Pack(ki,D_Redeclared (By_Machine l)))
  | D_Included_Or_Imported inc, S_Refined _ ->
    Some (Pack(ki,D_Redeclared (By_Included_Or_Imported inc)))
  | D_Disappearing, S_Included_Or_Imported inc ->
    Some (Pack(ki,D_Redeclared (By_Included_Or_Imported inc)))
  | D_Redeclared Implicitely, S_Included_Or_Imported inc ->
    Some (Pack(ki,D_Redeclared (By_Included_Or_Imported inc)))
  | _, _ -> None

let update_kind (type ac1 ac2)  (k1:ac1 t_global_kind) (k2:ac2 t_global_kind) : ac2 t_global_kind option =
  match k1, k2 with
  | K_Abstract_Variable, K_Abstract_Variable -> Some k2
  | K_Abstract_Constant, K_Abstract_Constant -> Some k2
  | K_Concrete_Variable,K_Concrete_Variable -> Some k2
  | K_Concrete_Constant, K_Concrete_Constant -> Some k2
  | K_Abstract_Set, K_Abstract_Set -> Some k2
  | K_Concrete_Set lst1, K_Concrete_Set lst2 ->
    begin try
        if List.for_all2 String.equal lst1 lst2 then Some k2
        else None
      with Invalid_argument _ -> None
    end
  | K_Enumerate, K_Enumerate -> Some k2
  | K_Abstract_Constant, K_Concrete_Constant -> Some k2
  | K_Abstract_Variable, K_Concrete_Variable -> Some k2
  | K_Parameter _, K_Parameter _ -> Some k2
  | _, _ -> None

let add_alias (s:'a t) (alias:string) (ty:Btype.t) : bool =
  match Btype.add_alias s.alias alias ty with
  | None -> false
  | Some alias -> (s.alias <- alias; true)

let _add_symbol (type mr ac) (env:mr t) (err_loc:loc) (id:string) (sy_typ:Btype.t)
    (ki:ac t_global_kind) (src:mr t_source) : unit
  =
  (match src, ki with
   | S_Included_Or_Imported inc, K_Abstract_Set ->
     let success = add_alias env id (Btype.mk_Abstract_Set (Btype.T_Seen inc.SyntaxCore.r_str) id) in
     assert success
   | S_Included_Or_Imported inc, K_Concrete_Set _ ->
     let success = add_alias env id (Btype.mk_Concrete_Set (Btype.T_Seen inc.SyntaxCore.r_str) id) in
     assert success
   | _, _ -> () );
  match Hashtbl.find_opt env.symb id with
  | Some infos ->
    begin match infos.sy_kind with
      | Pack (old_kind,old_decl) ->
        if not (Btype.is_equal_modulo_alias env.alias infos.sy_typ sy_typ) then
          Error.error  err_loc
            ("The identifier '" ^ id ^ "' has type " 
             ^ Btype.to_string sy_typ
             ^ " but was previously declared with type "
             ^ Btype.to_string infos.sy_typ ^ ".")
        else
          begin match update_kind old_kind ki with
            | None ->
              Error.error err_loc
                ("The kind of the identifier '" ^ id ^ "' is different from previous declaration.") 
            | Some kind ->
              begin match update_decl kind old_decl src with
                | None ->
                  Error.error err_loc ("The identifier '" ^ id ^ "' clashes with previous declaration.")
                | Some sy_kind -> Hashtbl.replace env.symb id { sy_typ; sy_kind }
              end
          end
    end
  | None ->
    let sy_kind:mr t_kind =
    match src with
      | S_Current lc -> Pack (ki,D_Machine lc)
      | S_Seen mch -> Pack (ki,D_Seen mch)
      | S_Used mch -> (Pack (ki,D_Used mch): t_mch t_kind)
      | S_Included_Or_Imported mch -> Pack (ki,D_Included_Or_Imported mch)
      | S_Refined _ ->
        begin match ki with
          | K_Parameter _ -> assert false
          | K_Abstract_Variable -> Pack(ki,D_Disappearing)
          | K_Abstract_Constant -> Pack(ki,D_Disappearing)
          | K_Concrete_Variable -> Pack(ki,D_Redeclared Implicitely)
          | K_Concrete_Constant -> Pack(ki,D_Redeclared Implicitely)
          | K_Abstract_Set -> Pack(ki,D_Redeclared Implicitely)
          | K_Concrete_Set _ -> Pack(ki,D_Redeclared Implicitely)
          | K_Enumerate -> Pack(ki,D_Redeclared Implicitely)
        end
    in
    Hashtbl.add env.symb id { sy_typ; sy_kind }

let add_symbol (type mr ac) (env:mr t) (loc:loc) (id:string) (typ:Btype.t) (ki:ac t_global_kind) : unit =
  _add_symbol env loc id typ ki (S_Current loc)

let update_op_source (type a) (current_source:a t_op_decl) (new_source:a t_op_source) : a t_op_decl option =
  match current_source, new_source with
  | OD_Refined ref, SO_Current l -> Some (OD_Current_And_Refined (l,ref))
  | OD_Refined ref, SO_Included_Or_Imported inc -> Some (OD_Included_Or_Imported_And_Refined (inc,ref))
  | OD_Included_Or_Imported inc, SO_Refined ref -> Some (OD_Included_Or_Imported_And_Refined (inc,ref))
  | OD_Local_Spec spe, SO_Current imp -> Some (OD_Local_Spec_And_Implem (spe,imp))
  | _, _ -> None

let check_args_type env (err_loc:loc) (args_old:(string*Btype.t)list) (args_new:(string*Btype.t)list) : unit =
  let aux (x1,ty1) (x2,ty2) =
    if String.equal x1 x2 then
      if Btype.is_equal_modulo_alias env.alias ty1 ty2 then ()
      else Error.error err_loc
          ("The parameter '"^x1^"' has type '"^Btype.to_string ty1^
           "' but parameter of type '"^Btype.to_string ty2^"' was expected.")
    else Error.error err_loc ("Parameter '"^x2^"' expected but '"^x1^"' was found.")
  in
  try List.iter2 aux args_old args_new;
  with Invalid_argument _ -> Error.error err_loc "Unexpected number of parameters."

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
      | K_Parameter _ -> ()
      | K_Abstract_Variable ->
        let ren_id = match mch.r_prefix with
          | None -> id
          | Some p -> p ^ "." ^ id 
        in
        _add_symbol env mch.SyntaxCore.r_loc ren_id
          (Btype.change_current (Btype.T_Seen mch.SyntaxCore.r_str) typ)
          kind (S_Seen mch)
      | K_Concrete_Variable ->
        let ren_id = match mch.r_prefix with
          | None -> id
          | Some p -> p ^ "." ^ id 
        in
        _add_symbol env mch.SyntaxCore.r_loc ren_id
          (Btype.change_current (Btype.T_Seen mch.SyntaxCore.r_str) typ)
          kind (S_Seen mch)
      | _ ->
        begin match mch.r_prefix with
          | None ->
            _add_symbol env mch.SyntaxCore.r_loc id
              (Btype.change_current (Btype.T_Seen mch.SyntaxCore.r_str) typ)
              kind (S_Seen mch)
          | Some _ ->
            if is_in_deps env mch.SyntaxCore.r_str then ()
            else
              _add_symbol env mch.SyntaxCore.r_loc id
                (Btype.change_current (Btype.T_Seen mch.SyntaxCore.r_str) typ)
                kind (S_Seen mch)
        end
    ) (get_symbols itf);
  let change_current = List.map (fun (s,ty) -> (s,Btype.change_current (Btype.T_Seen mch.SyntaxCore.r_str) ty)) in
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
      | K_Parameter _ -> ()
      | K_Abstract_Variable ->
        let ren_id = match mch.r_prefix with
          | None -> id
          | Some p -> p ^ "." ^ id 
        in
        _add_symbol env mch.SyntaxCore.r_loc ren_id
          (Btype.change_current (Btype.T_Seen mch.SyntaxCore.r_str) typ) (*FIXME seen*)
          kind (S_Used mch)
      | K_Concrete_Variable ->
        let ren_id = match mch.r_prefix with
          | None -> id
          | Some p -> p ^ "." ^ id 
        in
        _add_symbol env mch.SyntaxCore.r_loc ren_id
          (Btype.change_current (Btype.T_Seen mch.SyntaxCore.r_str) typ) (*FIXME seen*)
          kind (S_Used mch)
      | _ ->
        begin match mch.r_prefix with
          | None ->
            _add_symbol env mch.SyntaxCore.r_loc id
              (Btype.change_current (Btype.T_Seen mch.SyntaxCore.r_str) typ) (*FIXME seen*)
              kind (S_Used mch)
          | Some _ ->
            if is_in_deps env mch.SyntaxCore.r_str then ()
            else
              _add_symbol env mch.SyntaxCore.r_loc id
                (Btype.change_current (Btype.T_Seen mch.SyntaxCore.r_str) typ) (*FIXME seen*)
                kind (S_Used mch)
        end
    ) (get_symbols itf);
  env.deps <- mch.r_str::env.deps
(*FIXME pas les operations?*)

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
      | K_Parameter _ -> ()
      | K_Abstract_Variable ->
        let ren_id = match mch.r_prefix with
          | None -> id
          | Some p -> p ^ "." ^ id 
        in
        _add_symbol env mch.SyntaxCore.r_loc ren_id (Btype.subst alias typ) kind (S_Included_Or_Imported mch)
      | K_Concrete_Variable ->
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
        ( _add_symbol env hd1.lid_loc hd2.id hd2.typ (K_Parameter hd2.kind) (S_Current hd1.lid_loc);
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

let to_interface (type mr) (env:mr t) : MachineInterface.t = (*FIXME on peut se contenter de mr = t_mch*)
  let aux1 (x:string) (symb:mr t_symbol_infos) (lst:MachineInterface.t_symb list) =
    match symb.sy_kind with
    | Pack (kind,decl) ->
      if is_exported_symbol decl then
        (MachineInterface.S { id=x; typ=symb.sy_typ; kind })::lst
      else lst
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
        | Pack(K_Parameter kind,_) ->
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
      (*           Error.raise_exn err_loc ("The operation '"^x^"' is not refined.") *)
      | OD_Local_Spec lc ->
        Error.error lc ("The operation '"^x^"' is not implemented.")
      | OD_Included_Or_Imported_And_Refined _ -> ()
      (*           Error.raise_exn err_loc ("The operation '"^x^"' is not refined (missing promotion?).") *)
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
