open Utils
open Syntax
open Btype

let allow_becomes_such_that_in_implementation = ref false
let allow_out_parameters_in_precondition = ref false

(* *****************************************************************************
 * Type Checking for Components
 * ************************************************************************** *)

let raise_if_some str = function
  | None -> None
  | Some p -> Error.raise_exn p.P.prd_loc str 

let raise_if_not_empty str = function
  | [] -> []
  | lid::_ -> Error.raise_exn lid.lid_loc str 

let raise_if_not_empty_2 str = function
  | [] -> []
  | mi::_ -> Error.raise_exn mi.mi_mch.lid_loc str 

let type_set : P.set -> T.set = function
  | Abstract_Set v ->
    Abstract_Set { var_loc=v.lid_loc; var_id=v.lid_str;
                   var_typ=Btype.mk_Power (Btype.mk_Atomic v.lid_str) }
  | Concrete_Set (v,elts) ->
    Concrete_Set ({ var_loc=v.lid_loc; var_id=v.lid_str;
                    var_typ=Btype.mk_Power (Btype.mk_Atomic v.lid_str) },
                  List.map (fun e -> { T.var_loc=e.lid_loc; var_id=e.lid_str; var_typ=(Btype.mk_Atomic v.lid_str) }) elts )

let load_seen_mch_exn (f:Utils.loc->string->Global.t_interface option) (env:Global.t) (mch:lident) : unit =
  match f mch.lid_loc mch.lid_str with
  | None -> Error.raise_exn mch.lid_loc ("The machine '"^mch.lid_str^"' does not typecheck.")
  | Some itf ->
    begin match Global.load_interface_for_seen_machine env itf mch with
      | Ok () -> ()
      | Error err -> raise (Error.Error err)
    end

let load_included_mch_exn (f:Utils.loc->string->Global.t_interface option) (env:Global.t) (mi:_ machine_instanciation) : _ machine_instanciation =
  match mi.mi_params with
  | [] ->
    begin match f mi.mi_mch.lid_loc mi.mi_mch.lid_str with
      | None -> Error.raise_exn mi.mi_mch.lid_loc ("The machine '"^mi.mi_mch.lid_str^"' does not typecheck.")
      | Some itf ->
        begin match Global.load_interface_for_included_machine env itf mi.mi_mch with
          | Ok () -> { mi_mch=mi.mi_mch; mi_params=[] }
          | Error err -> raise (Error.Error err)
        end
    end
  | _::_ -> Error.raise_exn mi.mi_mch.lid_loc "Not implemented: inclusion of machine with parameters."

let declare_global_symbol_exn (env:Global.t)  (kind:Global.t_kind) (v:T.var) : unit =
  match Global.add_symbol env v.var_loc v.var_id v.var_typ kind with
  | Ok () -> ()
  | Error err -> raise (Error.Error err)

let declare_set_exn (env:Global.t) (s:P.set) : T.set =
  let ts = type_set s in
  let () = match ts with
    | Abstract_Set v -> declare_global_symbol_exn env Global.K_Abstract_Set v
    | Concrete_Set (v,elts) ->
      let lst = List.map (fun x -> x.T.var_id) elts in
      declare_global_symbol_exn env (Global.K_Concrete_Set lst) v;
      List.iter (declare_global_symbol_exn env Global.K_Enumerate) elts
  in
  ts

let declare (ctx:Inference.Local.t) (id:string) (ro:bool) : Inference.Local.t = 
  let mt = Btype.Open.new_meta () in
  Inference.Local.add ctx id mt ro

let declare_local_symbol_with_global_type (env:Global.t) (ctx:Inference.Local.t) (v:lident) : Inference.Local.t =
  match Global.get_symbol_type env v.lid_str with
  | None -> declare ctx v.lid_str false
  | Some ty -> Inference.Local.add ctx v.lid_str (ty :> Btype.Open.t) false

let type_var_exn (env:Global.t) (ctx:Inference.Local.t) (v:lident) : T.var =
  match Inference.Local.get ctx v.lid_str with
  | Some (var_typ,_) ->
    begin match Btype.close var_typ with
      | None ->
        let str = Printf.sprintf "The type of symbol '%s' could not be fully infered. The type infered so far is '%s'."
            v.lid_str (Btype.Open.to_string var_typ) in
        Error.raise_exn v.lid_loc str
      | Some var_typ -> { var_loc=v.lid_loc; var_id=v.lid_str; var_typ }
    end
  | None ->
    begin match Global.get_symbol_type env v.lid_str with
      | Some var_typ -> { var_loc=v.lid_loc; var_id=v.lid_str; var_typ }
      | None -> assert false
    end

let type_predicate_exn cl env ctx p =
  match Inference.type_predicate cl env ctx p with
  | Ok x -> x
  | Error err -> raise (Error.Error err)

let type_substitution_exn cl env ctx s =
  match Inference.type_substitution cl env ctx s with
  | Ok x -> x
  | Error err -> raise (Error.Error err)

let type_expression_exn cl env ctx e =
  match Inference.type_expression cl env ctx e with
  | Ok x -> x
  | Error err -> raise (Error.Error err)

let declare_constants_exn (env:Global.t) cconst aconst prop =
  let ctx = Inference.Local.create () in
  let ctx = List.fold_left (declare_local_symbol_with_global_type env) ctx cconst in
  let ctx = List.fold_left (declare_local_symbol_with_global_type env) ctx aconst in
  let t_prop = Utils.map_opt (type_predicate_exn Global.C_Properties env ctx) prop in
  let t_cconst = List.map (type_var_exn env ctx) cconst in
  let t_aconst = List.map (type_var_exn env ctx) aconst in
  let _ = List.iter (declare_global_symbol_exn env Global.K_Concrete_Constant) t_cconst in
  let _ = List.iter (declare_global_symbol_exn env Global.K_Abstract_Constant) t_aconst in
  (t_cconst,t_aconst,t_prop)

let declare_variables_exn (env:Global.t) cvars avars inv =
  let ctx = Inference.Local.create () in
  let ctx = List.fold_left (declare_local_symbol_with_global_type env) ctx cvars in
  let ctx = List.fold_left (declare_local_symbol_with_global_type env) ctx avars in
  let t_inv = Utils.map_opt
      (type_predicate_exn Global.C_Invariant_Or_Assertions env ctx) inv
  in
  let t_cvars = List.map (type_var_exn env ctx) cvars in
  let t_avars = List.map (type_var_exn env ctx) avars in
  let _ = List.iter (declare_global_symbol_exn env Global.K_Concrete_Variable) t_cvars in
  let _ = List.iter (declare_global_symbol_exn env Global.K_Abstract_Variable) t_avars in
  (t_cvars,t_avars,t_inv)

let declare_var_list (ctx:Inference.Local.t) (lst:lident list) (ro:bool) : Inference.Local.t =
  List.fold_left (fun ctx v -> declare ctx v.lid_str ro) ctx lst

let check_signature op s =
  let rec aux lst1 lst2 =
    match lst1, lst2 with
    | [], [] -> ()
    | v::lst1, (x,_)::lst2 ->
      if String.equal v.lid_str x then aux lst1 lst2
      else Error.raise_exn v.lid_loc ("Expecting parameter '"^x^"' instead of '"^v.lid_str^"'.")
    | v::_, [] -> Error.raise_exn v.lid_loc ("Unexpected parameter '"^v.lid_str^"'.")
    | [], (x,_)::_ ->
      Error.raise_exn op.op_name.lid_loc ("Missing parameter '"^x^"'.")
  in
  aux op.op_in s.Global.args_in;
  aux op.op_out s.Global.args_out

let get_operation_context_exn (env:Global.t) (op:P.operation) : Inference.Local.t*Inference.Local.t =
  match Global.get_operation_type2 env op.op_name.lid_str with
  | None ->
    let ctx = Inference.Local.create () in
    let ctx = declare_var_list ctx op.op_in true in
    let ctx2 = declare_var_list ctx op.op_out false in
    (ctx,ctx2)
  | Some args ->
    let ctx = Inference.Local.create () in
    let aux ro ctx (s,ty:_*Btype.t) = Inference.Local.add ctx s (ty :> Btype.Open.t) ro in
    let ctx = List.fold_left (aux true) ctx args.Global.args_in in
    let ctx2 = List.fold_left (aux false) ctx args.Global.args_out in
    let () = check_signature op args in
    (ctx,ctx2)

let mem id = List.exists (String.equal id)

let rec is_read_only (gl:Global.t) (ctx:P.ident list) (s:P.substitution) : bool =
  match s.sub_desc with
  | Skip -> true
  | Affectation (Tuple xlst,_) | BecomesElt (xlst,_) | BecomesSuch (xlst,_) ->
    let aux v = mem v.lid_str ctx in
    List.for_all aux (Nlist.to_list xlst)
  | Affectation (Function(v,_),_) | Affectation (Record(v,_),_) ->
    mem v.lid_str ctx
  | CallUp (args_out,id,args_in) ->
    let aux v = mem v.lid_str ctx in
    List.for_all aux args_out && Global.is_operation_readonly gl id.lid_str
  | Pre (p,s0) -> is_read_only gl ctx s0
  | Assert (p,s0) -> is_read_only gl ctx s0
  | Choice nlst -> List.for_all (is_read_only gl ctx) (Nlist.to_list nlst)
  | IfThenElse (nlst,opt) | Select (nlst,opt) ->
    let aux (_,s0) = is_read_only gl ctx s0 in
    let opt_ro = match opt with
      | None -> true
      | Some s0 -> is_read_only gl ctx s0
    in
    opt_ro && List.for_all aux (Nlist.to_list nlst)
  | Case (_,nlst,opt) ->
    let aux (_,s0) = is_read_only gl ctx s0 in
    let opt_ro = match opt with
      | None -> true
      | Some s0 -> is_read_only gl ctx s0
    in
    opt_ro && List.for_all aux (Nlist.to_list nlst)
  | Any (xlst,_,s0) | Let (xlst,_,s0) | Var (xlst,s0) ->
    let ctx = List.fold_left (fun ctx v -> v.lid_str::ctx) ctx (Nlist.to_list xlst) in
    is_read_only gl ctx s0
  | While (p1,s0,p2,e) -> is_read_only gl ctx s0
  | Sequencement (s1,s2) | Parallel (s1,s2) ->
    is_read_only gl ctx s1 && is_read_only gl ctx s2

let declare_local_operation_exn (env:Global.t) (op:P.operation) : T.operation =
  let ctx = Inference.Local.create () in
  let ctx = declare_var_list ctx op.op_in true in
  let (ctx,op_body) =
    if !allow_out_parameters_in_precondition then
      let ctx = declare_var_list ctx op.op_out false in
      let op_body = type_substitution_exn Global.C_Local_Operations env ctx op.op_body in
      (ctx,op_body)
    else
      begin match op.op_body.sub_desc with
        | Pre (p,s) ->
          let tp = type_predicate_exn Global.C_Local_Operations env ctx p in
          let ctx = declare_var_list ctx op.op_out false in
          let ts = type_substitution_exn Global.C_Local_Operations env ctx s in
          (ctx,{ sub_loc=op.op_body.sub_loc; sub_desc=Pre (tp,ts)})
        | _ ->
          let ctx = declare_var_list ctx op.op_out false in
          let op_body = type_substitution_exn Global.C_Local_Operations env ctx op.op_body in
          (ctx,op_body)
      end
  in
  let op_in = List.map (type_var_exn env ctx) op.op_in in
  let op_out = List.map (type_var_exn env ctx) op.op_out in
  let aux v = (v.T.var_id, v.T.var_typ) in
  let args_out = List.map aux op_out in
  let args_in  = List.map aux op_in in
  let loc = op.op_name.lid_loc in
  let is_readonly = is_read_only env (Inference.Local.get_vars ctx) op.op_body in
  match Global.add_operation env loc op.op_name.lid_str { Global.args_in; args_out } is_readonly true with
  | Ok () -> { op_name=op.op_name; op_in; op_out; op_body }
  | Error err -> raise (Error.Error err)

let declare_operation_exn (env:Global.t) (op:P.operation) : T.operation =
  let (ctx,ctx2) = get_operation_context_exn env op in
  let (ctx,op_body) =
    if !allow_out_parameters_in_precondition then
      let op_body = type_substitution_exn Global.C_Local_Operations env ctx2 op.op_body in
      (ctx2,op_body)
    else
      begin match op.op_body.sub_desc with
        | Pre (p,s) ->
          let tp = type_predicate_exn Global.C_Local_Operations env ctx p in
          let ts = type_substitution_exn Global.C_Local_Operations env ctx2 s in
          (ctx2,{ sub_loc=op.op_body.sub_loc; sub_desc=Pre (tp,ts)})
        | _ ->
          let op_body = type_substitution_exn Global.C_Local_Operations env ctx2 op.op_body in
          (ctx2,op_body)
      end
  in
  let op_in = List.map (type_var_exn env ctx) op.op_in in
  let op_out = List.map (type_var_exn env ctx) op.op_out in
  let aux v = (v.T.var_id, v.T.var_typ) in
  let args_out = List.map aux op_out in
  let args_in  = List.map aux op_in in
  let is_readonly = is_read_only env (Inference.Local.get_vars ctx) op.op_body in
  match Global.add_operation env op.op_name.lid_loc op.op_name.lid_str { Global.args_in; args_out } is_readonly false with
  | Ok () -> { op_name=op.op_name; op_in; op_out; op_body }
  | Error err -> raise (Error.Error err)

let rec is_machine_subst (s:P.substitution) : unit =
  match s.sub_desc with
  | Skip | BecomesElt _ | BecomesSuch _ | Affectation _ | CallUp _ -> ()
  | Any (_,_,s) | Let (_,_,s) | Pre (_,s) | Assert (_,s) -> is_machine_subst s
  | Choice slst -> List.iter is_machine_subst (Nlist.to_list slst)
  | IfThenElse (pslst,s_else)
  | Select (pslst,s_else) ->
    let aux (_,s) = is_machine_subst s in
    let () = List.iter aux (Nlist.to_list pslst) in
    begin match s_else with
      | None -> ()
      | Some s -> is_machine_subst s
    end
  | Case (_,nlst,c_else) ->
    let aux (_,s) = is_machine_subst s in
    let () = List.iter aux (Nlist.to_list nlst) in
    begin match c_else with
      | None -> ()
      | Some s -> is_machine_subst s
    end
  | Var _  -> Error.raise_exn s.sub_loc "The substitution VAR is not allowed in a machine."
  | While _ -> Error.raise_exn s.sub_loc "The substitution WHILE is not allowed in a machine."
  | Sequencement _ -> Error.raise_exn s.sub_loc "The substitution ';' is not allowed in a machine."
  | Parallel (s1,s2) -> ( is_machine_subst s1; is_machine_subst s2 )

let rec is_refinement_subst (s:P.substitution) : unit =
  match s.sub_desc with
  | Skip | BecomesElt _ | BecomesSuch _ | Affectation _ | CallUp _ -> ()
  | Var (_,s) | Any (_,_,s) | Let (_,_,s) | Pre (_,s) | Assert (_,s) -> is_refinement_subst s
  | Choice slst -> List.iter is_refinement_subst (Nlist.to_list slst)
  | IfThenElse (pslst,s_else)
  | Select (pslst,s_else) ->
    let aux (_,s) = is_refinement_subst s in
    let () = List.iter aux (Nlist.to_list pslst) in
    begin match s_else with
      | None -> ()
      | Some s -> is_refinement_subst s
    end
  | Case (_,nlst,c_else) ->
    let aux (_,s) = is_refinement_subst s in
    let () = List.iter aux (Nlist.to_list nlst) in
    begin match c_else with
      | None -> ()
      | Some s -> is_refinement_subst s
    end
  | While _ -> Error.raise_exn s.sub_loc "The substitution WHILE is not allowed in a refinement."
  | Sequencement (s1,s2)
  | Parallel (s1,s2) -> ( is_refinement_subst s1; is_refinement_subst s2 )

let rec is_implementation_subst (s:P.substitution) : unit =
  match s.sub_desc with
  | Skip |  Affectation _ | CallUp _ -> ()
  | Parallel _ | Select _ | BecomesElt _ | Any _ | Let _ | Pre _ | Choice _ ->
    Error.raise_exn s.sub_loc "This substitution is not allowed in an implementation."
  | BecomesSuch _ ->
    if not !allow_becomes_such_that_in_implementation then
      Error.raise_exn s.sub_loc "This substitution is not allowed in an implementation."
  | While (_,s,_,_) | Var (_,s) | Assert (_,s) -> is_implementation_subst s
  | IfThenElse (pslst,s_else) ->
    let aux (_,s) = is_implementation_subst s in
    let () = List.iter aux (Nlist.to_list pslst) in
    begin match s_else with
      | None -> ()
      | Some s -> is_implementation_subst s
    end
  | Case (_,nlst,c_else) ->
    let aux (_,s) = is_implementation_subst s in
    let () = List.iter aux (Nlist.to_list nlst) in
    begin match c_else with
      | None -> ()
      | Some s -> is_implementation_subst s
    end
  | Sequencement (s1,s2) ->
    ( is_implementation_subst s1; is_implementation_subst s2 )

let type_machine_exn (f:Utils.loc->string->Global.t_interface option) (env:Global.t) (mch:_ machine_desc) : _ machine_desc =
  let mch_constraints = raise_if_some "Not implemented: machine with clause CONSTRAINTS." mch.mch_constraints in
  let mch_promotes = raise_if_not_empty "Not implemented: clause PROMOTES." mch.mch_promotes in
  let mch_extends = raise_if_not_empty_2 "Not implemented: clause EXTENDS." mch.mch_extends in
  let mch_uses = raise_if_not_empty "Not implemented: clause USES." mch.mch_uses in
  let () = List.iter (load_seen_mch_exn f env) mch.mch_sees in
  let mch_includes = List.map (load_included_mch_exn f env) mch.mch_includes in
  let mch_sets = List.map (declare_set_exn env) mch.mch_sets in
  let (mch_concrete_constants,mch_abstract_constants,mch_properties) =
    declare_constants_exn env mch.mch_concrete_constants
      mch.mch_abstract_constants mch.mch_properties
  in
  let (mch_concrete_variables,mch_abstract_variables,mch_invariant) =
    declare_variables_exn env mch.mch_concrete_variables
      mch.mch_abstract_variables mch.mch_invariant
  in
  let ctx = Inference.Local.create () in
  let mch_assertions =
    List.map (type_predicate_exn Global.C_Invariant_Or_Assertions env ctx) mch.mch_assertions
  in
  let mch_initialisation =
    Utils.map_opt (type_substitution_exn Global.C_Operations env ctx) mch.mch_initialisation
  in
  let _ = Utils.map_opt is_machine_subst mch.mch_initialisation in
  let mch_operations = List.map (declare_operation_exn env) mch.mch_operations in
  let _ = List.iter (fun op -> is_machine_subst op.op_body) mch.mch_operations in
  { mch_constraints; mch_sees=mch.mch_sees; mch_includes; mch_promotes; mch_extends;
    mch_uses; mch_sets; mch_concrete_constants; mch_abstract_constants;
    mch_properties; mch_concrete_variables; mch_abstract_variables;
    mch_invariant; mch_assertions; mch_initialisation; mch_operations }

let load_refines_exn (f:Utils.loc->string->Global.t_interface option) (env:Global.t) (mch:lident) : unit =
  match f mch.lid_loc mch.lid_str with
  | None -> Error.raise_exn mch.lid_loc ("The machine '"^mch.lid_str^"' does not typecheck.")
  | Some itf ->
    begin match Global.load_interface_for_refined_machine env itf mch with
      | Ok () -> ()
      | Error err -> raise (Error.Error err)
    end

let type_refinement_exn (f:Utils.loc->string->Global.t_interface option) (env:Global.t) ref : _ refinement_desc =
  let () = load_refines_exn f env ref.ref_refines in
  let ref_promotes = raise_if_not_empty "Not implemented: clause PROMOTES." ref.ref_promotes in
  let ref_extends = raise_if_not_empty_2 "Not implemented: clause EXTENDS."ref.ref_extends in
  let () = List.iter (load_seen_mch_exn f env) ref.ref_sees in
  let ref_includes = List.map (load_included_mch_exn f env) ref.ref_includes in
  let ref_sets = List.map (declare_set_exn env) ref.ref_sets in
  let (ref_concrete_constants,ref_abstract_constants,ref_properties) =
    declare_constants_exn env ref.ref_concrete_constants
      ref.ref_abstract_constants ref.ref_properties
  in
  let (ref_concrete_variables,ref_abstract_variables,ref_invariant) =
    declare_variables_exn env ref.ref_concrete_variables
      ref.ref_abstract_variables ref.ref_invariant
  in
  let ctx = Inference.Local.create () in
  let ref_assertions =
    List.map (type_predicate_exn Global.C_Invariant_Or_Assertions env ctx) ref.ref_assertions
  in
  let ref_initialisation =
    Utils.map_opt (type_substitution_exn Global.C_Operations env ctx) ref.ref_initialisation
  in
  let _ = Utils.map_opt is_refinement_subst ref.ref_initialisation in
  let ref_local_operations =
    List.map (declare_local_operation_exn env) ref.ref_local_operations
  in
  let ref_operations = List.map (declare_operation_exn env) ref.ref_operations in
  let _ = List.map (fun op -> is_refinement_subst op.op_body) ref.ref_operations in
  { ref_refines=ref.ref_refines; ref_sees=ref.ref_sees; ref_includes; ref_promotes;
    ref_extends; ref_sets; ref_concrete_constants; ref_abstract_constants;
    ref_properties; ref_concrete_variables; ref_abstract_variables; ref_invariant;
    ref_assertions; ref_initialisation; ref_operations; ref_local_operations; }

let type_value_exn (env:Global.t) (v,e:lident*P.expression) : T.var*T.expression =
  match Global.get_symbol_type_in_clause env v.lid_loc v.lid_str Global.C_Values with
  | Error err -> raise (Error.Error err)
  | Ok var_typ ->
    let ctx = Inference.Local.create () in
    let te = type_expression_exn Global.C_Values env ctx e in
    if Btype.is_equal_modulo_alias (Global.get_alias env) te.exp_typ var_typ then
      ( {var_loc=v.lid_loc;var_id=v.lid_str;var_typ},te)
    else
      Error.raise_exn e.exp_loc
        ("This expression has type '" ^ to_string te.exp_typ ^
         "' but an expression of type '" ^ to_string var_typ ^"' was expected.")

let promote_op_exn (env:Global.t) (op_name:lident) : unit =
  match Global.promote_operation env op_name.lid_loc op_name.lid_str with
  | Ok () -> ()
  | Error err -> raise (Error.Error err)

let load_extended_mch_exn (f:Utils.loc->string->Global.t_interface option) (env:Global.t) (mi:_ machine_instanciation) : _ machine_instanciation =
  match mi.mi_params with
  | [] ->
    begin match f mi.mi_mch.lid_loc mi.mi_mch.lid_str with
      | None -> Error.raise_exn mi.mi_mch.lid_loc ("The machine '"^mi.mi_mch.lid_str^"' does not typecheck.")
      | Some itf ->
        begin match Global.load_interface_for_extended_machine env itf mi.mi_mch with
          | Ok () -> {mi_mch=mi.mi_mch;mi_params=[]}
          | Error err -> raise (Error.Error err)
        end
    end
  | _::_ -> Error.raise_exn mi.mi_mch.lid_loc "Not implemented: extension of machine with parameters."

let load_imported_mch_exn (f:Utils.loc->string->Global.t_interface option) (env:Global.t) (mi:_ machine_instanciation) : _ machine_instanciation =
  match mi.mi_params with
  | [] ->
    begin match f mi.mi_mch.lid_loc mi.mi_mch.lid_str with
      | None -> Error.raise_exn mi.mi_mch.lid_loc ("The machine '"^mi.mi_mch.lid_str^"' does not typecheck.")
      | Some itf ->
        begin match Global.load_interface_for_imported_machine env itf mi.mi_mch with
          | Ok () -> {mi_mch=mi.mi_mch;mi_params=[]}
          | Error err -> raise (Error.Error err)
        end
    end
  | _::_ -> Error.raise_exn mi.mi_mch.lid_loc "Not implemented: importation of machine with parameters."

let is_abstract_set env v =
  match Global.get_symbol_kind env v.lid_str with
  | None -> false
  | Some k -> k = Global.K_Abstract_Set

let manage_set_concretisation_exn (env:Global.t) (v,e:lident*P.expression) : unit =
  if is_abstract_set env v then
    let te = type_expression_exn Global.C_Values env (Inference.Local.create ()) e in
    match Btype.view te.exp_typ with
    | Btype.T_Power ty ->
      if not (Global.add_alias env v.lid_str ty) then
        Error.raise_exn v.lid_loc "Incorrect abstract set definition."
    | _ ->
      let str = Printf.sprintf
          "This expression has type '%s' but an expression of type '%s' was expected."
          (to_string te.exp_typ) (Btype.Open.to_string (Btype.Open.mk_Power (Btype.Open.new_meta ())))
      in
      Error.raise_exn e.exp_loc str

let type_implementation_exn (f:Utils.loc->string->Global.t_interface option) (env:Global.t) imp : _ implementation_desc =
  let () = load_refines_exn f env imp.imp_refines in
  let () = List.iter (load_seen_mch_exn f env) imp.imp_sees in
  let imp_sets = List.map (declare_set_exn env) imp.imp_sets in
  let () = List.iter (manage_set_concretisation_exn env) imp.imp_values in
  let imp_imports = List.map (load_imported_mch_exn f env) imp.imp_imports in
  let imp_extends = List.map (load_extended_mch_exn f env) imp.imp_extends in
  let () = List.iter (promote_op_exn env) imp.imp_promotes in
  let (imp_concrete_constants,imp_abstract_constants,imp_properties) =
    declare_constants_exn env imp.imp_concrete_constants [] imp.imp_properties
  in
  let (imp_concrete_variables,imp_abstract_variables,imp_invariant) =
    declare_variables_exn env imp.imp_concrete_variables [] imp.imp_invariant
  in
  let imp_values = List.map (type_value_exn env) imp.imp_values in
  let ctx = Inference.Local.create () in
  let imp_assertions =
    List.map (type_predicate_exn Global.C_Invariant_Or_Assertions env ctx) imp.imp_assertions
  in
  let imp_initialisation =
    Utils.map_opt (type_substitution_exn Global.C_Operations env ctx) imp.imp_initialisation
  in
  let _ = Utils.map_opt is_implementation_subst imp.imp_initialisation in
  let imp_local_operations = List.map (declare_local_operation_exn env) imp.imp_local_operations in
  let _ = List.map (fun op -> is_refinement_subst op.op_body) imp.imp_local_operations in
  let imp_operations = List.map (declare_operation_exn env) imp.imp_operations in
  let _ = List.map (fun op -> is_implementation_subst op.op_body) imp.imp_operations in
  { imp_refines=imp.imp_refines; imp_sees=imp.imp_sees; imp_imports;
    imp_promotes=imp.imp_promotes; imp_extends; imp_sets; imp_concrete_constants;
    imp_properties; imp_values; imp_concrete_variables; imp_invariant;
    imp_assertions; imp_initialisation; imp_operations; imp_local_operations; }

let mk_comp co_name co_parameters co_desc : T.component =
  { T.co_name; co_parameters; co_desc }

let type_component (f:Utils.loc -> string -> Global.t_interface option) (env:Global.t) (co:P.component) : T.component Error.t_result =
  try
    let params = match co.co_parameters with
      | [] -> []
      | x::_ -> Error.raise_exn x.lid_loc "Not implemented: machine with parameters."
    in
    let desc, is_imp = match co.co_desc with
      | Machine mch -> Machine (type_machine_exn f env mch), false
      | Refinement ref -> Refinement (type_refinement_exn f env ref), false
      | Implementation imp -> Implementation (type_implementation_exn f env imp), true
    in
    match Global.check_operation_coherence env co.co_name.lid_loc is_imp with
    | Error _ as err -> err
    | Ok () -> Ok (mk_comp co.co_name params desc)
  with
  | Error.Error err -> Error err

let get_interface (f:Utils.loc -> string -> Global.t_interface option) (co:P.component) : (T.component*Global.t_interface) Error.t_result =
  let env = Global.create () in
  match type_component f env co with
  | Ok cp -> Ok (cp,Global.to_interface env)
  | Error err -> Error err
