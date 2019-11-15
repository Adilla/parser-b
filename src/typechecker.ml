open Utils
open Btype
open SyntaxCore
module P = PSyntax
module T = TSyntax
module V = Visibility
module G = Global
module M = Global.Mch

let allow_becomes_such_that_in_implementation = ref false

(* *****************************************************************************
 * Type Checking for Components
 * ************************************************************************** *)
let close_exn (lc:loc) (ty:Btype.Open.t) : Btype.t =
  match Btype.close ty with
  | None ->
    Error.error lc
      ("The type of this expression could not be fully inferred. The type infered so far is '"^
       Btype.Open.to_string ty^"'.")
  | Some ty -> ty

let mk_expr exp_loc exp_typ exp_desc : (_,Btype.t) T.expression =
  let exp_typ = close_exn exp_loc exp_typ in
  { T.exp_loc; exp_typ ; exp_desc }

let mk_pred prd_loc prd_desc : (_,Btype.t) T.predicate =
  { T.prd_loc; prd_desc }

let mk_subst sub_loc sub_desc = { T.sub_loc; sub_desc; }

let close_ident t = { t with T.id_type=close_exn t.T.id_loc t.T.id_type }

let rec close_expr_exn  (e:(_,Btype.Open.t) T.expression) : (_,Btype.t) T.expression =
    match e.T.exp_desc with
  | T.Ident id -> mk_expr e.T.exp_loc e.T.exp_typ (T.Ident (close_ident id))
  | T.Dollar id -> mk_expr e.T.exp_loc e.T.exp_typ (T.Dollar (close_ident id))
  | T.Builtin_0 _ as d -> mk_expr e.T.exp_loc e.T.exp_typ d
  | T.Builtin_1 (bi,e) ->
    mk_expr e.T.exp_loc e.T.exp_typ (T.Builtin_1 (bi,close_expr_exn e))
  | T.Builtin_2 (bi,e1,e2) ->
    mk_expr e.T.exp_loc e.T.exp_typ (T.Builtin_2 (bi,close_expr_exn e1,close_expr_exn e2))
  | T.Pbool p ->
    mk_expr e.T.exp_loc e.T.exp_typ (T.Pbool (close_pred_exn p))
  | T.Sequence nlst ->
    mk_expr e.T.exp_loc e.T.exp_typ (T.Sequence (Nlist.map close_expr_exn nlst))
  | T.Extension nlst ->
    mk_expr e.T.exp_loc e.T.exp_typ (T.Extension (Nlist.map close_expr_exn nlst))
  | T.Comprehension (xlst,p) ->
    mk_expr e.T.exp_loc e.T.exp_typ (T.Comprehension (xlst,close_pred_exn p))
  | T.Binder (bi,xlst,p,e0) ->
    mk_expr e.T.exp_loc e.T.exp_typ (T.Binder (bi,xlst,close_pred_exn p,close_expr_exn e0))
  | T.Record_Field_Access (e0,id) ->
    mk_expr e.T.exp_loc e.T.exp_typ (T.Record_Field_Access (close_expr_exn e0,id))
  | T.Record nlst ->
    let aux (id,e) = (id,close_expr_exn e) in
    mk_expr e.T.exp_loc e.T.exp_typ (T.Record(Nlist.map aux nlst))
  | T.Record_Type nlst ->
    let aux (id,e) = (id,close_expr_exn e) in
    mk_expr e.T.exp_loc e.T.exp_typ (T.Record_Type(Nlist.map aux nlst))

and close_pred_exn (p:(_,Btype.Open.t) T.predicate) : (_,Btype.t) T.predicate =
  match p.T.prd_desc with
  | T.P_Builtin _ as d -> mk_pred p.T.prd_loc d
  | T.Binary_Prop (bop,p1,p2) -> mk_pred p.T.prd_loc (T.Binary_Prop (bop,close_pred_exn p1,close_pred_exn p2))
  | T.Binary_Pred (bop,e1,e2) -> mk_pred p.T.prd_loc (T.Binary_Pred (bop,close_expr_exn e1,close_expr_exn e2))
  | T.Negation p0 -> mk_pred p.T.prd_loc (T.Negation (close_pred_exn p0))
  | T.Universal_Q (xlst,p0) -> mk_pred p.T.prd_loc (T.Universal_Q (xlst,close_pred_exn p0)) 
  | T.Existential_Q (xlst,p0) -> mk_pred p.T.prd_loc (T.Existential_Q (xlst,close_pred_exn p0)) 

let close_mut_var_nlist (lst:(_,Btype.Open.t) T.t_ident Nlist.t) = Nlist.lb_map ~f:close_ident lst

let rec close_subst_exn (s:(_,_,_,_,Btype.Open.t) T.substitution) : (_,_,_,_,Btype.t) T.substitution =
  match s.T.sub_desc with
  | T.Skip ->
    mk_subst s.T.sub_loc T.Skip
  | T.Affectation (T.Tuple xlst,e) ->
    mk_subst s.T.sub_loc (T.Affectation(T.Tuple (close_mut_var_nlist xlst),close_expr_exn e))
  | T.Affectation (T.Function(v,nlst),e) ->
    mk_subst s.T.sub_loc (T.Affectation (T.Function(close_ident v,Nlist.map close_expr_exn nlst),close_expr_exn e))
  | T.Affectation (T.Record(v,id),e) ->
    mk_subst s.T.sub_loc (T.Affectation (T.Record(close_ident v,id),close_expr_exn e))
  | T.Pre (p,s0) ->
    mk_subst s.T.sub_loc (T.Pre(close_pred_exn p,close_subst_exn s0))
  | T.Assert (p,s0) -> mk_subst s.T.sub_loc (T.Assert(close_pred_exn p,close_subst_exn s0))
  | T.Choice nlst ->
    mk_subst s.T.sub_loc (T.Choice(Nlist.map close_subst_exn nlst))
  | T.IfThenElse (nlst,opt) ->
    let aux (p,s) = (close_pred_exn p,close_subst_exn s) in
    let topt = match opt with
      | None -> None
      | Some s0 -> Some (close_subst_exn s0)
    in
    mk_subst s.T.sub_loc (T.IfThenElse (Nlist.map aux nlst,topt))
  | T.Select (nlst,opt) ->
    let aux (p,s) = (close_pred_exn p,close_subst_exn s) in
    let topt = match opt with
      | None -> None
      | Some s0 -> Some (close_subst_exn s0)
    in
    mk_subst s.T.sub_loc (T.Select (Nlist.map aux nlst,topt))
  | T.Case (e,nlst,opt) -> 
    let aux (lst,s) = (Nlist.map close_expr_exn lst,close_subst_exn s) in
    let topt = match opt with
      | None -> None
      | Some s0 -> Some (close_subst_exn s0)
    in
    mk_subst s.T.sub_loc (T.Case (close_expr_exn e,Nlist.map aux nlst,topt))
  | T.Any (xlst,p,s0) ->
    mk_subst s.T.sub_loc (T.Any (xlst,close_pred_exn p,close_subst_exn s0))
  | T.Let (xlst,nlst,s0) ->
    let aux (v,e) = (v,close_expr_exn e) in
    mk_subst s.T.sub_loc (T.Let (xlst,Nlist.map aux nlst,close_subst_exn s0))
  | T.BecomesElt (xlst,e) ->
    mk_subst s.T.sub_loc (T.BecomesElt (close_mut_var_nlist xlst,close_expr_exn e))
  | T.BecomesSuch (xlst,p) ->
    mk_subst s.T.sub_loc (T.BecomesSuch (close_mut_var_nlist xlst,close_pred_exn p))
  | T.Var (xlst,s0) ->
    mk_subst s.T.sub_loc (T.Var (xlst,close_subst_exn s0))
  | T.CallUp (args_out,id,args_in) ->
    mk_subst s.T.sub_loc (T.CallUp (List.map close_ident args_out,id,List.map close_expr_exn args_in))
  | T.While (p1,s0,p2,e) ->
    mk_subst s.T.sub_loc (T.While (close_pred_exn p1,close_subst_exn s0,close_pred_exn p2,close_expr_exn e))
  | T.Sequencement (s1,s2) ->
    mk_subst s.T.sub_loc (T.Sequencement (close_subst_exn s1,close_subst_exn s2))
  | T.Parallel (s1,s2) ->
    mk_subst s.T.sub_loc (T.Parallel (close_subst_exn s1,close_subst_exn s2))

let type_predicate_exn cl env p =
  close_pred_exn (Inference.type_predicate_exn cl env Local.empty p)

let type_substitution_exn cl env (s:P.substitution) : _ T.substitution =
  let s = Inference.type_substitution_exn cl env Local.empty s in
  close_subst_exn s 

let rec is_read_only (gl:_ Global.t) (ctx:string list) (s:P.substitution) : bool =
  match s.P.sub_desc with
  | P.Skip -> true
  | P.Affectation (P.Tuple xlst,_) | P.BecomesElt (xlst,_) | P.BecomesSuch (xlst,_) ->
    let aux v =
     match v.r_prefix with
       | None -> List.exists (String.equal v.r_str) ctx
       | Some _ -> false
    in
    List.for_all aux (Nlist.to_list xlst)
  | P.Affectation (P.Function(v,_),_) | P.Affectation (P.Record(v,_),_) ->
    begin match v.r_prefix with
      | None -> List.exists (String.equal v.r_str) ctx
      | Some _ -> false
    end
  | P.CallUp (args_out,id,_) ->
    let aux v = 
      match v.r_prefix with
      | None -> List.exists (String.equal v.r_str) ctx
      | Some _ -> false
    in
    let op_name = match id.r_prefix with
      | Some p -> p ^ "." ^ id.r_str
      | None -> id.r_str
    in
    List.for_all aux args_out &&
    (match Global.get_operation gl op_name with
     | None -> Error.error s.P.sub_loc ("Unknown operation '"^op_name^"'.")
     | Some infos -> infos.Global.op_readonly)
  | P.Pre (_,s0) -> is_read_only gl ctx s0
  | P.Assert (_,s0) -> is_read_only gl ctx s0
  | P.Choice nlst -> List.for_all (is_read_only gl ctx) (Nlist.to_list nlst)
  | P.IfThenElse (nlst,opt) | P.Select (nlst,opt) ->
    let aux (_,s0) = is_read_only gl ctx s0 in
    let opt_ro = match opt with
      | None -> true
      | Some s0 -> is_read_only gl ctx s0
    in
    opt_ro && List.for_all aux (Nlist.to_list nlst)
  | P.Case (_,nlst,opt) ->
    let aux (_,s0) = is_read_only gl ctx s0 in
    let opt_ro = match opt with
      | None -> true
      | Some s0 -> is_read_only gl ctx s0
    in
    opt_ro && List.for_all aux (Nlist.to_list nlst)
  | P.Any (xlst,_,s0) | P.Let (xlst,_,s0) | P.Var (xlst,s0) ->
    let ctx = List.fold_left (fun ctx v -> v.lid_str::ctx) ctx (Nlist.to_list xlst) in
    is_read_only gl ctx s0
  | P.While (_,s0,_,_) -> is_read_only gl ctx s0
  | P.Sequencement (s1,s2) | P.Parallel (s1,s2) ->
    is_read_only gl ctx s1 && is_read_only gl ctx s2

let load_refines (f:Utils.loc->string->Global.t_interface option) env (mch:lident) params : lident =
  match f mch.lid_loc mch.lid_str with
  | None -> Error.error mch.lid_loc ("The machine '"^mch.lid_str^"' does not typecheck.")
  | Some itf -> ( G.load_interface_for_refined_machine env itf mch params; mch )

let load_seen (f:Utils.loc->string->Global.t_interface option) env (mch:ren_ident) : ren_ident =
  match f mch.r_loc mch.r_str with
  | None -> Error.error mch.r_loc ("The machine '"^mch.r_str^"' does not typecheck.")
  | Some itf -> ( G.load_interface_for_seen_machine env itf mch; mch )

let load_used (f:Utils.loc->string->Global.t_interface option) env (mch:ren_ident) : ren_ident =
  match f mch.r_loc mch.r_str with
  | None -> Error.error mch.r_loc ("The machine '"^mch.r_str^"' does not typecheck.")
  | Some itf -> ( G.load_interface_for_used_machine env itf mch; mch )

let load_included_or_imported cl (f:Utils.loc->string->Global.t_interface option)
    env (mi:P.machine_instanciation) : _ T.machine_instanciation
  = 
  match f mi.P.mi_mch.r_loc mi.P.mi_mch.r_str with
  | None -> Error.error mi.P.mi_mch.r_loc ("The machine '"^mi.P.mi_mch.r_str^"' does not typecheck.")
  | Some itf ->
    let mi_params = List.map (fun x ->
        close_expr_exn (Inference.type_expression_exn cl env Local.empty x)
      ) mi.P.mi_params in
    let params = List.map (fun e -> e.T.exp_loc,e.T.exp_typ) mi_params in
    ( G.load_interface_for_included_or_imported_machine env itf mi.P.mi_mch params;
      { mi_mch=mi.P.mi_mch; mi_params } )

let load_extended cl (f:Utils.loc->string->Global.t_interface option)
    env (mi:P.machine_instanciation) : _ T.machine_instanciation
  = 
  match f mi.P.mi_mch.r_loc mi.P.mi_mch.r_str with
  | None -> Error.error mi.P.mi_mch.r_loc ("The machine '"^mi.P.mi_mch.r_str^"' does not typecheck.")
  | Some itf ->
    let mi_params = List.map (fun x ->
        close_expr_exn (Inference.type_expression_exn cl env Local.empty x)
      ) mi.P.mi_params in
    let params = List.map (fun x -> (x.T.exp_loc,x.T.exp_typ)) mi_params in
    ( G.load_interface_for_extended_machine env itf mi.P.mi_mch params;
      { mi_mch=mi.P.mi_mch; mi_params } )

let declare_local_symbol (env:_ Global.t) (ctx:Local.t) (lid:lident) : Local.t =
  match Global.get_symbol env lid.lid_str with
  | None -> Local.declare ctx lid.lid_str Local.L_Expr_Binder
  | Some infos -> (*FIXME check refine and compatible kind*)
    Local.declare_with_type ctx lid.lid_str infos.Global.sy_typ Local.L_Expr_Binder

let promote_symbol_exn env (ctx:Local.t) (ki:G.t_global_kind) (lid:lident) : unit =
  match Local.get ctx lid.lid_str with
  | None -> assert false
  | Some (None,_) -> Error.error lid.lid_loc ("The type of '"^lid.lid_str^"' could not be inferred.")
  | Some (Some ty,_) -> G.add_symbol env lid.lid_loc lid.lid_str ty ki

let declare_mch_scalar_parameters_exn (env:G.mEnv)
    (parameters:lident list) (constraints:P.predicate option)
  : (V.Mch.Constraints.t,Btype.t) T.predicate option =
  let ctx = List.fold_left (declare_local_symbol env) Local.empty parameters in
  let t_constr = Utils.map_opt (Inference.type_predicate_exn Visibility.M_Constraints env ctx) constraints in
  List.iter (promote_symbol_exn env ctx (G.K_Parameter G.Scalar)) parameters;
  Utils.map_opt close_pred_exn t_constr

let declare_constants_exn (type a b) (cl:(a,b)V.clause) (env:(a,_)G.t) (cconst:lident list) (aconst:lident list) (prop:P.predicate option)
  : (b,Btype.t) T.predicate option =
  let ctx = Local.empty in
  let ctx = List.fold_left (declare_local_symbol env) ctx cconst in
  let ctx = List.fold_left (declare_local_symbol env) ctx aconst in
  let t_prop = Utils.map_opt (Inference.type_predicate_exn cl env ctx) prop in
  List.iter (promote_symbol_exn env ctx G.K_Concrete_Constant) cconst;
  List.iter (promote_symbol_exn env ctx G.K_Abstract_Constant) aconst;
  Utils.map_opt close_pred_exn t_prop

let declare_variables_exn (type a b) (cl:(a,b)V.clause) (env:(a,_)G.t) (cvars:lident list) (avars:lident list) (inv:P.predicate option)
  : (b,Btype.t) T.predicate option =
  let ctx = Local.empty in
  let ctx = List.fold_left (declare_local_symbol env) ctx cvars in
  let ctx = List.fold_left (declare_local_symbol env) ctx avars in
  let t_inv = Utils.map_opt (Inference.type_predicate_exn cl env ctx) inv in
  List.iter (promote_symbol_exn env ctx G.K_Concrete_Variable) cvars;
  List.iter (promote_symbol_exn env ctx G.K_Abstract_Variable) avars;
  Utils.map_opt close_pred_exn t_inv

let check_signature (op:P.operation) args_in args_out =
  let rec aux lst1 lst2 =
    match lst1, lst2 with
    | [], [] -> ()
    | v::lst1, (x,_)::lst2 ->
      if String.equal v.lid_str x then aux lst1 lst2
      else Error.error v.lid_loc ("Expecting parameter '"^x^"' instead of '"^v.lid_str^"'.")
    | v::_, [] -> Error.error v.lid_loc ("Unexpected parameter '"^v.lid_str^"'.")
    | [], (x,_)::_ ->
      Error.error op.P.op_name.lid_loc ("Missing parameter '"^x^"'.")
  in
  aux op.P.op_in args_in;
  aux op.P.op_out args_out

let get_operation_context_exn (type a b) (env:(a,b) Global.t) (op:P.operation) =
  match G.get_operation env op.P.op_name.lid_str with
  | None ->
    let aux ki ctx lid = Local.declare ctx lid.lid_str ki in
    let ctx = List.fold_left (aux Local.L_Param_In) Local.empty op.P.op_in in
    List.fold_left (aux Local.L_Param_Out) ctx op.P.op_out
  | Some infos -> (*FIXME check refine*)
    let aux (lk:Local.t_local_kind) (ctx:Local.t) (s,ty:string*Btype.t) =
      Local.declare_with_type ctx s ty lk
    in
    let ctx0 = List.fold_left (aux Local.L_Param_In) Local.empty infos.Global.op_args_in in
    let ctx = List.fold_left (aux Local.L_Param_Out) ctx0 infos.Global.op_args_out in
    let () = check_signature op infos.Global.op_args_in infos.Global.op_args_out in
    ctx

let declare_operation_exn (cl:_ V.sclause) (env:_ Global.t) (op:P.operation) : _ T.operation =
  let ctx = get_operation_context_exn env op in
  let op_body = close_subst_exn (Inference.type_substitution_exn cl env ctx op.P.op_body) in
  let type_arg_exn ctx lid : T.arg =
    match Local.get ctx lid.lid_str with
    | None ->assert false
    | Some (None,_) ->
      Error.error lid.lid_loc ("The type of parameter '"^lid.lid_str^"' could not be inferred.")
    | Some (Some arg_typ,_) ->
      { T.arg_loc = lid.lid_loc; arg_id=lid.lid_str; arg_typ }
  in
  let op_in = List.map (type_arg_exn ctx) op.P.op_in in
  let op_out = List.map (type_arg_exn ctx) op.P.op_out in
  let aux arg = (arg.T.arg_id,arg.T.arg_typ) in
  let args_in = List.map aux op_in in
  let args_out = List.map aux op_out in
  G.add_operation env op.P.op_name.lid_loc op.P.op_name.lid_str args_in args_out
    ~is_readonly:(is_read_only env [] op.P.op_body);
  { op_name=op.P.op_name; op_in; op_out; op_body }

type t_mch_symbols = {
  set_parameters: lident list;
  scalar_parameters: T.t_param list;
  abstract_sets: G.Mch.t_source T.symb list;
  concrete_sets: (G.Mch.t_source T.symb*string list) list;
  abstract_constants: G.Mch.t_source T.symb list;
  concrete_constants: G.Mch.t_source T.symb list;
  abstract_variables: G.Mch.t_source T.symb list;
  concrete_variables: G.Mch.t_source T.symb list;
}

let get_mch_symbols (env:G.mEnv) : t_mch_symbols =
  let aux (id:string) (infos:_ G.t_symbol_infos) rc =
    let add_symb f sy_src =
        f { T.sy_id = id; sy_typ = infos.Global.sy_typ; sy_src }
    in
    match infos.Global.sy_kind with
    | G.Mch.Abstract_Set src ->
      add_symb (fun x -> { rc with abstract_sets = (x::rc.abstract_sets) }) src
    | G.Mch.Concrete_Set (elts,src) ->
      add_symb (fun x -> { rc with concrete_sets = ((x,elts)::rc.concrete_sets) }) src
    | G.Mch.Abstract_Constant src ->
      add_symb (fun x -> { rc with abstract_constants = (x::rc.abstract_constants) }) src
    | G.Mch.Concrete_Constant src ->
      add_symb (fun x -> { rc with concrete_constants = (x::rc.concrete_constants) }) src
    | G.Mch.Abstract_Variable src ->
      add_symb (fun x -> { rc with abstract_variables = (x::rc.abstract_variables) }) src
    | G.Mch.Concrete_Variable src ->
      add_symb (fun x -> { rc with concrete_variables = (x::rc.concrete_variables) }) src
    | G.Mch.Parameter (G.Scalar,p_loc) ->
      let x = { T.p_id=id;p_typ=infos.Global.sy_typ;p_loc } in
      { rc with scalar_parameters = (x::rc.scalar_parameters) }
    | G.Mch.Parameter (G.Set,lid_loc) ->
      let x = { lid_str=id; lid_loc } in
      { rc with set_parameters = (x::rc.set_parameters) }
    | G.Mch.Enumerate _ -> rc
  in
  G.fold_symbols aux env
    { set_parameters=[]; scalar_parameters=[]; abstract_sets=[]; concrete_sets=[];
      abstract_constants=[]; concrete_constants=[]; abstract_variables=[];
      concrete_variables=[]; }

let get_mch_promoted_operations (env:G.mEnv) : T.promoted list =
  let aux lid_str infos lst =
    let op_out = infos.Global.op_args_out in
    let op_in = infos.Global.op_args_in in
    match infos.Global.op_src with
    | G.Mch.O_Included_And_Promoted op_source -> (*FIXME*)
      {T.op_out; op_name={lid_loc=Utils.dloc;lid_str}; op_in; op_source}::lst
    | _ -> lst
  in
  Global.fold_operations aux env []

let is_set_param s = String.equal s.lid_str (String.capitalize_ascii s.lid_str)

let type_machine_exn (f:Utils.loc->string->Global.t_interface option)
    (env:G.mEnv) (mch:P.machine) : T.machine
  =
  let mch_set_parameters = List.filter is_set_param mch.P.mch_parameters in
  List.iter (fun p ->
      let ty = Btype.mk_Power (Btype.mk_Concrete_Set T_Current p.lid_str) in
      G.add_symbol env p.lid_loc p.lid_str ty (G.K_Parameter G.Set)
    ) mch_set_parameters;
  let scalar_params = List.filter (fun x -> not (is_set_param x)) mch.P.mch_parameters in
  let mch_constraints = declare_mch_scalar_parameters_exn env scalar_params mch.P.mch_constraints in
  let mch_uses = List.map (load_used f env) mch.P.mch_uses in
  let mch_sees = List.map (load_seen f env) mch.P.mch_sees in
  let mch_includes = List.map (load_included_or_imported V.M_Includes f env) mch.P.mch_includes in
  let mch_extends = List.map (load_extended V.M_Includes f env) mch.P.mch_extends in
  let () = List.iter (function
      | P.Abstract_Set v -> G.add_abstract_set env v.lid_loc v.lid_str
      | P.Concrete_Set (v,elts) -> G.add_concrete_set env v.lid_loc v.lid_str elts
    ) mch.P.mch_sets
  in
  let mch_properties = declare_constants_exn V.M_Properties env mch.P.mch_concrete_constants
      mch.P.mch_abstract_constants mch.P.mch_properties
  in
  let mch_invariant = declare_variables_exn V.M_Invariant env mch.P.mch_concrete_variables
      mch.P.mch_abstract_variables mch.P.mch_invariant
  in
  let symbs = get_mch_symbols env in
  let mch_assertions = List.map (type_predicate_exn V.M_Invariant env) mch.P.mch_assertions in
  let () = List.iter (fun x -> G.promote_operation env x.lid_loc x.lid_str) mch.P.mch_promotes in
  let mch_initialisation = Utils.map_opt (type_substitution_exn V.MS_Operations env) mch.P.mch_initialisation in
  let mch_operations = List.map (declare_operation_exn V.MS_Operations env) mch.P.mch_operations in
  let mch_promoted = get_mch_promoted_operations env in
  { T.mch_sees; mch_includes; mch_extends; mch_uses; mch_set_parameters;
    mch_scalar_parameters = symbs.scalar_parameters;
    mch_abstract_sets = symbs.abstract_sets;
    mch_concrete_sets = symbs.concrete_sets;
    mch_concrete_constants = symbs.concrete_constants;
    mch_abstract_constants = symbs.abstract_constants;
    mch_concrete_variables = symbs.concrete_variables;
    mch_abstract_variables = symbs.abstract_variables;
    mch_constraints; mch_properties; mch_invariant; mch_assertions;
    mch_initialisation; mch_operations; mch_promoted
  }

type t_ref_symbols = {
  set_parameters: lident list;
  scalar_parameters: T.t_param list;
  abstract_sets: G.Ref.t_source T.symb list;
  concrete_sets: (G.Ref.t_source T.symb*string list) list;
  abstract_constants: G.Ref.t_source_2 T.symb list;
  concrete_constants: G.Ref.t_source_2 T.symb list;
  abstract_variables: G.Ref.t_source_2 T.symb list;
  concrete_variables: G.Ref.t_source_2 T.symb list;
}

let get_ref_symbols (env:G.rEnv) : t_ref_symbols =
  let aux (id:string) (infos:_ Global.t_symbol_infos) rc =
    let add_symb f sy_src =
      f { T.sy_id = id; sy_typ = infos.Global.sy_typ; sy_src }
    in
    match infos.Global.sy_kind with
    | G.Ref.Abstract_Set src ->
      add_symb (fun x -> { rc with abstract_sets = (x::rc.abstract_sets) }) src
    | G.Ref.Concrete_Set (elts,src) ->
      add_symb (fun x -> { rc with concrete_sets = ((x,elts)::rc.concrete_sets) }) src
    | G.Ref.Abstract_Constant src ->
      add_symb (fun x -> { rc with abstract_constants = (x::rc.abstract_constants) }) src
    | G.Ref.Concrete_Constant src ->
      add_symb (fun x -> { rc with concrete_constants = (x::rc.concrete_constants) }) src
    | G.Ref.Abstract_Variable src ->
      add_symb (fun x -> { rc with abstract_variables = (x::rc.abstract_variables) }) src
    | G.Ref.Concrete_Variable src ->
      add_symb (fun x -> { rc with concrete_variables = (x::rc.concrete_variables) }) src
    | G.Ref.Parameter (Global.Scalar,p_loc) ->
      let x = { T.p_id=id;p_typ=infos.Global.sy_typ;p_loc } in
      { rc with scalar_parameters = (x::rc.scalar_parameters) }
    | G.Ref.Parameter (Global.Set,lid_loc) ->
      let x = { lid_str=id; lid_loc } in
      { rc with set_parameters = (x::rc.set_parameters) }
    | G.Ref.Enumerate _ -> rc
  in
  Global.fold_symbols aux env
    { set_parameters=[]; scalar_parameters=[]; abstract_sets=[]; concrete_sets=[];
      abstract_constants=[]; concrete_constants=[]; abstract_variables=[]; concrete_variables=[]; }

let get_ref_promoted_operations (env:G.rEnv) : T.promoted list =
  let aux (lid_str:string) (infos:G.Ref.t_op_source G.t_operation_infos) lst =
    let op_out = infos.Global.op_args_out in
    let op_in = infos.Global.op_args_in in
    match infos.Global.op_src with
    | G.Ref.O_Refined_Included_And_Promoted op_source ->
      {T.op_out; op_name={lid_loc=Utils.dloc;lid_str}; op_in; op_source}::lst
    | _ -> lst
  in
  Global.fold_operations aux env []

let type_refinement_exn (f:Utils.loc->string->Global.t_interface option)
    (env:G.rEnv) (ref:P.refinement) : T.refinement
  =
  let ref_refines = load_refines f env ref.P.ref_refines ref.P.ref_parameters in
  let ref_sees = List.map (load_seen f env) ref.P.ref_sees in
  let ref_includes = List.map (load_included_or_imported V.R_Includes f env) ref.P.ref_includes in
  let ref_extends = List.map (load_extended V.R_Includes f env) ref.P.ref_extends in
  let () = List.iter (function
      | P.Abstract_Set v -> G.add_abstract_set env v.lid_loc v.lid_str
      | P.Concrete_Set (v,elts) -> G.add_concrete_set env v.lid_loc v.lid_str elts
    ) ref.P.ref_sets
  in
  let ref_properties = declare_constants_exn V.R_Properties env
      ref.P.ref_concrete_constants ref.P.ref_abstract_constants ref.P.ref_properties
  in
  let ref_invariant = declare_variables_exn V.R_Invariant env
      ref.P.ref_concrete_variables ref.P.ref_abstract_variables ref.P.ref_invariant
  in
  let symbs = get_ref_symbols env in
  let ref_assertions = List.map (type_predicate_exn V.R_Invariant env) ref.P.ref_assertions in
  let () = List.iter (fun op_name ->
      G.promote_operation env op_name.lid_loc op_name.lid_str
    ) ref.P.ref_promotes
  in
  let ref_initialisation = Utils.map_opt (type_substitution_exn V.RS_Operations env) ref.P.ref_initialisation in
  let ref_operations = List.map (declare_operation_exn V.RS_Operations env) ref.P.ref_operations in
  let ref_promoted = get_ref_promoted_operations env in
  { T.ref_refines; ref_sees;
    ref_includes; ref_extends;
    ref_set_parameters = symbs.set_parameters;
    ref_scalar_parameters = symbs.scalar_parameters;
    ref_abstract_sets = symbs.abstract_sets;
    ref_concrete_sets = symbs.concrete_sets;
    ref_concrete_constants = symbs.concrete_constants;
    ref_abstract_constants = symbs.abstract_constants;
    ref_concrete_variables = symbs.concrete_variables;
    ref_abstract_variables = symbs.abstract_variables;
    ref_properties; ref_invariant; ref_assertions; ref_initialisation;
    ref_operations; ref_promoted }

let type_value_exn (env:_ Global.t) (v,e:lident*P.expression) :
  (T.value*(_,Btype.t)T.expression) =
  match Global.get_symbol env v.lid_str with
  | None -> Error.error v.lid_loc ("Unknown identifier '"^v.lid_str^"'.")
  | Some infos ->
    let var_typ = infos.Global.sy_typ in
    let te = close_expr_exn
        (Inference.type_expression_exn V.I_Values env Local.empty e)
    in
    if Btype.is_equal_modulo_alias (Global.get_alias env) te.T.exp_typ var_typ then
      let val_kind = match infos.Global.sy_kind with
        | G.Imp.Abstract_Set _ -> T.VK_Abstract_Set
        | G.Imp.Concrete_Constant _ -> T.VK_Concrete_Constant
        | _ -> Error.error v.lid_loc
                 "This symbol is neither an abstract set nor a concrete constant."
      in
       ( {T.val_loc=v.lid_loc;val_id=v.lid_str;val_kind},te)
    else
      Error.error e.P.exp_loc
        ("This expression has type '" ^ to_string te.T.exp_typ ^
         "' but an expression of type '" ^ to_string var_typ ^"' was expected.")
(*
let is_abstract_set env v =
  match Global.get_symbol env v with
  | None -> false
  | Some infos ->
   begin match infos.Global.sy_kind with
     | Global.K_Abstract_Set _ -> true
     | _ -> false
   end

let manage_set_concretisation_exn (lst:(Btype.t_atomic_src*string) list) (env:Global.t_ref Global.t) (v,e:lident*P.expression) : unit =
  if is_abstract_set env v.lid_str then
    let alias = match e.exp_desc with
    | P.Ident (None,id) ->
      begin match List.find_opt (fun (_,x) -> String.equal id x) lst with
        | Some (x,y) -> Btype.mk_Abstract_Set x y
        | None ->
          if is_abstract_set env id then
            Btype.mk_Abstract_Set Btype.T_Current id
          else
            Btype.t_int
      end
    | P.Builtin_2 (Interval,_,_) -> Btype.t_int
    | _ ->
      Error.error v.lid_loc "Incorrect set valuation (rhs is neither an identifier nor an interval)."
    in
    if not (Global.add_alias env v.lid_str alias) then
      Error.error v.lid_loc "Incorrect abstract set definition (cyclic alias)."
*)
type t_imp_symbols = {
  set_parameters: lident list;
  scalar_parameters: T.t_param list;
  abstract_sets: G.Imp.t_concrete_const_decl T.symb list;
  concrete_sets: (G.Imp.t_concrete_const_decl T.symb*string list) list;
  abstract_constants: G.Imp.t_abstract_decl T.symb list;
  concrete_constants: G.Imp.t_concrete_const_decl T.symb list;
  abstract_variables: G.Imp.t_abstract_decl T.symb list;
  concrete_variables: G.Imp.t_concrete_var_decl T.symb list;
}

let get_imp_symbols (env:G.iEnv) : t_imp_symbols =
  let aux (id:string) (infos:_ Global.t_symbol_infos) rc =
    let add f sy_src = f { T.sy_id = id; sy_typ = infos.Global.sy_typ; sy_src } in
    match infos.Global.sy_kind with
    | G.Imp.Abstract_Set src ->
      add (fun x -> { rc with abstract_sets = (x::rc.abstract_sets) }) src
    | G.Imp.Concrete_Set (elts,src) ->
      add (fun x -> { rc with concrete_sets = ((x,elts)::rc.concrete_sets) }) src
    | G.Imp.Abstract_Constant src ->
      add (fun x -> { rc with abstract_constants = (x::rc.abstract_constants) }) src
    | G.Imp.Concrete_Constant src ->
      add (fun x -> { rc with concrete_constants = (x::rc.concrete_constants) }) src
    | G.Imp.Abstract_Variable src ->
      add (fun x -> { rc with abstract_variables = (x::rc.abstract_variables) }) src
    | G.Imp.Concrete_Variable src ->
      add (fun x -> { rc with concrete_variables = (x::rc.concrete_variables) }) src
    | G.Imp.Parameter (Global.Scalar,p_loc) ->
      let x = { T.p_id=id;p_typ=infos.Global.sy_typ;p_loc } in
      { rc with scalar_parameters = (x::rc.scalar_parameters) }
    | G.Imp.Parameter (Global.Set,lid_loc) ->
      let x = { lid_str=id; lid_loc } in
      { rc with set_parameters = (x::rc.set_parameters) }
    | G.Imp.Enumerate _ -> rc
  in
  Global.fold_symbols aux env
    { set_parameters=[]; scalar_parameters=[]; abstract_sets=[]; concrete_sets=[];
      abstract_constants=[]; concrete_constants=[]; abstract_variables=[]; concrete_variables=[]; }

(*
let type_imp_init_exn env s =
  close_subst_exn Imp (Inference.type_substitution_exn V.IS_Operations env Local.empty s)
*)
(*

module SMap = Map.Make(String)
type t_lops_map = (Global.t_ref,Btype.t) T.substitution SMap.t

let declare_imp_operation_exn (env:Global.t_ref Global.t) (lops:t_lops_map) (op:P.operation) : Global.t_ref T.operation =
  let (ctx0,ctx) = get_ref_operation_context_exn env op in
  let op_body =
    if !allow_out_parameters_in_precondition then
      Inference.type_substitution_exn V.M_IMP_OPERATIONS env ctx op.P.op_body
    else
      begin match op.P.op_body.P.sub_desc with
        | P.Pre (p,s) ->
          let tp = Inference.type_predicate_exn V.C_IMP_OPERATIONS env ctx0 p in
          let ts = Inference.type_substitution_exn V.M_IMP_OPERATIONS env ctx s in
          { T.sub_loc=op.P.op_body.P.sub_loc; sub_desc=T.Pre (tp,ts)}
        | _ -> Inference.type_substitution_exn V.M_IMP_OPERATIONS env ctx op.P.op_body
      end
  in
  let op_body = close_subst_exn Imp op_body in
  let type_arg_exn ctx lid : T.arg =
    match Local.get ctx lid.lid_str with
    | None -> assert false
    | Some (None,_) ->
      Error.error lid.lid_loc ("The type of parameter '"^lid.lid_str^"' could not be inferred.")
    | Some (Some arg_typ,_) ->
      { T.arg_loc = lid.lid_loc; arg_id=lid.lid_str; arg_typ }
  in
  let op_in = List.map (type_arg_exn ctx) op.P.op_in in
  let op_out = List.map (type_arg_exn ctx) op.P.op_out in
  let aux arg = (arg.T.arg_id,arg.T.arg_typ) in
  let args_in = List.map aux op_in in
  let args_out = List.map aux op_out in
  Global.add_ref_operation env op.P.op_name.lid_loc op.P.op_name.lid_str args_in args_out ~is_local:false;
  match Global.get_operation env op.P.op_name.lid_str with
  | None -> assert false 
  | Some { Global.op_src=Global.OD_Local_Spec_And_Implem (_,_); _ } ->
    begin match SMap.find_opt op.P.op_name.lid_str lops with
      | None -> assert false
      | Some op_spec ->
        T.O_Local { op_name=op.P.op_name; op_in; op_out; op_spec; op_body }
    end
  | Some _ -> T.O_Specified { op_name=op.P.op_name; op_in; op_out; op_body }

let declare_local_operation_exn (env:Global.t_ref Global.t) (map:t_lops_map) (op:P.operation) : t_lops_map =
  let (ctx0,ctx) = get_ref_operation_context_exn env op in
  let op_body =
    if !allow_out_parameters_in_precondition then
      Inference.type_substitution_exn V.M_LOCAL_OPERATIONS env ctx op.P.op_body
    else
      begin match op.P.op_body.P.sub_desc with
        | P.Pre (p,s) ->
          let tp = Inference.type_predicate_exn V.C_LOCAL_OPERATIONS env ctx0 p in
          let ts = Inference.type_substitution_exn V.M_LOCAL_OPERATIONS env ctx s in
          { T.sub_loc=op.P.op_body.P.sub_loc; sub_desc=T.Pre (tp,ts)}
        | _ -> Inference.type_substitution_exn V.M_LOCAL_OPERATIONS env ctx op.P.op_body
      end
  in
  let op_body = close_subst_exn Ref op_body in (*XXX which substitutions are allowed in local operations?*)
  let type_arg_exn ctx lid : T.arg =
    match Local.get ctx lid.lid_str with
    | None ->assert false
    | Some (None,_) ->
      Error.error lid.lid_loc ("The type of parameter '"^lid.lid_str^"' could not be inferred.")
    | Some (Some arg_typ,_) ->
      { T.arg_loc = lid.lid_loc; arg_id=lid.lid_str; arg_typ }
  in
  let op_in = List.map (type_arg_exn ctx) op.P.op_in in
  let op_out = List.map (type_arg_exn ctx) op.P.op_out in
  let aux arg = (arg.T.arg_id,arg.T.arg_typ) in
  let args_in = List.map aux op_in in
  let args_out = List.map aux op_out in
  Global.add_ref_operation env op.P.op_name.lid_loc op.P.op_name.lid_str args_in args_out ~is_local:true;
  SMap.add op.P.op_name.lid_str op_body map

let check_values rm_loc (env:_ Global.t) (vlst:(T.value*_) list) : unit =
  let aux id infos map =
    match infos.Global.sy_kind with
    | Global.K_Abstract_Set (Global.D_Machine l) ->
      SMap.add id (false,l) map
    | Global.K_Abstract_Set (Global.D_Redeclared Global.Implicitely) ->
      SMap.add id (false,rm_loc) map
    | Global.K_Abstract_Set (Global.D_Redeclared Global.By_Machine l) ->
      SMap.add id (false,l) map
    | Global.K_Concrete_Constant (Global.D_Machine l) ->
      SMap.add id (false,l) map
    | Global.K_Concrete_Constant (Global.D_Redeclared Global.Implicitely) ->
      SMap.add id (false,rm_loc) map
    | Global.K_Concrete_Constant (Global.D_Redeclared Global.By_Machine l) ->
      SMap.add id (false,l) map
    | _ -> map
  in
  let cconst = Global.fold_symbols aux env SMap.empty in
  let aux map (v,_) =
    match SMap.find_opt v.T.val_id map with
    | None -> assert false
    | Some (true,_) -> Error.error v.T.val_loc ("The constant '"^v.T.val_id^"' is valuated twice.")
    | Some (false,l) -> SMap.add v.T.val_id (true,l) map
  in
  let cconst = List.fold_left aux cconst vlst in
  SMap.iter (fun id (is_valuated,loc) ->
      if not is_valuated then
        Error.warn loc ("The constant '"^id^"' is not valuated.")
    ) cconst

let get_imported_or_seen_csets f sees imports : (Btype.t_atomic_src*string) list =
  let aux1 res seen =
    match f seen.r_loc seen.r_str with
    | None -> Error.error seen.r_loc ("The machine '"^seen.r_str^"' does not typecheck.")
    | Some itf -> Global.add_abstract_sets (Btype.T_Ext seen.r_str) res itf
  in
  let aux2 res imported =
    let imported = imported.P.mi_mch in
    match f imported.r_loc imported.r_str with
    | None -> Error.error imported.r_loc ("The machine '"^imported.r_str^"' does not typecheck.")
    | Some itf -> Global.add_abstract_sets Btype.T_Current res itf
  in
  let res = [] in
  let res = List.fold_left aux1 res sees in
  List.fold_left aux2 res imports
*)
(*
let declare_imp_constants_exn (env:(_,_) Global.t) (cl:_ V.clause)
    (cconst:lident list) (aconst:lident list) (prop:P.predicate option)
  : (_,Btype.t) T.predicate option =
  let ctx = Local.empty in
  let ctx = List.fold_left (declare_local_symbol_in_ref env) ctx cconst in
  let ctx = List.fold_left (declare_local_symbol_in_ref env) ctx aconst in
  let t_prop = Utils.map_opt (Inference.type_predicate_exn cl env ctx) prop in
  List.iter (promote_symbol_exn env ctx G.K_Concrete_Constant) cconst;
  List.iter (promote_symbol_exn env ctx G.K_Abstract_Variable) aconst;
  Utils.map_opt close_pred_exn t_prop

let declare_imp_variables_exn (env:_ Global.t) cl
    (cvars:lident list) (avars:lident list) (inv:P.predicate option)
  : (_,Btype.t) T.predicate option =
  let ctx = Local.empty in
  let ctx = List.fold_left (declare_local_symbol_in_ref env) ctx cvars in
  let ctx = List.fold_left (declare_local_symbol_in_ref env) ctx avars in
  let t_inv = Utils.map_opt (Inference.type_predicate_exn cl env ctx) inv in
  List.iter (promote_symbol_exn env ctx G.K_Concrete_Variable) cvars;
  List.iter (promote_symbol_exn env ctx G.K_Abstract_Variable) avars;
  Utils.map_opt close_pred_exn t_inv
*)

let get_imp_promoted_operations (env:G.iEnv) : T.promoted list =
  let aux (lid_str:string) (infos:G.Imp.t_op_source G.t_operation_infos) lst =
    let op_out = infos.Global.op_args_out in
    let op_in = infos.Global.op_args_in in
    match infos.Global.op_src with
    | G.Imp.O_Imported_Promoted_And_Refined (op_source,_) ->
      {T.op_out; op_name={lid_loc=Utils.dloc;lid_str}; op_in; op_source}::lst
    | _ -> lst
  in
  Global.fold_operations aux env []

let type_implementation_exn (f:Utils.loc->string->Global.t_interface option)
    (env:_ Global.t) (imp:P.implementation) : T.implementation
  =
  let imp_refines = load_refines f env imp.P.imp_refines imp.P.imp_parameters in
  let () = List.iter (function
      | P.Abstract_Set v -> G.add_abstract_set env v.lid_loc v.lid_str
      | P.Concrete_Set (v,elts) -> G.add_concrete_set env v.lid_loc v.lid_str elts
    ) imp.P.imp_sets
  in
(*
  let imported_or_seen_csets = get_imported_or_seen_csets f imp.P.imp_sees imp.P.imp_imports in
  let () = List.iter (manage_set_concretisation_exn imported_or_seen_csets env) imp.P.imp_values in
*)
  let imp_sees = List.map (load_seen f env) imp.P.imp_sees in
  let imp_imports = List.map (load_included_or_imported V.I_Imports f env) imp.P.imp_imports in
  let imp_extends = List.map (load_extended V.I_Imports f env) imp.P.imp_extends in
  let imp_properties = declare_constants_exn V.I_Properties env
      imp.P.imp_concrete_constants [] imp.P.imp_properties
  in
  let imp_values = List.map (type_value_exn env) imp.P.imp_values in
(*   let () = check_values imp.P.imp_refines.lid_loc env imp_values in *)
  let imp_invariant = declare_variables_exn V.I_Invariant env
      imp.P.imp_concrete_variables [] imp.P.imp_invariant
  in
  let symbs = get_imp_symbols env in
  let imp_assertions = List.map (type_predicate_exn V.I_Invariant env) imp.P.imp_assertions in
  let () = List.iter
      (fun op_name -> G.promote_operation env op_name.lid_loc op_name.lid_str)
      imp.P.imp_promotes
  in
(*   let lops_map = List.fold_left (declare_local_operation_exn env) SMap.empty imp.P.imp_local_operations in *)
  let imp_initialisation = Utils.map_opt (type_substitution_exn V.IS_Operations env) imp.P.imp_initialisation in
  let imp_operations = List.map (declare_operation_exn V.IS_Operations env) imp.P.imp_operations in (*FIXME filtrer les op locales*)
  let imp_promoted = get_imp_promoted_operations env in (*XXX we could check that there is no recursivité*)
  let imp_local_operations = [] in (*FIXME*)
  { T.imp_refines;
    imp_sees; imp_imports; imp_extends;
    imp_set_parameters = symbs.set_parameters;
    imp_scalar_parameters = symbs.scalar_parameters;
    imp_abstract_sets=symbs.abstract_sets;
    imp_concrete_sets=symbs.concrete_sets;
    imp_abstract_constants=symbs.abstract_constants;
    imp_concrete_constants=symbs.concrete_constants;
    imp_abstract_variables=symbs.abstract_variables;
    imp_concrete_variables=symbs.concrete_variables;
    imp_properties; imp_values; imp_invariant; imp_assertions;
    imp_initialisation; imp_promoted; imp_local_operations; imp_operations }

let type_component (f:Utils.loc -> string -> Global.t_interface option) (co:P.component) : (T.component*Global.t_interface option) =
  match co.P.co_desc with
  | P.Machine mch ->
    let env = G.create G.Mch mch.P.mch_parameters in
    let cp = { T.co_name = co.P.co_name; co_desc   = T.Machine (type_machine_exn f env mch) } in
    (cp,Some (G.to_interface env))
  | P.Refinement ref ->
    let env = G.create G.Ref ref.P.ref_parameters in
    let cp = { T.co_name = co.P.co_name; co_desc = T.Refinement (type_refinement_exn f env ref) } in
    (cp,Some (G.to_interface env))
  | P.Implementation imp ->
    let env = G.create G.Imp imp.P.imp_parameters in
    let cp = { T.co_name = co.P.co_name; co_desc   = T.Implementation (type_implementation_exn f env imp) } in
    G.check_operation_coherence env co.P.co_name.lid_loc;
    (cp,None)
