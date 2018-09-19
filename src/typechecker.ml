open Utils
open Btype
open SyntaxCore
module P = PSyntax
module T = TSyntax
module V = Visibility

let allow_becomes_such_that_in_implementation = ref false
let allow_out_parameters_in_precondition = ref false

(* *****************************************************************************
 * Type Checking for Components
 * ************************************************************************** *)

let load_seen_mch_exn (f:Utils.loc->string->Global.t_interface option) (env:'mr Global.t) (mch:lident) : unit =
  match f mch.lid_loc mch.lid_str with
  | None -> Error.raise_exn mch.lid_loc ("The machine '"^mch.lid_str^"' does not typecheck.")
  | Some itf ->
    begin match Global.load_interface_for_seen_machine env itf mch with
      | Ok () -> ()
      | Error err -> raise (Error.Error err)
    end

let load_included_or_imported_mch_exn (f:Utils.loc->string->Global.t_interface option) (env:'mr Global.t) (mi:P.machine_instanciation) : lident = 
  match mi.P.mi_params with
  | [] ->
    begin match f mi.P.mi_mch.lid_loc mi.P.mi_mch.lid_str with
      | None -> Error.raise_exn mi.P.mi_mch.lid_loc ("The machine '"^mi.P.mi_mch.lid_str^"' does not typecheck.")
      | Some itf ->
        begin match Global.load_interface_for_included_or_imported_machine env itf mi.P.mi_mch with
          | Ok () -> mi.P.mi_mch
          | Error err -> raise (Error.Error err)
        end
    end
  | _::_ -> Error.raise_exn mi.P.mi_mch.lid_loc "Not implemented: inclusion of machine with parameters."

let load_extended_mch_exn (f:Utils.loc->string->Global.t_interface option) (env:'mr Global.t) (mi:P.machine_instanciation) : lident = 
  match mi.P.mi_params with
  | [] ->
    begin match f mi.P.mi_mch.lid_loc mi.P.mi_mch.lid_str with
      | None -> Error.raise_exn mi.P.mi_mch.lid_loc ("The machine '"^mi.P.mi_mch.lid_str^"' does not typecheck.")
      | Some itf ->
        begin match Global.load_interface_for_extended_machine env itf mi.P.mi_mch with
          | Ok () -> mi.P.mi_mch
          | Error err -> raise (Error.Error err)
        end
    end
  | _::_ -> Error.raise_exn mi.P.mi_mch.lid_loc "Not implemented: extension of machine with parameters."

let close_exn (lc:loc) (ty:Btype.Open.t) : Btype.t =
  match Btype.close ty with
  | None ->
    Error.raise_exn lc
      ("The type of this expression could not be fully infered. The type infered so far is '"^
       Btype.Open.to_string ty^"'.")
  | Some ty -> ty

let close_bvar (v:Btype.Open.t T.bvar) : Btype.t T.bvar =
  match Btype.close v.T.bv_typ with
  | None -> Error.raise_exn v.T.bv_loc 
      ("The type of symbol '"^v.T.bv_id^
       "' could not be fully infered. The type infered so far is '"^
       Btype.Open.to_string v.T.bv_typ^"'.")
  | Some bv_typ -> { T.bv_loc=v.T.bv_loc; bv_id=v.T.bv_id; bv_typ }

let close_bvar_nlist = Nlist.lb_map ~f:close_bvar

let mk_expr exp_loc exp_typ exp_desc : (_,_,Btype.t) T.expression =
  let exp_typ = close_exn exp_loc exp_typ in
  { T.exp_loc; exp_typ ; exp_desc }

let mk_pred prd_loc prd_desc : (_,_,Btype.t) T.predicate =
  { T.prd_loc; prd_desc }

let rec close_expr_exn  (e:(_,_,Btype.Open.t) T.expression) : (_,_,Btype.t) T.expression =
    match e.T.exp_desc with
  | T.Ident id -> mk_expr e.T.exp_loc e.T.exp_typ (T.Ident id)
  | T.Dollar id -> mk_expr e.T.exp_loc e.T.exp_typ (T.Dollar id)
  | T.Builtin _ as d -> mk_expr e.T.exp_loc e.T.exp_typ d
  | T.Pbool p ->
    mk_expr e.T.exp_loc e.T.exp_typ (T.Pbool (close_pred_exn p))
  | T.Application (e1,e2) ->
    mk_expr e.T.exp_loc e.T.exp_typ (T.Application (close_expr_exn e1,close_expr_exn e2))
  | T.Couple (cm,e1,e2) ->
    mk_expr e.T.exp_loc e.T.exp_typ (T.Couple (cm,close_expr_exn e1,close_expr_exn e2))
  | T.Sequence nlst ->
    mk_expr e.T.exp_loc e.T.exp_typ (T.Sequence (Nlist.map close_expr_exn nlst))
  | T.Extension nlst ->
    mk_expr e.T.exp_loc e.T.exp_typ (T.Extension (Nlist.map close_expr_exn nlst))
  | T.Comprehension (xlst,p) ->
    mk_expr e.T.exp_loc e.T.exp_typ (T.Comprehension (close_bvar_nlist xlst,close_pred_exn p))
  | T.Binder (bi,xlst,p,e0) ->
    mk_expr e.T.exp_loc e.T.exp_typ (T.Binder (bi,close_bvar_nlist xlst,close_pred_exn p,close_expr_exn e0))
  | T.Record_Field_Access (e0,id) ->
    mk_expr e.T.exp_loc e.T.exp_typ (T.Record_Field_Access (close_expr_exn e0,id))
  | T.Record nlst ->
    let aux (id,e) = (id,close_expr_exn e) in
    mk_expr e.T.exp_loc e.T.exp_typ (T.Record(Nlist.map aux nlst))
  | T.Record_Type nlst ->
    let aux (id,e) = (id,close_expr_exn e) in
    mk_expr e.T.exp_loc e.T.exp_typ (T.Record_Type(Nlist.map aux nlst))

and close_pred_exn (p:(_,_,Btype.Open.t) T.predicate) : (_,_,Btype.t) T.predicate =
  match p.T.prd_desc with
  | T.P_Builtin _ as d -> mk_pred p.T.prd_loc d
  | T.Binary_Prop (bop,p1,p2) -> mk_pred p.T.prd_loc (T.Binary_Prop (bop,close_pred_exn p1,close_pred_exn p2))
  | T.Binary_Pred (bop,e1,e2) -> mk_pred p.T.prd_loc (T.Binary_Pred (bop,close_expr_exn e1,close_expr_exn e2))
  | T.Negation p0 -> mk_pred p.T.prd_loc (T.Negation (close_pred_exn p0))
  | T.Universal_Q (xlst,p0) -> mk_pred p.T.prd_loc (T.Universal_Q (close_bvar_nlist xlst,close_pred_exn p0)) 
  | T.Existential_Q (xlst,p0) -> mk_pred p.T.prd_loc (T.Existential_Q (close_bvar_nlist xlst,close_pred_exn p0)) 

type t_comp_type = Mch | Ref | Imp
let mk_subst sub_loc sub_desc = { T.sub_loc; sub_desc; }

let close_mut_var (v:(_,_,Btype.Open.t) T.mut_var) : (_,_,Btype.t) T.mut_var =
  match Btype.close v.T.mv_typ with
  | None -> Error.raise_exn v.T.mv_loc 
      ("The type of symbol '"^v.T.mv_id^
       "' could not be fully infered. The type infered so far is '"^
       Btype.Open.to_string v.T.mv_typ^"'.")
  | Some mv_typ -> { T.mv_loc=v.T.mv_loc; mv_id=v.T.mv_id; mv_typ; mv_kind=v.T.mv_kind }

let close_mut_var_nlist (lst:(_,_,Btype.Open.t) T.mut_var Nlist.t) = Nlist.lb_map ~f:close_mut_var lst

let rec close_subst_exn (t:t_comp_type) (s:(_,_,Btype.Open.t) T.substitution) : (_,_,Btype.t) T.substitution =
  match s.T.sub_desc with
  | T.Skip ->
    mk_subst s.T.sub_loc T.Skip
  | T.Affectation (T.Tuple xlst,e) ->
    mk_subst s.T.sub_loc (T.Affectation(T.Tuple (close_mut_var_nlist xlst),close_expr_exn e))
  | T.Affectation (T.Function(v,nlst),e) ->
    mk_subst s.T.sub_loc (T.Affectation (T.Function(close_mut_var v,Nlist.map close_expr_exn nlst),close_expr_exn e))
  | T.Affectation (T.Record(v,id),e) ->
    mk_subst s.T.sub_loc (T.Affectation (T.Record(close_mut_var v,id),close_expr_exn e))
  | T.Pre (p,s0) ->
    if t = Imp then
      Error.raise_exn s.T.sub_loc "The subsitution 'Precondition' is not allowed in implementations."
    else
      mk_subst s.T.sub_loc (T.Pre(close_pred_exn p,close_subst_exn t s0))
  | T.Assert (p,s0) -> mk_subst s.T.sub_loc (T.Assert(close_pred_exn p,close_subst_exn t s0))
  | T.Choice nlst ->
    if t = Imp then
      Error.raise_exn s.T.sub_loc "The subsitution 'Choice' is not allowed in implementations."
    else
      mk_subst s.T.sub_loc (T.Choice(Nlist.map (close_subst_exn t) nlst))
  | T.IfThenElse (nlst,opt) ->
    let aux (p,s) = (close_pred_exn p,close_subst_exn t s) in
    let topt = match opt with
      | None -> None
      | Some s0 -> Some (close_subst_exn t s0)
    in
    mk_subst s.T.sub_loc (T.IfThenElse (Nlist.map aux nlst,topt))
  | T.Select (nlst,opt) ->
    if t = Imp then
      Error.raise_exn s.T.sub_loc "The subsitution 'Select' is not allowed in implementations."
    else
      let aux (p,s) = (close_pred_exn p,close_subst_exn t s) in
      let topt = match opt with
        | None -> None
        | Some s0 -> Some (close_subst_exn t s0)
      in
      mk_subst s.T.sub_loc (T.Select (Nlist.map aux nlst,topt))
  | T.Case (e,nlst,opt) -> 
    let aux (lst,s) = (Nlist.map close_expr_exn lst,close_subst_exn t s) in
    let topt = match opt with
      | None -> None
      | Some s0 -> Some (close_subst_exn t s0)
    in
    mk_subst s.T.sub_loc (T.Case (close_expr_exn e,Nlist.map aux nlst,topt))
  | T.Any (xlst,p,s0) ->
    if t = Imp then
      Error.raise_exn s.T.sub_loc "The subsitution 'Any' is not allowed in implementations."
    else
      mk_subst s.T.sub_loc (T.Any (close_bvar_nlist xlst,close_pred_exn p,close_subst_exn t s0))
  | T.Let (xlst,nlst,s0) ->
    if t = Imp then
      Error.raise_exn s.T.sub_loc "The subsitution 'Let' is not allowed in implementations."
    else
      let aux (v,e) = (close_bvar v,close_expr_exn e) in
      mk_subst s.T.sub_loc (T.Let (close_bvar_nlist xlst,Nlist.map aux nlst,close_subst_exn t s0))
  | T.BecomesElt (xlst,e) ->
    if t = Imp then
      Error.raise_exn s.T.sub_loc "The subsitution 'Becomes Element' is not allowed in implementations."
    else
      mk_subst s.T.sub_loc (T.BecomesElt (close_mut_var_nlist xlst,close_expr_exn e))
  | T.BecomesSuch (xlst,p) ->
    if t = Imp && (not !allow_becomes_such_that_in_implementation) then
      Error.raise_exn s.T.sub_loc ("Substitution 'Becomes Such That' is not allowed in implementations.")
    else
      mk_subst s.T.sub_loc (T.BecomesSuch (close_mut_var_nlist xlst,close_pred_exn p))
  | T.Var (xlst,s0) ->
    if t = Mch then
      Error.raise_exn s.T.sub_loc ("Substitution 'Var' is not allowed in abstract machines.")
    else
      mk_subst s.T.sub_loc (T.Var (close_bvar_nlist xlst,close_subst_exn t s0))
  | T.CallUp (args_out,id,args_in) ->
    mk_subst s.T.sub_loc (T.CallUp (List.map close_mut_var args_out,id,List.map close_expr_exn args_in))
  | T.While (p1,s0,p2,e) ->
    if t != Imp then
      Error.raise_exn s.T.sub_loc ("Substitution 'While' is only allowed in implementations.")
    else
      mk_subst s.T.sub_loc (T.While (close_pred_exn p1,close_subst_exn t s0,close_pred_exn p2,close_expr_exn e))
  | T.Sequencement (s1,s2) ->
    if t = Mch then
      Error.raise_exn s.T.sub_loc ("Substitution 'Sequence' is not allowed in abstract machines.")
    else
      mk_subst s.T.sub_loc (T.Sequencement (close_subst_exn t s1,close_subst_exn t s2))
  | T.Parallel (s1,s2) ->
    if t = Imp then
      Error.raise_exn s.T.sub_loc ("Substitution '||' is only allowed in implementations.")
    else
      mk_subst s.T.sub_loc (T.Parallel (close_subst_exn t s1,close_subst_exn t s2))

let declare_global_symbol_exn env loc id typ kind : unit =
  match Global.add_symbol env loc id typ kind with
  | Ok () -> ()
  | Error err -> raise (Error.Error err)

let declare_set_exn (env:'mr Global.t) (s:P.set) : unit =
  match s with
  | P.Abstract_Set v ->
    let typ = Btype.mk_Power (Btype.mk_Abstract_Set Btype.T_Current v.lid_str) in
    declare_global_symbol_exn env v.lid_loc v.lid_str typ Global.K_Abstract_Set
  | P.Concrete_Set (v,elts) ->
    let typ = Btype.mk_Concrete_Set Btype.T_Current v.lid_str in
    let elts2 = List.map (fun lid -> lid.lid_str) elts in
    let () = declare_global_symbol_exn env v.lid_loc v.lid_str
        (Btype.mk_Power typ) (Global.K_Concrete_Set elts2)
    in
    List.iter (fun lid ->
        declare_global_symbol_exn env lid.lid_loc lid.lid_str typ Global.K_Enumerate
      ) elts

let declare_local_symbol (ctx:Local.t) (lid:lident) : Local.t =
  Local.add ctx lid.lid_str (Btype.Open.new_meta ()) Local.L_Expr_Binder

let promote_symbol_exn (type mr ac) (env:mr Global.t) (ctx:Local.t)
    (ki:ac Global.t_global_kind) (lid:lident) : unit =
      match Local.get ctx lid.lid_str with
      | None -> assert false
      | Some (ty,_) ->
        begin match Btype.close ty with
          | None -> Error.raise_exn lid.lid_loc ("The type of '"^lid.lid_str^"' could not be fully infered. Type infered: "^Btype.Open.to_string ty^".")
          | Some ty ->
           begin match Global.add_symbol env lid.lid_loc lid.lid_str ty ki with
             | Ok () -> ()
             | Error err -> raise (Error.Error err)
           end
        end

let declare_mch_constants_exn (type cl) (env:Global.t_mch Global.t) (cl:(Global.t_mch,cl) V.clause)
    (cconst:lident list) (aconst:lident list) (prop:P.predicate option)
  : (Global.t_mch,cl,Btype.t) T.predicate option =
  let ctx = Local.empty in
  let ctx = List.fold_left declare_local_symbol ctx cconst in
  let ctx = List.fold_left declare_local_symbol ctx aconst in
  let t_prop = Utils.map_opt (Inference.type_predicate_exn cl env ctx) prop in
  List.iter (promote_symbol_exn env ctx Global.K_Concrete_Constant) cconst;
  List.iter (promote_symbol_exn env ctx Global.K_Abstract_Constant) aconst;
  Utils.map_opt close_pred_exn t_prop

let declare_mch_variables_exn (type cl) (env:Global.t_mch Global.t) (cl:(Global.t_mch,cl) V.clause)
    (cvars:lident list) (avars:lident list) (inv:P.predicate option)
  : (Global.t_mch,cl,Btype.t) T.predicate option =
  let ctx = Local.empty in
  let ctx = List.fold_left declare_local_symbol ctx cvars in
  let ctx = List.fold_left declare_local_symbol ctx avars in
  let t_inv = Utils.map_opt (Inference.type_predicate_exn cl env ctx) inv in
  List.iter (promote_symbol_exn env ctx Global.K_Concrete_Variable) cvars;
  List.iter (promote_symbol_exn env ctx Global.K_Abstract_Variable) avars;
  Utils.map_opt close_pred_exn t_inv

let type_assertion_exn f env p =
  close_pred_exn (Inference.type_predicate_exn f env Local.empty p)

let type_mch_init_exn (env:Global.t_mch Global.t) (s:P.substitution) : (Global.t_mch,V.t_mch_op,Btype.t) T.substitution =
  let s = Inference.type_substitution_exn V.C_Mch_Op env Local.empty s in
  close_subst_exn Mch s 

let get_mch_operation_context_exn (env:Global.t_mch Global.t) (op:P.operation) : Local.t*Local.t =
  let aux ki ctx lid = 
    Local.add ctx lid.lid_str (Btype.Open.new_meta ()) ki
  in
  let ctx0 = List.fold_left (aux Local.L_Param_In) Local.empty op.P.op_in in
  let ctx  = List.fold_left (aux Local.L_Param_Out) ctx0 op.P.op_out in
  (ctx0,ctx)

let rec is_read_only (gl:'mr Global.t) (ctx:string list) (s:P.substitution) : bool =
  match s.P.sub_desc with
  | P.Skip -> true
  | P.Affectation (P.Tuple xlst,_) | P.BecomesElt (xlst,_) | P.BecomesSuch (xlst,_) ->
    let aux v = List.exists (String.equal v.lid_str) ctx in
    List.for_all aux (Nlist.to_list xlst)
  | P.Affectation (P.Function(v,_),_) | P.Affectation (P.Record(v,_),_) ->
    List.exists (String.equal v.lid_str) ctx
  | P.CallUp (args_out,id,args_in) ->
    let aux v = List.exists (String.equal v.lid_str) ctx in
    List.for_all aux args_out &&
    (match Global.get_operation gl id.lid_str with
     | None -> Error.raise_exn s.P.sub_loc ("Unknown operation '"^id.lid_str^"'.")
     | Some infos -> infos.Global.op_readonly)
  | P.Pre (p,s0) -> is_read_only gl ctx s0
  | P.Assert (p,s0) -> is_read_only gl ctx s0
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
  | P.While (p1,s0,p2,e) -> is_read_only gl ctx s0
  | P.Sequencement (s1,s2) | P.Parallel (s1,s2) ->
    is_read_only gl ctx s1 && is_read_only gl ctx s2

let declare_mch_operation_exn (env:Global.t_mch Global.t) (op:P.operation) : (Global.t_mch,V.t_mch_op) T.operation =
  let (ctx0,ctx) = get_mch_operation_context_exn env op in
  let op_body =
    if !allow_out_parameters_in_precondition then
      Inference.type_substitution_exn V.C_Mch_Op env ctx op.P.op_body
    else
      begin match op.P.op_body.P.sub_desc with
        | P.Pre (p,s) ->
          let tp = Inference.type_predicate_exn V.C_Mch_Op env ctx0 p in
          let ts = Inference.type_substitution_exn V.C_Mch_Op env ctx s in
          { T.sub_loc=op.P.op_body.P.sub_loc; sub_desc=T.Pre (tp,ts)}
        | _ -> Inference.type_substitution_exn V.C_Mch_Op env ctx op.P.op_body
      end
  in
  let op_body = close_subst_exn Mch op_body in
  let type_arg_exn ctx lid : T.arg =
    match Local.get ctx lid.lid_str with
    | None ->assert false
    | Some (ty,_) ->
      let arg_typ = close_exn lid.lid_loc ty in
      { T.arg_loc = lid.lid_loc; arg_id=lid.lid_str; arg_typ }
  in
  let op_in = List.map (type_arg_exn ctx) op.P.op_in in
  let op_out = List.map (type_arg_exn ctx) op.P.op_out in
  let aux arg = (arg.T.arg_id,arg.T.arg_typ) in
  let args_in = List.map aux op_in in
  let args_out = List.map aux op_out in
  let is_readonly = is_read_only env (Local.get_vars ctx) op.P.op_body in
  match Global.add_mch_operation env op.P.op_name.lid_loc op.P.op_name.lid_str args_in args_out is_readonly with
  | Ok () -> T.O_Specified { op_name=op.P.op_name; op_in; op_out; op_body }
  | Error err -> raise (Error.Error err)

type ('a_symb,'c_symb) t_symbols = {
  abstract_sets: 'c_symb list;
  concrete_sets: ('c_symb*string list) list;
  abstract_constants: 'a_symb list;
  concrete_constants: 'c_symb list;
  abstract_variables: 'a_symb list;
  concrete_variables: 'c_symb list;
}

let get_mch_symbols (env:Global.t_mch Global.t) : ((Global.t_mch,Global.t_abstract) T.symb,(Global.t_mch,Global.t_concrete) T.symb) t_symbols =
  let aux (id:string) (infos:Global.t_mch Global.t_symbol_infos) rc =
    let add_symb (type ac) (f:(Global.t_mch,ac) T.symb -> _) (d:(Global.t_mch,ac) Global.t_decl) =
        f { T.sy_id = id; sy_typ = infos.Global.sy_typ; sy_src=d }
    in
    match infos.Global.sy_kind with
    | Global.Pack (Global.K_Abstract_Set,src) ->
      add_symb (fun (x:(Global.t_mch,Global.t_concrete) T.symb) -> { rc with abstract_sets = (x::rc.abstract_sets) }) src
    | Global.Pack (Global.K_Concrete_Set elts,src) ->
      add_symb (fun (x:(Global.t_mch,Global.t_concrete) T.symb) -> { rc with concrete_sets = ((x,elts)::rc.concrete_sets) }) src
    | Global.Pack (Global.K_Abstract_Constant,src) ->
      add_symb (fun (x:(Global.t_mch,Global.t_abstract) T.symb) -> { rc with abstract_constants = (x::rc.abstract_constants) }) src
    | Global.Pack (Global.K_Concrete_Constant,src) ->
      add_symb (fun (x:(Global.t_mch,Global.t_concrete) T.symb) -> { rc with concrete_constants = (x::rc.concrete_constants) }) src
    | Global.Pack (Global.K_Abstract_Variable,src) ->
      add_symb (fun (x:(Global.t_mch,Global.t_abstract) T.symb) -> { rc with abstract_variables = (x::rc.abstract_variables) }) src
    | Global.Pack (Global.K_Concrete_Variable,src) ->
      add_symb (fun (x:(Global.t_mch,Global.t_concrete) T.symb) -> { rc with concrete_variables = (x::rc.concrete_variables) }) src
    | Global.Pack (Global.K_Enumerate,_) -> rc
  in
  Global.fold_symbols aux env
    { abstract_sets=[]; concrete_sets=[]; abstract_constants=[];
      concrete_constants=[]; abstract_variables=[]; concrete_variables=[]; }

let promote_operation env op_name : unit =
  match Global.promote_operation env op_name.lid_loc op_name.lid_str with
  | Error err -> raise (Error.Error err)
  | Ok () -> ()

let get_promoted_operations (type mr cl) (env:mr Global.t) : (mr,cl) T.operation list =
  let aux (lid_str:string) (infos:mr Global.t_operation_infos) lst =
    let op_out = infos.Global.op_args_out in
    let op_in = infos.Global.op_args_in in
    match infos.Global.op_src with
    | Global.OD_Included_Or_Imported_And_Promoted (op_source,lid_loc) ->
      (T.O_Promoted {op_out; op_name={lid_loc;lid_str}; op_in; op_source})::lst
    | Global.OD_Included_Or_Imported_Promoted_And_Refined (op_source,lid_loc,_) ->
      (T.O_Promoted {op_out; op_name={lid_loc;lid_str}; op_in; op_source})::lst
    | _ -> lst
  in
  Global.fold_operations aux env []

let type_machine_exn (f:Utils.loc->string->Global.t_interface option) (env:Global.t_mch Global.t) (mch:P.machine) : T.machine =
  let () = match mch.P.mch_constraints with
  | None -> ()
  | Some p -> Error.raise_exn p.P.prd_loc "Not implemented: CONSTRAINT clause."
  in
  let () = match mch.P.mch_uses with
    | [] -> ()
    | hd::_ -> Error.raise_exn hd.lid_loc "Not implemented: USES clause."
  in
  let () = List.iter (load_seen_mch_exn f env) mch.P.mch_sees in
  let mch_includes = List.map (load_included_or_imported_mch_exn f env) mch.P.mch_includes in
  let mch_extends = List.map (load_extended_mch_exn f env) mch.P.mch_extends in
  let () = List.iter (declare_set_exn env) mch.P.mch_sets in
  let mch_properties = declare_mch_constants_exn env V.C_Mch_Prop
      mch.P.mch_concrete_constants mch.P.mch_abstract_constants mch.P.mch_properties
  in
  let mch_invariant = declare_mch_variables_exn env V.C_Mch_Inv
      mch.P.mch_concrete_variables mch.P.mch_abstract_variables mch.P.mch_invariant
  in
  let symbs = get_mch_symbols env in
  let mch_assertions = List.map (type_assertion_exn V.C_Mch_Inv env) mch.P.mch_assertions in
  let () = List.iter (promote_operation env) mch.P.mch_promotes in
  let mch_initialisation = Utils.map_opt (type_mch_init_exn env) mch.P.mch_initialisation in
  let specified_operations = List.map (declare_mch_operation_exn env) mch.P.mch_operations in
  let mch_operations = (get_promoted_operations env)@specified_operations in
  { T.mch_sees=mch.P.mch_sees; mch_includes; mch_extends;
    mch_abstract_sets = symbs.abstract_sets;
    mch_concrete_sets = symbs.concrete_sets;
    mch_concrete_constants = symbs.concrete_constants;
    mch_abstract_constants = symbs.abstract_constants;
    mch_concrete_variables = symbs.concrete_variables;
    mch_abstract_variables = symbs.abstract_variables;
    mch_properties; mch_invariant;
    mch_assertions; mch_initialisation; mch_operations
  }

let declare_local_symbol_in_ref (env:Global.t_ref Global.t) (ctx:Local.t) (lid:lident) : Local.t =
  match Global.get_symbol env lid.lid_str with
  | None -> Local.add ctx lid.lid_str (Btype.Open.new_meta ()) Local.L_Expr_Binder
  | Some infos -> Local.add ctx lid.lid_str (infos.Global.sy_typ :> Btype.Open.t) Local.L_Expr_Binder

let declare_ref_constants_exn (type cl) (env:Global.t_ref Global.t) (cl:(Global.t_ref,cl) V.clause)
    (cconst:lident list) (aconst:lident list) (prop:P.predicate option)
  : (Global.t_ref,cl,Btype.t) T.predicate option =
  let ctx = Local.empty in
  let ctx = List.fold_left (declare_local_symbol_in_ref env) ctx cconst in
  let ctx = List.fold_left (declare_local_symbol_in_ref env) ctx aconst in
  let t_prop = Utils.map_opt (Inference.type_predicate_exn cl env ctx) prop in
  List.iter (promote_symbol_exn env ctx Global.K_Concrete_Constant) cconst;
  List.iter (promote_symbol_exn env ctx Global.K_Abstract_Constant) aconst;
  Utils.map_opt close_pred_exn t_prop

let declare_ref_variables_exn (type cl) (env:Global.t_ref Global.t) (cl:(Global.t_ref,cl) V.clause) 
    (cvars:lident list) (avars:lident list) (inv:P.predicate option)
  : (Global.t_ref,cl,Btype.t) T.predicate option =
  let ctx = Local.empty in
  let ctx = List.fold_left (declare_local_symbol_in_ref env) ctx cvars in
  let ctx = List.fold_left (declare_local_symbol_in_ref env) ctx avars in
  let t_inv = Utils.map_opt (Inference.type_predicate_exn cl env ctx) inv in
  List.iter (promote_symbol_exn env ctx Global.K_Concrete_Variable) cvars;
  List.iter (promote_symbol_exn env ctx Global.K_Abstract_Variable) avars;
  Utils.map_opt close_pred_exn t_inv

let load_refines_exn (f:Utils.loc->string->Global.t_interface option) (env:Global.t_ref Global.t) (mch:lident) : unit =
  match f mch.lid_loc mch.lid_str with
  | None -> Error.raise_exn mch.lid_loc ("The machine '"^mch.lid_str^"' does not typecheck.")
  | Some itf ->
    begin match Global.load_interface_for_refined_machine env itf mch with
      | Ok () -> ()
      | Error err -> raise (Error.Error err)
    end

let get_ref_symbols (env:Global.t_ref Global.t) :
  ((Global.t_ref,Global.t_abstract) T.symb,(Global.t_ref,Global.t_concrete) T.symb) t_symbols =
  let aux (id:string) (infos:Global.t_ref Global.t_symbol_infos) rc =
    let add_a_symb f (d:(Global.t_ref,Global.t_abstract) Global.t_decl) =
      f { T.sy_id = id; sy_typ = infos.Global.sy_typ; sy_src=d }
    in
    let add_c_symb f (d:(Global.t_ref,Global.t_concrete) Global.t_decl) =
      f { T.sy_id = id; sy_typ = infos.Global.sy_typ; sy_src=d }
    in
    match infos.Global.sy_kind with
    | Global.Pack (Global.K_Abstract_Set,src) ->
      add_c_symb (fun x -> { rc with abstract_sets = (x::rc.abstract_sets) }) src
    | Global.Pack (Global.K_Concrete_Set elts,src) ->
      add_c_symb (fun x -> { rc with concrete_sets = ((x,elts)::rc.concrete_sets) }) src
    | Global.Pack (Global.K_Abstract_Constant,src) ->
      add_a_symb (fun x -> { rc with abstract_constants = (x::rc.abstract_constants) }) src
    | Global.Pack (Global.K_Concrete_Constant,src) ->
      add_c_symb (fun x -> { rc with concrete_constants = (x::rc.concrete_constants) }) src
    | Global.Pack (Global.K_Abstract_Variable,src) ->
      add_a_symb (fun x -> { rc with abstract_variables = (x::rc.abstract_variables) }) src
    | Global.Pack (Global.K_Concrete_Variable,src) ->
      add_c_symb (fun x -> { rc with concrete_variables = (x::rc.concrete_variables) }) src
    | Global.Pack (Global.K_Enumerate,_) -> rc
  in
  Global.fold_symbols aux env
    { abstract_sets=[]; concrete_sets=[]; abstract_constants=[];
      concrete_constants=[]; abstract_variables=[]; concrete_variables=[]; }

let type_ref_init_exn env s =
  close_subst_exn Ref (Inference.type_substitution_exn V.C_Ref_Op env Local.empty s)

let check_signature (op:P.operation) args_in args_out =
  let rec aux lst1 lst2 =
    match lst1, lst2 with
    | [], [] -> ()
    | v::lst1, (x,_)::lst2 ->
      if String.equal v.lid_str x then aux lst1 lst2
      else Error.raise_exn v.lid_loc ("Expecting parameter '"^x^"' instead of '"^v.lid_str^"'.")
    | v::_, [] -> Error.raise_exn v.lid_loc ("Unexpected parameter '"^v.lid_str^"'.")
    | [], (x,_)::_ ->
      Error.raise_exn op.P.op_name.lid_loc ("Missing parameter '"^x^"'.")
  in
  aux op.P.op_in args_in;
  aux op.P.op_out args_out

let get_ref_operation_context_exn (env:Global.t_ref Global.t) (op:P.operation) =
  let open Inference in
  match Global.get_operation env op.P.op_name.lid_str with
  | None ->
    let aux ki ctx lid = 
      Local.add ctx lid.lid_str (Btype.Open.new_meta ()) ki
    in
    let ctx0 = List.fold_left (aux Local.L_Param_In) Local.empty op.P.op_in in
    let ctx  = List.fold_left (aux Local.L_Param_Out) ctx0 op.P.op_out in
    (ctx0,ctx)
  | Some infos ->
    let aux (lk:Local.t_local_kind) (ctx:Local.t) (s,ty:string*Btype.t) =
      Local.add ctx s (ty :> Btype.Open.t) lk
    in
    let ctx0 = List.fold_left (aux Local.L_Param_In) Local.empty infos.Global.op_args_in in
    let ctx = List.fold_left (aux Local.L_Param_Out) ctx0 infos.Global.op_args_out in
    let () = check_signature op infos.Global.op_args_in infos.Global.op_args_out in
    (ctx0,ctx)

let declare_ref_operation_exn (env:Global.t_ref Global.t) (op:P.operation) : (Global.t_ref,V.t_ref_op) T.operation =
  let (ctx0,ctx) = get_ref_operation_context_exn env op in
  let op_body =
    if !allow_out_parameters_in_precondition then
      Inference.type_substitution_exn V.C_Ref_Op env ctx op.P.op_body
    else
      begin match op.P.op_body.P.sub_desc with
        | P.Pre (p,s) ->
          let tp = Inference.type_predicate_exn V.C_Ref_Op env ctx0 p in
          let ts = Inference.type_substitution_exn V.C_Ref_Op env ctx s in
          { T.sub_loc=op.P.op_body.P.sub_loc; sub_desc=T.Pre (tp,ts)}
        | _ -> Inference.type_substitution_exn V.C_Ref_Op env ctx op.P.op_body
      end
  in
  let op_body = close_subst_exn Ref op_body in
  let type_arg_exn ctx lid : T.arg =
    match Local.get ctx lid.lid_str with
    | None ->assert false
    | Some (ty,_) ->
      let arg_typ = close_exn lid.lid_loc ty in
      { T.arg_loc = lid.lid_loc; arg_id=lid.lid_str; arg_typ }
  in
  let op_in = List.map (type_arg_exn ctx) op.P.op_in in
  let op_out = List.map (type_arg_exn ctx) op.P.op_out in
  let aux arg = (arg.T.arg_id,arg.T.arg_typ) in
  let args_in = List.map aux op_in in
  let args_out = List.map aux op_out in
  match Global.add_ref_operation env op.P.op_name.lid_loc op.P.op_name.lid_str args_in args_out false with
  | Ok () -> T.O_Specified { op_name=op.P.op_name; op_in; op_out; op_body }
  | Error err -> raise (Error.Error err)

let type_refinement_exn (f:Utils.loc->string->Global.t_interface option) (env:Global.t_ref Global.t) ref : T.refinement =
  let () = match ref.P.ref_local_operations with
    | [] -> ()
    | hd::_ -> Error.raise_exn hd.P.op_name.lid_loc "Not implemented: clause LOCAL_OPERATIONS in refinments."
  in
  let () = load_refines_exn f env ref.P.ref_refines in
  let () = List.iter (load_seen_mch_exn f env) ref.P.ref_sees in
  let ref_includes = List.map (load_included_or_imported_mch_exn f env) ref.P.ref_includes in
  let ref_extends = List.map (load_extended_mch_exn f env) ref.P.ref_extends in
  let () = List.iter (declare_set_exn env) ref.P.ref_sets in
  let ref_properties = declare_ref_constants_exn env V.C_Ref_Prop
      ref.P.ref_concrete_constants ref.P.ref_abstract_constants ref.P.ref_properties
  in
  let ref_invariant = declare_ref_variables_exn env V.C_Ref_Inv
      ref.P.ref_concrete_variables ref.P.ref_abstract_variables ref.P.ref_invariant
  in
  let symbs = get_ref_symbols env
  in
  let ref_assertions = List.map (type_assertion_exn V.C_Ref_Inv env) ref.P.ref_assertions in
  let () = List.iter (promote_operation env) ref.P.ref_promotes in
  let ref_initialisation = Utils.map_opt (type_ref_init_exn env) ref.P.ref_initialisation in
  let specified_operations = List.map (declare_ref_operation_exn env) ref.P.ref_operations in
  let ref_operations = (get_promoted_operations env)@specified_operations in
  { T.ref_refines=ref.P.ref_refines; ref_sees=ref.P.ref_sees; ref_includes; ref_extends;
    ref_abstract_sets = symbs.abstract_sets;
    ref_concrete_sets = symbs.concrete_sets;
    ref_concrete_constants = symbs.concrete_constants;
    ref_abstract_constants = symbs.abstract_constants;
    ref_concrete_variables = symbs.concrete_variables;
    ref_abstract_variables = symbs.abstract_variables;
    ref_properties; ref_invariant; ref_assertions; ref_initialisation; ref_operations }

let type_value_exn (env:Global.t_ref Global.t) (v,e:lident*P.expression) : (*FIXME verifier qu'on a bien tout*)
  (T.value*(Global.t_ref,V.t_imp_val,Btype.t)T.expression) =
  match Global.get_symbol env v.lid_str with
  | None -> Error.raise_exn v.lid_loc ("Unknown identifier '"^v.lid_str^"'.")
  | Some infos ->
    let var_typ = infos.Global.sy_typ in
    let te = close_expr_exn
        (Inference.type_expression_exn V.C_Imp_Val env Local.empty e)
    in
    if Btype.is_equal_modulo_alias (Global.get_alias env) te.T.exp_typ var_typ then
      let val_kind = match infos.Global.sy_kind with
        | Global.Pack(Global.K_Abstract_Set,Global.D_Machine _) ->
          T.VK_Abstract_Set T.VKS_Machine
        | Global.Pack(Global.K_Abstract_Set,Global.D_Redeclared Global.Implicitely) ->
          T.VK_Abstract_Set T.VKS_Implicit
        | Global.Pack(Global.K_Abstract_Set,Global.D_Redeclared Global.By_Machine _) ->
          T.VK_Abstract_Set T.VKS_Redeclared
        | Global.Pack(Global.K_Concrete_Constant,Global.D_Machine _) ->
          T.VK_Concrete_Constant T.VKS_Machine
        | Global.Pack(Global.K_Concrete_Constant,Global.D_Redeclared Global.Implicitely) ->
          T.VK_Concrete_Constant T.VKS_Implicit
        | Global.Pack(Global.K_Concrete_Constant,Global.D_Redeclared Global.By_Machine _) ->
          T.VK_Concrete_Constant T.VKS_Redeclared
        | Global.Pack(Global.K_Abstract_Set, Global.D_Seen _) ->
          Error.raise_exn v.lid_loc "Cannot give a value to an abstract set from a seen machine."
        | Global.Pack(Global.K_Abstract_Set, Global.D_Included_Or_Imported _) ->
          Error.raise_exn v.lid_loc "Cannot give a value to an abstract set from an imported machine."
        | Global.Pack(Global.K_Abstract_Set, Global.D_Redeclared Global.By_Included_Or_Imported _) ->
          Error.raise_exn v.lid_loc "Cannot give a value to an abstract set from an imported machine."
        | Global.Pack(Global.K_Concrete_Constant, Global.D_Seen _) ->
          Error.raise_exn v.lid_loc "Cannot give a value to a concrete constant from a seen machine."
        | Global.Pack(Global.K_Concrete_Constant, Global.D_Included_Or_Imported _) ->
          Error.raise_exn v.lid_loc "Cannot give a value to a concrete constant from an imported machine."
        | Global.Pack(Global.K_Concrete_Constant, Global.D_Redeclared Global.By_Included_Or_Imported _) ->
          Error.raise_exn v.lid_loc "Cannot give a value to a concrete constant from an imported machine."
(*
        | Global.Pack(Global.K_Concrete_Set _, _)
        | Global.Pack(Global.K_Concrete_Variable, _)
        | Global.Pack(Global.K_Abstract_Constant, _)
        | Global.Pack(Global.K_Abstract_Variable, _)
        | Global.Pack(Global.K_Enumerate, _) ->
*)
        | _ -> Error.raise_exn v.lid_loc "This symbol is neither an abstract set nor a concrete constant."
      in
       ( {T.val_loc=v.lid_loc;val_id=v.lid_str;val_kind},te)
    else
      Error.raise_exn e.P.exp_loc
        ("This expression has type '" ^ to_string te.T.exp_typ ^
         "' but an expression of type '" ^ to_string var_typ ^"' was expected.")

let is_abstract_set env v =
  match Global.get_symbol env v.lid_str with
  | None -> false
  | Some infos ->
   begin match infos.Global.sy_kind with
     | Global.Pack (Global.K_Abstract_Set,_) -> true
     | _ -> false
   end

let manage_set_concretisation_exn (env:Global.t_ref Global.t) (v,e:lident*P.expression) : unit =
  if is_abstract_set env v then
    let te = Inference.type_expression_exn V.C_Imp_Op env Local.empty e in
    let typ = close_exn te.T.exp_loc te.T.exp_typ in
    match Btype.view typ with
    | Btype.T_Power ty ->
      if not (Global.add_alias env v.lid_str ty) then
        Error.raise_exn v.lid_loc "Incorrect abstract set definition."
    | _ ->
      let str = Printf.sprintf
          "This expression has type '%s' but an expression of type '%s' was expected."
          (to_string typ) (Btype.Open.to_string (Btype.Open.mk_Power (Btype.Open.new_meta ())))
      in
      Error.raise_exn e.P.exp_loc str

let get_imp_symbols (env:Global.t_ref Global.t) :
  (T.t_abs_imp_symb,(Global.t_ref,Global.t_concrete) T.symb) t_symbols =
  let aux (id:string) (infos:Global.t_ref Global.t_symbol_infos) rc =
    let add_a_symb f (d:(Global.t_ref,Global.t_abstract) Global.t_decl) =
      let asy_src = match d with
      | Global.D_Redeclared (Global.By_Machine _) -> assert false
      | Global.D_Machine _ -> assert false
      | Global.D_Seen mch -> T.I_Seen mch
      | Global.D_Disappearing -> T.I_Disappearing
      | Global.D_Redeclared (Global.By_Included_Or_Imported mch) -> T.I_Redeclared_By_Importation mch
      | Global.D_Included_Or_Imported mch -> T.I_Imported mch
      in
      f { T.asy_id = id; asy_typ = infos.Global.sy_typ; asy_src }
    in
    let add_c_symb f (d:(Global.t_ref,Global.t_concrete) Global.t_decl) =
      f { T.sy_id = id; sy_typ = infos.Global.sy_typ; sy_src=d }
    in
    match infos.Global.sy_kind with
    | Global.Pack (Global.K_Abstract_Set,src) ->
      add_c_symb (fun x -> { rc with abstract_sets = (x::rc.abstract_sets) }) src
    | Global.Pack (Global.K_Concrete_Set elts,src) ->
      add_c_symb (fun x -> { rc with concrete_sets = ((x,elts)::rc.concrete_sets) }) src
    | Global.Pack (Global.K_Abstract_Constant,src) ->
      add_a_symb (fun x -> { rc with abstract_constants = (x::rc.abstract_constants) }) src
    | Global.Pack (Global.K_Concrete_Constant,src) ->
      add_c_symb (fun x -> { rc with concrete_constants = (x::rc.concrete_constants) }) src
    | Global.Pack (Global.K_Abstract_Variable,src) ->
      add_a_symb (fun x -> { rc with abstract_variables = (x::rc.abstract_variables) }) src
    | Global.Pack (Global.K_Concrete_Variable,src) ->
      add_c_symb (fun x -> { rc with concrete_variables = (x::rc.concrete_variables) }) src
    | Global.Pack (Global.K_Enumerate,_) -> rc
  in
  Global.fold_symbols aux env
    { abstract_sets=[]; concrete_sets=[]; abstract_constants=[];
      concrete_constants=[]; abstract_variables=[]; concrete_variables=[]; }

let type_imp_init_exn env s =
  close_subst_exn Imp (Inference.type_substitution_exn V.C_Imp_Op env Local.empty s)

module SMap = Map.Make(String)
type t_lops_map = (Global.t_ref,V.t_imp_lop,Btype.t) T.substitution SMap.t

let declare_imp_operation_exn (env:Global.t_ref Global.t) (lops:t_lops_map) (op:P.operation) : (Global.t_ref,V.t_imp_op) T.operation =
  let (ctx0,ctx) = get_ref_operation_context_exn env op in
  let op_body =
    if !allow_out_parameters_in_precondition then
      Inference.type_substitution_exn V.C_Imp_Op env ctx op.P.op_body
    else
      begin match op.P.op_body.P.sub_desc with
        | P.Pre (p,s) ->
          let tp = Inference.type_predicate_exn V.C_Imp_Op env ctx0 p in
          let ts = Inference.type_substitution_exn V.C_Imp_Op env ctx s in
          { T.sub_loc=op.P.op_body.P.sub_loc; sub_desc=T.Pre (tp,ts)}
        | _ -> Inference.type_substitution_exn V.C_Imp_Op env ctx op.P.op_body
      end
  in
  let op_body = close_subst_exn Imp op_body in
  let type_arg_exn ctx lid : T.arg =
    match Local.get ctx lid.lid_str with
    | None ->assert false
    | Some (ty,_) ->
      let arg_typ = close_exn lid.lid_loc ty in
      { T.arg_loc = lid.lid_loc; arg_id=lid.lid_str; arg_typ }
  in
  let op_in = List.map (type_arg_exn ctx) op.P.op_in in
  let op_out = List.map (type_arg_exn ctx) op.P.op_out in
  let aux arg = (arg.T.arg_id,arg.T.arg_typ) in
  let args_in = List.map aux op_in in
  let args_out = List.map aux op_out in
  match Global.add_ref_operation env op.P.op_name.lid_loc op.P.op_name.lid_str args_in args_out false with
  | Ok () ->
    begin match Global.get_operation env op.P.op_name.lid_str with
      | None -> assert false 
      | Some { Global.op_src=Global.OD_Local_Spec_And_Implem (lc,_) } ->
        begin match SMap.find_opt op.P.op_name.lid_str lops with
          | None -> assert false
          | Some op_spec ->
            T.O_Local { op_name=op.P.op_name; op_in; op_out; op_spec; op_body }
        end
      | Some _ -> T.O_Specified { op_name=op.P.op_name; op_in; op_out; op_body }
    end
  | Error err -> raise (Error.Error err)

let declare_local_operation_exn (env:Global.t_ref Global.t) (map:t_lops_map) (op:P.operation) : t_lops_map =
  let (ctx0,ctx) = get_ref_operation_context_exn env op in (*FIXME*)
  let op_body =
    if !allow_out_parameters_in_precondition then
      Inference.type_substitution_exn V.C_Imp_Lop env ctx op.P.op_body
    else
      begin match op.P.op_body.P.sub_desc with
        | P.Pre (p,s) ->
          let tp = Inference.type_predicate_exn V.C_Imp_Lop env ctx0 p in
          let ts = Inference.type_substitution_exn V.C_Imp_Lop env ctx s in
          { T.sub_loc=op.P.op_body.P.sub_loc; sub_desc=T.Pre (tp,ts)}
        | _ -> Inference.type_substitution_exn V.C_Imp_Lop env ctx op.P.op_body
      end
  in
  let op_body = close_subst_exn Ref op_body in (*FIXME *)
  let type_arg_exn ctx lid : T.arg =
    match Local.get ctx lid.lid_str with
    | None ->assert false
    | Some (ty,_) ->
      let arg_typ = close_exn lid.lid_loc ty in
      { T.arg_loc = lid.lid_loc; arg_id=lid.lid_str; arg_typ }
  in
  let op_in = List.map (type_arg_exn ctx) op.P.op_in in
  let op_out = List.map (type_arg_exn ctx) op.P.op_out in
  let aux arg = (arg.T.arg_id,arg.T.arg_typ) in
  let args_in = List.map aux op_in in
  let args_out = List.map aux op_out in
  match Global.add_ref_operation env op.P.op_name.lid_loc op.P.op_name.lid_str args_in args_out true with
  | Ok () -> SMap.add op.P.op_name.lid_str op_body map
  | Error err -> raise (Error.Error err)

let check_values rm_loc (env:_ Global.t) (vlst:(T.value*_) list) : unit =
  let aux id infos map =
    match infos.Global.sy_kind with
    | Global.Pack(Global.K_Abstract_Set,Global.D_Machine l) ->
      SMap.add id (false,l) map
    | Global.Pack(Global.K_Abstract_Set,Global.D_Redeclared Global.Implicitely) ->
      SMap.add id (false,rm_loc) map
    | Global.Pack(Global.K_Abstract_Set,Global.D_Redeclared Global.By_Machine l) ->
      SMap.add id (false,l) map
    | Global.Pack(Global.K_Concrete_Constant,Global.D_Machine l) ->
      SMap.add id (false,l) map
    | Global.Pack(Global.K_Concrete_Constant,Global.D_Redeclared Global.Implicitely) ->
      SMap.add id (false,rm_loc) map
    | Global.Pack(Global.K_Concrete_Constant,Global.D_Redeclared Global.By_Machine l) ->
      SMap.add id (false,l) map
    | _ -> map
  in
  let cconst = Global.fold_symbols aux env SMap.empty in
  let aux map (v,_) =
    match SMap.find_opt v.T.val_id map with
    | None -> assert false
    | Some (true,_) -> Error.raise_exn v.T.val_loc ("The constant '"^v.T.val_id^"' is valuated twice.")
    | Some (false,l) -> SMap.add v.T.val_id (true,l) map
  in
  let cconst = List.fold_left aux cconst vlst in
  SMap.iter (fun id (is_valuated,loc) ->
      if not is_valuated then
        Error.raise_exn loc ("The constant '"^id^"' is not valuated.")
    ) cconst

let type_implementation_exn (f:Utils.loc->string->Global.t_interface option)
    (env:Global.t_ref Global.t) (imp:P.implementation) : T.implementation =
  let () = load_refines_exn f env imp.P.imp_refines in
  let () = List.iter (load_seen_mch_exn f env) imp.P.imp_sees in
  let imp_imports = List.map (load_included_or_imported_mch_exn f env) imp.P.imp_imports in
  let imp_extends = List.map (load_extended_mch_exn f env) imp.P.imp_extends in
  let () = List.iter (declare_set_exn env) imp.P.imp_sets in
  let () = List.iter (manage_set_concretisation_exn env) imp.P.imp_values in
  let imp_properties = declare_ref_constants_exn env V.C_Imp_Prop
      imp.P.imp_concrete_constants [] imp.P.imp_properties
  in
  let imp_values = List.map (type_value_exn env) imp.P.imp_values in
  let () = check_values imp.P.imp_refines.lid_loc env imp_values in
  let imp_invariant = declare_ref_variables_exn env V.C_Imp_Inv
      imp.P.imp_concrete_variables [] imp.P.imp_invariant
  in
  let symbs = get_imp_symbols env in
  let imp_assertions = List.map (type_assertion_exn V.C_Imp_Inv env) imp.P.imp_assertions in
  let () = List.iter (promote_operation env) imp.P.imp_promotes in
  let lops_map = List.fold_left (declare_local_operation_exn env) SMap.empty imp.P.imp_local_operations in
  let imp_initialisation = Utils.map_opt (type_imp_init_exn env) imp.P.imp_initialisation in
  let specified_operations = List.map (declare_imp_operation_exn env lops_map) imp.P.imp_operations in
  let imp_operations = (get_promoted_operations env)@specified_operations in (*FIXME pas de recursivité*)
  { T.imp_refines=imp.P.imp_refines;
    imp_sees=imp.P.imp_sees;
    imp_imports;
    imp_extends;
    imp_abstract_sets=symbs.abstract_sets;
    imp_concrete_sets=symbs.concrete_sets;
    imp_abstract_constants=symbs.abstract_constants;
    imp_concrete_constants=symbs.concrete_constants;
    imp_abstract_variables=symbs.abstract_variables;
    imp_concrete_variables=symbs.concrete_variables;
    imp_properties;
    imp_values;
    imp_invariant;
    imp_assertions;
    imp_initialisation;
    imp_operations }

let type_component (f:Utils.loc -> string -> Global.t_interface option) (co:P.component) : (T.component*Global.t_interface option) Error.t_result =
  try
    let () = match co.P.co_parameters with
      | [] -> ()
      | x::_ -> Error.raise_exn x.lid_loc "Not implemented: machine with parameters."
    in
    begin match co.P.co_desc with
      | P.Machine mch ->
        let env = Global.create_mch () in
        let cp = { T.co_name = co.P.co_name; co_desc   = T.Machine (type_machine_exn f env mch) } in
        Ok (cp,Some (Global.to_interface env))
      | P.Refinement ref ->
        let env = Global.create_ref () in
        let cp = { T.co_name = co.P.co_name; co_desc = T.Refinement (type_refinement_exn f env ref) } in
        begin match Global.check_operation_coherence_ref env co.P.co_name.lid_loc with
          | Ok () -> Ok (cp,Some (Global.to_interface env))
          | Error err -> Error err
        end
      | P.Implementation imp ->
        let env = Global.create_ref () in
        let cp = { T.co_name = co.P.co_name; co_desc   = T.Implementation (type_implementation_exn f env imp) } in
        begin match Global.check_operation_coherence_imp env co.P.co_name.lid_loc with
          | Ok () -> Ok (cp,None)
          | Error err -> Error err
        end
    end
  with
  | Error.Error err -> Error err
