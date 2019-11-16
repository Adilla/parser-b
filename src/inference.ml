open SyntaxCore
module P = PSyntax
module T = TSyntax
module V = Visibility

let mk_expr (type ki ty) (exp_loc:Utils.loc) (exp_typ:ty) (exp_desc:(ki,ty) T.expression_desc) : (ki,ty) T.expression = { T.exp_loc; exp_typ; exp_desc }
let mk_pred prd_loc prd_desc : _ T.predicate = { T.prd_loc; prd_desc }
let mk_subst sub_loc sub_desc : _ T.substitution = { T.sub_loc; sub_desc  }

let declare_list (ctx:Local.t) (lst:lident list) (lk:Local.t_local_kind) : Local.t =
  List.fold_left (fun ctx v -> Local.declare ctx v.lid_str lk) ctx lst

let declare_nelist (ctx:Local.t) (xlst:lident Nlist.t) (lk:Local.t_local_kind) : Local.t =
  declare_list ctx (Nlist.to_list xlst) lk

let ids_to_product (ctx:Local.t) (xlst:lident Nlist.t) : Btype.Open.t =
  let aux pr v =
    match Local.get ctx v.lid_str with
    | None -> assert false
    | Some (None,_) ->
      (Error.warn v.lid_loc ("The type of '"^v.lid_str^"' could not be inferred. Assuming it is INTEGER."); Btype.Open.t_int)
    | Some (Some ty,_) -> Btype.Open.mk_Product pr (ty:>Btype.Open.t)
  in
  let v = Nlist.hd xlst in
  match Local.get ctx v.lid_str with
  | None -> assert false
  | Some (None,_) ->
    (Error.warn v.lid_loc ("The type of '"^v.lid_str^"' could not be inferred. Assuming it is INTEGER."); Btype.Open.t_int)
  | Some (Some ty,_) -> List.fold_left aux (ty:>Btype.Open.t) (Nlist.tl xlst)

let get_builtin0_type (e:e_builtin_0) : Btype.Open.t =
  let open Btype.Open in
  match e with
    | TRUE | FALSE -> t_bool
    | Integer _ | MaxInt | MinInt  -> t_int
    | String _ -> t_string
    | NATURAL | NATURAL1 | INT | NAT | NAT1 | INTEGER  -> mk_Power t_int
    | STRINGS  -> mk_Power t_string
    | BOOLEANS  -> mk_Power t_bool
    | Empty_Set -> mk_Power (new_meta ())
    | Empty_Seq -> type_of_unary_fun t_int (new_meta ())
    | Successor | Predecessor -> type_of_unary_fun t_int t_int

let get_builtin1_type_exn lc (e:e_builtin_1) : Btype.Open.t*Btype.Open.t =
  let open Btype.Open in
  match e with
    | Unary_Minus -> (t_int,t_int)
    | Max | Min  -> (mk_Power t_int,t_int)
    | Identity_Relation  ->
      let mt = new_meta () in
      (mk_Power mt,type_of_unary_fun mt mt)
    | Inverse_Relation  ->
      let mt1 = new_meta () in
      let mt2 = new_meta () in
      (type_of_unary_fun mt1 mt2,type_of_unary_fun mt2 mt1)
    | Closure | Transitive_Closure ->
      let mt = new_meta () in
      (type_of_unary_fun mt mt,type_of_unary_fun mt mt)
    | Domain  ->
      let t_arg = new_meta () in
      let t_res = new_meta () in
      (type_of_unary_fun t_arg t_res,mk_Power t_arg)
    | Range  ->
      let t_arg = new_meta () in
      let t_res = new_meta () in
      (type_of_unary_fun t_arg t_res,mk_Power t_res)
    | Fnc  ->
      let mt1 = new_meta () in
      let mt2 = new_meta () in
      let t_arg = mk_Power (mk_Product mt1 mt2) in
      let t_res = mk_Power (mk_Product mt1 (mk_Power mt2)) in
      (t_arg,t_res)
    | Rel  ->
      let t_arg = new_meta () in
      let t_res = new_meta () in
      (type_of_unary_fun t_arg (mk_Power t_res),type_of_unary_fun t_arg t_res)
    (* Sequence operators *)
    | Sequence_Set _ ->
      let mt = new_meta () in
      (mk_Power mt,mk_Power (type_of_sequence mt))
    | Size  -> (type_of_sequence (new_meta ()),t_int)
    | First | Last  ->
      let mt = new_meta () in
      (type_of_sequence mt,mt)
    | Reverse | Front | Tail ->
      let t_seq = type_of_sequence (new_meta ()) in
      (t_seq,t_seq)
    | G_Concatenation  ->
      let t_seq = type_of_sequence (new_meta ()) in
      (type_of_sequence t_seq,t_seq)
    | Cardinal  -> (mk_Power (new_meta ()),t_int)
    | Power_Set _ ->
      let t_set = mk_Power (new_meta ()) in
      (t_set,mk_Power t_set)
    | G_Intersection | G_Union  ->
      let t_set = mk_Power (new_meta ()) in
      (mk_Power t_set,t_set)

    | Tree | Btree | Const | Top | Sons | Prefix | Postfix | SizeT | Mirror
    | Rank | Father | Son | Subtree | Arity | Bin | Left | Right | Infix ->
      Error.error lc "Not implemented (tree operators)."

let get_builtin2_type (e:e_builtin_2) : Btype.Open.t*Btype.Open.t*Btype.Open.t =
  let open Btype.Open in
  match e with
    | Product  -> assert false
    | Difference -> assert false
    | Addition | Division | Modulo | Power  -> (t_int,t_int,t_int)
    | Interval  -> (t_int,t_int,mk_Power t_int)
    | Intersection | Union  ->
      let t_set = mk_Power (new_meta ()) in
      (t_set,t_set,t_set)
    | First_Projection ->
      let mt1 = new_meta () in
      let mt2 = new_meta () in
      (mk_Power mt1,mk_Power mt2,type_of_unary_fun (mk_Product mt1 mt2) mt1)
    | Second_Projection ->
      let mt1 = new_meta () in
      let mt2 = new_meta () in
      (mk_Power mt1,mk_Power mt2,type_of_unary_fun (mk_Product mt1 mt2) mt2)
    | Parallel_Product ->
      let mt1 = new_meta () in
      let mt2 = new_meta () in
      let mt3 = new_meta () in
      let mt4 = new_meta () in
      (type_of_unary_fun mt1 mt2,type_of_unary_fun mt3 mt4,
        mk_Power (mk_Product (mk_Product mt1 mt3) (mk_Product mt2 mt4)))
    | Direct_Product ->
      let mt1 = new_meta () in
      let mt2 = new_meta () in
      let mt3 = new_meta () in
      (type_of_unary_fun mt1 mt2,type_of_unary_fun mt1 mt3,
        mk_Power (mk_Product mt1 (mk_Product mt2 mt3)))
    | Composition ->
      let ty1 = new_meta () in
      let ty2 = new_meta () in
      let ty3 = new_meta () in
      (type_of_unary_fun ty1 ty2,type_of_unary_fun ty2 ty3,type_of_unary_fun ty1 ty3)
    | Iteration ->
      let mt = new_meta () in
      (type_of_unary_fun mt mt,t_int,type_of_unary_fun mt mt)
    | Image  ->
      let t_arg = new_meta () in
      let t_res = new_meta () in
      (type_of_unary_fun t_arg t_res,mk_Power t_arg,mk_Power t_res)
    | Domain_Restriction
    | Domain_Soustraction ->
      let mt1 = new_meta () in
      let mt2 = new_meta () in
      let ty_rel = type_of_unary_fun mt1 mt2 in
      let ty_dom = mk_Power mt1 in
      (ty_dom,ty_rel,ty_rel)
    | Codomain_Restriction
    | Codomain_Soustraction ->
      let mt1 = new_meta () in
      let mt2 = new_meta () in
      let ty_rel = type_of_unary_fun mt1 mt2 in
      let ty_ran = mk_Power mt2 in
      (ty_rel,ty_ran,ty_rel)
    | Surcharge  ->
      let ty_f = type_of_unary_fun (new_meta ()) (new_meta ()) in
      (ty_f,ty_f,ty_f)
    | Relations | Functions _ ->
      let mt1 = new_meta () in
      let mt2 = new_meta () in
      (mk_Power mt1,mk_Power mt2,mk_Power (type_of_unary_fun mt1 mt2))
    | Concatenation ->
      let t_seq = type_of_sequence (new_meta ()) in
      (t_seq,t_seq,t_seq)
    | Head_Insertion ->
      let mt = new_meta () in
      let t_seq = type_of_sequence mt in
      (mt,t_seq,t_seq)
    | Tail_Insertion ->
      let mt = new_meta () in
      let t_seq = type_of_sequence mt in
      (t_seq,mt,t_seq)
    | Head_Restriction | Tail_Restriction  ->
      let t_seq = type_of_sequence (new_meta ()) in
      (t_seq,t_int,t_seq)
    | Application ->
      let mt1 = new_meta () in
      let mt2 = new_meta () in
      (type_of_unary_fun mt1 mt2,mt1,mt2)
    | Couple _ ->
      let mt1 = new_meta () in
      let mt2 = new_meta () in
      (mt1,mt2,mk_Product mt1 mt2)

let unexpected_type_exn (lc:Utils.loc) (inf:Btype.Open.t) (exp:Btype.Open.t) =
  let str = Printf.sprintf
      "This expression has type '%s' but an expression of type '%s' was expected."
      (Btype.Open.to_string inf) (Btype.Open.to_string exp)
  in
  Error.error lc str

type t_int_or_power = C_Int | C_Power

let is_int_or_power_exn l alias (arg1:('mr,Btype.Open.t) T.expression) (arg2:('mr,Btype.Open.t) T.expression) : t_int_or_power =
  let open Btype.Open in
  match weak_norm alias arg1.T.exp_typ with
  | T_Int -> C_Int
  | T_Power _ -> C_Power
  | T_UVar _ ->
    begin match weak_norm alias arg2.T.exp_typ with
      | T_Int -> C_Int
      | T_Power _ -> C_Power
      | T_UVar _ ->
        ( Error.warn l "Cannot decide if this is an operation on integers or sets. Assuming it is on integers.";
          C_Int)
      | ty2 -> Error.error arg2.T.exp_loc
               ("This expression has type '"^ to_string ty2^
                "' but an expression of type INTEGER or POW(_) was expected.")
    end
  | ty1 -> Error.error arg1.T.exp_loc
           ("This expression has type '"^ to_string ty1^
            "' but an expression of type INTEGER or POW(_) was expected.")

let type_global_ident (type env_ki id_ki)
    (cl:(env_ki,id_ki) Visibility.clause) (env:(env_ki,_) Global.t)
    (id_loc:Utils.loc) (id_name:string) : (id_ki,Btype.Open.t) T.t_ident
  =
  match Global.get_symbol env id_name with
  | Some infos ->
    begin match Visibility.mk_global cl infos.Global.sy_kind with
      | Some id_kind ->
        { T.id_name;
          id_loc;
          id_type=(infos.Global.sy_typ :> Btype.Open.t);
          id_kind}
      | None ->
        Error.error id_loc ("The symbol '"^id_name^"' is not visible in this clause.") (*FIXME*)
    end
  | None -> Error.error id_loc ("Unknown identifier '"^id_name^"'.")

let type_ident (type env_ki id_ki)
    (cl:(env_ki,id_ki) Visibility.clause) (env:(env_ki,_) Global.t) (ctx:Local.t) 
    (id_loc:Utils.loc) (id_prefix:string option) (id_str:string) : (id_ki,Btype.Open.t) T.t_ident =
  match id_prefix with
  | None ->
    begin match Local.get ctx id_str with
      | Some(Some ty,ki) ->
        begin match Visibility.mk_local cl ki with
          | Some id_kind ->
            { T.id_name = id_str;
              id_loc;
              id_type=(ty :> Btype.Open.t);
              id_kind }
          | None ->
            Error.error id_loc ("The symbol '"^id_str^"' is not visible in this clause.") (*FIXME*)
        end
      | Some(None,_) ->
        Error.error id_loc ("The identifier '"^id_str^"' must be typed before use.")
      | None -> type_global_ident cl env id_loc id_str
    end
  | Some p ->
    type_global_ident cl env id_loc (p ^ "." ^ id_str)

type utuple =
  | T_Ident of (Utils.loc*string*Local.t_local_kind)
  | T_Couple of t_couple*Utils.loc*utuple*utuple
  | T_Expr of P.expression

let is_untyped_id ctx e =
  match e.P.exp_desc with
  | P.Ident (None,id_str) ->
    begin match Local.get ctx id_str with
      | Some (None,ki) -> Some (id_str,ki)
      | _ -> None
    end
  | _ -> None

let rec is_untyped_tuple (ctx:Local.t) (e:P.expression) : utuple =
  match e.P.exp_desc with
  | P.Ident (None,id) ->
    begin match Local.get ctx id with
      | Some (None,ki) -> T_Ident (e.P.exp_loc,id,ki)
      | _ -> T_Expr e
    end
  | P.Builtin_2 (Couple c,e1,e2) ->
    begin match is_untyped_tuple ctx e1, is_untyped_tuple ctx e2 with
      | T_Expr _, T_Expr _ -> T_Expr e
      | t1, t2 -> T_Couple(c,e.P.exp_loc,t1,t2)
    end
  | _ -> T_Expr e

let type_untyped_id (type env_ki id_ki) (cl:(env_ki,id_ki) Visibility.clause) (ctx:Local.t)
    (id_loc:Utils.loc) (id_name:string) (ki:Local.t_local_kind) (ty:Btype.Open.t)
  : (id_ki,Btype.Open.t) T.expression
  =
  match Btype.close ty with
  | None ->
    Error.error id_loc
      ("The type of this expression could not be fully inferred. The type infered so far is '"^
       Btype.Open.to_string ty^"'.")
  | Some cty ->
    let () = Local.set_type ctx id_name cty in
    begin match Visibility.mk_local cl ki with
      | Some id_kind -> mk_expr id_loc ty (T.Ident { T.id_name; id_loc; id_type=ty; id_kind })
      | None -> assert false (*FIXME*)
    end

let get_bv_types (ctx:Local.t) (ids:lident Nlist.t) : T.bvar Nlist.t =
  Nlist.map (
    fun lid ->
      match Local.get ctx lid.lid_str with
      | None -> assert false
      | Some (None,_) ->
        (Error.warn lid.lid_loc ("The type of '"^lid.lid_str^"' could not be inferred. Assuming it is INTEGER.");
         { T.bv_loc=lid.lid_loc; bv_typ=Btype.t_int; bv_id=lid.lid_str })
      | Some (Some bv_typ,_) ->
         { T.bv_loc=lid.lid_loc; bv_typ; bv_id=lid.lid_str }
  ) ids

let rec type_expression_exn : type env_ki id_ki.
  (env_ki,id_ki) Visibility.clause -> (env_ki,_) Global.t -> Local.t ->
  P.expression -> (id_ki,Btype.Open.t) T.expression
  = fun cl env ctx e ->
  let open Btype.Open in
  match e.P.exp_desc with

  | P.Ident (id_prefix,id_str) ->
    let id = type_ident cl env ctx e.P.exp_loc id_prefix id_str in
    mk_expr e.P.exp_loc id.T.id_type (T.Ident id)

  | P.Dollar (id_prefix,id_str) ->
    let id = type_ident cl env ctx e.P.exp_loc id_prefix id_str in
    mk_expr e.P.exp_loc id.T.id_type (T.Dollar id)

  | P.Builtin_0 bi ->
    mk_expr e.P.exp_loc (get_builtin0_type bi) (T.Builtin_0 bi)

  | P.Pbool p ->
    let tp = type_predicate_exn cl env ctx p in
    mk_expr e.P.exp_loc t_bool (T.Pbool tp)

  | P.Builtin_1 (bi,e) ->
    let (ty_arg,ty_res) = get_builtin1_type_exn e.P.exp_loc bi in
    let te = type_expression_exn cl env ctx e in
    begin match get_stype (Global.get_alias env) ty_arg te.T.exp_typ with
      | Some _ -> mk_expr e.P.exp_loc ty_res (T.Builtin_1 (bi,te))
      | None -> unexpected_type_exn e.P.exp_loc te.T.exp_typ ty_arg
    end

  | P.Builtin_2 (Product,e1,e2) ->
    let te1 = type_expression_exn cl env ctx e1 in
    let te2 = type_expression_exn cl env ctx e2 in
    let (ty_arg1,ty_arg2,ty_res) =
      match is_int_or_power_exn e.P.exp_loc (Global.get_alias env) te1 te2 with
      | C_Int -> (t_int,t_int,t_int)
      | C_Power ->
        let mt1 = new_meta () in
        let mt2 = new_meta () in
        (mk_Power mt1,mk_Power mt2,mk_Power (mk_Product mt1 mt2))
    in
    begin match get_stype (Global.get_alias env) ty_arg1 te1.T.exp_typ with
      | None -> unexpected_type_exn e1.P.exp_loc te1.T.exp_typ ty_arg1
      | Some _ ->
        begin match get_stype (Global.get_alias env) ty_arg2 te2.T.exp_typ with
          | None -> unexpected_type_exn e2.P.exp_loc te2.T.exp_typ ty_arg2
          | Some _ -> mk_expr e.P.exp_loc ty_res (T.Builtin_2 (Product,te1,te2))
        end
    end
  | P.Builtin_2 (Difference,e1,e2) ->
    let te1 = type_expression_exn cl env ctx e1 in
    let te2 = type_expression_exn cl env ctx e2 in
    let (ty_arg1,ty_arg2,ty_res) =
      match is_int_or_power_exn e.P.exp_loc (Global.get_alias env) te1 te2 with
      | C_Int -> (t_int,t_int,t_int)
      | C_Power ->
        let mt = new_meta () in
        (mk_Power mt,mk_Power mt,mk_Power mt)
    in
    begin match get_stype (Global.get_alias env) ty_arg1 te1.T.exp_typ with
      | None -> unexpected_type_exn e1.P.exp_loc te1.T.exp_typ ty_arg1
      | Some _ ->
        begin match get_stype (Global.get_alias env) ty_arg2 te2.T.exp_typ with
          | None -> unexpected_type_exn e2.P.exp_loc te2.T.exp_typ ty_arg2
          | Some _ -> mk_expr e.P.exp_loc ty_res (T.Builtin_2 (Difference,te1,te2))
        end
    end
  | P.Builtin_2 (bi,e1,e2) ->
    let (ty_arg1,ty_arg2,ty_res) = get_builtin2_type bi in
    let te1 = type_expression_exn cl env ctx e1 in
    let te2 = type_expression_exn cl env ctx e2 in
    begin match get_stype (Global.get_alias env) ty_arg1 te1.T.exp_typ with
      | None -> unexpected_type_exn e1.P.exp_loc te1.T.exp_typ ty_arg1
      | Some _ ->
        begin match get_stype (Global.get_alias env) ty_arg2 te2.T.exp_typ with
          | None -> unexpected_type_exn e2.P.exp_loc te2.T.exp_typ ty_arg2
          | Some _ -> mk_expr e.P.exp_loc ty_res (T.Builtin_2 (bi,te1,te2))
        end
    end

  | P.Sequence nlst ->
    begin
      let te = type_expression_exn cl env ctx (Nlist.hd nlst) in
      let aux elt =
        let t_elt = type_expression_exn cl env ctx elt in
        match get_stype (Global.get_alias env) t_elt.T.exp_typ te.T.exp_typ with
        | Some _ -> t_elt
        | None -> unexpected_type_exn elt.P.exp_loc t_elt.T.exp_typ te.T.exp_typ
      in
      let tlst = List.map aux (Nlist.tl nlst) in
      mk_expr e.P.exp_loc (mk_Power (mk_Product t_int te.T.exp_typ))
        (T.Sequence (Nlist.make te tlst))
    end

  | P.Extension nlst ->
    begin
      let te0 = type_expression_exn cl env ctx (Nlist.hd nlst) in
      let aux (elt:P.expression) =
        let t_elt = type_expression_exn cl env ctx elt in
        match get_stype (Global.get_alias env) t_elt.T.exp_typ te0.T.exp_typ with
        | Some _ -> t_elt
        | None -> unexpected_type_exn elt.P.exp_loc t_elt.T.exp_typ te0.T.exp_typ
      in
      let tlst = List.map aux (Nlist.tl nlst) in
      mk_expr e.P.exp_loc (mk_Power te0.T.exp_typ) (T.Extension (Nlist.make te0 tlst))
    end

  | P.Comprehension (ids,p) ->
    let ctx = declare_nelist ctx ids Local.L_Expr_Binder in
    let tp = type_predicate_exn cl env ctx p in
    let tids = get_bv_types ctx ids in
    mk_expr e.P.exp_loc (mk_Power (ids_to_product ctx ids)) (T.Comprehension (tids,tp))

  | P.Binder (bi,ids,p,e0) ->
    begin
      match bi with
      | Sum | Prod ->
        let ctx = declare_nelist ctx ids Local.L_Expr_Binder in
        let tp = type_predicate_exn cl env ctx p in
        let tids = get_bv_types ctx ids in
        let te = type_expression_exn cl env ctx e0 in
        begin match get_stype (Global.get_alias env) te.T.exp_typ t_int with
          | Some _ -> mk_expr e.P.exp_loc t_int (T.Binder (bi,tids,tp,te))
          | None -> unexpected_type_exn e0.P.exp_loc te.T.exp_typ t_int
        end
      | Q_Union | Q_Intersection ->
        let ctx = declare_nelist ctx ids Local.L_Expr_Binder in
        let tp = type_predicate_exn cl env ctx p in
        let tids = get_bv_types ctx ids in
        let te = type_expression_exn cl env ctx e0 in
        let ty_exp = mk_Power (new_meta ()) in
        begin match get_stype (Global.get_alias env) te.T.exp_typ ty_exp with
          | Some ty -> mk_expr e.P.exp_loc ty (T.Binder (bi,tids,tp,te))
          | None -> unexpected_type_exn e0.P.exp_loc te.T.exp_typ ty_exp
        end
      | Lambda ->
        let ctx = declare_nelist ctx ids Local.L_Expr_Binder in
        let tp = type_predicate_exn cl env ctx p in
        let tids = get_bv_types ctx ids in
        let te = type_expression_exn cl env ctx e0 in
        mk_expr e.P.exp_loc (mk_Power (mk_Product (ids_to_product ctx ids) te.T.exp_typ))
          (T.Binder (bi,tids,tp,te))
    end

  | P.Record nlst ->
    let aux (id,e) =
      let te = type_expression_exn cl env ctx e in
      (id,te)
    in
    let tnlst = Nlist.map aux nlst in
    let ty = mk_Record (List.map (fun (id,e) -> (id.lid_str,e.T.exp_typ)) (Nlist.to_list tnlst)) in
    mk_expr e.P.exp_loc ty (T.Record tnlst)

  | P.Record_Type nlst ->
    let aux (id,e) =
      let te = type_expression_exn cl env ctx e in
      (id,te)
    in
    let get_type (id,te) =
      let ty_exp = mk_Power (new_meta ()) in
      match get_stype (Global.get_alias env) te.T.exp_typ ty_exp with
      | Some (T_Power ty) -> (id.lid_str,ty)
      | None -> unexpected_type_exn e.P.exp_loc te.T.exp_typ ty_exp
      | _ -> assert false
    in
    let tlst = Nlist.map aux nlst in
    let ty = mk_Power (mk_Record (List.map get_type (Nlist.to_list tlst))) in
    mk_expr e.P.exp_loc ty (T.Record_Type tlst)

  | P.Record_Field_Access (e0,fd) ->
    let te = type_expression_exn cl env ctx e0 in
    begin match weak_norm (Global.get_alias env) te.T.exp_typ with
      | T_Record lst as ty ->
        begin
          let aux (s,_) = String.equal s fd.lid_str in
          try mk_expr e.P.exp_loc (snd (List.find aux lst)) (T.Record_Field_Access (te,fd))
          with Not_found ->
            Error.error fd.lid_loc
              ("The field '"^fd.lid_str ^"' does not belong to record type '"
               ^to_string ty^"'.")
        end
      | ty -> Error.error e.P.exp_loc
                ("This expression has type '" ^to_string ty
                 ^"' but is expected to be a record.")
    end

and type_predicate_exn : type env_ki id_ki.
    (env_ki,id_ki)Visibility.clause -> (env_ki,_) Global.t ->
    Local.t -> P.predicate -> (id_ki,Btype.Open.t) T.predicate
  = fun cl env ctx p ->
  let open Btype.Open in
  match p.P.prd_desc with
  | P.P_Builtin bi -> mk_pred p.P.prd_loc (T.P_Builtin bi)

  | P.Binary_Prop (op,p1,p2) ->
    let tp1 = type_predicate_exn cl env ctx p1 in
    let tp2 = type_predicate_exn cl env ctx p2 in
    mk_pred p.P.prd_loc (T.Binary_Prop (op,tp1,tp2))

  | P.Negation p ->
    let tp = type_predicate_exn cl env ctx p in
    mk_pred p.P.prd_loc (T.Negation tp)

  | P.Binary_Pred (bop,e1,e2) ->
    begin
      let type_eq () =
        let te1 = type_expression_exn cl env ctx e1 in
        let te2 = type_expression_exn cl env ctx e2 in
        match get_stype (Global.get_alias env) te1.T.exp_typ te2.T.exp_typ with
        | Some _ -> mk_pred p.P.prd_loc (T.Binary_Pred (bop,te1,te2))
        | None -> unexpected_type_exn e2.P.exp_loc te1.T.exp_typ te2.T.exp_typ
      in
      let type_mem () =
        let te1 = type_expression_exn cl env ctx e1 in
        let te2 = type_expression_exn cl env ctx e2 in
        match get_stype (Global.get_alias env) (mk_Power te1.T.exp_typ) te2.T.exp_typ with
        | Some _ -> mk_pred p.P.prd_loc (T.Binary_Pred (bop,te1,te2))
        | None -> unexpected_type_exn e2.P.exp_loc te2.T.exp_typ (mk_Power te1.T.exp_typ)
      in
      let type_inc () =
        let ty0 = mk_Power (new_meta ()) in
        let te1 = type_expression_exn cl env ctx e1 in
        let te2 = type_expression_exn cl env ctx e2 in
        match get_stype (Global.get_alias env) te1.T.exp_typ ty0 with
        | Some ty1_bis ->
          begin match get_stype (Global.get_alias env) ty1_bis te2.T.exp_typ with
            | Some _ -> mk_pred p.P.prd_loc (T.Binary_Pred (bop,te1,te2))
            | None -> unexpected_type_exn e2.P.exp_loc te2.T.exp_typ ty1_bis
          end
        | None -> unexpected_type_exn e1.P.exp_loc te1.T.exp_typ ty0
      in
      match bop with
      | Disequality -> type_eq ()
      | Equality ->
        begin match is_untyped_tuple ctx e1 with
          | T_Expr _ -> type_eq ()
          | tpl ->
            let te2 = type_expression_exn cl env ctx e2 in
            let te1 = check_utuple cl env ctx tpl te2.T.exp_typ in
            mk_pred p.P.prd_loc (T.Binary_Pred (Equality,te1,te2))
        end
      | Non_Membership -> type_mem ()
      | Membership ->
        begin match is_untyped_tuple ctx e1 with
          | T_Expr _ -> type_mem ()
          | tpl ->
            let te2 = type_expression_exn cl env ctx e2 in
            begin match weak_norm (Global.get_alias env) te2.T.exp_typ with
              | T_Power ty ->
                let te1 = check_utuple cl env ctx tpl ty in
                mk_pred p.P.prd_loc (T.Binary_Pred (Membership,te1,te2))
              | _ -> unexpected_type_exn e2.P.exp_loc te2.T.exp_typ (mk_Power (new_meta ()))
            end
        end
      | Inclusion (Not_Strict|Strict as op) ->
        begin match is_untyped_id ctx e1 with
          | Some (id,ki) ->
            let te2 = type_expression_exn cl env ctx e2 in
            begin match weak_norm (Global.get_alias env) te2.T.exp_typ with
            | (T_Power _) as ty ->
              let te1 = type_untyped_id cl ctx e1.P.exp_loc id ki ty in
              mk_pred p.P.prd_loc (T.Binary_Pred (Inclusion op,te1,te2))
            | _ -> unexpected_type_exn e2.P.exp_loc te2.T.exp_typ (mk_Power (new_meta ()))
            end
          | None -> type_inc ()
        end
      | Inclusion (Non_Inclusion|Non_Strict_Inclusion) -> type_inc ()
      | Inequality _ ->
        begin
          let te1 = type_expression_exn cl env ctx e1 in
          let te2 = type_expression_exn cl env ctx e2 in
          begin match get_stype (Global.get_alias env) te1.T.exp_typ t_int with
            | Some _ ->
              begin match get_stype (Global.get_alias env) te2.T.exp_typ t_int with
                | Some _ -> mk_pred p.P.prd_loc (T.Binary_Pred (bop,te1,te2))
                | None -> unexpected_type_exn e2.P.exp_loc te2.T.exp_typ t_int
              end
            | None -> unexpected_type_exn e1.P.exp_loc te1.T.exp_typ t_int
          end
        end
    end

  | P.Universal_Q (ids,p) ->
    let ctx = declare_nelist ctx ids Local.L_Expr_Binder in
    let tp = type_predicate_exn cl env ctx p in
    let tids = get_bv_types ctx ids in
    mk_pred p.P.prd_loc (T.Universal_Q (tids,tp))

  | P.Existential_Q (ids,p) ->
    let ctx = declare_nelist ctx ids Local.L_Expr_Binder in
    let tp = type_predicate_exn cl env ctx p in
    let tids = get_bv_types ctx ids in
    mk_pred p.P.prd_loc (T.Existential_Q (tids,tp))

and check_utuple : type  env_ki id_ki. (env_ki,id_ki) V.clause ->
  (env_ki,_) Global.t -> Local.t -> utuple -> Btype.Open.t ->
  (id_ki,Btype.Open.t) T.expression = fun cl env ctx tpl ty_exp ->
    match tpl with
    | T_Ident (id_loc,id_name,ki) ->
      begin match Btype.close ty_exp with
        | None ->
          Error.error id_loc
            ("The type of this expression could not be fully inferred. The type infered so far is '"^
             Btype.Open.to_string ty_exp^"'.")
        | Some cty ->
          let () = Local.set_type ctx id_name cty in
          begin match Visibility.mk_local cl ki with
            | Some id_kind ->
              mk_expr id_loc ty_exp (T.Ident { T.id_name; id_loc; id_type=ty_exp; id_kind })
            | None -> assert false (*FIXME*)
          end
      end
  | T_Couple (c,lc,t1,t2) ->
    begin match Btype.Open.weak_norm (Global.get_alias env) ty_exp with
      | Btype.Open.T_Product (ty1,ty2) ->
        let t1 = check_utuple cl env ctx t1 ty1 in
        let t2 = check_utuple cl env ctx t2 ty2 in
        mk_expr lc ty_exp (T.Builtin_2(Couple c,t1,t2))
      | _ ->
        let ty_inf = Btype.Open.mk_Product (Btype.Open.new_meta ()) (Btype.Open.new_meta ()) in
        unexpected_type_exn lc ty_inf ty_exp
    end
  | T_Expr e ->
    let te:(id_ki,Btype.Open.t) T.expression = type_expression_exn cl env ctx e in
    begin match Btype.Open.get_stype (Global.get_alias env) te.T.exp_typ ty_exp with
      | Some _ -> te
      | None -> unexpected_type_exn e.P.exp_loc te.T.exp_typ ty_exp
    end

(*
let type_global_mut_ident (type env_ki mut_ki)
    (cl:(env_ki,_,mut_ki,_,_,_) Visibility.sclause) (env:(env_ki,_) Global.env)
    (id_loc:Utils.loc) (id_name:string) : (mut_ki,Btype.Open.t) T.t_ident
  =
  match Global.get_symbol env id_name with
  | Some infos ->
    { T.id_name;
      id_loc;
      id_type=(infos.Global.sy_typ :> Btype.Open.t);
      id_kind=Visibility.mk_global_mut cl infos.Global.sy_kind }
  | None -> Error.error id_loc ("Unknown identifier '"^id_name^"'.")

let type_mut_ident (type env_ki mut_ki)
    (cl:(env_ki,_,mut_ki,_,_,_) Visibility.sclause) (env:(env_ki,_) Global.env) (ctx:Local.t) 
    (id_loc:Utils.loc) (id_prefix:string option) (id_str:string) : (mut_ki,Btype.Open.t) T.t_ident =
  match id_prefix with
  | None ->
    begin match Local.get ctx id_str with
      | Some(Some ty,ki) ->
        { T.id_name = id_str;
          id_loc;
          id_type=(ty :> Btype.Open.t);
          id_kind=Visibility.mk_local_mut cl ki }
      | Some(None,_) ->
        Error.error id_loc ("The identifier '"^id_str^"' must be typed before use.")
      | None -> type_global_mut_ident cl env id_loc id_str
    end
  | Some p ->
    type_global_mut_ident cl env id_loc (p ^ "." ^ id_str)
*)

let type_writable_var_exn : type env_ki mut_ki. (env_ki,_,mut_ki,_,_,_) Visibility.sclause ->
  (env_ki,_) Global.t -> Local.t -> ren_ident -> (mut_ki,Btype.Open.t) T.t_ident
  = fun cl env ctx x ->
    match x.r_prefix with
    | None ->
      begin match Local.get ctx x.r_str with
        | Some(None,_) -> Error.error x.r_loc ("The identifier '"^x.r_str^"' must be typed before use.")
        | Some (Some ty,ki) ->
          begin match V.mk_local_mut cl ki with
            | Some id_kind -> { T.id_loc=x.r_loc; id_name=x.r_str; id_type=(ty:>Btype.Open.t); id_kind }
            | None -> assert false (*FIXME*)
          end
        | None ->
          begin match Global.get_symbol env x.r_str with
            | Some infos ->
              begin match V.mk_global_mut cl infos.Global.sy_kind with
                | Some id_kind ->
                  { T.id_loc=x.r_loc; id_name=x.r_str;
                    id_type=(infos.Global.sy_typ:>Btype.Open.t);
                    id_kind }
                | None -> assert false (*FIXME*)
              end
            | None -> Error.error x.r_loc ("Unknown identifier '"^x.r_str^"'.")
          end
      end
    | Some p ->
      let id_name = p ^ "." ^ x.r_str in
      begin match Global.get_symbol env id_name with
        | Some infos ->
          begin match V.mk_global_mut cl infos.Global.sy_kind with
            | Some id_kind ->
              { T.id_loc=x.r_loc; id_name; id_type=(infos.Global.sy_typ:>Btype.Open.t);
                id_kind }
            | None -> assert false (*FIXME*)
          end
        | None -> Error.error x.r_loc ("Unknown identifier '"^id_name^"'.")
      end

let check_writable_nlist : type env_ki mut_ki. (env_ki,_,mut_ki,_,_,_) V.sclause ->
  (env_ki,_) Global.t -> Local.t -> ren_ident Nlist.t -> Utils.loc -> Btype.Open.t ->
  (mut_ki,Btype.Open.t) T.t_ident Nlist.t
  = fun cl env ctx xlst loc ty ->
  let mk_mut (lid:ren_ident) id_type =
    match lid.r_prefix with
    | None ->
      begin match Local.get ctx lid.r_str with
        | Some (None,ki) ->
          begin match Btype.close id_type with
            | None ->
              Error.error lid.r_loc
                ("The type of this expression could not be fully inferred. The type infered so far is '"^
                 Btype.Open.to_string id_type^"'.")
            | Some cty ->
              let () = Local.set_type ctx lid.r_str cty in
              begin match Visibility.mk_local_mut cl ki with
                | Some id_kind -> { T.id_loc=lid.r_loc; id_name=lid.r_str; id_type; id_kind }
                | None -> assert false (*FIXME*)
              end
          end
        | _ ->
          let v = type_writable_var_exn cl env ctx lid in
          begin match Btype.Open.get_stype (Global.get_alias env) v.T.id_type id_type with
            | None -> unexpected_type_exn v.id_loc v.T.id_type id_type
            | Some _ -> v
          end
      end
    | Some _ ->
      let v = type_writable_var_exn cl env ctx lid in
      begin match Btype.Open.get_stype (Global.get_alias env) v.T.id_type id_type with
        | None -> unexpected_type_exn v.id_loc v.T.id_type id_type
        | Some _ -> v
      end
  in
  let exception Product_Exn in
  let rec aux ty xlst =
    match Nlist.tl xlst with
    | [] -> Nlist.make1 (mk_mut (Nlist.hd xlst) ty)
    | hd::tl ->
      begin match Btype.Open.weak_norm (Global.get_alias env) ty with
        | Btype.Open.T_Product (ty1,ty2) ->
          Nlist.cons (mk_mut (Nlist.hd xlst) ty2) (aux ty1 (Nlist.make hd tl))
        | _ -> raise Product_Exn
      end
  in
  try Nlist.rev (aux ty (Nlist.rev xlst))
  with Product_Exn ->
    let rec mk_product acc = function
      | [] -> acc
      | _::tl ->
        mk_product (Btype.Open.mk_Product acc (Btype.Open.new_meta ())) tl
    in
    let ty_exp = mk_product (Btype.Open.new_meta ()) (Nlist.tl xlst) in
    unexpected_type_exn loc ty ty_exp

let type_out_parameter (type env_ki mut_ki) (cl:(env_ki,_,mut_ki,_,_,_) V.sclause)
    (env:(env_ki,_) Global.t) (ctx:Local.t) (id:ren_ident) (_,ty:_*Btype.t) :
  (mut_ki,Btype.Open.t) T.t_ident
  =
  let ty_exp = ( ty :> Btype.Open.t) in
  match id.r_prefix with
  | None ->
    begin match Local.get ctx id.r_str with
      | Some(None,ki) ->
        let () = Local.set_type ctx id.r_str ty in
        begin match V.mk_local_mut cl ki with
          | Some id_kind -> { T.id_loc=id.r_loc; id_name=id.r_str; id_type=ty_exp; id_kind }
          | None -> assert false (*FIXME*)
        end
      | _ ->
        let tid = type_writable_var_exn cl env ctx id in
        begin match Btype.Open.get_stype (Global.get_alias env) tid.T.id_type ty_exp with
          | None -> unexpected_type_exn tid.T.id_loc tid.T.id_type ty_exp
          | Some _ -> tid
        end
    end
  | Some _ ->
    let tid = type_writable_var_exn cl env ctx id in
    begin match Btype.Open.get_stype (Global.get_alias env) tid.T.id_type ty_exp with
      | None -> unexpected_type_exn tid.T.id_loc tid.T.id_type ty_exp
      | Some _ -> tid
    end

let type_in_parameter (type env_ki id_ki) (cl:(env_ki,id_ki) V.clause)
  (env:(env_ki,_) Global.t) (ctx:Local.t) (e:P.expression) (_,ty:string*Btype.t) :
  (id_ki,Btype.Open.t) T.expression
  =
    let ty_exp = ( ty :> Btype.Open.t) in
    let te = type_expression_exn cl env ctx e in
    match Btype.Open.get_stype (Global.get_alias env) te.T.exp_typ ty_exp with
    | None -> unexpected_type_exn te.T.exp_loc te.T.exp_typ ty_exp
    | Some _ -> te

let rec type_substitution_exn : type env_ki id_ki mut_ki assert_ki env_op_ki op_ki.
  (env_ki,id_ki,mut_ki,assert_ki,env_op_ki,op_ki) V.sclause -> (env_ki,env_op_ki) Global.t ->
  Local.t -> P.substitution -> (id_ki,mut_ki,assert_ki,op_ki,Btype.Open.t) T.substitution
  = fun cl env ctx s0 ->
  let open Btype.Open in
  match s0.P.sub_desc with
  | P.Skip -> mk_subst s0.P.sub_loc T.Skip

  | P.Pre (p,s) ->
    let p = type_predicate_exn (V.to_clause cl) env ctx p in
    let s = type_substitution_exn cl env ctx s in
    mk_subst s0.P.sub_loc (T.Pre (p,s))

  | P.Assert (p,s) ->
    let p = type_predicate_exn (V.to_assert cl) env ctx p in
    let s = type_substitution_exn cl env ctx s in
    mk_subst s0.P.sub_loc (T.Assert(p,s))

  | P.Affectation (P.Tuple xlst,e) ->
      let te = type_expression_exn (V.to_clause cl) env ctx e in
      let tlst = check_writable_nlist cl env ctx xlst te.T.exp_loc te.T.exp_typ in
      mk_subst s0.P.sub_loc (T.Affectation (T.Tuple tlst,te))

  | P.Affectation (P.Function(ff,nlst),e) ->
    let _ = type_writable_var_exn cl env ctx ff in
    let rec mk_app (lc:Utils.loc) f = function
      | [] -> f
      | x::tl -> mk_app lc { P.exp_loc=lc; exp_desc=(P.Builtin_2 (Application,f,x)); exp_par=false } tl
    in
    let lhs = mk_app ff.r_loc
        { P.exp_loc=ff.r_loc; exp_desc=(P.Ident (ff.r_prefix,ff.r_str)); exp_par=false} (Nlist.to_list nlst)
    in
    let tlhs = type_expression_exn (V.to_clause cl) env ctx lhs in
    let te = type_expression_exn (V.to_clause cl) env ctx e in
    let () = match get_stype (Global.get_alias env) te.T.exp_typ tlhs.T.exp_typ with
      | None -> unexpected_type_exn e.P.exp_loc te.T.exp_typ tlhs.T.exp_typ
      | Some _ -> ()
    in
    let tf = type_writable_var_exn cl env ctx ff in
    let tlst = Nlist.map (type_expression_exn (V.to_clause cl) env ctx) nlst in
    mk_subst s0.P.sub_loc (T.Affectation (T.Function(tf,tlst),te))

  | P.Affectation (P.Record(rc,fd),e) ->
    let rf_access =
      { P.exp_loc=rc.r_loc;
        exp_par=false;
        exp_desc=(P.Record_Field_Access (
            { P.exp_loc=rc.r_loc; exp_desc=(P.Ident (rc.r_prefix,rc.r_str)); exp_par=false},fd)) }
       in
    let rc = type_writable_var_exn cl env ctx rc in
    let trf_access = type_expression_exn (V.to_clause cl) env ctx rf_access in
    let te = type_expression_exn (V.to_clause cl) env ctx e in
    let () = match get_stype (Global.get_alias env) te.T.exp_typ trf_access.T.exp_typ with
      | None -> unexpected_type_exn e.P.exp_loc te.T.exp_typ trf_access.T.exp_typ 
      | Some _ -> ()
    in
    mk_subst s0.P.sub_loc (T.Affectation (T.Record(rc,fd),te))

  | P.Choice slst ->
    let tlst = Nlist.map (type_substitution_exn cl env ctx) slst in
    mk_subst s0.P.sub_loc (T.Choice tlst)

  | P.IfThenElse (pslst,s_else) ->
    let aux (p,s) =
      let tp = type_predicate_exn (V.to_clause cl) env ctx p in
      let ts = type_substitution_exn cl env ctx s in
      (tp,ts)
    in
    let tps = Nlist.map aux pslst in
    let t_else = match s_else with
      | None -> None
      | Some s -> Some (type_substitution_exn cl env ctx s)
    in
    mk_subst s0.P.sub_loc (T.IfThenElse (tps,t_else))

  | P.Select (pslst,s_else) ->
    let aux (p,s) =
      let tp = type_predicate_exn (V.to_clause cl) env ctx p in
      let ts = type_substitution_exn cl env ctx s in
      (tp,ts)
    in
    let tps = Nlist.map aux pslst in
    let t_else= match s_else with
      | None -> None
      | Some s -> Some (type_substitution_exn cl env ctx s)
    in
    mk_subst s0.P.sub_loc (T.Select (tps,t_else))

  | P.Case (e,nlst,c_else) ->
    let te = type_expression_exn (V.to_clause cl) env ctx e in
    let aux (lst,s) =
      let aux elt =
        let telt = type_expression_exn (V.to_clause cl) env ctx elt in
        match get_stype (Global.get_alias env) te.T.exp_typ telt.T.exp_typ with
        | None -> unexpected_type_exn telt.T.exp_loc telt.T.exp_typ te.T.exp_typ 
        | Some _ -> telt
      in
      let tlst = Nlist.map aux lst in
      let ts = type_substitution_exn cl env ctx s in
      (tlst,ts)
    in
    let tlst = Nlist.map aux nlst in
    let t_else = match c_else with
      | None -> None
      | Some s -> Some (type_substitution_exn cl env ctx s)
    in
    mk_subst s0.P.sub_loc (T.Case(te,tlst,t_else))

  | P.Any (ids,p,s) ->
    let ctx = declare_nelist ctx ids Local.L_Subst_Binder in
    let tp = type_predicate_exn (V.to_clause cl) env ctx p in
    let tids = get_bv_types ctx ids in
    let ts = type_substitution_exn cl env ctx s in
    mk_subst s0.P.sub_loc (T.Any (tids,tp,ts))

  | P.Let (ids,nlst,s) ->
    let ctx = declare_nelist ctx ids Local.L_Subst_Binder in
    let aux (v,e:lident*P.expression) =
      let te = type_expression_exn (V.to_clause cl) env ctx e in
      let () =
        if not (List.exists (fun id -> String.equal id.lid_str v.lid_str) (Nlist.to_list ids)) then
          Error.error v.lid_loc ("Unexpected variable '"^v.lid_str^"'.")
      in
      match Local.get ctx v.lid_str with
      | None -> assert false
      | Some (None,_) ->
        begin match Btype.close te.T.exp_typ with
          | None ->
            Error.error te.T.exp_loc
              ("The type of this expression could not be fully inferred. The type infered so far is '"^
               Btype.Open.to_string te.T.exp_typ^"'.")
          | Some bv_typ ->
            let () = Local.set_type ctx v.lid_str bv_typ in
            ({ T.bv_loc=v.lid_loc; bv_id=v.lid_str; bv_typ },te)
        end
      | Some (Some ty_v,_) ->
        begin match get_stype (Global.get_alias env) (ty_v:>Btype.Open.t) te.T.exp_typ with
          | None -> unexpected_type_exn te.T.exp_loc te.T.exp_typ (ty_v:>Btype.Open.t) 
          | Some _ -> ({ T.bv_loc=v.lid_loc; bv_id=v.lid_str; bv_typ=ty_v },te)
        end
    in
    let tlst = Nlist.map aux nlst in
    let tids = get_bv_types ctx ids in
    let ts = type_substitution_exn cl env ctx s in
    mk_subst s0.P.sub_loc (T.Let (tids,tlst,ts))

  | P.BecomesElt (xlst,e) ->
      let ty_exp = mk_Power (new_meta ()) in
      let te = type_expression_exn (V.to_clause cl) env ctx e in
      let ty = match get_stype (Global.get_alias env) te.T.exp_typ ty_exp with
        | None -> unexpected_type_exn e.P.exp_loc te.T.exp_typ ty_exp
        | Some (T_Power ty) -> ty
        | Some _ -> assert false
      in
      let tlst = check_writable_nlist cl env ctx xlst te.T.exp_loc ty in
      mk_subst s0.P.sub_loc (T.BecomesElt (tlst,te))

  | P.BecomesSuch (xlst,p) ->
    let p = type_predicate_exn (V.to_clause cl) env ctx p in
    let tlst = Nlist.map (type_writable_var_exn cl env ctx) xlst in
    mk_subst s0.P.sub_loc (T.BecomesSuch (tlst,p))

  | P.Var (vars,s) ->
    let ctx = declare_nelist ctx vars Local.L_Subst_Binder in
    let s = type_substitution_exn cl env ctx s in
    let tvars = get_bv_types ctx vars in
    mk_subst s0.P.sub_loc (T.Var(tvars,s))

  | P.CallUp (ids,op,params) ->
    let op_name = match op.r_prefix with
      | None -> op.r_str
      | Some p -> p ^ "." ^ op.r_str
    in
    begin match Global.get_operation env op_name with
      | None -> Error.error op.r_loc ("Unknown operation '"^op_name^"'.")
      | Some infos ->
(*
        begin match to_op_source infos.Global.op_readonly infos.Global.op_src with
          | None -> Error.error op.r_loc ("The operation '"^op_name^"' is not visible.")
          | Some op_src ->
*)
            begin try
                let op_src = match Visibility.mk_op cl infos.op_src with
                  | None -> assert false (*FIXME*)
                  | Some op_src -> op_src
                in
                let op = { T.op_prefix=op.r_prefix; op_id = op.r_str; op_loc = op.r_loc; op_src } in
                let tids = List.map2 (type_out_parameter cl env ctx) ids infos.Global.op_args_out in
                let tparams = List.map2 (type_in_parameter (V.to_clause cl) env ctx) params infos.Global.op_args_in in
                mk_subst s0.P.sub_loc (T.CallUp (tids,op,tparams))
              with Invalid_argument _ ->
                Error.error op.r_loc ("Incorrect number of in/out parameters.")
            end
(*         end *)
    end

  | P.While (p,s,inv,var) ->
    let tp = type_predicate_exn (V.to_clause cl) env ctx p in
    let ts = type_substitution_exn cl env ctx s in
    let t_inv = type_predicate_exn (V.to_assert cl) env ctx inv in
    let t_var = type_expression_exn (V.to_assert cl) env ctx var in
    let exp = t_int in
    let () = match get_stype (Global.get_alias env) t_var.T.exp_typ exp with
      | None -> unexpected_type_exn var.P.exp_loc t_var.T.exp_typ exp
      | Some _ -> ()
    in
    mk_subst s0.P.sub_loc (T.While (tp,ts,t_inv,t_var))

  | P.Sequencement (s1,s2) ->
    let s1 = type_substitution_exn cl env ctx s1 in
    let s2 = type_substitution_exn cl env ctx s2 in
    mk_subst s0.P.sub_loc (T.Sequencement(s1,s2))

  | P.Parallel (s1,s2) ->
    let s1 = type_substitution_exn cl env ctx s1 in
    let s2 = type_substitution_exn cl env ctx s2 in
    mk_subst s0.P.sub_loc (T.Parallel(s1,s2))
