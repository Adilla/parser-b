open SyntaxCore
module P = PSyntax
module T = TSyntax
module V = Visibility

let mk_expr exp_loc exp_typ exp_desc : ('a,'b,'c) T.expression = { T.exp_loc; exp_typ; exp_desc }
let mk_pred prd_loc prd_desc : ('a,'b,'c) T.predicate = { T.prd_loc; prd_desc }
let mk_subst sub_loc sub_desc : ('a,'b,'c) T.substitution = { T.sub_loc; sub_desc  }

let declare (ctx:Local.t) (id:string) (lk:Local.t_local_kind) : Local.t * Btype.Open.t = 
  let mt = Btype.Open.new_meta () in
  ( Local.add ctx id mt lk, mt )

let declare_list (ctx:Local.t) (lst:lident list) (lk:Local.t_local_kind) : Local.t * Btype.Open.t T.bvar list =
  let (ctx,tvars) = List.fold_left
      (fun (ctx,tvars) v ->
         let (ctx,bv_typ) = declare ctx v.lid_str lk in
         ( ctx, { T.bv_loc=v.lid_loc; bv_typ; bv_id=v.lid_str }::tvars )
      ) (ctx,[]) lst
  in
  (ctx,List.rev tvars)

let declare_nelist (ctx:Local.t) (xlst:lident Nlist.t) (lk:Local.t_local_kind) : Local.t * Btype.Open.t T.bvar Nlist.t =
  let (ctx,lst) = declare_list ctx (Nlist.to_list xlst) lk in
  (ctx,Nlist.from_list_exn lst)

let ids_to_product (ctx:Local.t) (xlst:lident Nlist.t) : Btype.Open.t =
  let aux pr v =
    match Local.get ctx v.lid_str with
    | None -> assert false
    | Some (ty,_) -> Btype.Open.mk_Product pr ty
  in
  match Local.get ctx (Nlist.hd xlst).lid_str with
  | None -> assert false
  | Some (ty,_) -> List.fold_left aux ty (Nlist.tl xlst)

let get_builtin_type_exn (lc:Utils.loc) (e:e_builtin) : Btype.Open.t =
  let open Btype.Open in
  match e with
    (* Booleans *)
    | TRUE | FALSE -> t_bool
    (* Integers *)
    | Integer _ | MaxInt | MinInt  -> t_int
    (* String *)
    | String _ -> t_string
    (* Arithmetic operators *)
    | Unary_Minus | Successor | Predecessor  -> type_of_unary_fun t_int t_int
    | Addition | Division | Modulo | Power  -> type_of_binary_fun t_int t_int t_int
    | Max | Min  -> type_of_unary_fun (mk_Power t_int) t_int
    (* Types *)
    | NATURAL | NATURAL1 | INT | NAT | NAT1 | INTEGER  -> mk_Power t_int
    | STRINGS  -> mk_Power t_string
    | BOOLEANS  -> mk_Power t_bool
    (* Empty set/sequence *)
    | Empty_Set -> mk_Power (new_meta ())
    | Empty_Seq -> type_of_unary_fun t_int (new_meta ())
    (* Arithmetic or Set operator *)
    | Product  -> assert false
    | Difference -> assert false
    (* Operations on sets *)
    | Interval  -> type_of_binary_fun t_int t_int (mk_Power t_int)
    | Intersection | Union  ->
      let t_set = mk_Power (new_meta ()) in
      type_of_binary_fun t_set t_set t_set
    | First_Projection ->
      let mt1 = new_meta () in
      let mt2 = new_meta () in
      type_of_binary_fun (mk_Power mt1) (mk_Power mt2)
        (type_of_unary_fun (mk_Product mt1 mt2) mt1)
    | Second_Projection ->
      let mt1 = new_meta () in
      let mt2 = new_meta () in
      type_of_binary_fun (mk_Power mt1) (mk_Power mt2)
        (type_of_unary_fun (mk_Product mt1 mt2) mt2)
    | Parallel_Product ->
      let mt1 = new_meta () in
      let mt2 = new_meta () in
      let mt3 = new_meta () in
      let mt4 = new_meta () in
      type_of_binary_fun (type_of_unary_fun mt1 mt2) (type_of_unary_fun mt3 mt4)
        (mk_Power (mk_Product (mk_Product mt1 mt3) (mk_Product mt2 mt4)))
    | Direct_Product ->
      let mt1 = new_meta () in
      let mt2 = new_meta () in
      let mt3 = new_meta () in
      type_of_binary_fun (type_of_unary_fun mt1 mt2) (type_of_unary_fun mt1 mt3)
        (mk_Power (mk_Product mt1 (mk_Product mt2 mt3)))
    | Cardinal  -> type_of_unary_fun (mk_Power (new_meta ())) t_int
    | Power_Set _ ->
      let t_set = mk_Power (new_meta ()) in
      type_of_unary_fun t_set (mk_Power t_set)
    | G_Intersection | G_Union  ->
      let t_set = mk_Power (new_meta ()) in
      type_of_unary_fun (mk_Power t_set) t_set
    (* Operations on relations *)
    | Composition ->
      let ty1 = new_meta () in
      let ty2 = new_meta () in
      let ty3 = new_meta () in
      type_of_binary_fun (type_of_unary_fun ty1 ty2) (type_of_unary_fun ty2 ty3) (type_of_unary_fun ty1 ty3)
    | Iteration ->
      let mt = new_meta () in
      type_of_binary_fun (type_of_unary_fun mt mt) t_int (type_of_unary_fun mt mt)
    | Image  ->
      let t_arg = new_meta () in
      let t_res = new_meta () in
      type_of_binary_fun (type_of_unary_fun t_arg t_res) (mk_Power t_arg) (mk_Power t_res)
    | Domain_Restriction
    | Domain_Soustraction ->
      let mt1 = new_meta () in
      let mt2 = new_meta () in
      let ty_rel = type_of_unary_fun mt1 mt2 in
      let ty_dom = mk_Power mt1 in
      type_of_binary_fun ty_dom ty_rel ty_rel
    | Codomain_Restriction
    | Codomain_Soustraction ->
      let mt1 = new_meta () in
      let mt2 = new_meta () in
      let ty_rel = type_of_unary_fun mt1 mt2 in
      let ty_ran = mk_Power mt2 in
      type_of_binary_fun ty_rel ty_ran ty_rel
    | Surcharge  ->
      let ty_f = type_of_unary_fun (new_meta ()) (new_meta ()) in
      type_of_binary_fun ty_f ty_f ty_f
    | Relations | Functions _ ->
      let mt1 = new_meta () in
      let mt2 = new_meta () in
      type_of_binary_fun (mk_Power mt1) (mk_Power mt2) (mk_Power (type_of_unary_fun mt1 mt2))
    | Identity_Relation  ->
      let mt = new_meta () in
      type_of_unary_fun (mk_Power mt) (type_of_unary_fun mt mt)
    | Inverse_Relation  ->
      let mt1 = new_meta () in
      let mt2 = new_meta () in
      type_of_unary_fun (type_of_unary_fun mt1 mt2) (type_of_unary_fun mt2 mt1)
    | Closure | Transitive_Closure ->
      let mt = new_meta () in
      type_of_unary_fun (type_of_unary_fun mt mt) (type_of_unary_fun mt mt)
    | Domain  ->
      let t_arg = new_meta () in
      let t_res = new_meta () in
      type_of_unary_fun (type_of_unary_fun t_arg t_res) (mk_Power t_arg)
    | Range  ->
      let t_arg = new_meta () in
      let t_res = new_meta () in
      type_of_unary_fun (type_of_unary_fun t_arg t_res) (mk_Power t_res)
    | Fnc  ->
      let t_arg = new_meta () in
      let t_res = new_meta () in
      mk_Power(type_of_unary_fun t_arg t_res)
    | Rel  ->
      let t_arg = new_meta () in
      let t_res = new_meta () in
      type_of_unary_fun (type_of_unary_fun t_arg (mk_Power t_res)) (type_of_unary_fun t_arg t_res)
    (* Sequence operators *)
    | Sequence_Set _ ->
      let mt = new_meta () in
      type_of_unary_fun (mk_Power mt) (mk_Power (type_of_sequence mt))
    | Size  -> type_of_unary_fun (type_of_sequence (new_meta ())) t_int 
    | First | Last  ->
      let mt = new_meta () in
      type_of_unary_fun (type_of_sequence mt) mt
    | Reverse | Front | Tail ->
      let t_seq = type_of_sequence (new_meta ()) in
      type_of_unary_fun t_seq t_seq
    | Concatenation ->
      let t_seq = type_of_sequence (new_meta ()) in
      type_of_binary_fun t_seq t_seq t_seq
    | Head_Insertion ->
      let mt = new_meta () in
      let t_seq = type_of_sequence mt in
      type_of_binary_fun mt t_seq t_seq
    | Tail_Insertion ->
      let mt = new_meta () in
      let t_seq = type_of_sequence mt in
      type_of_binary_fun t_seq mt t_seq
    | Head_Restriction | Tail_Restriction  ->
      let t_seq = type_of_sequence (new_meta ()) in
      type_of_binary_fun t_seq t_int t_seq
    | G_Concatenation  ->
      let t_seq = type_of_sequence (new_meta ()) in
      type_of_unary_fun (type_of_sequence t_seq) t_seq
    | Tree | Btree | Const | Top | Sons | Prefix | Postfix | SizeT | Mirror
    | Rank | Father | Son | Subtree | Arity | Bin | Left | Right | Infix ->
      Error.raise_exn lc "Not implemented (tree operators)."

let unexpected_type_exn (lc:Utils.loc) (inf:Btype.Open.t) (exp:Btype.Open.t) =
  let str = Printf.sprintf
      "This expression has type '%s' but an expression of type '%s' was expected."
      (Btype.Open.to_string inf) (Btype.Open.to_string exp)
  in
  Error.raise_exn lc str

type t_int_or_power = C_Int | C_Power

let rec weak_norm : Btype.Open.t -> Btype.Open.t = function
  | Btype.Open.T_UVar { contents=Btype.Open.Bound ty } -> weak_norm ty
  | ty -> ty

let is_int_or_power_exn l (arg:('mr,'cl,Btype.Open.t) T.expression) : t_int_or_power =
  let open Btype.Open in
  match weak_norm arg.T.exp_typ with
  | T_Product (t1,t2) as ty ->
    begin match t1 with
      | T_Int -> C_Int
      | T_Power _ -> C_Power
      | T_UVar _ ->
        begin match weak_norm t2 with
          | T_Int -> C_Int
          | T_Power _ -> C_Power
          | T_UVar _ -> Error.raise_exn l "Cannot decide from this is an operation on integers or sets."
          | _ -> Error.raise_exn arg.T.exp_loc
                   ("This expression has type '"^ to_string ty^
                    "' but an expression of type INTEGER*INTEGER or POW(_)*POW(_) was expected.")
        end
      | _ -> Error.raise_exn arg.T.exp_loc
            ("This expression has type '"^ to_string ty^
             "' but an expression of type INTEGER*INTEGER or POW(_)*POW(_) was expected.")
    end
  | ty -> Error.raise_exn arg.T.exp_loc
            ("This expression has type '"^ to_string ty^
             "' but an expression of product type was expected.")

let type_set_product_exn (app_lc:Utils.loc) (op_lc:Utils.loc) (env:'mr Global.t)
    (arg:('mr,'cl,Btype.Open.t) T.expression): ('mr,'cl,Btype.Open.t) T.expression =
  let open Btype.Open in
  let mt1 = new_meta () in
  let mt2 = new_meta () in
  let op_ty_exp = type_of_binary_fun (mk_Power mt1) (mk_Power mt2) (mk_Power (mk_Product mt1 mt2)) in
  let op_ty_inf = type_of_unary_fun arg.T.exp_typ (new_meta ()) in
  match get_stype (Global.get_alias env) op_ty_exp op_ty_inf with
  | None -> unexpected_type_exn op_lc op_ty_inf op_ty_exp
  | Some _ ->
    let op = mk_expr op_lc op_ty_exp (T.Builtin Product) in
    mk_expr app_lc (mk_Power (mk_Product mt1 mt2)) (T.Application (op,arg))

let type_int_product_exn (app_lc:Utils.loc) (op_lc:Utils.loc) (env:'mr Global.t)
    (arg:('mr,'cl,Btype.Open.t) T.expression) : ('mr,'cl,Btype.Open.t) T.expression =
  let open Btype.Open in
  let op_ty_exp = type_of_binary_fun t_int t_int t_int in
  let op_ty_inf = type_of_unary_fun arg.T.exp_typ (new_meta ()) in
  match get_stype (Global.get_alias env) op_ty_inf op_ty_exp with
  | None -> unexpected_type_exn op_lc op_ty_inf op_ty_exp
  | Some _ ->
    let op = mk_expr op_lc op_ty_exp (T.Builtin Product) in
    mk_expr app_lc t_int (T.Application (op,arg))

let type_int_difference_exn (app_lc:Utils.loc) (op_lc:Utils.loc) (env:'mr Global.t)
    (arg:('mc,'cl,Btype.Open.t) T.expression) : ('mr,'cl,Btype.Open.t) T.expression =
  let open Btype.Open in
  let op_ty_exp = type_of_binary_fun t_int t_int t_int in
  let op_ty_inf = type_of_unary_fun arg.T.exp_typ (new_meta ()) in
  match get_stype (Global.get_alias env) op_ty_inf op_ty_exp with
  | None -> unexpected_type_exn op_lc op_ty_inf op_ty_exp
  | Some _ ->
    let op = mk_expr op_lc op_ty_exp (T.Builtin Difference) in
    mk_expr app_lc t_int (T.Application (op,arg))

let type_set_difference_exn (app_lc:Utils.loc) (op_lc:Utils.loc) (env:'mr Global.t)
    (arg:('mr,'cl,Btype.Open.t) T.expression) : ('mr,'cl,Btype.Open.t) T.expression =
  let open Btype.Open in
  let mt = new_meta () in
  let op_ty_exp = type_of_binary_fun (mk_Power mt) (mk_Power mt) (mk_Power mt) in
  let op_ty_inf = type_of_unary_fun arg.T.exp_typ (new_meta ()) in
  match get_stype (Global.get_alias env) op_ty_exp op_ty_inf with
  | None -> unexpected_type_exn op_lc op_ty_inf op_ty_exp
  | Some _ ->
    let op = mk_expr op_lc op_ty_exp (T.Builtin Difference) in
    mk_expr app_lc (mk_Power mt) (T.Application (op,arg))

let type_ident (type mr cl) (env:mr Global.t) (ctx:Local.t) id_loc (id_str:string)
    (cl:(mr,cl) V.clause) : (mr,cl) T.t_ident * Btype.Open.t =
  match Local.get ctx id_str with
  | Some(ty,ki) -> (T.K_Local (id_str,ki),ty)
  | None ->
    begin match Global.get_symbol env id_str with
      | Some infos ->
       begin match V.get_ident_in_clause cl infos.Global.sy_kind with
         | Some ki -> (T.K_Global (id_str,ki), (infos.Global.sy_typ :> Btype.Open.t) )
         | None -> Error.raise_exn id_loc ("The identifier '"^id_str^"' is not visible in this clause.")
       end
      | None -> Error.raise_exn id_loc ("Unknown identifier '"^id_str^"'.")
    end

let rec type_expression_exn : 'mr 'cl. ('mr,'cl) V.clause ->
  'mr Global.t -> Local.t -> P.expression -> ('mr,'cl,Btype.Open.t) T.expression = fun cl env ctx e ->
  let open Btype.Open in
  match e.P.exp_desc with

  | P.Ident id_str ->
    let ki,ty = type_ident env ctx e.P.exp_loc id_str cl in
    mk_expr e.P.exp_loc ty (T.Ident ki)

  | P.Dollar id_str ->
    let ki,ty = type_ident env ctx e.P.exp_loc id_str cl in
    mk_expr e.P.exp_loc ty (T.Dollar ki)

  | P.Builtin bi ->
    mk_expr e.P.exp_loc (get_builtin_type_exn e.P.exp_loc bi) (T.Builtin bi)

  | P.Pbool p ->
    let tp = type_predicate_exn cl env ctx p in
    mk_expr e.P.exp_loc t_bool (T.Pbool tp)

  | P.Application (e1,e2) ->
    begin
      match e1.P.exp_desc with
      | P.Builtin Product ->
        let te2 = type_expression_exn cl env ctx e2 in
        begin match is_int_or_power_exn e1.P.exp_loc te2 with
          | C_Int -> type_int_product_exn e.P.exp_loc e1.P.exp_loc env te2
          | C_Power -> type_set_product_exn e.P.exp_loc e1.P.exp_loc env te2
        end
      | P.Builtin Difference ->
        let te2 = type_expression_exn cl env ctx e2 in
        begin match is_int_or_power_exn e1.P.exp_loc te2 with
          | C_Int -> type_int_difference_exn e.P.exp_loc e1.P.exp_loc env te2
          | C_Power -> type_set_difference_exn e.P.exp_loc e1.P.exp_loc env te2
        end
      | _ ->
        let te1 = type_expression_exn cl env ctx e1 in
        let ty_fun_exp = type_of_unary_fun (new_meta ()) (new_meta ()) in
        begin match get_stype (Global.get_alias env) te1.T.exp_typ ty_fun_exp with
          | Some (T_Power (T_Product (a,b))) ->
            let te2 = type_expression_exn cl env ctx e2 in
            ( match get_stype (Global.get_alias env) te2.T.exp_typ a with
              | Some _ -> mk_expr e.P.exp_loc b (T.Application (te1,te2))
              | None -> unexpected_type_exn e2.P.exp_loc te2.T.exp_typ a )
          | None -> unexpected_type_exn e1.P.exp_loc te1.T.exp_typ ty_fun_exp
          | _ -> assert false
        end
    end

  | P.Couple (cm,e1,e2) ->
    let te1 = type_expression_exn cl env ctx e1 in
    let te2 = type_expression_exn cl env ctx e2 in
    mk_expr e.P.exp_loc (mk_Product te1.T.exp_typ te2.T.exp_typ) (T.Couple (cm,te1,te2))

  | P.Sequence nlst ->
    begin
      let te = type_expression_exn cl env ctx (Nlist.hd nlst) in
      let aux (elt:P.expression) =
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
    let (ctx,tids) = declare_nelist ctx ids Local.L_Expr_Binder in
    let tp = type_predicate_exn cl env ctx p in
    mk_expr e.P.exp_loc (mk_Power (ids_to_product ctx ids)) (T.Comprehension (tids,tp))

  | P.Binder (bi,ids,p,e0) ->
    begin
      match bi with
      | Sum | Prod ->
        let (ctx,tids) = declare_nelist ctx ids Local.L_Expr_Binder in
        let tp = type_predicate_exn cl env ctx p in
        let te = type_expression_exn cl env ctx e0 in
        begin match get_stype (Global.get_alias env) te.T.exp_typ t_int with
          | Some _ -> mk_expr e.P.exp_loc t_int (T.Binder (bi,tids,tp,te)) 
          | None -> unexpected_type_exn e0.P.exp_loc te.T.exp_typ t_int
        end
      | Q_Union | Q_Intersection ->
        let (ctx,tids) = declare_nelist ctx ids Local.L_Expr_Binder in
        let tp = type_predicate_exn cl env ctx p in
        let te = type_expression_exn cl env ctx e0 in
        let ty_exp = mk_Power (new_meta ()) in
        begin match get_stype (Global.get_alias env) te.T.exp_typ ty_exp with
          | Some ty -> mk_expr e.P.exp_loc ty (T.Binder (bi,tids,tp,te))
          | None -> unexpected_type_exn e0.P.exp_loc te.T.exp_typ ty_exp
        end
      | Lambda ->
        begin
          let (ctx,tids) = declare_nelist ctx ids Local.L_Expr_Binder in
          let tp = type_predicate_exn cl env ctx p in
          let te = type_expression_exn cl env ctx e0 in
          mk_expr e.P.exp_loc (mk_Power (mk_Product (ids_to_product ctx ids) te.T.exp_typ))
            (T.Binder (bi,tids,tp,te))
        end
    end

  | P.Record nlst ->
    let aux (id,e) = (id,type_expression_exn cl env ctx e) in
    let tnlst = Nlist.map aux nlst in
    let ty = mk_Record (Nlist.to_list (Nlist.map (fun (id,e) -> (id.lid_str,e.T.exp_typ)) tnlst)) in
    mk_expr e.P.exp_loc ty (T.Record tnlst)

  | P.Record_Type nlst ->
    let aux (id,e) = (id,type_expression_exn cl env ctx e) in
    let get_type (id,te) =
      let ty_exp = mk_Power (new_meta ()) in
      match get_stype (Global.get_alias env) te.T.exp_typ ty_exp with
      | Some (T_Power ty) -> (id.lid_str,ty)
      | None -> unexpected_type_exn e.P.exp_loc te.T.exp_typ ty_exp
      | _ -> assert false
    in
    let tlst = Nlist.map aux nlst in
    let ty = mk_Power (mk_Record (Nlist.to_list (Nlist.map get_type tlst))) in
    mk_expr e.P.exp_loc ty (T.Record_Type tlst)

  | P.Record_Field_Access (e0,fd) ->
    let te = type_expression_exn cl env ctx e0 in
    begin match weak_norm te.T.exp_typ with
      | T_Record lst as ty ->
        begin
          let aux (s,_) = String.equal s fd.lid_str in
          try mk_expr e.P.exp_loc (snd (List.find aux lst)) (T.Record_Field_Access (te,fd))
          with Not_found ->
            Error.raise_exn fd.lid_loc
              ("The field '"^fd.lid_str ^"' does not belong to record type '"
               ^to_string ty^"'.")
        end
      | ty -> Error.raise_exn e.P.exp_loc
                ("This expression has type '" ^to_string ty
                 ^"' but is expected to be a record.")
    end

and type_predicate_exn : 'mr 'cl. ('mr,'cl) V.clause ->
  'mr Global.t -> Local.t -> P.predicate -> ('mr,'cl,Btype.Open.t) T.predicate = fun cl env ctx p ->
  let open Btype.Open in
  match p.P.prd_desc with
  | P.P_Builtin bi -> mk_pred p.P.prd_loc (T.P_Builtin bi)
  | P.Binary_Prop (op,p1,p2) ->
    let tp1 = type_predicate_exn cl env ctx p1 in
    let tp2 = type_predicate_exn cl env ctx p2 in
    mk_pred p.P.prd_loc (T.Binary_Prop (op,tp1,tp2))
  | P.Negation p ->
    mk_pred p.P.prd_loc (T.Negation (type_predicate_exn cl env ctx p))
  | P.Binary_Pred (bop,e1,e2) ->
    begin
      match bop with
      | Equality | Disequality ->
        begin
          let te1 = type_expression_exn cl env ctx e1 in
          let te2 = type_expression_exn cl env ctx e2 in
          match get_stype (Global.get_alias env) te1.T.exp_typ te2.T.exp_typ with
          | Some _ -> mk_pred p.P.prd_loc (T.Binary_Pred (bop,te1,te2))
          | None -> unexpected_type_exn e2.P.exp_loc te1.T.exp_typ te2.T.exp_typ
        end
      | Membership | Non_Membership ->
        begin
          let te1 = type_expression_exn cl env ctx e1 in
          let te2 = type_expression_exn cl env ctx e2 in
          begin
            match get_stype (Global.get_alias env) (mk_Power te1.T.exp_typ) te2.T.exp_typ with
            | Some _ -> mk_pred p.P.prd_loc (T.Binary_Pred (bop,te1,te2))
            | None -> unexpected_type_exn e2.P.exp_loc te2.T.exp_typ (mk_Power te1.T.exp_typ)
          end
        end
      | Inclusion _ ->
        begin
          let ty0 = mk_Power (new_meta ()) in
          let te1 = type_expression_exn cl env ctx e1 in
          let te2 = type_expression_exn cl env ctx e2 in
          begin match get_stype (Global.get_alias env) te1.T.exp_typ ty0 with
            | Some ty1_bis ->
              begin match get_stype (Global.get_alias env) ty1_bis te2.T.exp_typ with
                | Some _ -> mk_pred p.P.prd_loc (T.Binary_Pred (bop,te1,te2))
                | None -> unexpected_type_exn e2.P.exp_loc te2.T.exp_typ ty1_bis
              end
            | None -> unexpected_type_exn e1.P.exp_loc te1.T.exp_typ ty0
          end
        end
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
    let (ctx,tids) = declare_nelist ctx ids Local.L_Expr_Binder in
    mk_pred p.P.prd_loc (T.Universal_Q (tids,type_predicate_exn cl env ctx p))

  | P.Existential_Q (ids,p) ->
    let (ctx,tids) = declare_nelist ctx ids Local.L_Expr_Binder in
    mk_pred p.P.prd_loc (T.Existential_Q (tids,type_predicate_exn cl env ctx p))

let type_writable_var_exn (type mr cl) (cl:(mr,cl) V.clause) (env:mr Global.t) (ctx:Local.t)
    (x:lident) : (mr,cl,Btype.Open.t) T.mut_var =
  let mv_kind, mv_typ = match Local.get ctx x.lid_str with
  | Some(ty,Local.L_Param_Out) -> (T.MI_Out_Param,ty)
  | Some(ty,Local.L_Subst_Binder) -> (T.MI_Subst_Binder,ty)
  | Some _ -> Error.raise_exn x.lid_loc ("The variable '"^x.lid_str^"' is read-only.")
  | None ->
    begin match Global.get_symbol env x.lid_str with
      | Some infos ->
        begin match V.get_mutable_in_clause cl infos.Global.sy_kind with
          | Some ki -> (T.MI_Global ki, (infos.Global.sy_typ :> Btype.Open.t) )
          | None -> Error.raise_exn x.lid_loc ("The variable '"^x.lid_str^"' cannot be modified.")
        end
      | None -> Error.raise_exn x.lid_loc ("Unknown identifier '"^x.lid_str^"'.")
    end
  in
  { T.mv_loc=x.lid_loc; mv_id=x.lid_str; mv_typ; mv_kind }

let to_op_source (type a) (is_readonly:bool) : a Global.t_op_decl -> T.t_op_source option =
  function
  | Global.OD_Seen mch ->
    if is_readonly then Some (T.SO_Seen_Read_Only mch)
    else None
  | Global.OD_Included_Or_Imported mch -> Some (T.SO_Included_Or_Imported mch)
  | Global.OD_Included_Or_Imported_And_Refined (mch,_) -> Some(T.SO_Included_Or_Imported mch)
  | Global.OD_Included_Or_Imported_And_Promoted (mch,_) -> Some(T.SO_Included_Or_Imported mch)
  | Global.OD_Included_Or_Imported_Promoted_And_Refined (mch,_,_) -> Some(T.SO_Included_Or_Imported mch)
  | Global.OD_Local_Spec l  -> Some (T.SO_Local l)
  | Global.OD_Local_Spec_And_Implem (l,_) -> Some(T.SO_Local l)
  | Global.OD_Current _ -> None
  | Global.OD_Refined _ -> None
  | Global.OD_Current_And_Refined _ -> None

let rec type_substitution_exn : 'mr 'cl.  ('mr,'cl) V.clause ->
    'mr Global.t -> Local.t -> P.substitution -> ('mr,'cl,Btype.Open.t) T.substitution =
  fun cl env ctx s0 ->
  let open Btype.Open in
  match s0.P.sub_desc with
  | P.Skip -> mk_subst s0.P.sub_loc T.Skip

  | P.Pre (p,s) ->
    let p = type_predicate_exn cl env ctx p in
    let s = type_substitution_exn cl env ctx s in
    mk_subst s0.P.sub_loc (T.Pre (p,s))

  | P.Assert (p,s) ->
    let p = type_predicate_exn (V.mk_assert_clause cl) env ctx p in
    let s =  type_substitution_exn cl env ctx s in
    mk_subst s0.P.sub_loc (T.Assert(p,s))
  | P.Affectation (P.Tuple xlst,e) ->
    let rec mk_tuple (x:P.expression) : lident list -> P.expression = function
        | [] -> x
        | hd::tl ->
          let id = { P.exp_loc=hd.lid_loc; exp_desc=(P.Ident hd.lid_str); exp_par=false } in
          let cp = { P.exp_loc=x.P.exp_loc; exp_desc=(P.Couple (Comma,x,id)); exp_par=false } in
          mk_tuple cp tl
    in
    let hd = Nlist.hd xlst in
    let tuple = mk_tuple { P.exp_loc=hd.lid_loc; exp_desc=(P.Ident hd.lid_str); exp_par=false } (Nlist.tl xlst) in
    let ttuple = type_expression_exn cl env ctx tuple in
    let te = type_expression_exn cl env ctx e in
    let () = match get_stype (Global.get_alias env) te.T.exp_typ ttuple.T.exp_typ with
      | None -> unexpected_type_exn e.P.exp_loc te.T.exp_typ ttuple.T.exp_typ
      | Some _ -> ()
    in
    let tlst = Nlist.map (type_writable_var_exn cl env ctx) xlst in
    mk_subst s0.P.sub_loc (T.Affectation (T.Tuple tlst,te))

  | P.Affectation (P.Function(ff,nlst),e) ->
    let _ = type_writable_var_exn cl env ctx ff in
    let rec mk_app (lc:Utils.loc) f = function
      | [] -> f
      | x::tl -> mk_app lc { P.exp_loc=lc; exp_desc=(P.Application (f,x)); exp_par=false } tl
    in
    let lhs = mk_app ff.lid_loc
        { P.exp_loc=ff.lid_loc; exp_desc=(P.Ident ff.lid_str); exp_par=false} (Nlist.to_list nlst)
    in
    let tlhs = type_expression_exn cl env ctx lhs in
    let te = type_expression_exn cl env ctx e in
    let () = match get_stype (Global.get_alias env) te.T.exp_typ tlhs.T.exp_typ with
      | None -> unexpected_type_exn e.P.exp_loc te.T.exp_typ tlhs.T.exp_typ
      | Some _ -> ()
    in
    let tf = type_writable_var_exn cl env ctx ff in
    let tlst = Nlist.map (type_expression_exn cl env ctx) nlst in
    mk_subst s0.P.sub_loc (T.Affectation (T.Function(tf,tlst),te))

  | P.Affectation (P.Record(rc,fd),e) ->
    let rf_access =
      { P.exp_loc=rc.lid_loc;
        exp_par=false;
        exp_desc=(P.Record_Field_Access ({ P.exp_loc=rc.lid_loc; exp_desc=(P.Ident rc.lid_str); exp_par=false},fd)) }
       in
    let rc = type_writable_var_exn cl env ctx rc in
    let trf_access = type_expression_exn cl env ctx rf_access in
    let te = type_expression_exn cl env ctx e in
    let () = match get_stype (Global.get_alias env) te.T.exp_typ trf_access.T.exp_typ with
      | None -> unexpected_type_exn e.P.exp_loc te.T.exp_typ trf_access.T.exp_typ 
      | Some _ -> ()
    in
    mk_subst s0.P.sub_loc (T.Affectation (T.Record(rc,fd),te))

  | P.Choice slst ->
   mk_subst s0.P.sub_loc (T.Choice (Nlist.map (type_substitution_exn cl env ctx) slst))

  | P.IfThenElse (pslst,s_else) ->
    let aux (p,s) =
      (type_predicate_exn cl env ctx p,
       type_substitution_exn cl env ctx s)
    in
    let tps = Nlist.map aux pslst in
    let t_else = match s_else with
      | None -> None
      | Some s -> Some (type_substitution_exn cl env ctx s)
    in
    mk_subst s0.P.sub_loc (T.IfThenElse (tps,t_else))

  | P.Select (pslst,s_else) ->
    let aux (p,s) =
      (type_predicate_exn cl env ctx p,
       type_substitution_exn cl env ctx s)
    in
    let tps = Nlist.map aux pslst in
    let t_else = match s_else with
      | None -> None
      | Some s -> Some (type_substitution_exn cl env ctx s)
    in
    mk_subst s0.P.sub_loc (T.Select (tps,t_else))

  | P.Case (e,nlst,c_else) ->
    let te = type_expression_exn cl env ctx e in
    let aux (lst,s) =
      let aux elt =
        let telt = type_expression_exn cl env ctx elt in
        match get_stype (Global.get_alias env) te.T.exp_typ telt.T.exp_typ with
        | None -> unexpected_type_exn telt.T.exp_loc telt.T.exp_typ te.T.exp_typ 
        | Some _ -> telt
      in
      let ts = type_substitution_exn cl env ctx s in
      (Nlist.map aux lst,ts)
    in
    let t_else = match c_else with
      | None -> None
      | Some s -> Some (type_substitution_exn cl env ctx s)
    in
    mk_subst s0.P.sub_loc (T.Case(te,Nlist.map aux nlst,t_else))

  | P.Any (ids,p,s) ->
    let (ctx,tids) = declare_nelist ctx ids Local.L_Subst_Binder in
    let tp = type_predicate_exn cl env ctx p in
    let ts = type_substitution_exn cl env ctx s in
    mk_subst s0.P.sub_loc (T.Any (tids,tp,ts))

  | P.Let (ids,nlst,s) ->
    let (ctx,tids) = declare_nelist ctx ids Local.L_Subst_Binder in
    let aux (v,e:lident*P.expression) =
      let te = type_expression_exn cl env ctx e in
      match Local.get ctx v.lid_str with
      | None -> Error.raise_exn v.lid_loc ("Unknown symbol '"^v.lid_str^"'.")
      | Some (ty_exp,_) ->
        begin match get_stype (Global.get_alias env) te.T.exp_typ ty_exp with
          | None -> unexpected_type_exn e.P.exp_loc te.T.exp_typ ty_exp
          | Some bv_typ -> ({T.bv_loc=v.lid_loc;bv_typ;bv_id=v.lid_str},te)
        end
    in
    let nlst = Nlist.map aux nlst in
    let s = type_substitution_exn cl env ctx s in
    mk_subst s0.P.sub_loc (T.Let (tids,nlst,s))

  | P.BecomesElt (xlst,e) ->
    let rec mk_tuple (x:lident) : lident list -> P.expression = function
      | [] -> { P.exp_loc=x.lid_loc; exp_desc=(P.Ident x.lid_str); exp_par=false }
      | hd::tl -> { P.exp_loc=x.lid_loc;
                    exp_par=false;
                    exp_desc=(P.Couple (Comma,
                                        { P.exp_loc=x.lid_loc; exp_desc=(P.Ident x.lid_str); exp_par=false},
                                        mk_tuple hd tl)) }
    in
    let tuple = mk_tuple (Nlist.hd xlst) (Nlist.tl xlst) in
    let ttuple = type_expression_exn cl env ctx tuple in
    let ty_exp = mk_Power ttuple.T.exp_typ in
    let te = type_expression_exn cl env ctx e in
    let () = match get_stype (Global.get_alias env) te.T.exp_typ ty_exp with
      | None -> unexpected_type_exn e.P.exp_loc te.T.exp_typ ty_exp
      | Some _ -> ()
    in
    let tlst = (Nlist.map (type_writable_var_exn cl env ctx) xlst) in
    mk_subst s0.P.sub_loc (T.BecomesElt (tlst,te))

  | P.BecomesSuch (xlst,p) ->
    let tlst = Nlist.map (type_writable_var_exn cl env ctx) xlst in
    let p = type_predicate_exn cl env ctx p in
    mk_subst s0.P.sub_loc (T.BecomesSuch (tlst,p))

  | P.Var (vars,s) ->
    let (ctx,tvars) = declare_nelist ctx vars Local.L_Subst_Binder in
    let s = type_substitution_exn cl env ctx s in
    mk_subst s0.P.sub_loc (T.Var(tvars,s))

  | P.CallUp (ids,op,params) ->
    begin match Global.get_operation env op.lid_str with
      | None -> Error.raise_exn op.lid_loc ("Unknown operation '"^op.lid_str^"'.")
      | Some infos ->
        begin match to_op_source infos.Global.op_readonly infos.Global.op_src with
          | None -> Error.raise_exn op.lid_loc ("The operation '"^op.lid_str^"' is not visible.")
          | Some op_src ->
            let op = { T.op_id = op.lid_str; op_loc = op.lid_loc; op_src } in
            let tids = List.map (type_writable_var_exn cl env ctx) ids in
            let tparams = List.map (type_expression_exn cl env ctx) params in
            let aux_in te (_,ty_arg:_*Btype.t) =
              let ty_exp = ( ty_arg :> Btype.Open.t) in
              match get_stype (Global.get_alias env) te.T.exp_typ ty_exp with
              | None -> unexpected_type_exn te.T.exp_loc te.T.exp_typ ty_exp
              | Some _ -> ()
            in
            let aux_out tmv (_,ty_arg:_*Btype.t) =
              let ty_exp = ( ty_arg :> Btype.Open.t) in
              match get_stype (Global.get_alias env) tmv.T.mv_typ ty_exp with
              | None -> unexpected_type_exn tmv.T.mv_loc tmv.T.mv_typ ty_exp
              | Some _ -> ()
            in
            begin try
                let () = List.iter2 aux_out tids infos.Global.op_args_out in
                let () = List.iter2 aux_in tparams infos.Global.op_args_in in
                mk_subst s0.P.sub_loc (T.CallUp (tids,op,tparams)) 
              with Invalid_argument _ ->
                Error.raise_exn op.T.op_loc ("Incorrect number of in/out parameters.")
            end
        end
    end

  | P.While (p,s,inv,var) ->
    let tp = type_predicate_exn cl env ctx p in
    let ts = type_substitution_exn cl env ctx s in
    let t_inv = type_predicate_exn (V.mk_assert_clause cl) env ctx inv in
    let t_var = type_expression_exn (V.mk_assert_clause cl) env ctx var in
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
