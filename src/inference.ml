open Utils
open Syntax

module Local :
sig
  type t
  val create : unit -> t
  val add : t -> ident -> Btype.Open.t -> bool -> t
  val get : t -> ident -> (Btype.Open.t*bool) option
  val get_vars : t -> ident list
end = struct

  module M = Map.Make( (*FIXME*)
    struct
      type t = string
      let compare = String.compare
    end )

  type t = (Btype.Open.t*bool) M.t

  let create () = M.empty

  let add (ctx:t) (id:ident) (ty:Btype.Open.t) (ro:bool) : t =
    M.add id (ty,ro) ctx

  let get (ctx:t) (id:ident) : (Btype.Open.t*bool) option =
    try Some (M.find id ctx)
    with Not_found -> None

  let get_vars ctx = List.map fst (M.bindings ctx)

end

type t_var0 = (loc,Btype.Open.t) var 
type t_expression0 = (loc,Btype.Open.t) expression
type t_predicate0 = (loc,Btype.Open.t) predicate
type t_substitution0 = (loc,Btype.Open.t) substitution

type t_var = (loc,Btype.t) var 
type t_expression = (loc,Btype.t) expression
type t_predicate = (loc,Btype.t) predicate
type t_substitution = (loc,Btype.t) substitution

let mk_expr exp_loc exp_typ exp_desc = { exp_loc; exp_typ; exp_desc }
let mk_pred prd_loc prd_desc = { prd_loc; prd_desc }
let mk_subst sub_loc sub_desc = { sub_loc; sub_desc;  }

let declare (ctx:Local.t) (id:string) (ro:bool) : Local.t * Btype.Open.t = 
  let mt = Btype.Open.new_meta () in
  ( Local.add ctx id mt ro, mt )

let declare_list (ctx:Local.t) (lst:p_var list) (ro:bool) : Local.t * t_var0 list =
  let (ctx,tvars) = List.fold_left
      (fun (ctx,tvars) v ->
         let (ctx,var_typ) = declare ctx v.var_id ro in
         ( ctx, { var_loc=v.var_loc; var_typ; var_id=v.var_id }::tvars )
      ) (ctx,[]) lst
  in
  (ctx,List.rev tvars)

let declare_nelist (ctx:Local.t) (xlst:p_var Nlist.t) (ro:bool) : Local.t * t_var0 Nlist.t =
  let (ctx,lst) = declare_list ctx (Nlist.to_list xlst) ro in
  (ctx,Nlist.from_list_exn lst)

let ids_to_product (ctx:Local.t) (xlst:p_var Nlist.t) : Btype.Open.t =
  let aux pr v =
    match Local.get ctx v.var_id with
    | None -> assert false
    | Some (ty,_) -> Btype.Open.mk_Product pr ty
  in
  match Local.get ctx (Nlist.hd xlst).var_id with
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
  
let get_ident_type (cl:Global.t_clause) (env:Global.t) (ctx:Local.t) (lc:loc) (id:ident) : Btype.Open.t Error.t_result =
  match Local.get ctx id with
  | Some (ty,_) -> Ok ty
  | None ->
    begin match Global.get_symbol_type_in_clause env lc id cl with
      | Ok ty -> Ok (ty :> Btype.Open.t)
      | Error _ as err -> err
    end

let get_writable_ident_type (cl:Global.t_clause) (env:Global.t) (ctx:Local.t) (lc:loc) (id:ident) : Btype.Open.t Error.t_result =
  match Local.get ctx id with
  | Some (ty,false) -> Ok ty
  | Some (ty,_) -> Error { Error.err_loc=lc; err_txt=("The variable '"^id^"' is read-only") }
  | None ->
    begin match Global.get_writable_symbol_type_in_clause env lc id cl with
      | Ok ty -> Ok (ty :> Btype.Open.t)
      | Error _ as err -> err
    end

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

let is_int_or_power_exn l (arg:t_expression0) : t_int_or_power =
  let open Btype.Open in
  match weak_norm arg.exp_typ with
  | T_Product (t1,t2) as ty ->
    begin match t1 with
      | T_Int -> C_Int
      | T_Power _ -> C_Power
      | T_UVar _ ->
        begin match weak_norm t2 with
          | T_Int -> C_Int
          | T_Power _ -> C_Power
          | T_UVar _ -> Error.raise_exn l "Cannot decide from this is an operation on integers or sets."
          | _ -> Error.raise_exn arg.exp_loc
                   ("This expression has type '"^ to_string ty^
                    "' but an expression of type INTEGER*INTEGER or POW(_)*POW(_) was expected.")
        end
      | _ -> Error.raise_exn arg.exp_loc
            ("This expression has type '"^ to_string ty^
             "' but an expression of type INTEGER*INTEGER or POW(_)*POW(_) was expected.")
    end
  | ty -> Error.raise_exn arg.exp_loc
            ("This expression has type '"^ to_string ty^
             "' but an expression of product type was expected.")

let type_set_product_exn (app_lc:Utils.loc) (op_lc:Utils.loc) (env:Global.t) (arg:t_expression0): t_expression0 =
  let open Btype.Open in
  let mt1 = new_meta () in
  let mt2 = new_meta () in
  let op_ty_exp = type_of_binary_fun (mk_Power mt1) (mk_Power mt2) (mk_Power (mk_Product mt1 mt2)) in
  let op_ty_inf = type_of_unary_fun arg.exp_typ (new_meta ()) in
  match get_stype (Global.get_alias env) op_ty_exp op_ty_inf with
  | None -> unexpected_type_exn op_lc op_ty_inf op_ty_exp
  | Some _ ->
    let op = mk_expr op_lc op_ty_exp (Builtin Product) in
    mk_expr app_lc (mk_Power (mk_Product mt1 mt2)) (Application (op,arg))

let type_int_product_exn (app_lc:Utils.loc) (op_lc:Utils.loc) (env:Global.t) (arg:t_expression0) : t_expression0 =
  let open Btype.Open in
  let op_ty_exp = type_of_binary_fun t_int t_int t_int in
  let op_ty_inf = type_of_unary_fun arg.exp_typ (new_meta ()) in
  match get_stype (Global.get_alias env) op_ty_inf op_ty_exp with
  | None -> unexpected_type_exn op_lc op_ty_inf op_ty_exp
  | Some _ ->
    let op = mk_expr op_lc op_ty_exp (Builtin Product) in
    mk_expr app_lc t_int (Application (op,arg))

let type_int_difference_exn (app_lc:Utils.loc) (op_lc:Utils.loc) (env:Global.t) (arg:t_expression0) : t_expression0 =
  let open Btype.Open in
  let op_ty_exp = type_of_binary_fun t_int t_int t_int in
  let op_ty_inf = type_of_unary_fun arg.exp_typ (new_meta ()) in
  match get_stype (Global.get_alias env) op_ty_inf op_ty_exp with
  | None -> unexpected_type_exn op_lc op_ty_inf op_ty_exp
  | Some _ ->
    let op = mk_expr op_lc op_ty_exp (Builtin Difference) in
    mk_expr app_lc t_int (Application (op,arg))

let type_set_difference_exn (app_lc:Utils.loc) (op_lc:Utils.loc) (env:Global.t) (arg:t_expression0) : t_expression0 =
  let open Btype.Open in
  let mt = new_meta () in
  let op_ty_exp = type_of_binary_fun (mk_Power mt) (mk_Power mt) (mk_Power mt) in
  let op_ty_inf = type_of_unary_fun arg.exp_typ (new_meta ()) in
  match get_stype (Global.get_alias env) op_ty_exp op_ty_inf with
  | None -> unexpected_type_exn op_lc op_ty_inf op_ty_exp
  | Some _ ->
    let op = mk_expr op_lc op_ty_exp (Builtin Difference) in
    mk_expr app_lc (mk_Power mt) (Application (op,arg))

let rec type_expression_exn (cl:Global.t_clause) (env:Global.t) (ctx:Local.t) (e:p_expression) : t_expression0 =
  let open Btype.Open in
  match e.exp_desc with

  | Ident id | Dollar id as d ->
    begin match get_ident_type cl env ctx e.exp_loc id with
      | Ok ty -> mk_expr e.exp_loc ty d
      | Error err -> raise (Error.Error err)
    end

  | Builtin bi as d -> mk_expr e.exp_loc (get_builtin_type_exn e.exp_loc bi) d

  | Pbool p ->
    let tp = type_predicate_exn cl env ctx p in
    mk_expr e.exp_loc t_bool (Pbool tp)

  | Application (e1,e2) ->
    begin
      match e1.exp_desc with
      | Builtin Product ->
        let te2 = type_expression_exn cl env ctx e2 in
        begin match is_int_or_power_exn e1.exp_loc te2 with
          | C_Int -> type_int_product_exn e.exp_loc e1.exp_loc env te2
          | C_Power -> type_set_product_exn e.exp_loc e1.exp_loc env te2
        end
      | Builtin Difference ->
        let te2 = type_expression_exn cl env ctx e2 in
        begin match is_int_or_power_exn e1.exp_loc te2 with
          | C_Int -> type_int_difference_exn e.exp_loc e1.exp_loc env te2
          | C_Power -> type_set_difference_exn e.exp_loc e1.exp_loc env te2
        end
      | _ ->
        let te1 = type_expression_exn cl env ctx e1 in
        let ty_fun_exp = type_of_unary_fun (new_meta ()) (new_meta ()) in
        begin match get_stype (Global.get_alias env) te1.exp_typ ty_fun_exp with
          | Some (T_Power (T_Product (a,b))) ->
            let te2 = type_expression_exn cl env ctx e2 in
            ( match get_stype (Global.get_alias env) te2.exp_typ a with
              | Some _ -> mk_expr e.exp_loc b (Application (te1,te2))
              | None -> unexpected_type_exn e2.exp_loc te2.exp_typ a )
          | None -> unexpected_type_exn e1.exp_loc te1.exp_typ ty_fun_exp
          | _ -> assert false
        end
    end

  | Couple (cm,e1,e2) ->
    let te1 = type_expression_exn cl env ctx e1 in
    let te2 = type_expression_exn cl env ctx e2 in
    mk_expr e.exp_loc (mk_Product te1.exp_typ te2.exp_typ) (Couple (cm,te1,te2))

  | Sequence nlst ->
    begin
      let te = type_expression_exn cl env ctx (Nlist.hd nlst) in
      let aux (elt:p_expression) : t_expression0 =
        let t_elt = type_expression_exn cl env ctx elt in
        match get_stype (Global.get_alias env) t_elt.exp_typ te.exp_typ with
        | Some ty -> t_elt
        | None -> unexpected_type_exn elt.exp_loc t_elt.exp_typ te.exp_typ
      in
      let tlst = List.map aux (Nlist.tl nlst) in
      mk_expr e.exp_loc (mk_Power (mk_Product t_int te.exp_typ)) (Sequence (Nlist.make te tlst))
    end

  | Extension nlst ->
    begin
      let te0 = type_expression_exn cl env ctx (Nlist.hd nlst) in
      let aux (elt:p_expression) : t_expression0 =
        let t_elt = type_expression_exn cl env ctx elt in
        match get_stype (Global.get_alias env) t_elt.exp_typ te0.exp_typ with
        | Some _ -> t_elt
        | None -> unexpected_type_exn elt.exp_loc t_elt.exp_typ te0.exp_typ
      in
      let tlst = List.map aux (Nlist.tl nlst) in
      mk_expr e.exp_loc (mk_Power te0.exp_typ) (Extension (Nlist.make te0 tlst))
    end

  | Comprehension (ids,p) ->
    let (ctx,tids) = declare_nelist ctx ids true in
    let tp = type_predicate_exn cl env ctx p in
    mk_expr e.exp_loc (mk_Power (ids_to_product ctx ids)) (Comprehension (tids,tp))

  | Binder (bi,ids,p,e0) ->
    begin
      match bi with
      | Sum | Prod ->
        let (ctx,tids) = declare_nelist ctx ids true in
        let tp = type_predicate_exn cl env ctx p in
        let te = type_expression_exn cl env ctx e0 in
        begin match get_stype (Global.get_alias env) te.exp_typ t_int with
          | Some _ -> mk_expr e.exp_loc t_int (Binder (bi,tids,tp,te)) 
          | None -> unexpected_type_exn e0.exp_loc te.exp_typ t_int
        end
      | Q_Union | Q_Intersection ->
        let (ctx,tids) = declare_nelist ctx ids true in
        let tp = type_predicate_exn cl env ctx p in
        let te = type_expression_exn cl env ctx e0 in
        let ty_exp = mk_Power (new_meta ()) in
        begin match get_stype (Global.get_alias env) te.exp_typ ty_exp with
          | Some ty -> mk_expr e.exp_loc ty (Binder (bi,tids,tp,te))
          | None -> unexpected_type_exn e0.exp_loc te.exp_typ ty_exp
        end
      | Lambda ->
        begin
          let (ctx,tids) = declare_nelist ctx ids true in
          let tp = type_predicate_exn cl env ctx p in
          let te = type_expression_exn cl env ctx e0 in
          mk_expr e.exp_loc (mk_Power (mk_Product (ids_to_product ctx ids) te.exp_typ))
            (Binder (bi,tids,tp,te))
        end
    end

  | Record nlst ->
    let aux (id,e) = (id,type_expression_exn cl env ctx e) in
    let tnlst = Nlist.map aux nlst in
    let ty = mk_Record (Nlist.to_list (Nlist.map (fun (id,e) -> (id.lid_str,e.exp_typ)) tnlst)) in
    mk_expr e.exp_loc ty (Record tnlst)

  | Record_Type nlst ->
    let aux (id,e) = (id,type_expression_exn cl env ctx e) in
    let get_type (id,te) =
      let ty_exp = mk_Power (new_meta ()) in
      match get_stype (Global.get_alias env) te.exp_typ ty_exp with
      | Some (T_Power ty) -> (id.lid_str,ty)
      | None -> unexpected_type_exn e.exp_loc te.exp_typ ty_exp
      | _ -> assert false
    in
    let tlst = Nlist.map aux nlst in
    let ty = mk_Power (mk_Record (Nlist.to_list (Nlist.map get_type tlst))) in
    mk_expr e.exp_loc ty (Record_Type tlst)

  | Record_Field_Access (e0,fd) ->
    let te = type_expression_exn cl env ctx e0 in
    begin match weak_norm te.exp_typ with
      | T_Record lst as ty ->
        begin
          let aux (s,_) = String.equal s fd.lid_str in
          try mk_expr e.exp_loc (snd (List.find aux lst)) (Record_Field_Access (te,fd))
          with Not_found ->
            Error.raise_exn fd.lid_loc
              ("The field '"^fd.lid_str ^"' does not belong to record type '"
               ^to_string ty^"'.")
        end
      | ty -> Error.raise_exn e.exp_loc
                ("This expression has type '" ^to_string ty
                 ^"' but is expected to be a record.")
    end

and type_predicate_exn (cl:Global.t_clause) (env:Global.t) (ctx:Local.t) (p:p_predicate) : t_predicate0 =
  let open Btype.Open in
  match p.prd_desc with
  | P_Builtin _ as d -> mk_pred p.prd_loc d
  | Binary_Prop (op,p1,p2) ->
    let tp1 = type_predicate_exn cl env ctx p1 in
    let tp2 = type_predicate_exn cl env ctx p2 in
    mk_pred p.prd_loc (Binary_Prop (op,tp1,tp2))
  | Negation p -> mk_pred p.prd_loc (Negation (type_predicate_exn cl env ctx p))
  | Binary_Pred (bop,e1,e2) ->
    begin
      match bop with
      | Equality | Disequality ->
        begin
          let te1 = type_expression_exn cl env ctx e1 in
          let te2 = type_expression_exn cl env ctx e2 in
          match get_stype (Global.get_alias env) te1.exp_typ te2.exp_typ with
          | Some _ -> mk_pred p.prd_loc (Binary_Pred (bop,te1,te2))
          | None -> unexpected_type_exn e2.exp_loc te1.exp_typ te2.exp_typ
        end
      | Membership | Non_Membership ->
        begin
          let te1 = type_expression_exn cl env ctx e1 in
          let te2 = type_expression_exn cl env ctx e2 in
          begin
            match get_stype (Global.get_alias env) (mk_Power te1.exp_typ) te2.exp_typ with
            | Some _ -> mk_pred p.prd_loc (Binary_Pred (bop,te1,te2))
            | None -> unexpected_type_exn e2.exp_loc te2.exp_typ (mk_Power te1.exp_typ)
          end
        end
      | Inclusion _ ->
        begin
          let ty0 = mk_Power (new_meta ()) in
          let te1 = type_expression_exn cl env ctx e1 in
          let te2 = type_expression_exn cl env ctx e2 in
          begin match get_stype (Global.get_alias env) te1.exp_typ ty0 with
            | Some ty1_bis ->
              begin match get_stype (Global.get_alias env) ty1_bis te2.exp_typ with
                | Some _ -> mk_pred p.prd_loc (Binary_Pred (bop,te1,te2))
                | None -> unexpected_type_exn e2.exp_loc te2.exp_typ ty1_bis
              end
            | None -> unexpected_type_exn e1.exp_loc te1.exp_typ ty0
          end
        end
      | Inequality _ ->
        begin
          let te1 = type_expression_exn cl env ctx e1 in
          let te2 = type_expression_exn cl env ctx e2 in
          begin match get_stype (Global.get_alias env) te1.exp_typ t_int with
            | Some _ ->
              begin match get_stype (Global.get_alias env) te2.exp_typ t_int with
                | Some _ -> mk_pred p.prd_loc (Binary_Pred (bop,te1,te2))
                | None -> unexpected_type_exn e2.exp_loc te2.exp_typ t_int
              end
            | None -> unexpected_type_exn e1.exp_loc te1.exp_typ t_int
          end
        end
    end

  | Universal_Q (ids,p) ->
    let (ctx,tids) = declare_nelist ctx ids true in
    mk_pred p.prd_loc (Universal_Q (tids,type_predicate_exn cl env ctx p))

  | Existential_Q (ids,p) ->
    let (ctx,tids) = declare_nelist ctx ids true in
    mk_pred p.prd_loc (Existential_Q (tids,type_predicate_exn cl env ctx p))

let type_var_exn (cl:Global.t_clause) (env:Global.t) (ctx:Local.t) (v:p_var) : t_var0 =
  match get_ident_type cl env ctx v.var_loc v.var_id with
  | Ok var_typ -> { var_loc=v.var_loc; var_id=v.var_id; var_typ }
  | Error err -> raise (Error.Error err)

let type_writable_var_exn (cl:Global.t_clause) (env:Global.t) (ctx:Local.t) (v:p_var) : t_var0 =
  match get_writable_ident_type cl env ctx v.var_loc v.var_id with
  | Ok var_typ -> { var_loc=v.var_loc; var_id=v.var_id; var_typ }
  | Error err -> raise (Error.Error err)

let rec type_substitution_exn (cl:Global.t_clause) (env:Global.t) (ctx:Local.t) (s0:p_substitution) : t_substitution0 =
  let open Btype.Open in
  match s0.sub_desc with
  | Skip -> mk_subst s0.sub_loc Skip

  | Pre (p,s) ->
    let p = type_predicate_exn cl env ctx p in
    let s = type_substitution_exn cl env ctx s in
    mk_subst s0.sub_loc (Pre (p,s))

  | Assert (p,s) ->
    let p = type_predicate_exn Global.C_Assert_Or_While_Invariant env ctx p in
    let s =  type_substitution_exn cl env ctx s in
    mk_subst s0.sub_loc (Assert(p,s))

  | Affectation (xlst,e) ->
    let rec mk_tuple (x:p_expression) : p_var list -> p_expression = function
        | [] -> x
        | hd::tl ->
          let id = mk_expr hd.var_loc () (Ident hd.var_id) in
          let cp = mk_expr x.exp_loc () (Couple (Comma false,x,id)) in
          mk_tuple cp tl
    in
    let hd = Nlist.hd xlst in
    let tuple = mk_tuple (mk_expr hd.var_loc () (Ident hd.var_id)) (Nlist.tl xlst) in
    let ttuple = type_expression_exn cl env ctx tuple in
    let te = type_expression_exn cl env ctx e in
    let () = match get_stype (Global.get_alias env) te.exp_typ ttuple.exp_typ with
      | None -> unexpected_type_exn e.exp_loc te.exp_typ ttuple.exp_typ
      | Some _ -> ()
    in
    let tlst = Nlist.map (type_writable_var_exn cl env ctx) xlst in
    mk_subst s0.sub_loc (Affectation (tlst,te))

  | Function_Affectation (f,nlst,e) ->
    let _ = type_writable_var_exn cl env ctx f in
    let rec mk_app (lc:Utils.loc) f = function
      | [] -> f
      | x::tl -> mk_app lc (mk_expr lc () (Application (f,x))) tl
    in
    let lhs = mk_app f.var_loc (mk_expr f.var_loc () (Ident f.var_id)) (Nlist.to_list nlst) in
    let tlhs = type_expression_exn cl env ctx lhs in
    let te = type_expression_exn cl env ctx e in
    let () = match get_stype (Global.get_alias env) te.exp_typ tlhs.exp_typ with
      | None -> unexpected_type_exn e.exp_loc te.exp_typ tlhs.exp_typ
      | Some _ -> ()
    in
    let tf = type_var_exn cl env ctx f in
    let tlst = Nlist.map (type_expression_exn cl env ctx) nlst in
    mk_subst s0.sub_loc (Function_Affectation (tf,tlst,te))

  | Record_Affectation (rc,fd,e) ->
    let _ = type_writable_var_exn cl env ctx rc in
    let rc = type_var_exn cl env ctx rc in
    let rf_access = mk_expr rc.var_loc ()
        (Record_Field_Access (mk_expr rc.var_loc () (Ident rc.var_id),fd)) in
    let trf_access = type_expression_exn cl env ctx rf_access in
    let te = type_expression_exn cl env ctx e in
    let () = match get_stype (Global.get_alias env) te.exp_typ trf_access.exp_typ with
      | None -> unexpected_type_exn e.exp_loc te.exp_typ trf_access.exp_typ 
      | Some _ -> ()
    in
    mk_subst s0.sub_loc (Record_Affectation (rc,fd,te))

  | Choice slst ->
   mk_subst s0.sub_loc (Choice (Nlist.map (type_substitution_exn cl env ctx) slst))

  | IfThenElse (pslst,s_else) ->
    let aux (p,s) =
      (type_predicate_exn cl env ctx p, type_substitution_exn cl env ctx s)
    in
    let tps = Nlist.map aux pslst in
    let t_else = match s_else with
      | None -> None
      | Some s -> Some (type_substitution_exn cl env ctx s)
    in
    mk_subst s0.sub_loc (IfThenElse (tps,t_else))

  | Select (pslst,s_else) ->
    let aux (p,s) =
      (type_predicate_exn cl env ctx p, type_substitution_exn cl env ctx s)
    in
    let tps = Nlist.map aux pslst in
    let t_else = match s_else with
      | None -> None
      | Some s -> Some (type_substitution_exn cl env ctx s)
    in
    mk_subst s0.sub_loc (Select (tps,t_else))

  | Case (e,nlst,c_else) ->
    let te = type_expression_exn cl env ctx e in
    let aux (lst,s) =
      let aux elt =
        let telt = type_expression_exn cl env ctx elt in
        match get_stype (Global.get_alias env) te.exp_typ telt.exp_typ with
        | None -> unexpected_type_exn telt.exp_loc telt.exp_typ te.exp_typ 
        | Some _ -> telt
      in
      let ts = type_substitution_exn cl env ctx s in
      (Nlist.map aux lst,ts)
    in
    let t_else = match c_else with
      | None -> None
      | Some s -> Some (type_substitution_exn cl env ctx s)
    in
    mk_subst s0.sub_loc (Case(te,Nlist.map aux nlst,t_else))

  | Any (ids,p,s) ->
    let (ctx,tids) = declare_nelist ctx ids false in
    let tp = type_predicate_exn cl env ctx p in
    let ts = type_substitution_exn cl env ctx s in
    mk_subst s0.sub_loc (Any (tids,tp,ts))

  | Let (ids,nlst,s) ->
    let (ctx,tids) = declare_nelist ctx ids false in
    let aux (v,e) =
      let te = type_expression_exn cl env ctx e in
      match Local.get ctx v.var_id with
      | None -> Error.raise_exn v.var_loc ("Unknown symbol '"^v.var_id^"'.")
      | Some (ty_exp,_) ->
        begin match get_stype (Global.get_alias env) te.exp_typ ty_exp with
          | None -> unexpected_type_exn e.exp_loc te.exp_typ ty_exp
          | Some var_typ -> ({var_loc=v.var_loc;var_typ;var_id=v.var_id},te)
        end
    in
    let nlst = Nlist.map aux nlst in
    let s = type_substitution_exn cl env ctx s in
    mk_subst s0.sub_loc (Let (tids,nlst,s))

  | BecomesElt (xlst,e) ->
    let rec mk_tuple (x:p_var) : p_var list -> p_expression = function
      | [] -> mk_expr x.var_loc () (Ident x.var_id)
      | hd::tl -> mk_expr x.var_loc () (Couple (Comma false,mk_expr x.var_loc () (Ident x.var_id),mk_tuple hd tl))
    in
    let tuple = mk_tuple (Nlist.hd xlst) (Nlist.tl xlst) in
    let ttuple = type_expression_exn cl env ctx tuple in
    let ty_exp = mk_Power ttuple.exp_typ in
    let te = type_expression_exn cl env ctx e in
    let () = match get_stype (Global.get_alias env) te.exp_typ ty_exp with
      | None -> unexpected_type_exn e.exp_loc te.exp_typ ty_exp
      | Some _ -> ()
    in
    let tlst = (Nlist.map (type_writable_var_exn cl env ctx) xlst) in
    mk_subst s0.sub_loc (BecomesElt (tlst,te))

  | BecomesSuch (xlst,p) ->
    let tlst = Nlist.map (type_writable_var_exn cl env ctx) xlst in
    let p = type_predicate_exn cl env ctx p in
    mk_subst s0.sub_loc (BecomesSuch (tlst,p))

  | Var (vars,s) ->
    let (ctx,tvars) = declare_nelist ctx vars false in
    let s = type_substitution_exn cl env ctx s in
    mk_subst s0.sub_loc (Var(tvars,s))

  | CallUp (ids,op,params) ->
    begin match Global.get_operation_type env op.lid_loc op.lid_str with
      | Error err -> raise (Error.Error err)
      | Ok args ->
        let tids = List.map (type_writable_var_exn cl env ctx) ids in
        let tparams = List.map (type_expression_exn cl env ctx) params in
        let aux te (_,ty_arg:_*Btype.t) =
          let ty_exp = ( ty_arg :> Btype.Open.t) in
          match get_stype (Global.get_alias env) te.exp_typ ty_exp with
          | None -> unexpected_type_exn te.exp_loc te.exp_typ ty_exp
          | Some _ -> ()
        in
        begin try
            let () = List.iter2 (fun v -> aux (mk_expr v.var_loc v.var_typ (Ident v.var_id))) tids args.Global.args_out in
            let () = List.iter2 aux tparams args.Global.args_in in
            mk_subst s0.sub_loc (CallUp (tids,op,tparams))
          with Invalid_argument _ ->
            Error.raise_exn op.lid_loc ("Incorrect number of in/out parameters.")
        end
    end

  | While (p,s,inv,var) ->
    let tp = type_predicate_exn cl env ctx p in
    let ts = type_substitution_exn cl env ctx s in
    let t_inv = type_predicate_exn Global.C_Assert_Or_While_Invariant env ctx inv in
    let t_var = type_expression_exn Global.C_Assert_Or_While_Invariant env ctx var in
    let exp = t_int in
    let () = match get_stype (Global.get_alias env) t_var.exp_typ exp with
      | None -> unexpected_type_exn var.exp_loc t_var.exp_typ exp
      | Some _ -> ()
    in
    mk_subst s0.sub_loc (While (tp,ts,t_inv,t_var))

  | Sequencement (s1,s2) ->
    let s1 = type_substitution_exn cl env ctx s1 in
    let s2 = type_substitution_exn cl env ctx s2 in
    mk_subst s0.sub_loc (Sequencement(s1,s2))

  | Parallel (s1,s2) ->
    let s1 = type_substitution_exn cl env ctx s1 in
    let s2 = type_substitution_exn cl env ctx s2 in
    mk_subst s0.sub_loc (Parallel(s1,s2))

let close_exn (lc:loc) (ty:Btype.Open.t) : Btype.t =
  match Btype.close ty with
  | None ->
    Error.raise_exn lc
      ("The type of this expression could not be fully infered. The type infered so far is '"^
       Btype.Open.to_string ty^"'.")
  | Some ty -> ty

let close_var (v:t_var0) : t_var =
  match Btype.close v.var_typ with
  | None -> Error.raise_exn v.var_loc 
      ("The type of symbol '"^v.var_id^
       "' could not be fully infered. The type infered so far is '"^
       Btype.Open.to_string v.var_typ^"'.")
  | Some var_typ -> { var_loc=v.var_loc; var_id=v.var_id; var_typ }

let close_var_nlist = Nlist.map close_var

let mk_expr exp_loc exp_typ exp_desc =
  let exp_typ = close_exn exp_loc exp_typ in
  { exp_loc; exp_typ ; exp_desc }

let mk_pred prd_loc prd_desc = { prd_loc; prd_desc }
let mk_subst sub_loc sub_desc = { sub_loc; sub_desc;  }

let rec close_expr (e:t_expression0) : t_expression =
  match e.exp_desc with
  | Ident _ | Dollar _ | Builtin _ as d ->
    mk_expr e.exp_loc e.exp_typ d
  | Pbool p ->
    mk_expr e.exp_loc e.exp_typ (Pbool (close_pred p))
  | Application (e1,e2) ->
    mk_expr e.exp_loc e.exp_typ (Application (close_expr e1,close_expr e2))
  | Couple (cm,e1,e2) ->
    mk_expr e.exp_loc e.exp_typ (Couple (cm,close_expr e1,close_expr e2))
  | Sequence nlst ->
    mk_expr e.exp_loc e.exp_typ (Sequence (Nlist.map close_expr nlst))
  | Extension nlst ->
    mk_expr e.exp_loc e.exp_typ (Extension (Nlist.map close_expr nlst))
  | Comprehension (xlst,p) ->
    mk_expr e.exp_loc e.exp_typ (Comprehension (close_var_nlist xlst,close_pred p))
  | Binder (bi,xlst,p,e0) ->
    mk_expr e.exp_loc e.exp_typ (Binder (bi,close_var_nlist xlst,close_pred p,close_expr e0))
  | Record_Field_Access (e0,id) ->
    mk_expr e.exp_loc e.exp_typ (Record_Field_Access (close_expr e0,id))
  | Record nlst ->
    let aux (id,e) = (id,close_expr e) in
    mk_expr e.exp_loc e.exp_typ (Record(Nlist.map aux nlst))
  | Record_Type nlst ->
    let aux (id,e) = (id,close_expr e) in
    mk_expr e.exp_loc e.exp_typ (Record_Type(Nlist.map aux nlst))

and close_pred (p:t_predicate0) : t_predicate =
  match p.prd_desc with
  | P_Builtin _ as d -> mk_pred p.prd_loc d
  | Binary_Prop (bop,p1,p2) -> mk_pred p.prd_loc (Binary_Prop (bop,close_pred p1,close_pred p2))
  | Binary_Pred (bop,e1,e2) -> mk_pred p.prd_loc (Binary_Pred (bop,close_expr e1,close_expr e2))
  | Negation p0 -> mk_pred p.prd_loc (Negation (close_pred p0))
  | Universal_Q (xlst,p0) -> mk_pred p.prd_loc (Universal_Q (close_var_nlist xlst,close_pred p0)) 
  | Existential_Q (xlst,p0) -> mk_pred p.prd_loc (Existential_Q (close_var_nlist xlst,close_pred p0)) 

let rec close_subst (s:t_substitution0) : t_substitution =
  match s.sub_desc with
  | Skip ->
    mk_subst s.sub_loc Skip
  | Affectation (xlst,e) ->
    mk_subst s.sub_loc (Affectation(close_var_nlist xlst,close_expr e))
  | Function_Affectation (v,nlst,e) ->
    mk_subst s.sub_loc (Function_Affectation (close_var v,Nlist.map close_expr nlst,close_expr e))
  | Record_Affectation (v,id,e) ->
    mk_subst s.sub_loc (Record_Affectation (close_var v,id,close_expr e))
  | Pre (p,s0) -> mk_subst s.sub_loc (Pre(close_pred p,close_subst s0))
  | Assert (p,s0) -> mk_subst s.sub_loc (Assert(close_pred p,close_subst s0))
  | Choice nlst -> mk_subst s.sub_loc (Choice(Nlist.map close_subst nlst))
  | IfThenElse (nlst,opt) ->
    let aux (p,s) = (close_pred p,close_subst s) in
    let topt = match opt with
      | None -> None
      | Some s0 -> Some (close_subst s0)
    in
    mk_subst s.sub_loc (IfThenElse (Nlist.map aux nlst,topt))
  | Select (nlst,opt) ->
    let aux (p,s) = (close_pred p,close_subst s) in
    let topt = match opt with
      | None -> None
      | Some s0 -> Some (close_subst s0)
    in
    mk_subst s.sub_loc (Select (Nlist.map aux nlst,topt))
  | Case (e,nlst,opt) -> 
    let aux (lst,s) = (Nlist.map close_expr lst,close_subst s) in
    let topt = match opt with
      | None -> None
      | Some s0 -> Some (close_subst s0)
    in
    mk_subst s.sub_loc (Case (close_expr e,Nlist.map aux nlst,topt))
  | Any (xlst,p,s0) ->
    mk_subst s.sub_loc (Any (close_var_nlist xlst,close_pred p,close_subst s0))
  | Let (xlst,nlst,s0) ->
    let aux (v,e) = (close_var v,close_expr e) in
    mk_subst s.sub_loc (Let (close_var_nlist xlst,Nlist.map aux nlst,close_subst s0))
  | BecomesElt (xlst,e) -> mk_subst s.sub_loc (BecomesElt (close_var_nlist xlst,close_expr e))
  | BecomesSuch (xlst,p) -> mk_subst s.sub_loc (BecomesSuch (close_var_nlist xlst,close_pred p))
  | Var (xlst,s0) -> mk_subst s.sub_loc (Var (close_var_nlist xlst,close_subst s0))
  | CallUp (args_out,id,args_in) ->
    mk_subst s.sub_loc (CallUp (List.map close_var args_out,id,List.map close_expr args_in))
  | While (p1,s0,p2,e) -> mk_subst s.sub_loc (While (close_pred p1,close_subst s0,close_pred p2,close_expr e))
  | Sequencement (s1,s2) -> mk_subst s.sub_loc (Sequencement (close_subst s1,close_subst s2))
  | Parallel (s1,s2) -> mk_subst s.sub_loc (Parallel (close_subst s1,close_subst s2))

let type_expression (cl:Global.t_clause) (env:Global.t) (ctx:Local.t) (e:p_expression) : t_expression Error.t_result =
  try Ok (close_expr (type_expression_exn cl env ctx e))
  with Error.Error err -> Error err

let type_predicate (cl:Global.t_clause) (env:Global.t) (ctx:Local.t) (p:p_predicate) : t_predicate Error.t_result =
  try Ok (close_pred (type_predicate_exn cl env ctx p))
  with Error.Error err -> Error err

let type_substitution (cl:Global.t_clause) (env:Global.t) (ctx:Local.t) (s:p_substitution) : t_substitution Error.t_result =
  try Ok (close_subst (type_substitution_exn cl env ctx s))
  with Error.Error err -> Error err
