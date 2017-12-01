open Utils
open Syntax

module Local :
sig
  type t
  val create : unit -> t
  val add : t -> ident -> Btype_mt.t -> bool -> t
  val get : t -> ident -> (Btype_mt.t*bool) option
  val get_vars : t -> ident list
end = struct

  module M = Map.Make(
    struct
      type t = string
      let compare = String.compare
    end )

  type t = (Btype_mt.t*bool) M.t

  let create () = M.empty

  let add (ctx:t) (id:ident) (ty:Btype_mt.t) (ro:bool) : t =
    M.add id (ty,ro) ctx

  let get (ctx:t) (id:ident) : (Btype_mt.t*bool) option =
    try Some (M.find id ctx)
    with Not_found -> None

  let get_vars ctx = List.map fst (M.bindings ctx)

end

type env = { gl: Global.t; uf:Unif.t; cl:Global.t_clause }

type t_var0 = (loc,Btype_mt.t) var 
type t_expression0 = (loc,Btype_mt.t) expression
type t_predicate0 = (loc,Btype_mt.t) predicate
type t_substitution0 = (loc,Btype_mt.t) substitution

type t_var = (loc,Btype.t) var 
type t_expression = (loc,Btype.t) expression
type t_predicate = (loc,Btype.t) predicate
type t_substitution = (loc,Btype.t) substitution

let mk_expr exp_loc exp_typ exp_desc = { exp_loc; exp_typ; exp_desc }
let mk_pred prd_loc prd_desc = { prd_loc; prd_desc }
let mk_subst sub_loc sub_desc = { sub_loc; sub_desc;  }

let declare (uf:Unif.t) (ctx:Local.t) (id:string) (ro:bool) : Local.t * Btype_mt.t = 
  let mt = Unif.new_meta uf in
  ( Local.add ctx id mt ro, mt )

let declare_list (uf:Unif.t) (ctx:Local.t) (lst:p_var list) (ro:bool) : Local.t * t_var0 list =
  let (ctx,tvars) = List.fold_left
      (fun (ctx,tvars) v ->
         let (ctx,var_typ) = declare uf ctx v.var_id ro in
         ( ctx, { var_loc=v.var_loc; var_typ; var_id=v.var_id }::tvars )
      ) (ctx,[]) lst
  in
  (ctx,List.rev tvars)

let declare_nelist (uf:Unif.t) (ctx:Local.t) (xlst:p_var Nlist.t) (ro:bool) : Local.t * t_var0 Nlist.t =
  let (ctx,lst) = declare_list uf ctx (Nlist.to_list xlst) ro in
  (ctx,Nlist.from_list_exn lst)

let ids_to_product (ctx:Local.t) (xlst:p_var Nlist.t) : Btype_mt.t =
  let aux pr v =
    match Local.get ctx v.var_id with
    | None -> assert false
    | Some (ty,_) -> Btype_mt.T_Product (pr,ty)
  in
  match Local.get ctx (Nlist.hd xlst).var_id with
  | None -> assert false
  | Some (ty,_) -> List.fold_left aux ty (Nlist.tl xlst)

let get_builtin_type_exn (uf:Unif.t) (lc:Utils.loc) (e:e_builtin) : Btype_mt.t =
  let open Btype_mt in
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
    | Max | Min  -> type_of_unary_fun (T_Power t_int) t_int
    (* Types *)
    | NATURAL | NATURAL1 | INT | NAT | NAT1 | INTEGER  -> T_Power t_int
    | STRINGS  -> T_Power t_string
    | BOOLEANS  -> T_Power t_bool
    (* Empty set/sequence *)
    | Empty_Set -> T_Power (Unif.new_meta uf)
    | Empty_Seq -> type_of_unary_fun t_int (Unif.new_meta uf)
    (* Arithmetic or Set operator *)
    | Product  -> assert false
    | Difference -> assert false
    (* Operations on sets *)
    | Interval  -> type_of_binary_fun t_int t_int (T_Power t_int)
    | Intersection | Union  ->
      let t_set = T_Power (Unif.new_meta uf) in
      type_of_binary_fun t_set t_set t_set
    | First_Projection ->
      let mt1 = Unif.new_meta uf in
      let mt2 = Unif.new_meta uf in
      type_of_binary_fun (T_Power mt1) (T_Power mt2)
        (type_of_unary_fun (T_Product (mt1,mt2)) mt1)
    | Second_Projection ->
      let mt1 = Unif.new_meta uf in
      let mt2 = Unif.new_meta uf in
      type_of_binary_fun (T_Power mt1) (T_Power mt2)
        (type_of_unary_fun (T_Product (mt1,mt2)) mt2)
    | Parallel_Product ->
      let mt1 = Unif.new_meta uf in
      let mt2 = Unif.new_meta uf in
      let mt3 = Unif.new_meta uf in
      let mt4 = Unif.new_meta uf in
      type_of_binary_fun (type_of_unary_fun mt1 mt2) (type_of_unary_fun mt3 mt4)
        (T_Power (T_Product (T_Product (mt1,mt3),T_Product (mt2,mt4))))
    | Direct_Product ->
      let mt1 = Unif.new_meta uf in
      let mt2 = Unif.new_meta uf in
      let mt3 = Unif.new_meta uf in
      type_of_binary_fun (type_of_unary_fun mt1 mt2) (type_of_unary_fun mt1 mt3)
        (T_Power (T_Product (mt1,T_Product (mt2,mt3))))
    | Cardinal  -> type_of_unary_fun (T_Power (Unif.new_meta uf)) t_int
    | Power_Set _ ->
      let t_set = T_Power (Unif.new_meta uf) in
      type_of_unary_fun t_set (T_Power t_set)
    | G_Intersection | G_Union  ->
      let t_set = T_Power (Unif.new_meta uf) in
      type_of_unary_fun (T_Power t_set) t_set
    (* Operations on relations *)
    | Composition ->
      let ty1 = Unif.new_meta uf in
      let ty2 = Unif.new_meta uf in
      let ty3 = Unif.new_meta uf in
      type_of_binary_fun (type_of_unary_fun ty1 ty2) (type_of_unary_fun ty2 ty3) (type_of_unary_fun ty1 ty3)
    | Iteration ->
      let mt = Unif.new_meta uf in
      type_of_binary_fun (type_of_unary_fun mt mt) t_int (type_of_unary_fun mt mt)
    | Image  ->
      let t_arg = Unif.new_meta uf in
      let t_res = Unif.new_meta uf in
      type_of_binary_fun (type_of_unary_fun t_arg t_res) (T_Power t_arg) (T_Power t_res)
    | Domain_Restriction
    | Domain_Soustraction ->
      let mt1 = Unif.new_meta uf in
      let mt2 = Unif.new_meta uf in
      let ty_rel = type_of_unary_fun mt1 mt2 in
      let ty_dom = T_Power mt1 in
      type_of_binary_fun ty_dom ty_rel ty_rel
    | Codomain_Restriction
    | Codomain_Soustraction ->
      let mt1 = Unif.new_meta uf in
      let mt2 = Unif.new_meta uf in
      let ty_rel = type_of_unary_fun mt1 mt2 in
      let ty_ran = T_Power mt2 in
      type_of_binary_fun ty_rel ty_ran ty_rel
    | Surcharge  ->
      let ty_f = type_of_unary_fun (Unif.new_meta uf) (Unif.new_meta uf) in
      type_of_binary_fun ty_f ty_f ty_f
    | Relations | Functions _ ->
      let mt1 = Unif.new_meta uf in
      let mt2 = Unif.new_meta uf in
      type_of_binary_fun (T_Power mt1) (T_Power mt2) (T_Power (type_of_unary_fun mt1 mt2))
    | Identity_Relation  ->
      let mt = Unif.new_meta uf in
      type_of_unary_fun (T_Power mt) (type_of_unary_fun mt mt)
    | Inverse_Relation  ->
      let mt1 = Unif.new_meta uf in
      let mt2 = Unif.new_meta uf in
      type_of_unary_fun (type_of_unary_fun mt1 mt2) (type_of_unary_fun mt2 mt1)
    | Closure | Transitive_Closure ->
      let mt = Unif.new_meta uf in
      type_of_unary_fun (type_of_unary_fun mt mt) (type_of_unary_fun mt mt)
    | Domain  ->
      let t_arg = Unif.new_meta uf in
      let t_res = Unif.new_meta uf in
      type_of_unary_fun (type_of_unary_fun t_arg t_res) (T_Power t_arg)
    | Range  ->
      let t_arg = Unif.new_meta uf in
      let t_res = Unif.new_meta uf in
      type_of_unary_fun (type_of_unary_fun t_arg t_res) (T_Power t_res)
    | Fnc  ->
      let t_arg = Unif.new_meta uf in
      let t_res = Unif.new_meta uf in
      T_Power(type_of_unary_fun t_arg t_res)
    | Rel  ->
      let t_arg = Unif.new_meta uf in
      let t_res = Unif.new_meta uf in
      type_of_unary_fun (type_of_unary_fun t_arg (T_Power t_res)) (type_of_unary_fun t_arg t_res)
    (* Sequence operators *)
    | Sequence_Set _ ->
      let mt = Unif.new_meta uf in
      type_of_unary_fun (T_Power mt) (T_Power (type_of_sequence mt))
    | Size  -> type_of_unary_fun (type_of_sequence (Unif.new_meta uf)) t_int 
    | First | Last  ->
      let mt = Unif.new_meta uf in
      type_of_unary_fun (type_of_sequence mt) mt
    | Reverse | Front | Tail ->
      let t_seq = type_of_sequence (Unif.new_meta uf) in
      type_of_unary_fun t_seq t_seq
    | Concatenation ->
      let t_seq = type_of_sequence (Unif.new_meta uf) in
      type_of_binary_fun t_seq t_seq t_seq
    | Head_Insertion ->
      let mt = Unif.new_meta uf in
      let t_seq = type_of_sequence mt in
      type_of_binary_fun mt t_seq t_seq
    | Tail_Insertion ->
      let mt = Unif.new_meta uf in
      let t_seq = type_of_sequence mt in
      type_of_binary_fun t_seq mt t_seq
    | Head_Restriction | Tail_Restriction  ->
      let t_seq = type_of_sequence (Unif.new_meta uf) in
      type_of_binary_fun t_seq t_int t_seq
    | G_Concatenation  ->
      let t_seq = type_of_sequence (Unif.new_meta uf) in
      type_of_unary_fun (type_of_sequence t_seq) t_seq
    | Tree | Btree | Const | Top | Sons | Prefix | Postfix | SizeT | Mirror
    | Rank | Father | Son | Subtree | Arity | Bin | Left | Right | Infix ->
      Error.raise_exn lc "Not implemented (tree operators)."
  
let get_ident_type (env:env) (ctx:Local.t) (lc:loc) (id:ident) : Btype_mt.t Error.t_result =
  match Local.get ctx id with
  | Some (ty,_) -> Ok ty
  | None ->
    begin match Global.get_symbol_type_in_clause env.gl lc id env.cl with
      | Ok ty -> Ok (Btype.to_btype_mt ty)
      | Error _ as err -> err
    end

let get_writable_ident_type (env:env) (ctx:Local.t) (lc:loc) (id:ident) : Btype_mt.t Error.t_result =
  match Local.get ctx id with
  | Some (ty,false) -> Ok ty
  | Some (ty,_) -> Error { Error.err_loc=lc; err_txt=("The variable '"^id^"' is read-only") }
  | None ->
    begin match Global.get_writable_symbol_type_in_clause env.gl lc id env.cl with
      | Ok ty -> Ok (Btype.to_btype_mt ty)
      | Error _ as err -> err
    end

let unexpected_type_exn (uf:Unif.t) (lc:Utils.loc) (inf:Btype_mt.t) (exp:Btype_mt.t) =
  let str = Printf.sprintf
      "This expression has type '%s' but an expression of type '%s' was expected."
      (Btype_mt.to_string (Unif.normalize uf inf)) (Btype_mt.to_string (Unif.normalize uf exp))
  in
  Error.raise_exn lc str

type t_int_or_power = C_Int | C_Power

let is_int_or_power_exn l (uf:Unif.t) (arg:t_expression0) : t_int_or_power =
  let open Btype_mt in
  match Unif.normalize uf arg.exp_typ with
  | T_Product (t1,t2) as ty ->
    begin match t1 with
      | T_Atomic s when String.equal s "INTEGER" -> C_Int
      | T_Power _ -> C_Power
      | T_Meta _ ->
        begin match t2 with
          | T_Atomic s when String.equal s "INTEGER" -> C_Int
          | T_Power _ -> C_Power
          | T_Meta _ -> Error.raise_exn l "Cannot decide from this is an operation on integers or sets."
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

let type_set_product_exn (app_lc:Utils.loc) (op_lc:Utils.loc) (uf:Unif.t) (arg:t_expression0): t_expression0 =
  let open Btype_mt in
  let mt1 = Unif.new_meta uf in
  let mt2 = Unif.new_meta uf in
  let op_ty_exp = type_of_binary_fun (T_Power mt1) (T_Power mt2) (T_Power (T_Product (mt1,mt2))) in
  let op_ty_inf = type_of_unary_fun arg.exp_typ (Unif.new_meta uf) in
  match Unif.get_stype uf op_ty_exp op_ty_inf with
  | None -> unexpected_type_exn uf op_lc op_ty_inf op_ty_exp
  | Some _ ->
    let op = mk_expr op_lc op_ty_exp (Builtin Product) in
    mk_expr app_lc (T_Power (T_Product (mt1,mt2))) (Application (op,arg))

let type_int_product_exn (app_lc:Utils.loc) (op_lc:Utils.loc) (uf:Unif.t) (arg:t_expression0) : t_expression0 =
  let open Btype_mt in
  let op_ty_exp = type_of_binary_fun t_int t_int t_int in
  let op_ty_inf = type_of_unary_fun arg.exp_typ (Unif.new_meta uf) in
  match Unif.get_stype uf op_ty_inf op_ty_exp with
  | None -> unexpected_type_exn uf op_lc op_ty_inf op_ty_exp
  | Some _ ->
    let op = mk_expr op_lc op_ty_exp (Builtin Product) in
    mk_expr app_lc t_int (Application (op,arg))

let type_int_difference_exn (app_lc:Utils.loc) (op_lc:Utils.loc) (uf:Unif.t) (arg:t_expression0) : t_expression0 =
  let open Btype_mt in
  let op_ty_exp = type_of_binary_fun t_int t_int t_int in
  let op_ty_inf = type_of_unary_fun arg.exp_typ (Unif.new_meta uf) in
  match Unif.get_stype uf op_ty_inf op_ty_exp with
  | None -> unexpected_type_exn uf op_lc op_ty_inf op_ty_exp
  | Some _ ->
    let op = mk_expr op_lc op_ty_exp (Builtin Difference) in
    mk_expr app_lc t_int (Application (op,arg))

let type_set_difference_exn (app_lc:Utils.loc) (op_lc:Utils.loc) (uf:Unif.t) (arg:t_expression0) : t_expression0 =
  let open Btype_mt in
  let mt = Unif.new_meta uf in
  let op_ty_exp = type_of_binary_fun (T_Power mt) (T_Power mt) (T_Power mt) in
  let op_ty_inf = type_of_unary_fun arg.exp_typ (Unif.new_meta uf) in
  match Unif.get_stype uf op_ty_exp op_ty_inf with
  | None -> unexpected_type_exn uf op_lc op_ty_inf op_ty_exp
  | Some _ ->
    let op = mk_expr op_lc op_ty_exp (Builtin Difference) in
    mk_expr app_lc (T_Power mt) (Application (op,arg))

let rec type_expression_exn (env:env) (ctx:Local.t) (e:p_expression) : t_expression0 =
  let open Btype_mt in
  match e.exp_desc with

  | Ident id | Dollar id as d ->
    begin match get_ident_type env ctx e.exp_loc id with
      | Ok ty -> mk_expr e.exp_loc ty d
      | Error err -> raise (Error.Error err)
    end

  | Builtin bi as d -> mk_expr e.exp_loc (get_builtin_type_exn env.uf e.exp_loc bi) d

  | Pbool p ->
    let tp = type_predicate_exn env ctx p in
    mk_expr e.exp_loc t_bool (Pbool tp)

  | Application (e1,e2) ->
    begin
      match e1.exp_desc with
      | Builtin Product ->
        let te2 = type_expression_exn env ctx e2 in
        begin match is_int_or_power_exn e1.exp_loc env.uf te2 with
          | C_Int -> type_int_product_exn e.exp_loc e1.exp_loc env.uf te2
          | C_Power -> type_set_product_exn e.exp_loc e1.exp_loc env.uf te2
        end
      | Builtin Difference ->
        let te2 = type_expression_exn env ctx e2 in
        begin match is_int_or_power_exn e1.exp_loc env.uf te2 with
          | C_Int -> type_int_difference_exn e.exp_loc e1.exp_loc env.uf te2
          | C_Power -> type_set_difference_exn e.exp_loc e1.exp_loc env.uf te2
        end
      | _ ->
        let te1 = type_expression_exn env ctx e1 in
        let ty_fun_exp = type_of_unary_fun (Unif.new_meta env.uf) (Unif.new_meta env.uf) in
        begin match Unif.get_stype env.uf te1.exp_typ ty_fun_exp with
          | Some (T_Power (T_Product (a,b))) ->
            let te2 = type_expression_exn env ctx e2 in
            ( match Unif.get_stype env.uf te2.exp_typ a with
              | Some _ -> mk_expr e.exp_loc b (Application (te1,te2))
              | None -> unexpected_type_exn env.uf e2.exp_loc te2.exp_typ a )
          | _ -> unexpected_type_exn env.uf e1.exp_loc te1.exp_typ ty_fun_exp
        end
    end

  | Couple (cm,e1,e2) ->
    let te1 = type_expression_exn env ctx e1 in
    let te2 = type_expression_exn env ctx e2 in
    mk_expr e.exp_loc (T_Product (te1.exp_typ,te2.exp_typ)) (Couple (cm,te1,te2))

  | Sequence nlst ->
    begin
      let te = type_expression_exn env ctx (Nlist.hd nlst) in
      let aux (elt:p_expression) : t_expression0 =
        let t_elt = type_expression_exn env ctx elt in
        match Unif.get_stype env.uf t_elt.exp_typ te.exp_typ with
        | Some ty -> t_elt
        | None -> unexpected_type_exn env.uf elt.exp_loc t_elt.exp_typ te.exp_typ
      in
      let tlst = List.map aux (Nlist.tl nlst) in
      mk_expr e.exp_loc (T_Power (T_Product (t_int,te.exp_typ))) (Sequence (Nlist.make te tlst))
    end

  | Extension nlst ->
    begin
      let te0 = type_expression_exn env ctx (Nlist.hd nlst) in
      let aux (elt:p_expression) : t_expression0 =
        let t_elt = type_expression_exn env ctx elt in
        match Unif.get_stype env.uf t_elt.exp_typ te0.exp_typ with
        | Some _ -> t_elt
        | None -> unexpected_type_exn env.uf elt.exp_loc t_elt.exp_typ te0.exp_typ
      in
      let tlst = List.map aux (Nlist.tl nlst) in
      mk_expr e.exp_loc (T_Power te0.exp_typ) (Extension (Nlist.make te0 tlst))
    end

  | Comprehension (ids,p) ->
    let (ctx,tids) = declare_nelist env.uf ctx ids true in
    let tp = type_predicate_exn env ctx p in
    mk_expr e.exp_loc (T_Power (ids_to_product ctx ids)) (Comprehension (tids,tp))

  | Binder (bi,ids,p,e0) ->
    begin
      match bi with
      | Sum | Prod ->
        let (ctx,tids) = declare_nelist env.uf ctx ids true in
        let tp = type_predicate_exn env ctx p in
        let te = type_expression_exn env ctx e0 in
        begin match Unif.get_stype env.uf te.exp_typ t_int with
          | Some _ -> mk_expr e.exp_loc t_int (Binder (bi,tids,tp,te)) 
          | None -> unexpected_type_exn env.uf e0.exp_loc te.exp_typ t_int
        end
      | Q_Union | Q_Intersection ->
        let (ctx,tids) = declare_nelist env.uf ctx ids true in
        let tp = type_predicate_exn env ctx p in
        let te = type_expression_exn env ctx e0 in
        let ty_exp = T_Power (Unif.new_meta env.uf) in
        begin match Unif.get_stype env.uf te.exp_typ ty_exp with
          | Some ty -> mk_expr e.exp_loc ty (Binder (bi,tids,tp,te))
          | _ -> unexpected_type_exn env.uf e0.exp_loc te.exp_typ ty_exp
        end
      | Lambda ->
        begin
          let (ctx,tids) = declare_nelist env.uf ctx ids true in
          let tp = type_predicate_exn env ctx p in
          let te = type_expression_exn env ctx e0 in
          mk_expr e.exp_loc (T_Power (T_Product (ids_to_product ctx ids,te.exp_typ)))
            (Binder (bi,tids,tp,te))
        end
    end

  | Record nlst ->
    let aux (id,e) = (id,type_expression_exn env ctx e) in
    let tnlst = Nlist.map aux nlst in
    let ty = T_Record (Nlist.to_list (Nlist.map (fun (id,e) -> (id.lid_str,e.exp_typ)) tnlst)) in
    mk_expr e.exp_loc ty (Record tnlst)

  | Record_Type nlst ->
    let aux (id,e) = (id,type_expression_exn env ctx e) in
    let get_type (id,te) =
      let ty_exp = T_Power (Unif.new_meta env.uf) in
      match Unif.get_stype env.uf te.exp_typ ty_exp with
      | Some (T_Power ty) -> (id.lid_str,ty)
      | _ -> unexpected_type_exn env.uf e.exp_loc te.exp_typ ty_exp
    in
    let tlst = Nlist.map aux nlst in
    let ty = T_Power (T_Record (Nlist.to_list (Nlist.map get_type tlst))) in
    mk_expr e.exp_loc ty (Record_Type tlst)

  | Record_Field_Access (e0,fd) ->
    let te = type_expression_exn env ctx e0 in
    begin match Unif.normalize env.uf te.exp_typ with
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

and type_predicate_exn (env:env) (ctx:Local.t) (p:p_predicate) : t_predicate0 =
  let open Btype_mt in
  match p.prd_desc with
  | P_Builtin _ as d -> mk_pred p.prd_loc d
  | Binary_Prop (op,p1,p2) ->
    let tp1 = type_predicate_exn env ctx p1 in
    let tp2 = type_predicate_exn env ctx p2 in
    mk_pred p.prd_loc (Binary_Prop (op,tp1,tp2))
  | Negation p -> mk_pred p.prd_loc (Negation (type_predicate_exn env ctx p))
  | Binary_Pred (bop,e1,e2) ->
    begin
      match bop with
      | Equality | Disequality ->
        begin
          let te1 = type_expression_exn env ctx e1 in
          let te2 = type_expression_exn env ctx e2 in
          match Unif.get_stype env.uf te1.exp_typ te2.exp_typ with
          | Some _ -> mk_pred p.prd_loc (Binary_Pred (bop,te1,te2))
          | None -> unexpected_type_exn env.uf e2.exp_loc te1.exp_typ te2.exp_typ
        end
      | Membership | Non_Membership ->
        begin
          let te1 = type_expression_exn env ctx e1 in
          let te2 = type_expression_exn env ctx e2 in
          begin
            match Unif.get_stype env.uf (T_Power te1.exp_typ) te2.exp_typ with
            | Some _ -> mk_pred p.prd_loc (Binary_Pred (bop,te1,te2))
            | None -> unexpected_type_exn env.uf e2.exp_loc te2.exp_typ (T_Power te1.exp_typ)
          end
        end
      | Inclusion _ ->
        begin
          let ty0 = T_Power (Unif.new_meta env.uf) in
          let te1 = type_expression_exn env ctx e1 in
          let te2 = type_expression_exn env ctx e2 in
          begin match Unif.get_stype env.uf te1.exp_typ ty0 with
            | Some ty1_bis ->
              begin match Unif.get_stype env.uf ty1_bis te2.exp_typ with
                | Some _ -> mk_pred p.prd_loc (Binary_Pred (bop,te1,te2))
                | None -> unexpected_type_exn env.uf e2.exp_loc te2.exp_typ ty1_bis
              end
            | None -> unexpected_type_exn env.uf e1.exp_loc te1.exp_typ ty0
          end
        end
      | Inequality _ ->
        begin
          let te1 = type_expression_exn env ctx e1 in
          let te2 = type_expression_exn env ctx e2 in
          begin match Unif.get_stype env.uf te1.exp_typ t_int with
            | Some _ ->
              begin match Unif.get_stype env.uf te2.exp_typ t_int with
                | Some _ -> mk_pred p.prd_loc (Binary_Pred (bop,te1,te2))
                | None -> unexpected_type_exn env.uf e2.exp_loc te2.exp_typ t_int
              end
            | None -> unexpected_type_exn env.uf e1.exp_loc te1.exp_typ t_int
          end
        end
    end

  | Universal_Q (ids,p) ->
    let (ctx,tids) = declare_nelist env.uf ctx ids true in
    mk_pred p.prd_loc (Universal_Q (tids,type_predicate_exn env ctx p))

  | Existential_Q (ids,p) ->
    let (ctx,tids) = declare_nelist env.uf ctx ids true in
    mk_pred p.prd_loc (Existential_Q (tids,type_predicate_exn env ctx p))

let type_var_exn (env:env) (ctx:Local.t) (v:p_var) : t_var0 =
  match get_ident_type env ctx v.var_loc v.var_id with
  | Ok var_typ -> { var_loc=v.var_loc; var_id=v.var_id; var_typ }
  | Error err -> raise (Error.Error err)

let type_writable_var_exn (env:env) (ctx:Local.t) (v:p_var) : t_var0 =
  match get_writable_ident_type env ctx v.var_loc v.var_id with
  | Ok var_typ -> { var_loc=v.var_loc; var_id=v.var_id; var_typ }
  | Error err -> raise (Error.Error err)

let rec type_substitution_exn (env:env) (ctx:Local.t) (s0:p_substitution) : t_substitution0 =
  match s0.sub_desc with
  | Skip -> mk_subst s0.sub_loc Skip

  | Pre (p,s) ->
    mk_subst s0.sub_loc
      (Pre (type_predicate_exn env ctx p, type_substitution_exn env ctx s))

  | Assert (p,s) ->
    mk_subst s0.sub_loc
      (Assert(type_predicate_exn {env with cl=Global.C_Assert_Or_While_Invariant} ctx p,
              type_substitution_exn env ctx s))

  | Affectation (xlst,e) ->
    let rec mk_tuple (x:p_var) : p_var list -> p_expression = function
        | [] -> mk_expr x.var_loc () (Ident x.var_id)
        | hd::tl ->
          mk_expr x.var_loc ()
            (Couple (Comma false,mk_expr x.var_loc () (Ident x.var_id),mk_tuple hd tl))
    in
    let tuple = mk_tuple (Nlist.hd xlst) (Nlist.tl xlst) in
    let ttuple = type_expression_exn env ctx tuple in
    let te = type_expression_exn env ctx e in
    let () = match Unif.get_stype env.uf te.exp_typ ttuple.exp_typ with
      | None -> unexpected_type_exn env.uf e.exp_loc te.exp_typ ttuple.exp_typ
      | Some _ -> ()
    in
    let tlst = Nlist.map (type_writable_var_exn env ctx) xlst in
    mk_subst s0.sub_loc (Affectation (tlst,te))

  | Function_Affectation (f,nlst,e) ->
    let _ = type_writable_var_exn env ctx f in
    let rec mk_app (lc:Utils.loc) f = function
      | [] -> f
      | x::tl -> mk_app lc (mk_expr lc () (Application (f,x))) tl
    in
    let lhs = mk_app f.var_loc (mk_expr f.var_loc () (Ident f.var_id)) (Nlist.to_list nlst) in
    let tlhs = type_expression_exn env ctx lhs in
    let te = type_expression_exn env ctx e in
    let () = match Unif.get_stype env.uf te.exp_typ tlhs.exp_typ with
      | None -> unexpected_type_exn env.uf e.exp_loc te.exp_typ tlhs.exp_typ
      | Some _ -> ()
    in
    let tf = type_var_exn env ctx f in
    let tlst = Nlist.map (type_expression_exn env ctx) nlst in
    mk_subst s0.sub_loc (Function_Affectation (tf,tlst,te))

  | Record_Affectation (rc,fd,e) ->
    let _ = type_writable_var_exn env ctx rc in
    let rf_access = mk_expr rc.var_loc ()
        (Record_Field_Access (mk_expr rc.var_loc () (Ident rc.var_id),fd)) in
    let trf_access = type_expression_exn env ctx rf_access in
    let te = type_expression_exn env ctx e in
    let () = match Unif.get_stype env.uf te.exp_typ trf_access.exp_typ with
      | None -> unexpected_type_exn env.uf e.exp_loc te.exp_typ trf_access.exp_typ 
      | Some _ -> ()
    in
    mk_subst s0.sub_loc (Record_Affectation (type_var_exn env ctx rc,fd,te))

  | Choice slst ->
   mk_subst s0.sub_loc (Choice (Nlist.map (type_substitution_exn env ctx) slst))

  | IfThenElse (pslst,s_else) ->
    let aux (p,s) =
      (type_predicate_exn env ctx p, type_substitution_exn env ctx s)
    in
    let tps = Nlist.map aux pslst in
    let t_else = match s_else with
      | None -> None
      | Some s -> Some (type_substitution_exn env ctx s)
    in
    mk_subst s0.sub_loc (IfThenElse (tps,t_else))

  | Select (pslst,s_else) ->
    let aux (p,s) =
      (type_predicate_exn env ctx p, type_substitution_exn env ctx s)
    in
    let tps = Nlist.map aux pslst in
    let t_else = match s_else with
      | None -> None
      | Some s -> Some (type_substitution_exn env ctx s)
    in
    mk_subst s0.sub_loc (Select (tps,t_else))

  | Case (e,nlst,c_else) ->
    let te = type_expression_exn env ctx e in
    let aux (elt,s) =
      let telt =  type_expression_exn env ctx elt in
      match Unif.get_stype env.uf te.exp_typ telt.exp_typ with
      | None -> unexpected_type_exn env.uf e.exp_loc telt.exp_typ te.exp_typ 
      | Some _ -> (telt,type_substitution_exn env ctx s)
    in
    let t_else = match c_else with
      | None -> None
      | Some s -> Some (type_substitution_exn env ctx s)
    in
    mk_subst s0.sub_loc (Case(te,Nlist.map aux nlst,t_else))

  | Any (ids,p,s) ->
    let (ctx,tids) = declare_nelist env.uf ctx ids false in
    let tp = type_predicate_exn env ctx p in
    let ts = type_substitution_exn env ctx s in
    mk_subst s0.sub_loc (Any (tids,tp,ts))

  | Let (ids,nlst,s) ->
    let (ctx,tids) = declare_nelist env.uf ctx ids false in
    let aux (v,e) =
      let te = type_expression_exn env ctx e in
      match Local.get ctx v.var_id with
      | None -> Error.raise_exn v.var_loc ("Unknown symbol '"^v.var_id^"'.")
      | Some (ty_exp,_) ->
        begin match Unif.get_stype env.uf te.exp_typ ty_exp with
          | None -> unexpected_type_exn env.uf e.exp_loc te.exp_typ ty_exp
          | Some var_typ -> ({var_loc=v.var_loc;var_typ;var_id=v.var_id},te)
        end
    in
    mk_subst s0.sub_loc (Let (tids,Nlist.map aux nlst,type_substitution_exn env ctx s))

  | BecomesElt (xlst,e) ->
    let rec mk_tuple (x:p_var) : p_var list -> p_expression = function
      | [] -> mk_expr x.var_loc () (Ident x.var_id)
      | hd::tl -> mk_expr x.var_loc () (Couple (Comma false,mk_expr x.var_loc () (Ident x.var_id),mk_tuple hd tl))
    in
    let tuple = mk_tuple (Nlist.hd xlst) (Nlist.tl xlst) in
    let ttuple = type_expression_exn env ctx tuple in
    let ty_exp = Btype_mt.T_Power ttuple.exp_typ in
    let te = type_expression_exn env ctx e in
    let () = match Unif.get_stype env.uf te.exp_typ ty_exp with
      | None -> unexpected_type_exn env.uf e.exp_loc te.exp_typ ty_exp
      | Some _ -> ()
    in
    let tlst = (Nlist.map (type_writable_var_exn env ctx) xlst) in
    mk_subst s0.sub_loc (BecomesElt (tlst,te))

  | BecomesSuch (xlst,p) ->
    let tlst = Nlist.map (type_writable_var_exn env ctx) xlst in
    mk_subst s0.sub_loc (BecomesSuch (tlst,type_predicate_exn env ctx p))

  | Var (vars,s) ->
    let (ctx,tvars) = declare_nelist env.uf ctx vars false in
    mk_subst s0.sub_loc (Var(tvars,type_substitution_exn env ctx s))

  | CallUp (ids,op,params) ->
    begin match Global.get_operation_type env.gl op.lid_loc op.lid_str with
      | Error err -> raise (Error.Error err)
      | Ok args ->
        let tids = List.map (type_writable_var_exn env ctx) ids in
        let tparams = List.map (type_expression_exn env ctx) params in
        let aux te (_,ty_arg) =
          let ty_exp = Btype.to_btype_mt ty_arg in
          match Unif.get_stype env.uf te.exp_typ ty_exp with
          | None -> unexpected_type_exn env.uf te.exp_loc te.exp_typ ty_exp
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
    let tp = type_predicate_exn env ctx p in
    let ts = type_substitution_exn env ctx s in
    let t_inv = type_predicate_exn { env with cl=Global.C_Assert_Or_While_Invariant } ctx inv in
    let t_var = type_expression_exn { env with cl=Global.C_Assert_Or_While_Invariant } ctx var in
    let exp = Btype_mt.t_int in
    let () = match Unif.get_stype env.uf t_var.exp_typ exp with
      | None -> unexpected_type_exn env.uf var.exp_loc t_var.exp_typ exp
      | Some _ -> ()
    in
    mk_subst s0.sub_loc (While (tp,ts,t_inv,t_var))

  | Sequencement (s1,s2) ->
    mk_subst s0.sub_loc
      (Sequencement(type_substitution_exn env ctx s1,
                    type_substitution_exn env ctx s2))

  | Parallel (s1,s2) ->
    mk_subst s0.sub_loc
      (Parallel(type_substitution_exn env ctx s1,
                type_substitution_exn env ctx s2))

let close_exn (lc:loc) (uf:Unif.t) (ty:Btype_mt.t) : Btype.t =
  match Btype.from_btype_mt (Unif.normalize uf ty) with
  | None ->
    Error.raise_exn lc
      ("The type of this expression could not be fully infered. The type infered so far is '"^
       Btype_mt.to_string (Unif.normalize uf ty)^"'.")
  | Some ty -> ty

let close_var (uf:Unif.t) (v:t_var0) : t_var =
  match Btype.from_btype_mt (Unif.normalize uf v.var_typ) with
  | None -> Error.raise_exn v.var_loc 
      ("The type of symbol '"^v.var_id^
       "' could not be fully infered. The type infered so far is '"^
       Btype_mt.to_string (Unif.normalize uf v.var_typ)^"'.")
  | Some var_typ -> { var_loc=v.var_loc; var_id=v.var_id; var_typ }

let close_var_nlist ctx = Nlist.map (close_var ctx)

let mk_expr ctx exp_loc exp_typ exp_desc =
  let exp_typ = close_exn exp_loc ctx exp_typ in
  { exp_loc; exp_typ ; exp_desc }

let mk_pred prd_loc prd_desc = { prd_loc; prd_desc }
let mk_subst sub_loc sub_desc = { sub_loc; sub_desc;  }

let rec close_expr (uf:Unif.t) (e:t_expression0) : t_expression =
  match e.exp_desc with
  | Ident _ | Dollar _ | Builtin _ as d ->
    mk_expr uf e.exp_loc e.exp_typ d
  | Pbool p -> mk_expr uf e.exp_loc e.exp_typ (Pbool (close_pred uf p))
  | Application (e1,e2) -> 
    mk_expr uf e.exp_loc e.exp_typ
      (Application (close_expr uf e1,close_expr uf e2))
  | Couple (cm,e1,e2) -> 
    mk_expr uf e.exp_loc e.exp_typ
      (Couple (cm,close_expr uf e1,close_expr uf e2))
  | Sequence nlst ->
    mk_expr uf e.exp_loc e.exp_typ
      (Sequence (Nlist.map (close_expr uf) nlst))
  | Extension nlst ->
    mk_expr uf e.exp_loc e.exp_typ
      (Extension (Nlist.map (close_expr uf) nlst))
  | Comprehension (xlst,p) ->
    mk_expr uf e.exp_loc e.exp_typ
      (Comprehension (close_var_nlist uf xlst,close_pred uf p))
  | Binder (bi,xlst,p,e0) ->
    mk_expr uf e.exp_loc e.exp_typ
      (Binder (bi,close_var_nlist uf xlst,close_pred uf p,close_expr uf e0))
  | Record_Field_Access (e0,id) ->
    mk_expr uf e.exp_loc e.exp_typ
      (Record_Field_Access (close_expr uf e0,id))
  | Record nlst ->
    let aux (id,e) = (id,close_expr uf e) in
    mk_expr uf e.exp_loc e.exp_typ (Record(Nlist.map aux nlst))
  | Record_Type nlst ->
    let aux (id,e) = (id,close_expr uf e) in
    mk_expr uf e.exp_loc e.exp_typ (Record_Type(Nlist.map aux nlst))

and close_pred (uf:Unif.t) (p:t_predicate0) : t_predicate =
  match p.prd_desc with
  | P_Builtin _ as d -> mk_pred p.prd_loc d
  | Binary_Prop (bop,p1,p2) -> mk_pred p.prd_loc (Binary_Prop (bop,close_pred uf p1,close_pred uf p2))
  | Binary_Pred (bop,e1,e2) -> mk_pred p.prd_loc (Binary_Pred (bop,close_expr uf e1,close_expr uf e2))
  | Negation p0 -> mk_pred p.prd_loc (Negation (close_pred uf p0))
  | Universal_Q (xlst,p0) -> mk_pred p.prd_loc (Universal_Q (close_var_nlist uf xlst,close_pred uf p0)) 
  | Existential_Q (xlst,p0) -> mk_pred p.prd_loc (Existential_Q (close_var_nlist uf xlst,close_pred uf p0)) 

let rec close_subst (uf:Unif.t) (s:t_substitution0) : t_substitution =
  match s.sub_desc with
  | Skip -> mk_subst s.sub_loc Skip
  | Affectation (xlst,e) -> mk_subst s.sub_loc (Affectation(close_var_nlist uf xlst,close_expr uf e))
  | Function_Affectation (v,nlst,e) ->
    mk_subst s.sub_loc (Function_Affectation (close_var uf v,Nlist.map (close_expr uf) nlst,close_expr uf e))
  | Record_Affectation (v,id,e) ->
    mk_subst s.sub_loc (Record_Affectation (close_var uf v,id,close_expr uf e))
  | Pre (p,s0) -> mk_subst s.sub_loc (Pre(close_pred uf p,close_subst uf s0))
  | Assert (p,s0) -> mk_subst s.sub_loc (Assert(close_pred uf p,close_subst uf s0))
  | Choice nlst -> mk_subst s.sub_loc (Choice(Nlist.map (close_subst uf) nlst))
  | IfThenElse (nlst,opt) ->
    let aux (p,s) = (close_pred uf p,close_subst uf s) in
    let topt = match opt with
      | None -> None
      | Some s0 -> Some (close_subst uf s0)
    in
    mk_subst s.sub_loc (IfThenElse (Nlist.map aux nlst,topt))
  | Select (nlst,opt) ->
    let aux (p,s) = (close_pred uf p,close_subst uf s) in
    let topt = match opt with
      | None -> None
      | Some s0 -> Some (close_subst uf s0)
    in
    mk_subst s.sub_loc (Select (Nlist.map aux nlst,topt))
  | Case (e,nlst,opt) -> 
    let aux (e,s) = (close_expr uf e,close_subst uf s) in
    let topt = match opt with
      | None -> None
      | Some s0 -> Some (close_subst uf s0)
    in
    mk_subst s.sub_loc (Case (close_expr uf e,Nlist.map aux nlst,topt))
  | Any (xlst,p,s0) ->
    mk_subst s.sub_loc (Any (close_var_nlist uf xlst,close_pred uf p,close_subst uf s0))
  | Let (xlst,nlst,s0) ->
    let aux (v,e) = (close_var uf v,close_expr uf e) in
    mk_subst s.sub_loc (Let (close_var_nlist uf xlst,Nlist.map aux nlst,close_subst uf s0))
  | BecomesElt (xlst,e) -> mk_subst s.sub_loc (BecomesElt (close_var_nlist uf xlst,close_expr uf e))
  | BecomesSuch (xlst,p) -> mk_subst s.sub_loc (BecomesSuch (close_var_nlist uf xlst,close_pred uf p))
  | Var (xlst,s0) -> mk_subst s.sub_loc (Var (close_var_nlist uf xlst,close_subst uf s0))
  | CallUp (args_out,id,args_in) ->
    mk_subst s.sub_loc (CallUp (List.map (close_var uf) args_out,id,List.map (close_expr uf) args_in))
  | While (p1,s0,p2,e) -> mk_subst s.sub_loc (While (close_pred uf p1,close_subst uf s0,close_pred uf p2,close_expr uf e))
  | Sequencement (s1,s2) -> mk_subst s.sub_loc (Sequencement (close_subst uf s1,close_subst uf s2))
  | Parallel (s1,s2) -> mk_subst s.sub_loc (Parallel (close_subst uf s1,close_subst uf s2))

let type_expression (env:env) (ctx:Local.t) (e:p_expression) : t_expression Error.t_result =
  try Ok (close_expr env.uf (type_expression_exn env ctx e))
  with Error.Error err -> Error err

let type_predicate (env:env) (ctx:Local.t) (p:p_predicate) : t_predicate Error.t_result =
  try Ok (close_pred env.uf (type_predicate_exn env ctx p))
  with Error.Error err -> Error err

let type_substitution (env:env) (ctx:Local.t) (s:p_substitution) : t_substitution Error.t_result =
  try Ok (close_subst env.uf (type_substitution_exn env ctx s))
  with Error.Error err -> Error err
