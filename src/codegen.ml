open SyntaxCore
module T = TSyntax
module V = Visibility
module G = Global

module Make (
    Ident:
    sig
      type t
      type t_pkg_id
      val make:string -> t option
      val to_string: t -> string
      val make_pkg_id:string -> t_pkg_id option
    end) =
struct
  type t_id = Ident.t
  type t_pkg_id = Ident.t_pkg_id

  type t_b0_constant =
    | B0_Integer of Int32.t
    | B0_String of string
    | B0_MaxInt
    | B0_MinInt
    | B0_True
    | B0_False

  type t_b0_unary_op =
    | B0_Negation
    | B0_Minus

  type t_b0_binary_op =
    | B0_Conjonction
    | B0_Disjonction
    | B0_Equality
    | B0_Disequality
    | B0_Inequality of SyntaxCore.inequality
    | B0_Product
    | B0_Difference
    | B0_Addition
    | B0_Division
    | B0_Modulo
    | B0_Power

  type qident = { q_nspace:Ident.t_pkg_id option; q_id:Ident.t }

  type t_b0_type =
    | T_Int
    | T_Bool
    | T_String
    | T_Abstract of qident
    | T_Enum of qident
    | T_Array of int*t_b0_type
    | T_Record of (t_id*t_b0_type) list

  type t_ident_kind =
    | IK_Constant of Ident.t_pkg_id option
    | IK_Variable of Ident.t_pkg_id option
    | IK_Enum of Ident.t_pkg_id option

  type t_full_ident_kind =
    | IK_Concrete_Set of Ident.t_pkg_id option
    | IK_Abstract_Set of Ident.t_pkg_id option
    | IK_Other of t_ident_kind

  type t_mut_ident_kind =
    | MIK_Variable
    | MIK_Param
    | MIK_Local

  type t_exp0_desc =
    | B0_Local_Ident of Ident.t*Local.t_local_kind
    | B0_Global_Ident of Ident.t*t_ident_kind
    | B0_Builtin_0 of t_b0_constant
    | B0_Builtin_1 of t_b0_unary_op * t_b0_expr
    | B0_Builtin_2 of t_b0_binary_op * t_b0_expr * t_b0_expr
    | B0_Array_Access of t_b0_expr * t_b0_expr Nlist.t
    | B0_Array of t_b0_expr list
    | B0_Array_Init of t_b0_range Nlist.t*t_b0_expr
    | B0_Record of (t_id*t_b0_expr) list
    | B0_Record_Access of t_b0_expr*t_id
    | B0_Fun_App of qident*t_b0_expr Nlist.t

  and t_b0_range =
    | R_Interval of t_b0_expr*t_b0_expr
    | R_Concrete_Set of int*qident

  and t_b0_expr =
    { exp0_loc: Utils.loc;
      exp0_type: t_b0_type;
      exp0_desc: t_exp0_desc }

  type t_constant_init =
    | Init of t_b0_expr
    | Promoted of Ident.t_pkg_id

  type t_constant =
    { c_name: Ident.t;
      c_loc: Utils.loc;
      c_type: t_b0_type;
      c_init: t_constant_init }

  type t_arg =
    { arg_loc: Utils.loc;
      arg_name: Ident.t;
      arg_type: t_b0_type }

  type t_fun =
    { f_name: t_id;
      f_loc: Utils.loc;
      f_args: t_arg Nlist.t;
      f_ret: t_b0_expr }

  type t_constant_or_fun =
    | Cst of t_constant 
    | Fun of t_fun

  type t_variable =
    { v_name: Ident.t;
      v_loc: Utils.loc;
      v_type: t_b0_type;
      v_promoted_from:Ident.t_pkg_id option }
  
  type t_case =
    | CS_Int of Int32.t
    | CS_Bool of bool
    | CS_Enum of qident

  type t_sub0_desc =
    | B0_Null
    | B0_Affectation of t_b0_lhs*t_b0_expr
    | B0_IfThenElse of (t_b0_expr*t_b0_subst) Nlist.t * t_b0_subst option
    | B0_Case of t_b0_expr * (t_case Nlist.t*t_b0_subst) Nlist.t * t_b0_subst option
    | B0_Var of (Ident.t*t_b0_type) Nlist.t * t_b0_subst
    | B0_While of t_b0_expr * t_b0_subst
    | B0_CallUp of (t_mut_ident_kind*Ident.t) list * qident * t_b0_expr list
    | B0_Sequencement of t_b0_subst * t_b0_subst

  and t_b0_lhs =
    | LHS_Variable of t_id*t_mut_ident_kind
    | LHS_Array of t_id*t_mut_ident_kind*t_b0_expr Nlist.t
    | LHS_Record of t_id*t_mut_ident_kind*t_id

  and t_b0_subst =
    { sub0_loc: Utils.loc;
      sub0_desc: t_sub0_desc }

  type t_procedure_body =
    | Renames of qident
    | Body of t_b0_subst

  type t_procedure =
    { p_name: Ident.t;
      p_is_local: bool;
      p_args_in: t_arg list;
      p_args_out: t_arg list;
      p_body: t_procedure_body }

  type t_type_def =
    | D_Int
    | D_Alias of qident
    | D_Enum of Ident.t list

  type t_type =
    { ty_name: Ident.t;
      ty_def: t_type_def }

  type t_dep_kind = DK_Sees | DK_Imports | DK_Extends

  type t_package =
    { pkg_name: Ident.t_pkg_id;
      pkg_dependencies: (Ident.t_pkg_id*t_dep_kind) list;
      pkg_types: t_type list;
      pkg_constants: t_constant_or_fun list;
      pkg_variables: t_variable list;
      pkg_procedures: t_procedure list;
      pkg_init: t_b0_subst option }


  let prop_bop_to_bop = function
    | Conjonction -> Some B0_Conjonction
    | Disjonction -> Some B0_Disjonction
    | Implication -> None
    | Equivalence -> None

  let pred_bop_to_bop = function
    | Equality -> Some B0_Equality
    | Disequality -> Some B0_Disequality
    | Membership -> None
    | Non_Membership -> None
    | Inclusion _ -> None
    | Inequality ine -> Some (B0_Inequality ine)

  let to_b0_constant = function
    | Integer i -> Some (B0_Integer i)
    | MaxInt -> Some B0_MaxInt
    | MinInt -> Some B0_MinInt
    | TRUE -> Some B0_True
    | FALSE -> Some B0_False
    | String s -> Some (B0_String s)
    | Successor | Predecessor | INTEGER | NATURAL | NATURAL1 | INT | NAT
    | NAT1 | STRINGS | BOOLEANS | Empty_Set | Empty_Seq | Product | Difference
    | Addition | Division | Modulo | Power | Interval | Union | Intersection
    | Relations | First_Projection | Second_Projection | Composition | Direct_Product
    | Parallel_Product | Iteration | Image | Domain_Restriction | Domain_Soustraction
    | Codomain_Restriction | Codomain_Soustraction | Surcharge | Functions _
    | Concatenation | Head_Insertion | Tail_Insertion | Head_Restriction | Tail_Restriction
    | Cardinal | Power_Set  _ | Identity_Relation | Inverse_Relation | Closure
    | Transitive_Closure | Domain | Range | Fnc | Rel | Sequence_Set _ | Size
    | First | Last | Front | Tail | Reverse | G_Union | G_Intersection
    | G_Concatenation | Unary_Minus | Max | Min | Tree | Btree | Const | Top | Sons
    | Prefix | Postfix | SizeT | Mirror | Rank | Father | Son | Subtree | Arity
    | Bin | Left | Right | Infix -> None

  let normalize_app e =
    let rec aux e args =
      match e.T.exp_desc with
      | T.Application (e1,e2) -> aux e1 (e2::args)
      | _ -> (e,args)
    in aux e []

  let get_args e : _ Nlist.t =
    let rec aux lst e =
      match e.T.exp_desc with
      | T.Couple (_,e1,e2) -> aux (e2::lst) e1
      | _ -> e::lst
    in
    Nlist.from_list_exn (aux [] e)

  let is_int ty =
    match Btype.view ty with
    | Btype.T_Int -> true
    | _ -> false
   
  let mk_ident (lc:Utils.loc) (s:string) : Ident.t =
    match Ident.make s with 
    | None -> Error.raise_exn lc ("The symbol '"^s^"' is not a valid identifier.")
    | Some id -> id

  let mk_pkg (lc:Utils.loc) (s:string) : Ident.t_pkg_id =
    match Ident.make_pkg_id s with 
    | None -> Error.raise_exn lc ("The symbol '"^s^"' is not a valid package identifier.")
    | Some id -> id

  let is_valid_range_type rg =
    match Btype.view rg with
    | Btype.T_Int | Btype.T_Concrete_Set _ -> true
    | _ -> false

  let rec to_b0_type (lc:Utils.loc) (ty:Btype.t) : t_b0_type =
    match Btype.view ty with
    | Btype.T_Int -> T_Int
    | Btype.T_Bool -> T_Bool
    | Btype.T_String -> T_String
    | Btype.T_Concrete_Set (Btype.T_Current,s) -> T_Enum { q_nspace=None; q_id=mk_ident lc s }
    | Btype.T_Concrete_Set (Btype.T_Seen m,s) ->
      T_Enum { q_nspace=Some(mk_pkg lc m); q_id=mk_ident lc s }
    | Btype.T_Abstract_Set (Btype.T_Current,s) -> T_Abstract { q_nspace=None; q_id=mk_ident lc s }
    | Btype.T_Abstract_Set (Btype.T_Seen m,s) ->
      T_Abstract { q_nspace=Some(mk_pkg lc m); q_id=mk_ident lc s }
    | Btype.T_Power ty0 ->
      begin match Btype.view ty0 with
        | Btype.T_Product (rg,tg) ->
          let tg = to_b0_type lc tg in
          let dim = get_array_dim lc rg in
          T_Array (dim,tg)
        | _ -> Error.raise_exn lc "Power types are not supported by the translator."
      end
    | Btype.T_Record lst -> T_Record (List.map (fun (id,ty) -> (mk_ident lc id,to_b0_type lc ty)) lst)
    | Btype.T_Product (t1,t2) -> Error.raise_exn lc "Product types are not supported by the translator."

  and get_array_dim lc rg =
    match Btype.view rg with
    | Btype.T_Product (ty1,ty2) ->
      if is_valid_range_type ty2 then
        (get_array_dim lc ty1) + 1
      else Error.raise_exn lc "Only arrays indexed by integers or concrete sets are supported."
    | _ -> 
      if is_valid_range_type rg then 1
      else Error.raise_exn lc "Only arrays indexed by integers or concrete sets are supported."

  let is_func e =
    try
      let _ = to_b0_type e.T.exp_loc e.T.exp_typ in
      false
    with
    Error.Error _ -> true

  let rec to_b0_expr : 'mr 'cl. (('mr,'cl) V.t_global_ident -> t_full_ident_kind) ->
    ('mr,'cl,Btype.t) T.expression -> t_b0_expr = fun f e ->
    let add_lt exp0_desc =
      { exp0_loc = e.T.exp_loc;
        exp0_type = to_b0_type e.T.exp_loc e.T.exp_typ;
        exp0_desc }
    in
    match e.T.exp_desc with
    | T.Ident T.K_Local (id,ki) -> add_lt (B0_Local_Ident(mk_ident e.T.exp_loc id,ki))
    | T.Ident T.K_Global (id,ki) ->
      begin match f ki with
        | IK_Other ki -> add_lt (B0_Global_Ident(mk_ident e.T.exp_loc id,ki))
        | IK_Concrete_Set _ | IK_Abstract_Set _ ->
          Error.raise_exn e.T.exp_loc "This is not a valid B0 expression (Abstract or concrete set)."
      end
    | T.Builtin bi ->
      begin match to_b0_constant bi with
        | None -> Error.raise_exn e.T.exp_loc ("This is not a valid B0 constant.")
        | Some bi -> add_lt (B0_Builtin_0 bi)
      end
    | T.Pbool p -> pred_to_b0_expr f p
    | T.Application (ff,arg) ->
      begin match ff.T.exp_desc with
        | T.Builtin bi ->
          let mk_bin_op op =
            match arg.T.exp_desc with
            | T.Couple (_,arg1,arg2) ->
              let arg1 = to_b0_expr f arg1 in
              let arg2 = to_b0_expr f arg2 in
              add_lt (B0_Builtin_2 (op,arg1,arg2))
            | _ -> Error.raise_exn e.T.exp_loc "This is not a valid B0 expression."
          in
          begin match bi with
            | Successor ->
              add_lt (B0_Builtin_2(B0_Addition,to_b0_expr f arg,add_lt (B0_Builtin_0 (B0_Integer Int32.one))))
            | Predecessor ->
              add_lt (B0_Builtin_2(B0_Difference,to_b0_expr f arg,add_lt (B0_Builtin_0 (B0_Integer Int32.one))))
            | Unary_Minus ->
              add_lt (B0_Builtin_1 (B0_Minus,to_b0_expr f arg))
            | Product ->
              if is_int e.T.exp_typ then mk_bin_op B0_Product
              else add_lt (get_array_init f arg)
            | Difference -> mk_bin_op B0_Difference
            | Addition -> mk_bin_op B0_Addition
            | Division -> mk_bin_op B0_Division
            | Modulo -> mk_bin_op B0_Modulo
            | Power -> mk_bin_op B0_Power
            | _ -> Error.raise_exn e.T.exp_loc "This is not a valid B0 operator."
          end
        | T.Ident (T.K_Global (id_str,ki)) when is_func ff ->
          let rec get_args arg =
            match arg.T.exp_desc with
            | T.Couple (_,a1,a2) -> Nlist.cons (to_b0_expr f a1) (get_args a2)
            | _ -> Nlist.make1 (to_b0_expr f arg)
          in
          let args = get_args arg in
          begin match f ki with
            | IK_Other IK_Constant q_nspace ->
              add_lt (B0_Fun_App({ q_nspace; q_id=mk_ident ff.T.exp_loc id_str },args))
            | IK_Other IK_Enum _ | IK_Other IK_Variable _
            | IK_Concrete_Set _ | IK_Abstract_Set _ ->
              Error.raise_exn e.T.exp_loc "This is not a valid B0 operator."
          end
        | _ ->
          let ff = to_b0_expr f ff in
          let rec get_args arg =
            match arg.T.exp_desc with
            | T.Couple (_,a1,a2) -> Nlist.cons (to_b0_expr f a1) (get_args a2)
            | _ -> Nlist.make1 (to_b0_expr f arg)
          in
          let args = get_args arg in
          add_lt (B0_Array_Access(ff,args))
      end
    | T.Extension nle ->
      let aux i e = match e.T.exp_desc with
        | T.Couple (_,e1,e2) ->
          begin match (to_b0_expr f e1).exp0_desc with
            | B0_Builtin_0 (B0_Integer j) ->
              if (Int32.of_int i) = j then (j,to_b0_expr f e2)
              else Error.raise_exn e1.T.exp_loc "Ill-formed array."
            | _ -> Error.raise_exn e1.T.exp_loc "Ill-formed array."
          end
        | _ -> Error.raise_exn e.T.exp_loc "Ill-formed array."
      in
      let lst = List.mapi aux (Nlist.to_list nle) in
      let lst = List.fast_sort (fun (a,_) (b,_) -> Int32.compare a b) lst in
      let lst = List.map snd lst in
      add_lt (B0_Array lst)
    | T.Record lst ->
      add_lt (B0_Record (List.map (fun (id,e) ->
          (mk_ident id.lid_loc id.lid_str,to_b0_expr f e)) (Nlist.to_list lst)
        ))
    | T.Dollar _ -> Error.raise_exn e.T.exp_loc "This is not a valid B0-expression (Dollar)."
    | T.Couple _ -> Error.raise_exn e.T.exp_loc "This is not a valid B0-expression (Couple)."
    | T.Sequence _ -> Error.raise_exn e.T.exp_loc "This is not a valid B0-expression (Sequence)."
    | T.Comprehension _ -> Error.raise_exn e.T.exp_loc "This is not a valid B0-expression (Comprehension)."
    | T.Binder _ -> Error.raise_exn e.T.exp_loc "This is not a valid B0-expression (Binder)."
    | T.Record_Field_Access (e,fd) ->
      let fd = mk_ident fd.lid_loc fd.lid_str in
      let e = to_b0_expr f e in
      add_lt (B0_Record_Access (e,fd))
    | T.Record_Type _ -> Error.raise_exn e.T.exp_loc "This is not a valid B0-expression (Record type)."

  and get_array_init : 'mr 'cl.(('mr,'cl) V.t_global_ident -> t_full_ident_kind) -> ('mr,'cl,Btype.t) T.expression -> t_exp0_desc = fun f e ->
    match e.T.exp_desc with
    | T.Couple (_,e1,e2) ->
      let v = match e2.T.exp_desc with
        | T.Extension lst ->
          begin match Nlist.to_list lst with
            | [s] -> to_b0_expr f s
            | _ -> Error.raise_exn e.T.exp_loc "Invalid array initialisation (not a singleton)."
          end
        | _ -> Error.raise_exn e.T.exp_loc "Invalid array initialisation (not a singleton)."
      in
      B0_Array_Init (get_array_range f e1,v)
    | _ -> Error.raise_exn e.T.exp_loc "Invalid array initialisation."
  
  and get_array_range : 'mr 'cl. (('mr,'cl) V.t_global_ident -> t_full_ident_kind) -> ('mr,'cl,Btype.t) T.expression -> t_b0_range Nlist.t = fun f e ->
    match e.T.exp_desc with
    | T.Ident (T.K_Global (id,ki)) ->
      let q_id = mk_ident e.T.exp_loc id in
      begin match f ki with
        | IK_Concrete_Set q_nspace ->
          Nlist.make1 (R_Concrete_Set (42,{ q_nspace; q_id })) (*FIXME*)
        | IK_Abstract_Set _ | IK_Other _ ->
          Error.raise_exn e.T.exp_loc "Invalid array range."
      end
    | T.Application (ff,arg) ->
      begin match ff.T.exp_desc, arg.T.exp_desc with
        | T.Builtin Interval, T.Couple (_,int_start,int_end) ->
          let min = to_b0_expr f int_start in
          let max = to_b0_expr f int_end in
          Nlist.make1 (R_Interval (min,max))
        | T.Builtin Product, T.Couple (_,st,ed) ->
          let rg1 = get_array_range f st in
          let rg2 = get_array_range f ed in
          begin match Nlist.to_list rg2 with
            | [x] -> Nlist.from_list_exn ((Nlist.to_list rg1)@[x]) (*FIXME*)
            | _ -> Error.raise_exn e.T.exp_loc "Invalid array range."
          end
        | _, _ -> Error.raise_exn e.T.exp_loc "Invalid array range."
      end
    | _ -> Error.raise_exn e.T.exp_loc "Invalid array range."

  and pred_to_b0_expr : 'mr 'cl. (('mr,'cl) V.t_global_ident -> t_full_ident_kind) -> ('mr,'cl,Btype.t) T.predicate -> t_b0_expr = fun f p ->
    let add_loc exp0_desc = { exp0_loc = p.T.prd_loc; exp0_type = T_Bool; exp0_desc } in
    match p.T.prd_desc with
    | T.P_Builtin Btrue -> add_loc (B0_Builtin_0 B0_True)
    | T.P_Builtin Bfalse -> add_loc (B0_Builtin_0 B0_False)
    | T.Binary_Prop (op,p1,p2) ->
      begin match prop_bop_to_bop op with
        | None -> Error.raise_exn p.T.prd_loc "This is not a valid B0 binary operator."
        | Some op -> add_loc (B0_Builtin_2 (op,pred_to_b0_expr f p1,
                                            pred_to_b0_expr f p2))
      end
    | T.Binary_Pred (op,e1,e2) ->
      begin match pred_bop_to_bop op with
        | None -> Error.raise_exn p.T.prd_loc "This is not a valid B0 binary operator."
        | Some (B0_Equality|B0_Disequality as op) ->
          let e1 = to_b0_expr f e1 in
          let e2 = to_b0_expr f e2 in
          add_loc (B0_Builtin_2 (op,e1,e2))
        | Some (B0_Inequality _ as op) ->
          let e1 = to_b0_expr f e1 in
          let e2 = to_b0_expr f e2 in
          add_loc (B0_Builtin_2 (op,e1,e2))
        | Some _ -> assert false
      end
    | T.Negation p -> add_loc (B0_Builtin_1 (B0_Negation,pred_to_b0_expr f p))
    | T.Universal_Q _ -> Error.raise_exn p.T.prd_loc "This is not a valid B0-expression (Universal quantifier)."
    | T.Existential_Q _ -> Error.raise_exn p.T.prd_loc "This is not a valid B0-expression (Existensial quantifier)."

  let rec get_exp_nle e =
    match e.T.exp_desc with
    | T.Couple (_,x,y) -> Nlist.cons x (get_exp_nle y)
    | _ -> Nlist.make1 e

  let get_enum f (e:(_,_,Btype.t) T.expression) : t_case =
    match e.T.exp_desc with
    | T.Builtin (Integer i) -> CS_Int i
    | T.Builtin TRUE -> CS_Bool true
    | T.Builtin FALSE -> CS_Bool false
(*
    | Ident (K_Local (id,_)) ->
      let q_id = mk_ident e.T.exp_loc id in
      CS_Enum { q_nspace=None; q_id }
*)
    | T.Ident (T.K_Global (id,ki)) ->
      let q_id = mk_ident e.T.exp_loc id in
      begin match f ki with
        | IK_Other IK_Enum q_nspace -> CS_Enum { q_nspace; q_id }
        | _ -> Error.raise_exn e.T.exp_loc "Invalid case."
      end
    | _ -> Error.raise_exn e.T.exp_loc "Invalid case."

  let rec to_b0_subst : 'mr 'cl. (('mr,'cl) V.t_global_ident -> t_full_ident_kind) ->
    (('mr,'cl) T.t_mutable_ident -> t_mut_ident_kind) ->
    ('mr,'cl,Btype.t) T.substitution -> t_b0_subst = fun f g stmt ->
    let add_loc sub0_desc = { sub0_loc=stmt.T.sub_loc; sub0_desc } in
    match stmt.T.sub_desc with
    | T.Skip -> add_loc B0_Null
    | T.Affectation (T.Tuple vars,e) ->
      begin match Nlist.to_list vars with
        | [v] ->
          let lhs = LHS_Variable(mk_ident v.T.mv_loc v.T.mv_id,g v.T.mv_kind) in
          add_loc (B0_Affectation (lhs,to_b0_expr f e))
        | _ -> Error.raise_exn stmt.T.sub_loc "This is not a valid B0-substitution (Parallel affectation)."
      end
    | T.Assert (_,s) -> to_b0_subst f g s
    | T.IfThenElse (nle,def) ->
      let aux (c,s) =
        let c = pred_to_b0_expr f c in
        let s = to_b0_subst f g s in
        (c,s)
      in
      let nle = Nlist.map aux nle in
      let def = match def with
        | None -> None
        | Some s -> Some (to_b0_subst f g s)
      in
      add_loc (B0_IfThenElse (nle,def))
    | T.Case (e,cases,def) ->
      let e = to_b0_expr f e in
      let aux (lst,s) =
        let lst = Nlist.map (get_enum f) lst in
        let s = to_b0_subst f g s in
        (lst,s)
      in
      let cases = Nlist.map aux cases in
      let def = match def with
        | None -> None
        | Some s -> Some (to_b0_subst f g s)
      in
      add_loc (B0_Case (e,cases,def))
    | T.Var (vars,s) ->
      let ss = to_b0_subst f g s in
      let aux v = (mk_ident v.T.bv_loc v.T.bv_id,to_b0_type v.T.bv_loc v.T.bv_typ) in
      let vars = Nlist.map aux vars in
      add_loc (B0_Var (vars,ss))
    | T.CallUp (outs,op,args) ->
      let outs = List.map (fun (v:_ T.mut_var) ->
          (g v.T.mv_kind,mk_ident v.T.mv_loc v.T.mv_id)
        ) outs
      in
      let args = List.map (to_b0_expr f) args in
      let op = match op.T.op_src with
          | T.SO_Seen_Read_Only mch
          | T.SO_Included_Or_Imported mch ->
          { q_nspace=Some(mk_pkg mch.lid_loc mch.lid_str);
            q_id=mk_ident op.T.op_loc op.T.op_id }
          | T.SO_Local _ ->
            { q_nspace=None;
              q_id=mk_ident op.T.op_loc op.T.op_id }
      in
      add_loc (B0_CallUp (outs,op,args))
    | T.While (cond,body,_,_) ->
      let cond = pred_to_b0_expr f cond in
      let body = to_b0_subst f g body in
      add_loc (B0_While (cond,body))
    | T.Sequencement (s1,s2) ->
      let s1 = to_b0_subst f g s1 in
      let s2 = to_b0_subst f g s2 in
      add_loc (B0_Sequencement (s1,s2))
    | T.Affectation (T.Function(ff,args),e) ->
      let ki = g ff.T.mv_kind in
      let ff = mk_ident ff.T.mv_loc ff.T.mv_id in
      let args = Nlist.map (to_b0_expr f) args in
      let e = to_b0_expr f e in
      add_loc (B0_Affectation (LHS_Array(ff,ki,args),e))
    | T.Affectation (T.Record (rd,fd),e) ->
      let rrd = mk_ident rd.T.mv_loc rd.T.mv_id in
      let fd = mk_ident fd.lid_loc fd.lid_str in
      let e = to_b0_expr f e in
      add_loc (B0_Affectation (LHS_Record(rrd,g rd.T.mv_kind,fd),e))
    | T.Pre _ -> Error.raise_exn stmt.T.sub_loc "This is not a valid B0-substitution (Precondition)."
    | T.Choice _ -> Error.raise_exn stmt.T.sub_loc "This is not a valid B0-substitution (Choice)."
    | T.Select _ -> Error.raise_exn stmt.T.sub_loc "This is not a valid B0-substitution (Select)."
    | T.Any _ -> Error.raise_exn stmt.T.sub_loc "This is not a valid B0-substitution (Any)."
    | T.Let _ -> Error.raise_exn stmt.T.sub_loc "This is not a valid B0-substitution (Let)."
    | T.BecomesElt _ -> Error.raise_exn stmt.T.sub_loc "This is not a valid B0-substitution (::)."
    | T.BecomesSuch _ -> add_loc B0_Null (*FIXME warning*)
    | T.Parallel _ -> Error.raise_exn stmt.T.sub_loc "This is not a valid B0-substitution (||)."

  let get_dependencies sees imports extends : (Ident.t_pkg_id*t_dep_kind) list =
    let lst1 = List.map (fun x -> (mk_pkg x.lid_loc x.lid_str,DK_Sees)) sees in
    let lst2 = List.map (fun x -> (mk_pkg x.lid_loc x.lid_str,DK_Imports)) imports in
    let lst3 = List.map (fun x -> (mk_pkg x.lid_loc x.lid_str,DK_Extends)) extends in
    lst1@lst2@lst3

  let get_imp_types (concrete_sets:((G.t_ref,G.t_concrete) T.symb*string list) list)
      (abstract_sets:(G.t_ref,G.t_concrete) T.symb list) : t_type list =
    let add_concrete_set lst (s,elts) : t_type list =
      match s.T.sy_src with
      | G.D_Machine loc | G.D_Redeclared G.By_Machine loc ->
        let elts = List.map (mk_ident loc) elts in
        { ty_name=mk_ident loc s.T.sy_id; ty_def=D_Enum elts }::lst
      | G.D_Included_Or_Imported _ | G.D_Seen _ -> lst
      | G.D_Redeclared G.Implicitely ->
        let loc = Utils.dloc in (*FIXME*)
        let elts = List.map (mk_ident loc) elts in
        { ty_name=mk_ident loc s.T.sy_id; ty_def=D_Enum elts }::lst
      | G.D_Redeclared G.By_Included_Or_Imported mch ->
        let ty_name = mk_ident mch.lid_loc s.T.sy_id in
        let qid = { q_nspace=Some(mk_pkg mch.lid_loc mch.lid_str); q_id=ty_name } in
        { ty_name; ty_def=D_Alias qid }::lst
    in
    let add_abstract_set lst s : t_type list = (*FIXME regarder clause value*)
      match s.T.sy_src with
      | G.D_Machine loc | G.D_Redeclared G.By_Machine loc ->
        { ty_name=mk_ident loc s.T.sy_id; ty_def=D_Int }::lst
      | G.D_Included_Or_Imported _ | G.D_Seen _ -> lst
      | G.D_Redeclared G.By_Included_Or_Imported mch ->
        let loc = mch.lid_loc in
        let ty_name = mk_ident loc s.T.sy_id in
        let qid = { q_nspace=Some(mk_pkg mch.lid_loc mch.lid_str); q_id=ty_name } in
        { ty_name; ty_def=D_Alias qid }::lst
      | G.D_Redeclared G.Implicitely ->
        { ty_name=mk_ident Utils.dloc (*FIXME*) s.T.sy_id; ty_def=D_Int }::lst
    in
    let lst = List.fold_left add_concrete_set [] concrete_sets in
    List.fold_left add_abstract_set lst abstract_sets (*FIXME List.rev*)

  let from_imp_val (x:(G.t_ref,V.t_imp_val) V.t_global_ident) : t_full_ident_kind =
    let get_pkg : (G.t_ref,G.t_concrete) G.t_decl -> _ = function
      | G.D_Machine _ | G.D_Redeclared _ -> None
      | G.D_Included_Or_Imported mch | G.D_Seen mch ->
        Some (mk_pkg mch.lid_loc mch.lid_str)
    in
    match V.view_imp_val x with
    | V.IVV_Concrete_Constant d -> IK_Other (IK_Constant (get_pkg d))
    | V.IVV_Enumerate d -> IK_Other (IK_Enum (get_pkg d))
    | V.IVV_Abstract_Set d -> IK_Abstract_Set (get_pkg d)
    | V.IVV_Concrete_Set (elts,d) -> IK_Concrete_Set (get_pkg d)

  let from_imp_op (x:(G.t_ref,V.t_imp_op) V.t_global_ident) : t_full_ident_kind =
    let get_pkg : (G.t_ref,G.t_concrete) G.t_decl -> _ = function
      | G.D_Machine _ | G.D_Redeclared _ -> None
      | G.D_Included_Or_Imported mch | G.D_Seen mch ->
        Some (mk_pkg mch.lid_loc mch.lid_str)
    in
    match V.view_imp_op x with
    | V.IOV_Concrete_Variable d -> IK_Other (IK_Variable (get_pkg d))
    | V.IOV_Concrete_Constant d -> IK_Other (IK_Constant (get_pkg d))
    | V.IOV_Enumerate d -> IK_Other (IK_Enum (get_pkg d))
    | V.IOV_Concrete_Set (elts,d) -> IK_Concrete_Set (get_pkg d)
    | V.IOV_Abstract_Set d -> IK_Abstract_Set (get_pkg d)

  let from_imp_mut (x:(G.t_ref,V.t_imp_op) T.t_mutable_ident) : t_mut_ident_kind =
    match x with
    | T.MI_Out_Param -> MIK_Param
    | T.MI_Subst_Binder -> MIK_Param
    | T.MI_Global x ->
      match V.view_imp_mut x with
      | V.IMV_Concrete_Variable_From_Machine _
      | V.IMV_Concrete_Variable_Implicitely_Redeclared
      | V.IMV_Concrete_Variable_Redeclared_By_Machine _ -> MIK_Variable

  let get_imp_constants (v,e:T.value*(G.t_ref,V.t_imp_val,Btype.t) T.expression) : t_constant_or_fun option =
    match v.T.val_kind with
    | T.VK_Abstract_Set _ -> None
    | T.VK_Concrete_Constant _ ->
      Some (Cst { c_name = mk_ident v.T.val_loc v.T.val_id;
                  c_loc = v.T.val_loc;
                  c_type = to_b0_type v.T.val_loc e.T.exp_typ;
                  c_init = Init (to_b0_expr from_imp_val e) })

  let get_imp_variables (v:(G.t_ref,G.t_concrete) T.symb) : t_variable option =
    let aux v_loc v_promoted_from =
      Some { v_name=mk_ident v_loc v.T.sy_id;
             v_loc;
             v_type=to_b0_type v_loc v.T.sy_typ;
             v_promoted_from}
    in
    match v.T.sy_src with
    | G.D_Machine loc | G.D_Redeclared G.By_Machine loc -> aux loc None
    | G.D_Included_Or_Imported _ | G.D_Seen _ -> None
    | G.D_Redeclared G.By_Included_Or_Imported mch ->
      aux mch.lid_loc (Some (mk_pkg mch.lid_loc mch.lid_str))
    | G.D_Redeclared G.Implicitely ->
      aux Utils.dloc (*FIXME*) None

  let get_imp_operations (op:(Global.t_ref,V.t_imp_op) T.operation) : t_procedure =
    match op with
    | T.O_Specified { op_out; op_name; op_in; op_body } ->
      let to_arg v =
        { arg_name=mk_ident v.T.arg_loc v.T.arg_id;
          arg_loc=v.T.arg_loc;
          arg_type=to_b0_type v.T.arg_loc v.T.arg_typ }
      in
      { p_name = mk_ident op_name.lid_loc op_name.lid_str;
        p_is_local = false;
        p_args_in = List.map to_arg op_in;
        p_args_out = List.map to_arg op_out;
        p_body = Body (to_b0_subst from_imp_op from_imp_mut op_body) }
    | T.O_Promoted { op_out; op_name; op_in; op_source } ->
      let id = mk_ident op_name.lid_loc op_name.lid_str in
      let to_arg (str,ty) =
        let lc = op_name.lid_loc in
        { arg_name=mk_ident lc str;
          arg_loc=lc;
          arg_type=to_b0_type lc ty }
      in
      { p_name = id;
        p_is_local = false;
        p_args_in = List.map to_arg op_in;
        p_args_out = List.map to_arg op_out;
        p_body = Renames { q_nspace=Some (mk_pkg op_source.lid_loc op_source.lid_str); q_id=id } }
    | T.O_Local { op_out; op_name; op_in; op_body } ->
      let to_arg v =
        { arg_name=mk_ident v.T.arg_loc v.T.arg_id;
          arg_loc=v.T.arg_loc;
          arg_type=to_b0_type v.T.arg_loc v.T.arg_typ }
      in
      { p_name = mk_ident op_name.lid_loc op_name.lid_str;
        p_is_local = true;
        p_args_in = List.map to_arg op_in;
        p_args_out = List.map to_arg op_out;
        p_body = Body (to_b0_subst from_imp_op from_imp_mut op_body) }
(*


  let get_mch_operations (mch:T.machine) : t_procedure list =
    let to_arg v =
      { arg_name=mk_ident v.T.var_loc v.T.var_id;
        arg_loc=v.T.var_loc;
        arg_type=to_b0_type v.T.var_loc v.T.var_typ }
    in
    let to_arg2 lc (str,ty) =
      { arg_name=mk_ident lc str;
        arg_loc=lc;
        arg_type=to_b0_type lc ty }
    in
    let aux op =
      { p_name = mk_ident op.op_name.lid_loc op.op_name.lid_str;
        p_is_local = false;
        p_args_in = List.map (to_arg) op.op_in;
        p_args_out = List.map (to_arg) op.op_out;
        p_body = Body { sub0_loc=op.op_name.lid_loc; sub0_desc=B0_Null} }
    in
    let aux2 op =
      { p_name = mk_ident op.T.po_name.lid_loc op.T.po_name.lid_str;
        p_is_local = false;
        p_args_in = List.map (to_arg2 op.T.po_name.lid_loc) op.T.po_in;
        p_args_out = List.map (to_arg2 op.T.po_name.lid_loc) op.T.po_out;
        p_body = Body { sub0_loc=op.T.po_name.lid_loc; sub0_desc=B0_Null} }
    in
    (List.map aux2 mch.T.m_included_promoted_operations)@(List.map aux mch.T.m_desc.mch_operations)

*)
  let imp_to_package_exn (pkg_name:Ident.t_pkg_id) (imp:T.implementation) : t_package  =
    { pkg_name;
      pkg_dependencies =
        get_dependencies imp.T.imp_sees imp.T.imp_imports imp.T.imp_extends; (*FIXME il peut en manquer*)
      pkg_types = get_imp_types imp.T.imp_concrete_sets imp.T.imp_abstract_sets;
      pkg_constants = Utils.filter_map get_imp_constants imp.T.imp_values;
      pkg_variables = Utils.filter_map get_imp_variables imp.T.imp_concrete_variables;
      pkg_procedures = List.map get_imp_operations imp.T.imp_operations;
      pkg_init = Utils.map_opt (to_b0_subst from_imp_op from_imp_mut) imp.T.imp_initialisation
    }

  let get_mch_types (concrete_sets:((G.t_mch,G.t_concrete) T.symb*string list) list)
      (abstract_sets:(G.t_mch,G.t_concrete) T.symb list) : t_type list =
    let add_concrete_set lst (s,elts:(G.t_mch,G.t_concrete)T.symb*_) =
      match s.T.sy_src with
      | G.D_Machine loc ->
        let elts = List.map (mk_ident loc) elts in
        { ty_name=mk_ident loc s.T.sy_id; ty_def=D_Enum elts }::lst
      | G.D_Included_Or_Imported mch ->
        let loc = mch.lid_loc in
        let elts = List.map (mk_ident loc) elts in
        { ty_name=mk_ident loc s.T.sy_id; ty_def=D_Enum elts }::lst
      | G.D_Seen _ -> lst
    in
    let add_abstract_set lst (s:(G.t_mch,G.t_concrete)T.symb) = (*FIXME regarder clause value*)
      match s.T.sy_src with
      | G.D_Machine loc ->
        { ty_name=mk_ident loc s.T.sy_id; ty_def=D_Int }::lst
      | G.D_Included_Or_Imported mch ->
        { ty_name=mk_ident mch.lid_loc s.T.sy_id; ty_def=D_Int }::lst
      | G.D_Seen _ -> lst
    in
    let lst = List.fold_left add_concrete_set [] concrete_sets in
    List.fold_left add_abstract_set lst abstract_sets (*FIXME List.rev*)

  let rec get_default_value (lc:Utils.loc) (ty:t_b0_type) : t_b0_expr =
    let mk exp0_type exp0_desc = { exp0_loc=lc; exp0_desc; exp0_type } in
    match ty with
    | T_Int -> mk ty (B0_Builtin_0 (B0_Integer Int32.zero))
    | T_String -> mk ty (B0_Builtin_0 (B0_String ""))
    | T_Bool -> mk ty (B0_Builtin_0 B0_True)
    | T_Abstract s -> mk ty (B0_Builtin_0 (B0_Integer Int32.zero))
    | T_Enum s -> mk ty (B0_Builtin_0 (B0_Integer Int32.zero)) (*FIXME*)
    | T_Array (_,tg) -> mk ty (B0_Array [])
    | T_Record lst ->
      let aux (id,ty) = (id,get_default_value lc ty) in
      mk ty (B0_Record (List.map aux lst))

  let flatten_product (pr:Btype.t) : Btype.t Nlist.t =
    let rec aux (accu:Btype.t list) (ty:Btype.t) : Btype.t list =
      match Btype.view ty with
      | Btype.T_Product (t1,t2) -> aux (t2::accu) t1
      | _ -> ty::accu
    in
    Nlist.from_list_exn (aux [] pr)

  let to_b0_fun_type (ty:Btype.t) : (t_b0_type Nlist.t*t_b0_type) option =
    match Btype.view ty with
    | Btype.T_Power ty ->
      begin match Btype.view ty with
        | Btype.T_Product (rg,tg) ->
          (try Some (Nlist.map (to_b0_type Utils.dloc) (flatten_product rg),to_b0_type Utils.dloc tg)
          with Error.Error _ -> None)
        | _ -> None
      end
     | _ -> None

  let get_mch_constant (c:(G.t_mch,G.t_concrete) T.symb) : t_constant_or_fun option =
    let aux loc =
      match (try Some(to_b0_type loc c.T.sy_typ) with Error.Error _ -> None) with
      | Some ty ->
        Some (Cst { c_name = mk_ident loc c.T.sy_id;
                    c_type = ty;
                    c_loc = loc;
                    c_init = Init (get_default_value loc ty) })
      | None ->
        begin match to_b0_fun_type c.T.sy_typ with
          | None -> (Error.print_error {Error.err_loc=loc;
                                        Error.err_txt=
                      ("The type of the constant "^c.T.sy_id^
                       " is neither a B0-type or a B0 function type ("^
                       Btype.to_string c.T.sy_typ^").")}; None) (*FIXME*)
          | Some (t_args,t_ret) ->
            let to_arg i ty =
              match Ident.make ("_arg"^string_of_int i) with
              | None -> assert false
              | Some id -> { arg_name=id; arg_loc=loc; arg_type=ty }
            in
            Some (Fun { f_name = mk_ident loc c.T.sy_id;
                        f_loc = loc;
                        f_args = Nlist.from_list_exn (List.mapi to_arg (Nlist.to_list t_args));
                        f_ret = get_default_value loc t_ret })
        end
    in
    match c.T.sy_src with
    | G.D_Machine loc -> (aux loc)
    | G.D_Included_Or_Imported mch -> (aux mch.lid_loc)
    | G.D_Seen _ -> None

  let get_mch_variable (v:(G.t_mch,G.t_concrete) T.symb) : t_variable option =
    let aux v_loc =
      { v_name=mk_ident v_loc v.T.sy_id;
        v_loc;
        v_type=to_b0_type v_loc v.T.sy_typ;
        v_promoted_from = None}
    in
    match v.T.sy_src with
    | G.D_Machine loc -> Some (aux loc)
    | G.D_Included_Or_Imported mch -> Some (aux mch.lid_loc)
    | G.D_Seen _ -> None

  let get_mch_operation (op:(G.t_mch,V.t_mch_op) T.operation) : t_procedure =
    match op with
    | T.O_Specified { op_out; op_name; op_in; op_body } ->
      let to_arg v =
        { arg_name=mk_ident v.T.arg_loc v.T.arg_id;
          arg_loc=v.T.arg_loc;
          arg_type=to_b0_type v.T.arg_loc v.T.arg_typ }
      in
      { p_name = mk_ident op_name.lid_loc op_name.lid_str;
        p_is_local = false;
        p_args_in = List.map to_arg op_in;
        p_args_out = List.map to_arg op_out;
        p_body = Body { sub0_loc=op_name.lid_loc; sub0_desc=B0_Null} }
    | T.O_Promoted { op_out; op_name; op_in; op_source } ->
      let p_name = mk_ident op_name.lid_loc op_name.lid_str in
      let to_arg lc (str,ty) =
      { arg_name=mk_ident lc str;
        arg_loc=lc;
        arg_type=to_b0_type lc ty }
      in
      { p_name;
        p_is_local = false;
        p_args_in = List.map (to_arg op_name.lid_loc) op_in;
        p_args_out = List.map (to_arg op_name.lid_loc) op_out;
        p_body = Renames
            {q_nspace=Some(mk_pkg op_source.lid_loc op_source.lid_str);
             q_id=p_name} }

  let mch_to_package_exn (pkg_name:Ident.t_pkg_id) (mch:T.machine) : t_package  =
    { pkg_name;
      pkg_dependencies = get_dependencies mch.T.mch_sees [] [] (*FIXME*);
      pkg_types = get_mch_types mch.T.mch_concrete_sets mch.T.mch_abstract_sets;
      pkg_constants = Utils.filter_map get_mch_constant mch.T.mch_concrete_constants;
      pkg_variables = Utils.filter_map get_mch_variable mch.T.mch_concrete_variables;
      pkg_procedures = List.map get_mch_operation mch.T.mch_operations;
      pkg_init = None
    }

  let to_package (pkg_name:Ident.t_pkg_id) (comp:T.component) : t_package Error.t_result =
    try match comp.T.co_desc with
      | T.Machine mch -> Ok (mch_to_package_exn pkg_name mch)
      | T.Refinement _ -> Error { Error.err_loc=comp.T.co_name.lid_loc;
                                err_txt="Machine or Implementation expected." }
      | T.Implementation imp -> Ok (imp_to_package_exn pkg_name imp)
    with
    | Error.Error err -> Error err

end

module SSet = Set.Make(String)

module Ada_ident =
struct
  let reserved_list = [
    "abort"; "else"; "new"; "return"; "abs"; "elsif"; "not"; "reverse"; "abstract";
    "end"; "null"; "accept"; "entry"; "select"; "access"; "exception"; "of"; "separate";
    "aliased"; "exit"; "or"; "some"; "all"; "others"; "subtype"; "and"; "for"; "out";
    "synchronized"; "array"; "function"; "overriding"; "at"; "tagged"; "generic";
    "package"; "task"; "begin"; "goto"; "pragma"; "terminate"; "body"; "private";
    "then"; "if"; "procedure"; "type"; "case"; "in"; "protected"; "constant";
    "interface"; "until"; "is"; "raise"; "use"; "declare"; "range"; "delay"; "limited";
    "record"; "when"; "delta"; "loop"; "rem"; "while"; "digits"; "renames"; "with";
    "do"; "mod"; "requeue"; "xor" ]

  let reserved = Hashtbl.create 47
  let reserved_set = List.fold_left (fun x y -> SSet.add y x) SSet.empty reserved_list

  type t = string
  let to_string x = x
  type t_pkg_id = string
  let pkg_to_string x = x

  let is_valid_ada_id (id:string) : bool =
    let reg = Str.regexp {|[a-zA-Z]\(_?[a-zA-Z0-9]\)*$|} in
    not ( SSet.mem (String.lowercase_ascii id) reserved_set) &&
    (Str.string_match reg id 0)

  let make x =
    if is_valid_ada_id x then Some x
    else None

  let make_pkg_id x =
    let _3u = Str.regexp_string "___" in
    let x = Str.global_replace _3u "." x in
    let lst = String.split_on_char '.' x in
    if List.for_all is_valid_ada_id lst then Some x
    else None
end

module Ada = Make(Ada_ident)

module Rust_ident =
struct
  let reserved_list = [
    "as"; "break"; "const"; "continue"; "crate"; "else"; "enum"; "extern"; "false";
    "fn"; "for"; "if"; "impl"; "in"; "let"; "loop"; "match"; "mod"; "move"; "mut";
    "pub"; "ref"; "return"; "self"; "Self"; "static"; "struct"; "super"; "trait";
    "true"; "type"; "unsafe"; "use"; "where"; "while"; "abstract"; "become"; "box";
    "do"; "final"; "macro"; "override"; "priv"; "typeof"; "unsized"; "virtual";
    "yield"; "union"; "dyn"]

  let reserved = Hashtbl.create 47
  let reserved_set = List.fold_left (fun x y -> SSet.add y x) SSet.empty reserved_list

  type t = string
  let to_string x = x
  type t_pkg_id = string
  let pkg_to_string x = x

  let is_valid_rust_id (id:string) : bool =
    let reg = Str.regexp {|\([a-zA-Z][a-zA-Z0-9_]*\)\|\(_[a-zA-Z0-9_]+\)$|} in
    (Str.string_match reg id 0)

  let make x =
      if is_valid_rust_id x then
        ( if SSet.mem x reserved_set then Some (x ^ "_")
          else Some x )
    else
      None

  let make_pkg_id = make
end

module Rust = Make(Rust_ident)
