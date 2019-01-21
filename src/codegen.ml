open SyntaxCore
module T = TSyntax
module V = Visibility
module G = Global

type ident = string

type t_id_2 =
  | Intern of lident
  | Extern of lident*string

type t_pkg_id = lident
type t_id = lident
let mk_lident lid_loc lid_str = { lid_loc; lid_str }

let ident_to_string x = x
let pkg_to_lident x = x
let id_to_lident x = x

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

type qident = { q_nspace:t_pkg_id option; q_id:t_id }
type t_type_id = { t_nspace:string option; t_id:string }

type t_array_index =
  | I_Int
  | I_Bool
  | I_Enum of t_type_id

type t_b0_type =
  | T_Int
  | T_Bool
  | T_String
  | T_Abstract of t_type_id
  | T_Enum of t_type_id
  | T_Array of t_array_index*t_b0_type
  | T_Record of (string*t_b0_type) list

type t_ident_kind =
  | IK_Constant of t_pkg_id option
  | IK_Variable of t_pkg_id option
  | IK_Enum of t_pkg_id option

type t_full_ident_kind =
  | IK_Concrete_Set of t_pkg_id option
  | IK_Abstract_Set of t_pkg_id option
  | IK_Other of t_ident_kind

type t_mut_ident_kind =
  | MIK_Variable
  | MIK_Param
  | MIK_Local

type t_exp0_desc =
  | B0_Local_Ident of t_id*Local.t_local_kind
  | B0_Global_Ident of t_id*t_ident_kind
  | B0_Builtin_0 of t_b0_constant
  | B0_Builtin_1 of t_b0_unary_op * t_b0_expr
  | B0_Builtin_2 of t_b0_binary_op * t_b0_expr * t_b0_expr
  | B0_Array_Access of t_b0_expr * t_b0_expr Nlist.t
  | B0_Array of t_b0_expr list
  | B0_Array_Init of t_b0_range Nlist.t*t_b0_expr
  | B0_Record of (t_id*t_b0_expr) list
  | B0_Record_Access of t_b0_expr*t_id

and t_b0_range =
  | R_Interval of t_b0_expr*t_b0_expr
  | R_Concrete_Set of qident

and t_b0_expr =
  { exp0_loc: Utils.loc;
    exp0_type: t_b0_type;
    exp0_desc: t_exp0_desc }

type t_constant_init =
  | Init of t_b0_expr
  | Promoted of t_pkg_id

type t_constant =
  { c_name: t_id_2;
    c_type: t_b0_type;
    c_init: t_constant_init }

type t_arg = {
    arg_name: t_id_2;
    arg_type: t_b0_type }

type t_fun =
  { f_name: t_id;
    f_args: t_arg Nlist.t;
    f_ret: t_b0_expr }

type t_variable =
  { v_name: t_id_2;
    v_type: t_b0_type }

type t_case =
  | CS_Int of Int32.t
  | CS_Bool of bool
  | CS_Enum of qident

type t_sub0_desc =
  | B0_Null
  | B0_Affectation of t_b0_lhs*t_b0_expr
  | B0_IfThenElse of (t_b0_expr*t_b0_subst) Nlist.t * t_b0_subst option
  | B0_Case of t_b0_expr * (t_case Nlist.t*t_b0_subst) Nlist.t * t_b0_subst option
  | B0_Var of (t_id*t_b0_type) Nlist.t * t_b0_subst
  | B0_While of t_b0_expr * t_b0_subst
  | B0_CallUp of (t_mut_ident_kind*t_id) list * qident * t_b0_expr list
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
  { p_name: t_id;
    p_is_local: bool;
    p_args_in: t_arg list;
    p_args_out: t_arg list;
    p_body: t_procedure_body }

type t_type_def =
  | D_Int
  | D_Alias of t_pkg_id*string
  | D_Enum of string list

type t_type =
  { ty_name: t_id_2;
    ty_def: t_type_def }

type t_dep_kind = DK_Sees | DK_Imports | DK_Extends

type t_package =
  { pkg_name: t_pkg_id;
    pkg_dependencies: (t_pkg_id*t_dep_kind) list;
    pkg_types: t_type list;
    pkg_constants: t_constant list;
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
  | INTEGER | NATURAL | NATURAL1 | INT | NAT
  | NAT1 | STRINGS | BOOLEANS | Empty_Set | Empty_Seq -> None

let is_int ty =
  match Btype.view ty with
  | Btype.T_Int -> true
  | _ -> false

let to_b0_type (ty:Btype.t) : t_b0_type option =
  let exception Local_Exn in
  let rec aux ty = match Btype.view ty with
  | Btype.T_Int -> T_Int
  | Btype.T_Bool -> T_Bool
  | Btype.T_String -> T_String
  | Btype.T_Concrete_Set (Btype.T_Current,t_id) ->
    T_Enum { t_nspace=None; t_id }
  | Btype.T_Concrete_Set (Btype.T_Seen m,t_id) ->
    T_Enum { t_nspace=Some m; t_id }
  | Btype.T_Abstract_Set (Btype.T_Current,t_id) ->
    T_Abstract { t_nspace=None; t_id }
  | Btype.T_Abstract_Set (Btype.T_Seen m,t_id) ->
    T_Abstract { t_nspace=Some m; t_id }
  | Btype.T_Power ty0 ->
    begin match Btype.view ty0 with
      | Btype.T_Product (rg,tg) ->
        let tg = aux tg in
        let index = match Btype.view rg with
          | Btype.T_Int -> I_Int
          | Btype.T_Bool -> I_Bool
          | Btype.T_Concrete_Set (Btype.T_Current,t_id) ->
            I_Enum { t_nspace=None; t_id }
          | Btype.T_Concrete_Set (Btype.T_Seen m,t_id) ->
            I_Enum { t_nspace=Some m; t_id }
          | _ -> raise Local_Exn
        in
        T_Array (index,tg)
      | _ -> raise Local_Exn
    end
  | Btype.T_Record lst -> T_Record (List.map (fun (id,ty) -> (id,aux ty)) lst)
  | Btype.T_Product _ -> raise Local_Exn
  in
  try Some(aux ty)
  with Local_Exn -> None

let rec to_b0_expr : 'mr 'cl. (('mr,'cl) V.t_global_ident -> t_full_ident_kind) ->
  ('mr,'cl,Btype.t) T.expression -> t_b0_expr = fun f e ->
  let add_lt exp0_desc =
    { exp0_loc = e.T.exp_loc;
      exp0_type = 
        (match to_b0_type e.T.exp_typ with
        | Some ty -> ty
        | None  ->
          Error.raise_exn e.T.exp_loc
            ("This expression has type '" ^ Btype.to_string e.T.exp_typ 
             ^"'. This is not a valid B0-expression."));
      exp0_desc }
  in
  match e.T.exp_desc with
  | T.Ident T.K_Local (id,ki) -> add_lt (B0_Local_Ident({lid_str=id;lid_loc=e.T.exp_loc},ki))
  | T.Ident T.K_Global (id,ki) ->
    begin match f ki with
      | IK_Other ki -> add_lt (B0_Global_Ident({lid_str=id;lid_loc=e.T.exp_loc},ki))
      | IK_Concrete_Set _ ->
        Error.raise_exn e.T.exp_loc "A concrete set is not a valid B0-expression."
      | IK_Abstract_Set _ ->
        Error.raise_exn e.T.exp_loc "an abstract set is not a valid B0-expression."
    end
  | T.Builtin_0 bi ->
    begin match to_b0_constant bi with
      | None -> Error.raise_exn e.T.exp_loc ("This is not a valid B0 constant.")
      | Some bi -> add_lt (B0_Builtin_0 bi)
    end
  | T.Builtin_1 (Unary_Minus,arg) ->
    add_lt (B0_Builtin_1 (B0_Minus,to_b0_expr f arg))
  | T.Builtin_1 (Successor,arg) ->
    add_lt (B0_Builtin_2(B0_Addition,to_b0_expr f arg,
                         add_lt (B0_Builtin_0 (B0_Integer Int32.one))))
  | T.Builtin_1 (Predecessor,arg) ->
    add_lt (B0_Builtin_2(B0_Difference,to_b0_expr f arg,
                         add_lt (B0_Builtin_0 (B0_Integer Int32.one))))
  | T.Builtin_1 (_,_) ->
    Error.raise_exn e.T.exp_loc "This is not a valid B0 operator."
  | T.Builtin_2 (Product,arg1,arg2) ->
    if is_int e.T.exp_typ then
      add_lt (B0_Builtin_2 (B0_Product,to_b0_expr f arg1,to_b0_expr f arg2))
    else
      add_lt (get_array_init e.T.exp_loc f arg1 arg2)
  | T.Builtin_2 (Difference,arg1,arg2) ->
    add_lt (B0_Builtin_2 (B0_Difference,to_b0_expr f arg1,to_b0_expr f arg2))
  | T.Builtin_2 (Addition,arg1,arg2) ->
    add_lt (B0_Builtin_2 (B0_Addition,to_b0_expr f arg1,to_b0_expr f arg2))
  | T.Builtin_2 (Division,arg1,arg2) ->
    add_lt (B0_Builtin_2 (B0_Division,to_b0_expr f arg1,to_b0_expr f arg2))
  | T.Builtin_2 (Modulo,arg1,arg2) ->
    add_lt (B0_Builtin_2 (B0_Modulo,to_b0_expr f arg1,to_b0_expr f arg2))
  | T.Builtin_2 (Power,arg1,arg2) ->
    add_lt (B0_Builtin_2 (B0_Power,to_b0_expr f arg1,to_b0_expr f arg2))
  | T.Builtin_2 (Application,ff,arg) ->
    let ff = to_b0_expr f ff in
    let rec get_args arg =
      match arg.T.exp_desc with
      | T.Builtin_2 (Couple _,a1,a2) -> Nlist.cons (to_b0_expr f a1) (get_args a2)
      | _ -> Nlist.make1 (to_b0_expr f arg)
    in
    let args = get_args arg in
    add_lt (B0_Array_Access(ff,args))

  | T.Builtin_2 (_,_,_) ->
    Error.raise_exn e.T.exp_loc "This is not a valid B0 operator."
  | T.Pbool p -> pred_to_b0_expr f p
  | T.Extension nle ->
    let aux i e = match e.T.exp_desc with
      | T.Builtin_2 (Couple _,e1,e2) ->
        begin match (to_b0_expr f e1).exp0_desc with
          | B0_Builtin_0 (B0_Integer j) ->
            if (Int32.of_int i) = j then (j,to_b0_expr f e2)
            else Error.raise_exn e1.T.exp_loc
                ("This is not a valid B0-array. The literal '"^string_of_int i^"' is expected here.")
          | _ -> Error.raise_exn e1.T.exp_loc
                   "This is not a valid B0-array. An integer literal is expected here."
        end
      | _ -> Error.raise_exn e.T.exp_loc "Ill-formed array."
    in
    let lst = List.mapi aux (Nlist.to_list nle) in
    let lst = List.fast_sort (fun (a,_) (b,_) -> Int32.compare a b) lst in
    let lst = List.map snd lst in
    add_lt (B0_Array lst)
  | T.Record lst ->
    add_lt (B0_Record (List.map (fun (id,e) ->
        (id,to_b0_expr f e)) (Nlist.to_list lst) ))
  | T.Dollar _ ->
    Error.raise_exn e.T.exp_loc "The dollar expression s is not a valid B0-expression."
  | T.Sequence _ ->
    Error.raise_exn e.T.exp_loc "A sequence is not a valid B0-expression."
  | T.Comprehension _ ->
    Error.raise_exn e.T.exp_loc "A set is not a valid B0-expression."
  | T.Binder _ ->
    Error.raise_exn e.T.exp_loc "Such binder is not a valid B0-expression."
  | T.Record_Field_Access (e,fd) ->
    let e = to_b0_expr f e in
    add_lt (B0_Record_Access (e,fd))
  | T.Record_Type _ ->
    Error.raise_exn e.T.exp_loc "A record type is not a valid B0-expression."

and get_array_init : 'mr 'cl. Utils.loc -> (('mr,'cl) V.t_global_ident -> t_full_ident_kind) ->
  ('mr,'cl,Btype.t) T.expression ->
  ('mr,'cl,Btype.t) T.expression ->
  t_exp0_desc = fun lc f e1 e2 ->
  let v = match e2.T.exp_desc with
    | T.Extension lst ->
      begin match Nlist.to_list lst with
        | [s] -> to_b0_expr f s
        | _ ->
          Error.raise_exn lc "Invalid array initialisation (not a singleton)."
      end
    | _ ->
      Error.raise_exn lc "Invalid array initialisation (not a singleton)."
  in
  B0_Array_Init (get_array_range f e1,v)

and get_array_range : 'mr 'cl. (('mr,'cl) V.t_global_ident -> t_full_ident_kind) ->
  ('mr,'cl,Btype.t) T.expression -> t_b0_range Nlist.t = fun f e ->
  match e.T.exp_desc with
  | T.Ident (T.K_Global (id,ki)) ->
    begin match f ki with
      | IK_Concrete_Set q_nspace ->
        Nlist.make1 (R_Concrete_Set { q_nspace; q_id=mk_lident e.T.exp_loc id })
      | IK_Abstract_Set _ | IK_Other _ ->
        Error.raise_exn e.T.exp_loc "Invalid array range."
    end
  | T.Builtin_2 (Interval,int_start,int_end) ->
    let min = to_b0_expr f int_start in
    let max = to_b0_expr f int_end in
    Nlist.make1 (R_Interval (min,max))
  | T.Builtin_2(Product,st,ed) ->
    let rg1 = get_array_range f st in
    let rg2 = get_array_range f ed in
    begin match Nlist.to_list rg2 with
      | [x] -> Nlist.from_list_exn ((Nlist.to_list rg1)@[x]) (*FIXME*)
      | _ -> Error.raise_exn e.T.exp_loc "Invalid array range."
    end
  |  _ -> Error.raise_exn e.T.exp_loc "Invalid array range."

and pred_to_b0_expr : 'mr 'cl. (('mr,'cl) V.t_global_ident -> t_full_ident_kind) ->
  ('mr,'cl,Btype.t) T.predicate -> t_b0_expr = fun f p ->
  let add_loc exp0_desc = { exp0_loc = p.T.prd_loc; exp0_type = T_Bool; exp0_desc } in
  match p.T.prd_desc with
  | T.P_Builtin Btrue -> add_loc (B0_Builtin_0 B0_True)
  | T.P_Builtin Bfalse -> add_loc (B0_Builtin_0 B0_False)
  | T.Binary_Prop (op,p1,p2) ->
    begin match prop_bop_to_bop op with
      | None ->
        Error.raise_exn p.T.prd_loc "This is not a valid B0 binary operator."
      | Some op ->
        add_loc (B0_Builtin_2 (op,pred_to_b0_expr f p1, pred_to_b0_expr f p2))
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
  | T.Universal_Q _ ->
    Error.raise_exn p.T.prd_loc "This is not a valid B0-expression (Universal quantifier)."
  | T.Existential_Q _ ->
    Error.raise_exn p.T.prd_loc "This is not a valid B0-expression (Existensial quantifier)."

let get_enum f (e:(_,_,Btype.t) T.expression) : t_case =
  match e.T.exp_desc with
  | T.Builtin_0 (Integer i) -> CS_Int i
  | T.Builtin_0 TRUE -> CS_Bool true
  | T.Builtin_0 FALSE -> CS_Bool false
  | T.Ident (T.K_Global (id,ki)) ->
    let q_id = mk_lident e.T.exp_loc id in
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
        let lhs = LHS_Variable(mk_lident v.T.mv_loc v.T.mv_id,g v.T.mv_kind) in
        add_loc (B0_Affectation (lhs,to_b0_expr f e))
      | _ ->
        Error.raise_exn stmt.T.sub_loc "This is not a valid B0-substitution (Parallel affectation)."
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
    let aux v =
      match to_b0_type v.T.bv_typ with
      | Some ty -> (mk_lident v.T.bv_loc v.T.bv_id,ty)
      | None -> Error.raise_exn v.T.bv_loc
                  ("The variable '"^v.T.bv_id^"' has type '"^
                   Btype.to_string v.T.bv_typ^"'. This is not a valid B0-type.")
    in
    let vars = Nlist.map aux vars in
    add_loc (B0_Var (vars,ss))
  | T.CallUp (outs,op,args) ->
    let outs = List.map (fun (v:_ T.mut_var) ->
        (g v.T.mv_kind, mk_lident v.T.mv_loc v.T.mv_id)
      ) outs
    in
    let args = List.map (to_b0_expr f) args in
    let op = match op.T.op_src with
      | T.SO_Seen_Read_Only mch
      | T.SO_Included_Or_Imported mch ->
        { q_nspace=Some mch;
          q_id=mk_lident op.T.op_loc op.T.op_id }
      | T.SO_Local _ ->
        { q_nspace=None;
          q_id= mk_lident op.T.op_loc op.T.op_id }
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
    let ff = mk_lident ff.T.mv_loc ff.T.mv_id in
    let args = Nlist.map (to_b0_expr f) args in
    let e = to_b0_expr f e in
    add_loc (B0_Affectation (LHS_Array(ff,ki,args),e))
  | T.Affectation (T.Record (rd,fd),e) ->
    let rrd = mk_lident rd.T.mv_loc rd.T.mv_id in
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

let get_dependencies sees imports extends : (t_pkg_id*t_dep_kind) list =
  let lst1 = List.map (fun x -> (x,DK_Sees)) sees in
  let lst2 = List.map (fun x -> (x,DK_Imports)) imports in
  let lst3 = List.map (fun x -> (x,DK_Extends)) extends in
  lst1@lst2@lst3

let get_imp_types (concrete_sets:((G.t_ref,G.t_concrete) T.symb*string list) list)
    (abstract_sets:(G.t_ref,G.t_concrete) T.symb list) : t_type list =
  let add_concrete_set lst (s,elts) : t_type list =
    match s.T.sy_src with
    | G.D_Machine loc | G.D_Redeclared G.By_Machine loc ->
      { ty_name=Intern (mk_lident loc s.T.sy_id); ty_def=D_Enum elts }::lst
    | G.D_Included_Or_Imported _ | G.D_Seen _ -> lst
    | G.D_Redeclared G.Implicitely ->
      let mch = assert false (*FIXME*) in
      { ty_name=Extern(mch,s.T.sy_id); ty_def=D_Enum elts }::lst
    | G.D_Redeclared G.By_Included_Or_Imported mch ->
      let ty_name = Extern(mch,s.T.sy_id) in
      { ty_name; ty_def=D_Alias (mch,s.T.sy_id) }::lst
  in
  let add_abstract_set lst s : t_type list = (*FIXME regarder clause value*)
    match s.T.sy_src with
    | G.D_Machine loc | G.D_Redeclared G.By_Machine loc ->
      { ty_name=Intern(mk_lident loc s.T.sy_id); ty_def=D_Int }::lst
    | G.D_Included_Or_Imported _ | G.D_Seen _ -> lst
    | G.D_Redeclared G.By_Included_Or_Imported mch ->
      let ty_name = Extern(mch,s.T.sy_id) in
      { ty_name; ty_def=D_Alias (mch,s.T.sy_id) }::lst
    | G.D_Redeclared G.Implicitely ->
      let mch = assert false (*FIXME*) in
      let ty_name = Extern(mch,s.T.sy_id) in
      { ty_name; ty_def=D_Int }::lst
  in
  let lst = List.fold_left add_concrete_set [] concrete_sets in
  List.rev (List.fold_left add_abstract_set lst abstract_sets)

let from_imp_val (x:(G.t_ref,V.t_imp_val) V.t_global_ident) : t_full_ident_kind =
  let get_pkg : (G.t_ref,G.t_concrete) G.t_decl -> _ = function
    | G.D_Machine _ | G.D_Redeclared _ -> None
    | G.D_Included_Or_Imported mch | G.D_Seen mch -> Some mch
  in
  match V.view_imp_val x with
  | V.IVV_Concrete_Constant d -> IK_Other (IK_Constant (get_pkg d))
  | V.IVV_Enumerate d -> IK_Other (IK_Enum (get_pkg d))
  | V.IVV_Abstract_Set d -> IK_Abstract_Set (get_pkg d)
  | V.IVV_Concrete_Set (_,d) -> IK_Concrete_Set (get_pkg d)

let from_imp_op (x:(G.t_ref,V.t_imp_op) V.t_global_ident) : t_full_ident_kind =
  let get_pkg : (G.t_ref,G.t_concrete) G.t_decl -> _ = function
    | G.D_Machine _ | G.D_Redeclared _ -> None
    | G.D_Included_Or_Imported mch | G.D_Seen mch -> Some mch
  in
  match V.view_imp_op x with
  | V.IOV_Concrete_Variable d -> IK_Other (IK_Variable (get_pkg d))
  | V.IOV_Concrete_Constant d -> IK_Other (IK_Constant (get_pkg d))
  | V.IOV_Enumerate d -> IK_Other (IK_Enum (get_pkg d))
  | V.IOV_Concrete_Set (_,d) -> IK_Concrete_Set (get_pkg d)
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

let get_imp_constants (v,e:T.value*(G.t_ref,V.t_imp_val,Btype.t) T.expression) : t_constant option =
  match v.T.val_kind with
  | T.VK_Abstract_Set _ -> None
  | T.VK_Concrete_Constant _ ->
    Some { c_name = Intern(mk_lident v.T.val_loc v.T.val_id);
           c_type = (match to_b0_type e.T.exp_typ with
               | Some ty -> ty
               | None -> assert false (*FIXME*) );
           c_init = Init (to_b0_expr from_imp_val e) }

let get_imp_variables (v:(G.t_ref,G.t_concrete) T.symb) : t_variable option =
  let aux v_name =
    Some { v_name; v_type=(match to_b0_type v.T.sy_typ with
        | Some ty -> ty
        | None -> assert false (*FIXME*)) }
  in
  match v.T.sy_src with
  | G.D_Machine loc | G.D_Redeclared G.By_Machine loc ->
    aux (Intern (mk_lident loc v.T.sy_id))
  | G.D_Included_Or_Imported _ | G.D_Seen _ -> None
  | G.D_Redeclared G.By_Included_Or_Imported mch ->
    aux (Extern(mch,v.T.sy_id))
  | G.D_Redeclared G.Implicitely ->
    let mch = assert false (*FIXME*) in
    aux (Extern(mch,v.T.sy_id))

let get_imp_operations (op:(Global.t_ref,V.t_imp_op) T.operation) : t_procedure =
  match op with
  | T.O_Specified { op_out; op_name; op_in; op_body } ->
    let to_arg v =
      { arg_name=Intern(mk_lident v.T.arg_loc v.T.arg_id);
        arg_type= (match to_b0_type v.T.arg_typ with
            | Some ty -> ty
            | None -> assert false (*FIXME*)) }
    in
    { p_name = op_name;
      p_is_local = false;
      p_args_in = List.map to_arg op_in;
      p_args_out = List.map to_arg op_out;
      p_body = Body (to_b0_subst from_imp_op from_imp_mut op_body) }
  | T.O_Promoted { op_out; op_name; op_in; op_source } ->
    let to_arg (str,ty) =
      { arg_name=Extern(op_name, str);
        arg_type=(match to_b0_type ty with
            | Some ty -> ty
            | None -> assert false (*FIXME*)) }
    in
    { p_name = op_name;
      p_is_local = false;
      p_args_in = List.map to_arg op_in;
      p_args_out = List.map to_arg op_out;
      p_body = Renames { q_nspace=Some op_source; q_id=op_name } }
  | T.O_Local { op_out; op_name; op_in; op_body } ->
    let to_arg v =
      { arg_name=Intern(mk_lident v.T.arg_loc v.T.arg_id);
        arg_type=(match to_b0_type v.T.arg_typ with
            | Some ty -> ty
            | None -> assert false (*FIXME*)) }
    in
    { p_name = op_name;
      p_is_local = true;
      p_args_in = List.map to_arg op_in;
      p_args_out = List.map to_arg op_out;
      p_body = Body (to_b0_subst from_imp_op from_imp_mut op_body) }
 
let imp_to_package_exn (pkg_name:t_pkg_id) (imp:T.implementation) : t_package  =
  { pkg_name;
    pkg_dependencies =
      (* Some dependencies might be missing. For instance the type of some symbols
       * declared in a seen machine may be declared in a machine that is neither
       * senn imported nor extended. *)
      get_dependencies imp.T.imp_sees imp.T.imp_imports imp.T.imp_extends;
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
      { ty_name=Intern(mk_lident loc s.T.sy_id); ty_def=D_Enum elts }::lst
    | G.D_Included_Or_Imported mch ->
      { ty_name=Extern(mch,s.T.sy_id); ty_def=D_Enum elts }::lst
    | G.D_Seen _ -> lst
  in
  let add_abstract_set lst (s:(G.t_mch,G.t_concrete)T.symb) =
    match s.T.sy_src with
    | G.D_Machine loc ->
      { ty_name=Intern(mk_lident loc s.T.sy_id); ty_def=D_Int }::lst
    | G.D_Included_Or_Imported mch ->
      { ty_name=Extern(mch,s.T.sy_id); ty_def=D_Int }::lst
    | G.D_Seen _ -> lst
  in
  let lst = List.fold_left add_concrete_set [] concrete_sets in
  List.rev (List.fold_left add_abstract_set lst abstract_sets)

let rec get_default_value (exp0_loc:Utils.loc) (ty:t_b0_type) : t_b0_expr =
  let mk exp0_type exp0_desc = { exp0_loc; exp0_desc; exp0_type } in
  match ty with
  | T_Int -> mk ty (B0_Builtin_0 (B0_Integer Int32.zero))
  | T_String -> mk ty (B0_Builtin_0 (B0_String ""))
  | T_Bool -> mk ty (B0_Builtin_0 B0_True)
  | T_Abstract _ -> mk ty (B0_Builtin_0 (B0_Integer Int32.zero))
  | T_Enum _ -> mk ty (B0_Builtin_0 (B0_Integer Int32.zero))
  | T_Array _ -> mk ty (B0_Array [])
  | T_Record lst ->
    let aux (id,ty) = (mk_lident exp0_loc id,get_default_value exp0_loc ty) in
    mk ty (B0_Record (List.map aux lst))

let get_mch_constant (c:(G.t_mch,G.t_concrete) T.symb) : t_constant option =
  let aux c_name =
    match to_b0_type c.T.sy_typ with
    | Some ty ->
      let loc = match c_name with
        | Intern lid -> lid.lid_loc
        | Extern (pkg,_) -> pkg.lid_loc
      in
      Some { c_name; c_type = ty;
             c_init = Init (get_default_value loc ty) }
    | None ->
      begin match c_name with
        | Intern lid ->
          Error.raise_exn lid.lid_loc
            ("The type of the constant '"^lid.lid_str^"' is '"^
             Btype.to_string c.T.sy_typ^"'. This is not a valid B0-type.")
        | Extern (pkg,id) ->
          Error.raise_exn pkg.lid_loc
            ("The type of the constant '"^id^"' is '"^
             Btype.to_string c.T.sy_typ^"'. This is not a valid B0-type.")
      end
  in
  match c.T.sy_src with
  | G.D_Machine loc -> aux (Intern (mk_lident loc c.T.sy_id))
  | G.D_Included_Or_Imported mch -> aux (Extern(mch,c.T.sy_id))
  | G.D_Seen _ -> None

let get_mch_variable (v:(G.t_mch,G.t_concrete) T.symb) : t_variable option =
  let aux v_name =
    Some { v_name;
           v_type=(match to_b0_type v.T.sy_typ with
               | Some ty -> ty
               | None -> assert false (*FIXME*))
         }
  in
  match v.T.sy_src with
  | G.D_Machine loc -> aux (Intern (mk_lident loc v.T.sy_id))
  | G.D_Included_Or_Imported mch -> aux (Extern(mch,v.T.sy_id))
  | G.D_Seen _ -> None

let get_mch_operation (op:(G.t_mch,V.t_mch_op) T.operation) : t_procedure =
  match op with
  | T.O_Specified { op_out; op_name; op_in } ->
    let to_arg v =
      { arg_name=Intern(mk_lident v.T.arg_loc v.T.arg_id);
        arg_type=(match to_b0_type v.T.arg_typ with
            | Some ty -> ty
            | None ->
              Error.raise_exn v.T.arg_loc 
                ("This is not a valid B0-expression. Its type is '"
                 ^Btype.to_string v.T.arg_typ^"'.")
          ) }
    in
    { p_name = op_name;
      p_is_local = false;
      p_args_in = List.map to_arg op_in;
      p_args_out = List.map to_arg op_out;
      p_body = Body { sub0_loc=op_name.lid_loc; sub0_desc=B0_Null} }
  | T.O_Promoted { op_out; op_name; op_in; op_source } ->
    let to_arg (str,ty) =
      { arg_name=Extern(op_source,str);
        arg_type=(match to_b0_type ty with
            | Some ty -> ty
            | None -> assert false (*FIXME*)) }
    in
    { p_name=op_name;
      p_is_local = false;
      p_args_in = List.map to_arg op_in;
      p_args_out = List.map to_arg op_out;
      p_body = Renames {q_nspace=Some op_source; q_id=op_name} }

let mch_to_package_exn (pkg_name:t_pkg_id) (mch:T.machine) : t_package  =
  { pkg_name;
    pkg_dependencies = get_dependencies mch.T.mch_sees [] [];
    pkg_types = get_mch_types mch.T.mch_concrete_sets mch.T.mch_abstract_sets;
    pkg_constants = Utils.filter_map get_mch_constant mch.T.mch_concrete_constants;
    pkg_variables = Utils.filter_map get_mch_variable mch.T.mch_concrete_variables;
    pkg_procedures = List.map get_mch_operation mch.T.mch_operations;
    pkg_init = None
  }

let to_package (pkg_name:t_pkg_id) (comp:T.component) : t_package Error.t_result =
  try match comp.T.co_desc with
    | T.Machine mch -> Ok (mch_to_package_exn pkg_name mch)
    | T.Refinement _ ->
      Error { Error.err_loc=comp.T.co_name.lid_loc;
              err_txt="Machine or Implementation expected. Found a refinement." }
    | T.Implementation imp -> Ok (imp_to_package_exn pkg_name imp)
  with
  | Error.Error err -> Error err
