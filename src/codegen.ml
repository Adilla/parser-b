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
    | B0_Integer of int
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
    | B0_Inequality of Syntax.inequality
    | B0_Product
    | B0_Difference
    | B0_Addition
    | B0_Division
    | B0_Modulo
    | B0_Power

  type qident = { q_nspace:Ident.t_pkg_id option; q_id:Ident.t }

  type t_array_element_type =
    | A_Int
    | A_Bool
    | A_String

  type t_b0_type =
    | T_Int
    | T_Bool
    | T_String
    | T_Abstract of qident
    | T_Array of int*t_array_element_type

  type t_exp0_desc =
    | B0_Ident of qident
    | B0_Builtin_0 of t_b0_constant
    | B0_Builtin_1 of t_b0_unary_op * t_b0_expr
    | B0_Builtin_2 of t_b0_binary_op * t_b0_expr * t_b0_expr
    | B0_Application of qident * t_b0_expr Nlist.t
    | B0_Array_Access of qident * t_b0_expr
    | B0_Array of t_b0_expr list

  and t_b0_expr =
    { exp0_loc: Utils.loc;
      exp0_type: t_b0_type;
      exp0_desc: t_exp0_desc }

  type t_constant_type =
    | Cst of t_b0_type
    | Fun of t_b0_type Nlist.t*t_b0_type

  type t_constant =
    { c_name: Ident.t;
      c_type: t_constant_type;
      c_init: t_b0_expr }

  type t_variable =
    { v_name: Ident.t;
      v_type: t_b0_type }

  type t_arg =
    { arg_loc: Utils.loc;
      arg_name: Ident.t;
      arg_type: t_b0_type }

  type t_sub0_desc =
    | B0_Null
    | B0_Affectation of (Ident.t*t_b0_expr) Nlist.t
    | B0_Array_Affectation of Ident.t*t_b0_expr Nlist.t*t_b0_expr
    | B0_IfThenElse of (t_b0_expr*t_b0_subst) Nlist.t * t_b0_subst option
    | B0_Case of t_b0_expr * (int Nlist.t*t_b0_subst) Nlist.t * t_b0_subst option
    | B0_Var of (Ident.t*t_b0_type) Nlist.t * t_b0_subst
    | B0_While of t_b0_expr * t_b0_subst
    | B0_CallUp of Ident.t list * qident * t_b0_expr list
    | B0_Sequencement of t_b0_subst * t_b0_subst

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
    | D_Enumerate of Ident.t list

  type t_type =
    { ty_name: Ident.t;
      ty_def: t_type_def }

  type t_package =
    { pkg_name: Ident.t_pkg_id;
      pkg_dependencies: Ident.t_pkg_id list;
      pkg_types: t_type list;
      pkg_constants: t_constant list;
      pkg_variables: t_variable list;
      pkg_procedures: t_procedure list;
      pkg_init: t_b0_subst option }

  open Syntax

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
      match e.exp_desc with
      | Application (e1,e2) -> aux e1 (e2::args)
      | _ -> (e,args)
    in aux e []

  let get_args e : _ Nlist.t =
    let rec aux lst e =
      match e.exp_desc with
      | Couple (_,e1,e2) -> aux (e2::lst) e1
      | _ -> e::lst
    in
    Nlist.from_list_exn (aux [] e)

  let is_int ty =
    match Btype.view ty with
    | Btype.T_Int -> true
    | _ -> false
   
  let mk_ident (lc:Utils.loc) (s:ident) : Ident.t =
    match Ident.make s with 
    | None -> Error.raise_exn lc ("The symbol '"^s^"' is not a valid identifier.")
    | Some id -> id

  let mk_pkg (lc:Utils.loc) (s:ident) : Ident.t_pkg_id =
    match Ident.make_pkg_id s with 
    | None -> Error.raise_exn lc ("The symbol '"^s^"' is not a valid package identifier.")
    | Some id -> id

  let get_qident (env:Global.t) (ctx:string list) (lc:Utils.loc) (id:ident) : qident =
    let aux = String.equal id in
    if List.exists aux ctx then
      { q_nspace=None; q_id=mk_ident lc id }
    else
      { q_nspace=Utils.map_opt (mk_pkg lc) (Global.get_symbol_source env id);
           q_id=mk_ident lc id }

  let get_op_qident (env:Global.t) (lc:Utils.loc) (id:ident) : qident =
    { q_nspace=Utils.map_opt (mk_pkg lc) (Global.get_op_source env id);
      q_id=mk_ident lc id }

  let rec to_b0_type (lc:Utils.loc) (env:Global.t) (ty:Btype.t) : t_b0_type =
    match Btype.view ty with
    | Btype.T_Int -> T_Int
    | Btype.T_Bool -> T_Bool
    | Btype.T_String -> T_String
    | Btype.T_Atomic s ->
        begin match Global.get_symbol_kind env s with
          | Some (Global.K_Concrete_Set _) -> T_Int
          | Some Global.K_Abstract_Set -> T_Abstract (get_qident env [] lc s)
          | _ -> Error.raise_exn lc ("Unknown type '"^s^"'.")
        end
    | Btype.T_Power ty0 ->
      begin match Btype.view ty0 with
        | Btype.T_Product (rg,tg) ->
          let tg = to_b0_type lc env tg in
          begin match Btype.view rg with
            | Btype.T_Atomic s ->
              begin match Global.get_symbol_kind env s with
                | Some (Global.K_Concrete_Set lst) ->
                  begin match tg with
                    | T_Int -> T_Array (List.length lst,A_Int)
                    | T_Bool -> T_Array (List.length lst,A_Bool)
                    | T_String -> T_Array (List.length lst,A_String)
                    | _ -> Error.raise_exn lc "Only arrays of Integers, Enumerates, Booleans or Strings are supported."
                  end
                | _ -> Error.raise_exn lc "Only arrays indexed by an concrete set are supported."
              end
            | _ -> Error.raise_exn lc "Only arrays indexed by an concrete set are supported."
          end
        | _ -> Error.raise_exn lc "Power types are not supported by the translator."
      end
    | Btype.T_Record lst -> Error.raise_exn lc "Records are not supported by the translator."
    | Btype.T_Product (t1,t2) -> Error.raise_exn lc "Product types are not supported by the translator."

  let get_pos (lst:ident list) (elt:ident) : int option =
    let rec aux i = function
    | [] -> None
    | hd::tl -> 
      if String.equal elt hd then Some i
      else aux (i+1) tl
    in aux 1 lst

  let check_completeness (env:Global.t) (ctx:ident list)(lst:(Utils.loc*ident*Btype.t*t_b0_expr) list) : (qident*t_b0_expr) list =
    let (fst_lc,elts,cs) = match lst with
      | [] -> assert false
      | (lc,id,ty,_)::tl ->
        begin match Btype.view ty with
        | Btype.T_Atomic s ->
          begin match Global.get_symbol_kind env s with
          | Some (Global.K_Concrete_Set elts) -> (lc,elts,s)
          | _ -> Error.raise_exn lc ("'"^id^"' is not part of a concrete set.")
          end
        | _ -> Error.raise_exn lc ("'"^id^"' is not part of a concrete set.")
        end
    in
    let arr_elt = Array.of_list elts in
    let arr_pre = Array.make (Array.length arr_elt) false in
    let lst = List.map (fun (lc,id,_,e) ->
      match get_pos elts id with
      | Some i -> ( arr_pre.(i-1) <- true; (get_qident env ctx lc id,e) )
      | None -> Error.raise_exn lc ("The symbol '"^id^"' is not part of the concrete set '"^cs^"'.")
    ) lst in
    let () = Array.iteri (fun i b ->
      if not b then Error.raise_exn fst_lc ("This array is incomplete. The field '"^ arr_elt.(i) ^"' is missing.")
    ) arr_pre
    in lst

  let get_enum_type (lc:Utils.loc) (env:Global.t) (ty:Btype.t) : qident option =
    match Btype.view ty with
    | Btype.T_Atomic s ->
        begin match Global.get_symbol_kind env s with
          | Some (Global.K_Concrete_Set _) -> Some (get_qident env [] lc s)
          | _ -> None
        end
    | _ -> None
       
  let is_int_array ty =
    match Btype.view ty with
    | Btype.T_Power ty ->
      begin match Btype.view ty with
        | Btype.T_Product (ty,_) ->
          begin match Btype.view ty with
            | Btype.T_Int -> true
            | _ -> false
          end
        | _ -> false
      end
    | _ -> false

  let rec list_init n v = if n<1 then [] else v::(list_init (n-1) v)
                                                 
  let get_all_args e =
    let rec aux accu e =
      match e.exp_desc with
      | Couple (_,e1,e2) -> aux (e2::accu) e1
      | _ -> e::accu
    in
    Nlist.from_list_exn (aux [] e)

  let get_pos2 (elts:ident list) (id:ident) : int option =
    let rec aux p = function
      | [] -> None
      | hd::tl when String.equal id hd -> Some p
      | _::tl -> aux (p+1) tl
    in
    aux 0 elts

  type ckind = C_Type | C_Function | C_Constant

  let is_valid_array_range env ty =
    match Btype.view ty with
    | Btype.T_Atomic s ->
      begin match Global.get_symbol_kind env s with
        | Some (Global.K_Concrete_Set _) -> true
        | _ -> false
      end
    | _ -> false

  let is_valid_array_target env ty =
    match Btype.view ty with
    | Btype.T_Int | Btype.T_Bool | Btype.T_String -> true
    | Btype.T_Atomic s ->
      begin match Global.get_symbol_kind env s with
        | Some (Global.K_Concrete_Set _) -> true
        | _ -> false
      end
    | _ -> false

  let get_constant_kind env (ty:Btype.t) : ckind =
    match Btype.view ty with
    | Btype.T_Power ty ->
      begin match Btype.view ty with
        | Btype.T_Product (rg,tg) ->
          if is_valid_array_range env rg && is_valid_array_target env tg then C_Constant
          else C_Function
        | _ -> C_Type
      end
    | _ -> C_Constant

  let rec to_b0_expr (env:Global.t) (ctx:ident list) (e:Inference.t_expression) : t_b0_expr =
    let add_lt exp0_desc =
      { exp0_loc = e.exp_loc;
        exp0_type = to_b0_type e.exp_loc env e.exp_typ;
        exp0_desc }
    in
    match e.exp_desc with
    | Ident id ->
      let enum_pos =
        if List.exists (String.equal id) ctx then None
        else
          match Btype.view e.exp_typ with
          | Btype.T_Atomic enum ->
            begin match Global.get_symbol_kind env enum with
              | Some (Global.K_Concrete_Set elts) -> get_pos2 elts id
              | _ -> None
            end
          | _ -> None
      in
      begin match enum_pos with
      | None -> add_lt (B0_Ident (get_qident env ctx e.exp_loc id))
      | Some pos -> add_lt (B0_Builtin_0 (B0_Integer pos)) 
      end
    | Builtin bi ->
      begin match to_b0_constant bi with
        | None -> Error.raise_exn e.exp_loc ("This is not a valid B0 constant.")
        | Some bi -> add_lt (B0_Builtin_0 bi)
      end
    | Pbool p -> pred_to_b0_expr env ctx p
    | Application (f,arg) ->
      begin match f.exp_desc with
        | Builtin bi ->
          let mk_bin_op op =
            match arg.exp_desc with
            | Couple (_,arg1,arg2) ->
              let arg1 = to_b0_expr env ctx arg1 in
              let arg2 = to_b0_expr env ctx arg2 in
              add_lt (B0_Builtin_2 (op,arg1,arg2))
            | _ -> Error.raise_exn e.exp_loc "This is not a valid B0 expression."
          in
          begin match bi with
            | Successor -> add_lt (B0_Builtin_2(B0_Addition,to_b0_expr env ctx arg,add_lt (B0_Builtin_0 (B0_Integer 1))))
            | Predecessor -> add_lt (B0_Builtin_2(B0_Difference,to_b0_expr env ctx arg,add_lt (B0_Builtin_0 (B0_Integer 1))))
            | Unary_Minus -> add_lt (B0_Builtin_1 (B0_Minus,to_b0_expr env ctx arg))
            | Product ->
              if is_int e.exp_typ then mk_bin_op B0_Product
              else add_lt (get_array_init env ctx arg)
            | Difference -> mk_bin_op B0_Difference
            | Addition -> mk_bin_op B0_Addition
            | Division -> mk_bin_op B0_Division
            | Modulo -> mk_bin_op B0_Modulo
            | Power -> mk_bin_op B0_Power
            | _ -> Error.raise_exn e.exp_loc "This is not a valid B0 operator."
          end
        | Ident id ->
          begin match get_constant_kind env f.exp_typ with
            | C_Type -> Error.raise_exn e.exp_loc "This is not a valid B0 application."
            | C_Function ->
              let args = get_all_args arg in
              let args = Nlist.map (to_b0_expr env ctx) args in
              add_lt (B0_Application(get_qident env ctx e.exp_loc id,args))
            | C_Constant ->
              let arg = to_b0_expr env ctx arg in
              add_lt (B0_Array_Access(get_qident env ctx e.exp_loc id,arg))
          end
        | _ -> Error.raise_exn e.exp_loc "This is not a valid B0 application."
      end
    | Extension nle ->
      let aux e = match e.exp_desc with
        | Couple (_,e1,e2) ->
          begin match (to_b0_expr env ctx e1).exp0_desc with
            | B0_Builtin_0 (B0_Integer i) -> (i,to_b0_expr env ctx e2)
            | _ -> Error.raise_exn e1.exp_loc "Ill-formed array."
          end
        | _ -> Error.raise_exn e.exp_loc "Ill-formed array."
      in
      let sz = match to_b0_type e.exp_loc env e.exp_typ with
        | T_Array (sz,_) -> sz
        | _ -> Error.raise_exn e.exp_loc "Ill-formed array."
      in
      let lst = Nlist.map aux nle in
      let lst = List.fast_sort (fun (a,_) (b,_) -> a-b) (Nlist.to_list lst) in
      let lst = List.map snd lst in
      if List.length lst = sz then add_lt (B0_Array lst)
      else Error.raise_exn e.exp_loc "Ill-formed array."
    | Dollar _ -> Error.raise_exn e.exp_loc "This is not a valid B0-expression (Dollar)."
    | Couple _ -> Error.raise_exn e.exp_loc "This is not a valid B0-expression (Couple)."
    | Sequence _ -> Error.raise_exn e.exp_loc "This is not a valid B0-expression (Sequence)."
    | Comprehension _ -> Error.raise_exn e.exp_loc "This is not a valid B0-expression (Comprehension)."
    | Binder _ -> Error.raise_exn e.exp_loc "This is not a valid B0-expression (Binder)."
    | Record_Field_Access _ -> Error.raise_exn e.exp_loc "This is not a valid B0-expression (Record access)."
    | Record _ -> Error.raise_exn e.exp_loc "This is not a valid B0-expression (Record)."
    | Record_Type _ -> Error.raise_exn e.exp_loc "This is not a valid B0-expression (Record type)."

  and get_array_init (env:Global.t) (ctx:ident list) (e:_ expression) : t_exp0_desc =
    match e.exp_desc with
    | Couple (_,e1,e2) ->
      begin
      let v = match e2.exp_desc with
        | Extension lst ->
          begin match Nlist.to_list lst with
            | [s] -> to_b0_expr env ctx s
            | _ -> Error.raise_exn e.exp_loc "Invalid array initialisation."
          end
        | _ -> Error.raise_exn e.exp_loc "Invalid array initialisation."
      in
      match e1.exp_desc with
      | Ident id ->
        begin match Global.get_symbol_kind env id with
          | None -> Error.raise_exn e1.exp_loc ("Unknown symbol '"^id^"'.")
          | Some (Global.K_Concrete_Set elts) -> B0_Array (List.map (fun _ -> v) elts)
          | Some _ ->
            Error.raise_exn e1.exp_loc ("Invalid array range. '"^id^"' is not a concrete set.")
        end
      | _ -> Error.raise_exn e.exp_loc "Invalid array initialisation."
    end
    | _ -> Error.raise_exn e.exp_loc "Invalid array initialisation."

  and pred_to_b0_expr (env:Global.t) (ctx:ident list) (p:_ predicate) : t_b0_expr =
    let add_loc exp0_desc = { exp0_loc = p.prd_loc; exp0_type = T_Bool; exp0_desc } in
    match p.prd_desc with
    | P_Builtin Btrue -> add_loc (B0_Builtin_0 B0_True)
    | P_Builtin Bfalse -> add_loc (B0_Builtin_0 B0_False)
    | Binary_Prop (op,p1,p2) ->
      begin match prop_bop_to_bop op with
        | None -> Error.raise_exn p.prd_loc "This is not a valid B0 binary operator."
        | Some op -> add_loc (B0_Builtin_2 (op,pred_to_b0_expr env ctx p1,
                                            pred_to_b0_expr env ctx p2))
      end
    | Binary_Pred (op,e1,e2) ->
      begin match pred_bop_to_bop op with
        | None -> Error.raise_exn p.prd_loc "This is not a valid B0 binary operator."
        | Some (B0_Equality|B0_Disequality as op) ->
          let e1 = to_b0_expr env ctx e1 in
          let e2 = to_b0_expr env ctx e2 in
          add_loc (B0_Builtin_2 (op,e1,e2))
        | Some (B0_Inequality _ as op) ->
          let e1 = to_b0_expr env ctx e1 in
          let e2 = to_b0_expr env ctx e2 in
          add_loc (B0_Builtin_2 (op,e1,e2))
        | Some _ -> assert false
      end
    | Negation p -> add_loc (B0_Builtin_1 (B0_Negation,pred_to_b0_expr env ctx p))
    | Universal_Q _ -> Error.raise_exn p.prd_loc "This is not a valid B0-expression (Universal quantifier)."
    | Existential_Q _ -> Error.raise_exn p.prd_loc "This is not a valid B0-expression (Existensial quantifier)."

  let rec get_exp_nle e =
    match e.exp_desc with
    | Couple (_,x,y) -> Nlist.cons x (get_exp_nle y)
    | _ -> Nlist.make1 e

  let rec merge_map2 vars exps : _ Nlist.t option =
    try
      let aux v e = (v,e) in
      let lst = List.map2 aux (Nlist.to_list vars) (Nlist.to_list exps) in
      Some (Nlist.from_list_exn lst)
    with
      Invalid_argument _ -> None

  let get_enum (env:Global.t) (ctx:ident list) (e:_ expression) : int =
    match e.exp_desc with
    | Ident id ->
      begin match Btype.view e.exp_typ with
        | Btype.T_Atomic s ->
          begin match Global.get_symbol_kind env s with
            | Some (Global.K_Concrete_Set elts) ->
              begin match get_pos2 elts id with
                | Some i -> i
                | None -> assert false
              end
            | _ -> Error.raise_exn e.exp_loc "Enumerate expected."
          end
        | _ -> Error.raise_exn e.exp_loc "Enumerate expected."
      end
    | _ -> Error.raise_exn e.exp_loc "Enumerate expected."

  let rec to_b0_subst (env:Global.t) (ctx:ident list) (stmt:Inference.t_substitution) : t_b0_subst =
    let add_loc sub0_desc = { sub0_loc=stmt.sub_loc; sub0_desc } in
    match stmt.sub_desc with
    | Skip -> add_loc B0_Null
    | Affectation (vars,e) ->
      let e_nle = get_exp_nle e in
      begin match merge_map2 vars e_nle with
        | None -> Error.raise_exn stmt.sub_loc "Ill-formed affectation."
        | Some nle ->
          let nle = Nlist.map (fun (x,y) -> (mk_ident x.var_loc x.var_id,to_b0_expr env ctx y)) nle in
          add_loc (B0_Affectation nle)
      end
    | Assert (_,s) -> to_b0_subst env ctx s
    | IfThenElse (nle,def) ->
      let aux (c,s) =
        let c = pred_to_b0_expr env ctx c in
        let s = to_b0_subst env ctx s in
        (c,s)
      in
      let nle = Nlist.map aux nle in
      let def = match def with
        | None -> None
        | Some s -> Some (to_b0_subst env ctx s)
      in
      add_loc (B0_IfThenElse (nle,def))
    | Case (e,cases,def) ->
      let e = to_b0_expr env ctx e in
      let aux (lst,s) =
        let lst = Nlist.map (get_enum env ctx) lst in
        let s = to_b0_subst env ctx s in
        (lst,s)
      in
      let cases = Nlist.map aux cases in
      let def = match def with
        | None -> None
        | Some s -> Some (to_b0_subst env ctx s)
      in
      add_loc (B0_Case (e,cases,def))
    | Var (vars,s) ->
      let ctx =  (List.map (fun v -> v.var_id) (Nlist.to_list vars))@ctx in
      let ss = to_b0_subst env ctx s in
      let aux v = (mk_ident v.var_loc v.var_id,to_b0_type v.var_loc env v.var_typ) in
      let vars = Nlist.map aux vars in
      add_loc (B0_Var (vars,ss))
    | CallUp (outs,f,args) ->
      let outs = List.map (fun v -> mk_ident v.var_loc v.var_id) outs in
      let args = List.map (to_b0_expr env ctx) args in
      add_loc (B0_CallUp (outs,get_op_qident env f.lid_loc f.lid_str,args))
    | While (cond,body,_,_) ->
      let cond = pred_to_b0_expr env ctx cond in
      let body = to_b0_subst env ctx body in
      add_loc (B0_While (cond,body))
    | Sequencement (s1,s2) ->
      let s1 = to_b0_subst env ctx s1 in
      let s2 = to_b0_subst env ctx s2 in
      add_loc (B0_Sequencement (s1,s2))
    | Function_Affectation (f,args,e) ->
      let f = mk_ident f.var_loc f.var_id in
      let args = Nlist.map (to_b0_expr env ctx) args in
      let e = to_b0_expr env ctx e in
      add_loc (B0_Array_Affectation (f,args,e))
    | Record_Affectation _ -> Error.raise_exn stmt.sub_loc "Records are not supported by the translator."
    | Pre _ -> Error.raise_exn stmt.sub_loc "This is not a valid B0-substitution (Precondition)."
    | Choice _ -> Error.raise_exn stmt.sub_loc "This is not a valid B0-substitution (Choice)."
    | Select _ -> Error.raise_exn stmt.sub_loc "This is not a valid B0-substitution (Select)."
    | Any _ -> Error.raise_exn stmt.sub_loc "This is not a valid B0-substitution (Any)."
    | Let _ -> Error.raise_exn stmt.sub_loc "This is not a valid B0-substitution (Let)."
    | BecomesElt _ -> Error.raise_exn stmt.sub_loc "This is not a valid B0-substitution (::)."
    | BecomesSuch _ -> add_loc B0_Null (*FIXME warning*)
    | Parallel _ -> Error.raise_exn stmt.sub_loc "This is not a valid B0-substitution (||)."

  let get_dependencies sees imports extends : Ident.t_pkg_id list =
    let lst1 = match sees with
      | None -> []
      | Some (_,nle) ->
        List.map (fun x -> mk_pkg x.lid_loc x.lid_str) (Nlist.to_list nle)
    in
    let aux x = match x.mi_params with
      | [] -> mk_pkg x.mi_mch.lid_loc x.mi_mch.lid_str
      | z::_ -> Error.raise_exn z.exp_loc "Not implemented (Parameters)."
    in
    let lst2 = match imports with
      | None -> []
      | Some (_,nle) -> List.map aux (Nlist.to_list nle)
    in
    let lst3 = match extends with
      | None -> []
      | Some (_,nle) -> List.map aux (Nlist.to_list nle)
    in
    lst1@lst2@lst3

  let get_enumerates (env:Global.t) : t_type list =
    let aux accu id kd src ty =
      match kd with
      | Global.K_Concrete_Set elts ->
        begin match src with
          | Global.S_Current_Mch_Only lc
          | Global.S_Current_And_Refined_Mch (lc,_) ->
            { ty_name=mk_ident lc id;
              ty_def=D_Enumerate (List.map (mk_ident lc) elts) }::accu
          | Global.S_Included_Mch_Only _
          | Global.S_Included_And_Refined_Mch _
          | Global.S_Refined_Mch_Only _
          | Global.S_Seen_Mch_Only _
          | Global.S_Imported_Mch_Only _
          | Global.S_Current_And_Imported_Mch _
          | Global.S_Imported_And_Refined_Mch _
          | Global.S_Current_Imported_And_Refined_Mch _ -> accu
        end
      | _ -> accu
    in
    Global.fold_symbols aux env []

  let get_type_decl (env:Global.t) (ty_name:Ident.t) (e:_ expression) : t_type =
    let ty_def = match e.exp_desc with
      | Ident id ->
        begin match Global.get_symbol_kind env id with
        | None -> Error.raise_exn e.exp_loc ("Unknown type '"^id^"'.")
        | Some Global.K_Abstract_Set -> D_Alias (get_qident env [] e.exp_loc id)
        | Some _ -> Error.raise_exn e.exp_loc "This is not a valid type definition. An abstract set is expected here."
        end
      | _ ->
        begin match Btype.view e.exp_typ with
          | Btype.T_Power ty ->
            begin match Btype.view ty with
              | Btype.T_Int -> D_Int
              | _ -> Error.raise_exn e.exp_loc "This is not a valid type definition."
            end
          | _ -> Error.raise_exn e.exp_loc "This is not a valid type definition."
        end
    in
    { ty_name; ty_def }

  let get_type_decl_list (env:Global.t) (imp:_ implementation_desc) : t_type list =
    match imp.imp_values with
    | None -> []
    | Some (_,nle) ->
      let aux lst (v,e) =
        match Global.get_symbol_kind env v.var_id with
        | None -> Error.raise_exn e.exp_loc ("Unknown symbol '"^v.var_id^"'.")
        | Some Global.K_Abstract_Set -> (get_type_decl env (mk_ident v.var_loc v.var_id) e)::lst
        | Some Global.K_Concrete_Constant -> lst
        | Some _ -> Error.raise_exn e.exp_loc ("Unexpected symbol '"^v.var_id^"'. Abstract set or concrete constant expected.")
      in
      List.rev (List.fold_left aux [] (Nlist.to_list nle))


  let get_constants (env:Global.t) (imp:_ implementation_desc) : t_constant list =
    match imp.imp_values with
    | None -> []
    | Some (_,nle) ->
      let aux lst (v,e) =
        match Global.get_symbol_kind env v.var_id with
        | None -> Error.raise_exn e.exp_loc ("Unknown symbol '"^v.var_id^"'.")
        | Some Global.K_Concrete_Constant ->
          begin match get_constant_kind env v.var_typ with
            | C_Type -> lst
            | C_Function | C_Constant ->
              let c_type = to_b0_type v.var_loc env v.var_typ in
              { c_name = mk_ident v.var_loc v.var_id;
                c_type = Cst c_type;
                c_init = to_b0_expr env [] e }::lst
          end
        | Some Global.K_Abstract_Set -> lst
        | Some _ -> Error.raise_exn e.exp_loc ("Unexpected symbol '"^v.var_id^"'. Abstract set or concrete constant expected.")
      in
      List.rev (List.fold_left aux [] (Nlist.to_list nle))

  let get_variables (env:Global.t) : t_variable list =
    let aux accu id kd src ty =
      match kd with
      | Global.K_Concrete_Variable ->
        begin match src with
          | Global.S_Current_Mch_Only lc
          | Global.S_Current_And_Refined_Mch (lc,_) ->
            { v_name=mk_ident lc id; v_type=to_b0_type lc env ty}::accu
          | Global.S_Included_Mch_Only _
          | Global.S_Included_And_Refined_Mch _
          | Global.S_Refined_Mch_Only _
          | Global.S_Seen_Mch_Only _
          | Global.S_Imported_Mch_Only _
          | Global.S_Current_And_Imported_Mch _
          | Global.S_Imported_And_Refined_Mch _
          | Global.S_Current_Imported_And_Refined_Mch _ -> accu
        end
      | _ -> accu
    in
    Global.fold_symbols aux env []

  let get_procedures (env:Global.t) (imp:_ implementation_desc) : t_procedure list =
    match imp.imp_operations with
    | None -> []
    | Some (_,nle) ->
      let to_arg op_name v = { arg_name=mk_ident v.var_loc v.var_id;
                       arg_loc=v.var_loc;
                       arg_type=to_b0_type v.var_loc env v.var_typ}
      in
      let aux (pr:_ operation) =
        let ctx =  (List.map (fun v -> v.var_id) pr.op_out)@(List.map (fun v -> v.var_id) pr.op_in) in
        { p_name = mk_ident pr.op_name.lid_loc pr.op_name.lid_str;
          p_is_local = Global.is_operation_local env pr.op_name.lid_str;
          p_args_in = List.map (to_arg pr.op_name.lid_str) pr.op_in;
          p_args_out = List.map (to_arg pr.op_name.lid_str) pr.op_out;
          p_body = Body (to_b0_subst env ctx pr.op_body) }
      in
      List.map aux (Nlist.to_list nle)

  let get_promoted_procedures (env:Global.t) (imp:_ implementation_desc) : t_procedure list =
    let aux (accu:t_procedure list) (id:ident) (src:Global.t_op_source) (top:Global.t_op_type) : t_procedure list =
      match src with
      | Global.OS_Imported_And_Promoted (lc,_) | Global.OS_Imported_Promoted_And_Refined (lc,_,_) ->
        let to_arg (x,ty)= { arg_name=mk_ident Utils.dloc x; arg_loc=Utils.dloc; arg_type=to_b0_type Utils.dloc env ty } in
        { p_name = mk_ident lc id;
          p_is_local = false;
          p_args_in = List.map to_arg top.Global.args_in;
          p_args_out = List.map to_arg top.Global.args_out;
          p_body = Renames (get_op_qident env lc id) }::accu
      | _ -> accu
    in
    Global.fold_operations aux env []

  let get_mch_types mch =
    match mch.mch_sets with
    | None -> []
    | Some (_,lst) ->
      let aux = function
        | Abstract_Set id -> { ty_name=mk_ident id.var_loc id.var_id; ty_def=D_Int }
        | Concrete_Set (id,elts) ->
          { ty_name=mk_ident id.var_loc id.var_id;
            ty_def=D_Enumerate (List.map (fun v -> mk_ident v.var_loc v.var_id) elts) }
      in
      List.map aux (Nlist.to_list lst)

  let rec init_list n v =
    if n < 1 then []
    else v::(init_list (n-1) v)

  let rec get_default_value (env:Global.t) (lc:Utils.loc) (ty:t_b0_type) : t_b0_expr =
    let mk exp0_type exp0_desc = { exp0_loc=lc; exp0_desc; exp0_type } in
    match ty with
    | T_Int -> mk ty (B0_Builtin_0 (B0_Integer 0))
    | T_String -> mk ty (B0_Builtin_0 (B0_String ""))
    | T_Bool -> mk ty (B0_Builtin_0 B0_True)
    | T_Abstract s -> mk ty (B0_Builtin_0 (B0_Integer 0))
    | T_Array (sz,tg) ->
      let tg = match tg with
        | A_Int -> T_Int
        | A_Bool -> T_Bool
        | A_String -> T_String
      in
      let v = get_default_value env lc tg in
      mk ty (B0_Array (init_list sz v))

let flatten_product (pr:Btype.t) : Btype.t Nlist.t =
  let rec aux (accu:Btype.t list) (ty:Btype.t) : Btype.t list =
    match Btype.view ty with
    | Btype.T_Product (t1,t2) -> aux (t2::accu) t1
    | _ -> ty::accu
  in
  Nlist.from_list_exn (aux [] pr)

  let to_b0_fun_type lc env ty =
    match Btype.view ty with
    | Btype.T_Power ty ->
      begin match Btype.view ty with
        | Btype.T_Product (rg,tg) ->
          (try Some (Nlist.map (to_b0_type lc env) (flatten_product rg),to_b0_type lc env tg)
          with Error.Error _ -> None)
        | _ -> None
      end
    | _ -> None

  let get_mch_constants (env:Global.t) (mch:_ machine_desc) : t_constant list =
    match mch.mch_concrete_constants with
    | None -> []
    | Some (_,nle) ->
      let aux lst v =
        match Global.get_symbol_kind env v.var_id with
        | None -> Error.raise_exn v.var_loc ("Unknown symbol '"^v.var_id^"'.")
        | Some Global.K_Concrete_Constant ->
          begin match get_constant_kind env v.var_typ with
            | C_Type -> lst
            | C_Function ->
              begin match to_b0_fun_type v.var_loc env v.var_typ with
                | Some (t_args,t_res) ->
                  { c_name = mk_ident v.var_loc v.var_id;
                    c_type = Fun (t_args,t_res);
                    c_init = get_default_value env v.var_loc t_res }::lst
                | None -> lst (*FIXME*)
              end
            | C_Constant ->
              let c_type = to_b0_type v.var_loc env v.var_typ in
              { c_name = mk_ident v.var_loc v.var_id;
                c_type = Cst c_type;
                c_init = get_default_value env v.var_loc c_type }::lst
          end
        | Some _ -> Error.raise_exn v.var_loc "Concrete constant expected."
      in
      List.rev (List.fold_left aux [] (Nlist.to_list nle))

  let get_mch_operations (env:Global.t) (mch:_ machine_desc) : t_procedure list =
    match mch.mch_operations with
    | None -> []
    | Some (_,nle) ->
      let to_arg op_name v= { arg_name=mk_ident v.var_loc v.var_id; arg_loc=v.var_loc; arg_type=to_b0_type v.var_loc env v.var_typ } in
      let aux op =
        { p_name = mk_ident op.op_name.lid_loc op.op_name.lid_str;
          p_is_local = false;
          p_args_in = List.map (to_arg op.op_name.lid_str) op.op_in;
          p_args_out = List.map (to_arg op.op_name.lid_str) op.op_out;
          p_body = Body { sub0_loc=op.op_name.lid_loc; sub0_desc=B0_Null} }
      in
      List.map aux (Nlist.to_list nle)

  let to_package (env:Global.t) (pkg_name:Ident.t_pkg_id) (comp:Typechecker.t_component) : t_package Error.t_result =
    try match comp.co_desc with
    | Implementation imp ->
      Ok { pkg_name;
           pkg_dependencies = get_dependencies imp.imp_sees imp.imp_imports imp.imp_extends;
           pkg_types = (get_enumerates env)@(get_type_decl_list env imp);
           pkg_constants = get_constants env imp;
           pkg_variables = get_variables env;
           pkg_procedures = (get_promoted_procedures env imp)@(get_procedures env imp);
           pkg_init = match imp.imp_initialisation with
             | None -> None
             | Some (_,s) -> Some (to_b0_subst env [] s)
         }
    | Machine mch ->
      Ok { pkg_name;
           pkg_dependencies = get_dependencies mch.mch_sees None None;
           pkg_types = get_mch_types mch;
           pkg_constants = get_mch_constants env mch;
           pkg_variables = get_variables env;
           pkg_procedures = get_mch_operations env mch;
           pkg_init = None
         }
    | Refinement _ -> Error { Error.err_loc=comp.co_loc; err_txt="Machine or Implementation expected." }
    with
    | Error.Error err -> Error err

end

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
  module SSet = Set.Make(String)
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
