open Utils
open Syntax
open Btype

(* *****************************************************************************
 * Global contexts 
 * ************************************************************************** *)

type t_kind = 
  | K_Abstract_Variable | K_Concrete_Variable
  | K_Abstract_Constant | K_Concrete_Constant
  | K_Abstract_Set | K_Concrete_Set
  | K_Enumerate

type t_source =
  | From_Current_Mch of bool
  | From_Seen_Mch of p_lident
  | From_Refined_Mch of p_lident
  | From_Imported_Mch of p_lident

type t_symb = { loc:loc; typ:btype; kind:t_kind; }
type t_op  = { loc:loc; args_in: (string*btype) list; args_out: (string*btype) list; }

module MachineInterface :
sig
  type t
  val make : (string*t_symb) list -> (string*t_op) list -> t
  val get_symbols : t -> (string*t_symb) list
  val get_operations : t -> (string*t_op) list
end = struct
  type t = (string*t_symb) list * (string*t_op) list

  let make l1 l2 = (l1,l2)
  let get_symbols = fst
  let get_operations = snd
end

module Global : 
  sig
    type t
    val create : unit -> t
    val get_symbol : t -> ident -> (t_symb*t_source) option
    val get_operation : t -> ident -> (t_op*t_source) option
    val add_symbol : t -> loc -> ident -> btype -> t_kind -> unit
    val add_operation : t -> loc -> ident -> (ident*btype) list -> (ident*btype) list -> bool -> unit
    val mem_symbol : t -> ident -> bool
    val mem_operation : t -> ident -> bool
    val redefine_operation : t -> ident -> unit
    val redeclare_symbol : t -> ident -> unit
    val promote_operation : t -> loc -> ident -> bool
    val load_interface : t -> MachineInterface.t -> t_source -> unit
    val to_interface : t -> MachineInterface.t
  end = struct

  type t = { symb:(string,t_symb*t_source) Hashtbl.t; ops:(string,t_op*t_source) Hashtbl.t }

  let create () : t = { symb=Hashtbl.create 47; ops=Hashtbl.create 47 }

  let get_symbol (env:t) (id:ident) : (t_symb*t_source) option =
    try Some (Hashtbl.find env.symb id)
    with Not_found -> None

  let get_operation (env:t) (id:ident) : (t_op*t_source) option =
    try Some (Hashtbl.find env.ops id)
    with Not_found -> None

  let add_symbol (env:t) (lc:loc) (id:ident) (typ:btype) (kind:t_kind) : unit =
    Hashtbl.add env.symb id ({ loc=lc; typ; kind; }, From_Current_Mch false)

  let mem_symbol (env:t) (id:ident) : bool = Hashtbl.mem env.symb id

  let add_operation (env:t) (loc:loc) (id:ident) (args_in:(ident*btype) list) (args_out:(ident*btype) list) (is_local:bool) : unit =
    Hashtbl.add env.ops id ( { loc; args_in; args_out; }, From_Current_Mch is_local)

  let mem_operation (env:t)(id:ident) : bool = Hashtbl.mem env.ops id

  let redefine_operation (env:t) (id:ident) : unit =
    if Hashtbl.mem env.ops id then
      let (op,_) = Hashtbl.find env.ops id in
      Hashtbl.replace env.ops id (op,From_Current_Mch false)
    else
      failwith "Internal error (Global.redefine_operation)."

  let redeclare_symbol (env:t) (id:ident) : unit =
    if Hashtbl.mem env.symb id then
      let (s,_) = Hashtbl.find env.symb id in
      Hashtbl.replace env.symb id (s,From_Current_Mch false)
    else
      failwith "Internal error (Global.redeclare_symbol)."

  let promote_operation (env:t) (lc:loc) (id:ident) : bool =
    if Hashtbl.mem env.ops id then
      let (op,src) = Hashtbl.find env.ops id in
      match src with
      | From_Imported_Mch _ -> ( Hashtbl.replace env.ops id (op,From_Current_Mch false); true )
      | _ -> false
    else
      false

  let load_interface (env:t) (itf:MachineInterface.t) (source:t_source) : unit =
    let symbs = MachineInterface.get_symbols itf in
    let ops = MachineInterface.get_operations itf in
    List.iter (
      fun (x,s) -> Hashtbl.add env.symb x (s,source)
    ) symbs;
    List.iter (
      fun (x,s) -> Hashtbl.add env.ops x (s,source)
    ) ops

  let to_interface (env:t) : MachineInterface.t =
    let aux1 x (symb,src) lst =
      match src with
      | From_Current_Mch _ -> (x,symb)::lst
      | From_Refined_Mch _ ->
       begin match symb.kind with
         | K_Concrete_Constant
         | K_Concrete_Variable
         | K_Concrete_Set
         | K_Abstract_Set
         | K_Enumerate -> (x,symb)::lst
         | _ -> lst
       end
      | From_Imported_Mch _ -> lst
      | From_Seen_Mch _ -> lst
    in
    let aux2 x (op,src) lst =
      match src with
      | From_Current_Mch is_local -> if is_local then lst else (x,op)::lst
      | From_Seen_Mch _ -> lst
      | From_Refined_Mch _ -> lst
      | From_Imported_Mch _ -> lst
    in
    let lst1 = Hashtbl.fold aux1 env.symb [] in
    let lst2 = Hashtbl.fold aux2 env.ops [] in
    MachineInterface.make lst1 lst2

end

(* *****************************************************************************
 * Local contexts 
 * ************************************************************************** *)

module Local :
sig
  type t
  val create : unit -> t
  val declare : t -> ident -> t * opn typ
  val add : t -> ident -> opn typ -> t
  val get : t -> ident -> (opn typ) option
  val iter : (string -> opn typ -> unit) -> t -> unit

  val new_meta : t -> opn typ
  val get_stype : t -> opn typ -> opn typ -> (opn typ) option
  val normalize : t -> opn typ -> opn typ
  val close : t -> opn typ -> btype option
end = struct

  module M = Map.Make(
    struct
      type t = string
      let compare = String.compare
    end )

  type t = { map:opn typ M.t; unif:Unif.t }

  let create () = { map=M.empty; unif=Unif.create () }

  let declare (ctx:t) (id:ident) : t * opn typ =
    let mt = Unif.new_meta ctx.unif in
    ( { map=M.add id mt ctx.map; unif=ctx.unif }, mt )

  let add (ctx:t) (id:ident) (ty:opn typ) : t =
    { map=M.add id ty ctx.map; unif=ctx.unif }

  let get (ctx:t) (id:ident) : (opn typ) option =
    try Some (M.find id ctx.map)
    with Not_found -> None

  let new_meta ctx = Unif.new_meta ctx.unif 
  let get_stype ctx = Unif.get_stype ctx.unif 
  let normalize ctx = Unif.normalize ctx.unif 
  let iter f ctx = M.iter f ctx.map

  let close (ctx:t) (ty:opn typ) : btype option =
    Btype.close (Unif.normalize ctx.unif ty)
end

(* *****************************************************************************
 * Type Inference (open types)
 * ************************************************************************** *)

type t_var0 = (Utils.loc,opn_btype) var 
type t_expression0 = (Utils.loc,opn_btype) expression
type t_predicate0 = (Utils.loc,opn_btype) predicate
type t_substitution0 = (Utils.loc,opn_btype) substitution

let mk_expr exp_loc exp_typ exp_desc = { exp_loc; exp_typ; exp_desc }
let mk_pred prd_loc prd_desc = { prd_loc; prd_desc }
let mk_subst sub_loc sub_desc = { sub_loc; sub_desc;  }

let declare_list (ctx:Local.t) (lst:p_var list) : Local.t * t_var0 list =
  let (ctx,tvars) = List.fold_left
      (fun (ctx,tvars) v ->
         let (ctx,var_typ) = Local.declare ctx v.var_id in
         ( ctx, { var_loc=v.var_loc; var_typ; var_id=v.var_id }::tvars )
      ) (ctx,[]) lst
  in
  (ctx,List.rev tvars)

let declare_nelist (ctx:Local.t) (hd,tl:p_var Utils.non_empty_list) : Local.t * t_var0 Utils.non_empty_list =
  let (ctx,lst) = declare_list ctx (hd::tl) in
  (ctx,(List.hd lst,List.tl lst))

let ids_to_product (ctx:Local.t) (x,xlst:p_var Utils.non_empty_list) : opn_btype =
  let aux pr v =
    match Local.get ctx v.var_id with
    | None -> assert false
    | Some ty -> T_Product (pr,ty)
  in
  match Local.get ctx x.var_id with
  | None -> assert false
  | Some ty -> List.fold_left aux ty xlst

let get_builtin_type_exn (ctx:Local.t) (lc:Utils.loc) : e_builtin -> opn_btype = function
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
    | Empty_Set -> T_Power (Local.new_meta ctx)
    | Empty_Seq -> type_of_unary_fun t_int (Local.new_meta ctx)
    (* Arithmetic or Set operator *)
    | Product  -> assert false
    | Difference -> assert false
    (* Operations on sets *)
    | Interval  -> type_of_binary_fun t_int t_int (T_Power t_int)
    | Intersection | Union  ->
      let t_set = T_Power (Local.new_meta ctx) in
      type_of_binary_fun t_set t_set t_set
    | First_Projection ->
      let mt1 = Local.new_meta ctx in
      let mt2 = Local.new_meta ctx in
      type_of_binary_fun (T_Power mt1) (T_Power mt2)
        (type_of_unary_fun (T_Product (mt1,mt2)) mt1)
    | Second_Projection ->
      let mt1 = Local.new_meta ctx in
      let mt2 = Local.new_meta ctx in
      type_of_binary_fun (T_Power mt1) (T_Power mt2)
        (type_of_unary_fun (T_Product (mt1,mt2)) mt2)
    | Parallel_Product ->
      let mt1 = Local.new_meta ctx in
      let mt2 = Local.new_meta ctx in
      let mt3 = Local.new_meta ctx in
      let mt4 = Local.new_meta ctx in
      type_of_binary_fun (type_of_unary_fun mt1 mt2) (type_of_unary_fun mt3 mt4)
        (T_Power (T_Product (T_Product (mt1,mt3),T_Product (mt2,mt4))))
    | Direct_Product ->
      let mt1 = Local.new_meta ctx in
      let mt2 = Local.new_meta ctx in
      let mt3 = Local.new_meta ctx in
      type_of_binary_fun (type_of_unary_fun mt1 mt2) (type_of_unary_fun mt1 mt3)
        (T_Power (T_Product (mt1,T_Product (mt2,mt3))))
    | Cardinal  -> type_of_unary_fun (T_Power (Local.new_meta ctx)) t_int
    | Power_Set _ ->
      let t_set = T_Power (Local.new_meta ctx) in
      type_of_unary_fun t_set (T_Power t_set)
    | G_Intersection | G_Union  ->
      let t_set = T_Power (Local.new_meta ctx) in
      type_of_unary_fun (T_Power t_set) t_set
    (* Operations on relations *)
    | Composition ->
      let ty1 = Local.new_meta ctx in
      let ty2 = Local.new_meta ctx in
      let ty3 = Local.new_meta ctx in
      type_of_binary_fun (type_of_unary_fun ty1 ty2) (type_of_unary_fun ty2 ty3) (type_of_unary_fun ty1 ty3)
    | Iteration ->
      let mt = Local.new_meta ctx in
      type_of_binary_fun (type_of_unary_fun mt mt) t_int (type_of_unary_fun mt mt)
    | Image  ->
      let t_arg = Local.new_meta ctx in
      let t_res = Local.new_meta ctx in
      type_of_binary_fun (type_of_unary_fun t_arg t_res) (T_Power t_arg) (T_Power t_res)
    | Domain_Restriction
    | Domain_Soustraction ->
      let mt1 = Local.new_meta ctx in
      let mt2 = Local.new_meta ctx in
      let ty_rel = type_of_unary_fun mt1 mt2 in
      let ty_dom = T_Power mt1 in
      type_of_binary_fun ty_dom ty_rel ty_rel
    | Codomain_Restriction
    | Codomain_Soustraction ->
      let mt1 = Local.new_meta ctx in
      let mt2 = Local.new_meta ctx in
      let ty_rel = type_of_unary_fun mt1 mt2 in
      let ty_ran = T_Power mt2 in
      type_of_binary_fun ty_rel ty_ran ty_rel
    | Surcharge  ->
      let ty_f = type_of_unary_fun (Local.new_meta ctx) (Local.new_meta ctx) in
      type_of_binary_fun ty_f ty_f ty_f
    | Relations | Functions _ ->
      let mt1 = Local.new_meta ctx in
      let mt2 = Local.new_meta ctx in
      type_of_binary_fun (T_Power mt1) (T_Power mt2) (T_Power (type_of_unary_fun mt1 mt2))
    | Identity_Relation  ->
      let mt = Local.new_meta ctx in
      type_of_unary_fun (T_Power mt) (type_of_unary_fun mt mt)
    | Inverse_Relation  ->
      let mt1 = Local.new_meta ctx in
      let mt2 = Local.new_meta ctx in
      type_of_unary_fun (type_of_unary_fun mt1 mt2) (type_of_unary_fun mt2 mt1)
    | Closure | Transitive_Closure ->
      let mt = Local.new_meta ctx in
      type_of_unary_fun (type_of_unary_fun mt mt) (type_of_unary_fun mt mt)
    | Domain  ->
      let t_arg = Local.new_meta ctx in
      let t_res = Local.new_meta ctx in
      type_of_unary_fun (type_of_unary_fun t_arg t_res) (T_Power t_arg)
    | Range  ->
      let t_arg = Local.new_meta ctx in
      let t_res = Local.new_meta ctx in
      type_of_unary_fun (type_of_unary_fun t_arg t_res) (T_Power t_res)
    | Fnc  ->
      let t_arg = Local.new_meta ctx in
      let t_res = Local.new_meta ctx in
      T_Power(type_of_unary_fun t_arg t_res)
    | Rel  ->
      let t_arg = Local.new_meta ctx in
      let t_res = Local.new_meta ctx in
      type_of_unary_fun (type_of_unary_fun t_arg (T_Power t_res)) (type_of_unary_fun t_arg t_res)
    (* Sequence operators *)
    | Sequence_Set _ ->
      let mt = Local.new_meta ctx in
      type_of_unary_fun (T_Power mt) (T_Power (type_of_sequence mt))
    | Size  -> type_of_unary_fun (type_of_sequence (Local.new_meta ctx)) t_int 
    | First | Last  ->
      let mt = Local.new_meta ctx in
      type_of_unary_fun (type_of_sequence mt) mt
    | Reverse | Front | Tail ->
      let t_seq = type_of_sequence (Local.new_meta ctx) in
      type_of_unary_fun t_seq t_seq
    | Concatenation ->
      let t_seq = type_of_sequence (Local.new_meta ctx) in
      type_of_binary_fun t_seq t_seq t_seq
    | Head_Insertion ->
      let mt = Local.new_meta ctx in
      let t_seq = type_of_sequence mt in
      type_of_binary_fun mt t_seq t_seq
    | Tail_Insertion ->
      let mt = Local.new_meta ctx in
      let t_seq = type_of_sequence mt in
      type_of_binary_fun t_seq mt t_seq
    | Head_Restriction | Tail_Restriction  ->
      let t_seq = type_of_sequence (Local.new_meta ctx) in
      type_of_binary_fun t_seq t_int t_seq
    | G_Concatenation  ->
      let t_seq = type_of_sequence (Local.new_meta ctx) in
      type_of_unary_fun (type_of_sequence t_seq) t_seq
    | Tree | Btree | Const | Top | Sons | Prefix | Postfix | SizeT | Mirror
    | Rank | Father | Son | Subtree | Arity | Bin | Left | Right | Infix ->
      Error.raise_exn lc "Not implemented (tree operators)."

let get_ident_type (env:Global.t) (ctx:Local.t) (id:ident) : opn_btype option =
  match Local.get ctx id with
  | Some _ as opt -> opt
  | None ->
    begin match Global.get_symbol env id with
      | Some (s,_) -> Some (to_open s.typ)
      | None -> None
    end

let unexpected_type_exn (ctx:Local.t) (lc:Utils.loc) (inf:opn_btype) (exp:opn_btype) =
  let str = Printf.sprintf
      "This expression has type '%s' but an expression of type '%s' was expected."
      (to_string (Local.normalize ctx inf)) (to_string (Local.normalize ctx exp))
  in
  Error.raise_exn lc str

type t_int_or_power = C_Int | C_Power

let is_int_or_power_exn l (ctx:Local.t) (arg:t_expression0) : t_int_or_power =
  match Local.normalize ctx arg.exp_typ with
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

let type_set_product_exn (app_lc:Utils.loc) (op_lc:Utils.loc) (ctx:Local.t) (arg:t_expression0): t_expression0 =
  let mt1 = Local.new_meta ctx in
  let mt2 = Local.new_meta ctx in
  let op_ty_exp = type_of_binary_fun (T_Power mt1) (T_Power mt2) (T_Power (T_Product (mt1,mt2))) in
  let op_ty_inf = type_of_unary_fun arg.exp_typ (Local.new_meta ctx) in
  match Local.get_stype ctx op_ty_exp op_ty_inf with
  | None -> unexpected_type_exn ctx op_lc op_ty_inf op_ty_exp
  | Some _ ->
    let op = mk_expr op_lc op_ty_exp (Builtin Product) in
    mk_expr app_lc (T_Power (T_Product (mt1,mt2))) (Application (op,arg))

let type_int_product_exn (app_lc:Utils.loc) (op_lc:Utils.loc) (ctx:Local.t) (arg:t_expression0) : t_expression0 =
  let op_ty_exp = type_of_binary_fun t_int t_int t_int in
  let op_ty_inf = type_of_unary_fun arg.exp_typ (Local.new_meta ctx) in
  match Local.get_stype ctx op_ty_inf op_ty_exp with
  | None -> unexpected_type_exn ctx op_lc op_ty_inf op_ty_exp
  | Some _ ->
    let op = mk_expr op_lc op_ty_exp (Builtin Product) in
    mk_expr app_lc t_int (Application (op,arg))

let type_int_difference_exn (app_lc:Utils.loc) (op_lc:Utils.loc) (ctx:Local.t) (arg:t_expression0) : t_expression0 =
  let op_ty_exp = type_of_binary_fun t_int t_int t_int in
  let op_ty_inf = type_of_unary_fun arg.exp_typ (Local.new_meta ctx) in
  match Local.get_stype ctx op_ty_inf op_ty_exp with
  | None -> unexpected_type_exn ctx op_lc op_ty_inf op_ty_exp
  | Some _ ->
    let op = mk_expr op_lc op_ty_exp (Builtin Difference) in
    mk_expr app_lc t_int (Application (op,arg))

let type_set_difference_exn (app_lc:Utils.loc) (op_lc:Utils.loc) (ctx:Local.t) (arg:t_expression0) : t_expression0 =
  let mt = Local.new_meta ctx in
  let op_ty_exp = type_of_binary_fun (T_Power mt) (T_Power mt) (T_Power mt) in
  let op_ty_inf = type_of_unary_fun arg.exp_typ (Local.new_meta ctx) in
  match Local.get_stype ctx op_ty_exp op_ty_inf with
  | None -> unexpected_type_exn ctx op_lc op_ty_inf op_ty_exp
  | Some _ ->
    let op = mk_expr op_lc op_ty_exp (Builtin Difference) in
    mk_expr app_lc (T_Power mt) (Application (op,arg))

let rec type_expression_exn (env:Global.t) (ctx:Local.t) (e:p_expression) : t_expression0 =
  match e.exp_desc with

  | Ident id | Dollar id as d ->
    begin match get_ident_type env ctx id with
      | Some ty -> mk_expr e.exp_loc ty d
      | None -> Error.raise_exn e.exp_loc ("Unknown identifier '"^id^"'.")
    end

  | Builtin bi as d -> mk_expr e.exp_loc (get_builtin_type_exn ctx e.exp_loc bi) d

  | Pbool p ->
    let tp = type_predicate_exn env ctx p in mk_expr e.exp_loc t_bool (Pbool tp)

  | Application (e1,e2) ->
    begin
      match e1.exp_desc with
      | Builtin Product ->
        let te2 = type_expression_exn env ctx e2 in
        begin match is_int_or_power_exn e1.exp_loc ctx te2 with
          | C_Int -> type_int_product_exn e.exp_loc e1.exp_loc ctx te2
          | C_Power -> type_set_product_exn e.exp_loc e1.exp_loc ctx te2
        end
      | Builtin Difference ->
        let te2 = type_expression_exn env ctx e2 in
        begin match is_int_or_power_exn e1.exp_loc ctx te2 with
          | C_Int -> type_int_difference_exn e.exp_loc e1.exp_loc ctx te2
          | C_Power -> type_set_difference_exn e.exp_loc e1.exp_loc ctx te2
        end
      | _ ->
        let te1 = type_expression_exn env ctx e1 in
        let ty_fun_exp = type_of_unary_fun (Local.new_meta ctx) (Local.new_meta ctx) in
        begin match Local.get_stype ctx te1.exp_typ ty_fun_exp with
          | Some (T_Power (T_Product (a,b))) ->
            let te2 = type_expression_exn env ctx e2 in
            ( match Local.get_stype ctx te2.exp_typ a with
              | Some _ -> mk_expr e.exp_loc b (Application (te1,te2))
              | None -> unexpected_type_exn ctx e2.exp_loc te2.exp_typ a )
          | _ -> unexpected_type_exn ctx e1.exp_loc te1.exp_typ ty_fun_exp
        end
    end

  | Couple (cm,e1,e2) ->
    let te1 = type_expression_exn env ctx e1 in
    let te2 = type_expression_exn env ctx e2 in
    mk_expr e.exp_loc (T_Product (te1.exp_typ,te2.exp_typ)) (Couple (cm,te1,te2))

  | Sequence ((e0,lst)) ->
    begin
      let te = type_expression_exn env ctx e0 in
      let aux (elt:p_expression) : t_expression0 =
        let t_elt = type_expression_exn env ctx elt in
        match Local.get_stype ctx t_elt.exp_typ te.exp_typ with
        | Some ty -> t_elt
        | None -> unexpected_type_exn ctx elt.exp_loc t_elt.exp_typ te.exp_typ
      in
      let tlst = List.map aux lst in
      mk_expr e.exp_loc (T_Power (T_Product (t_int,te.exp_typ))) (Sequence (te,tlst))
    end

  | Extension ((e0,lst)) ->
    begin
      let te0 = type_expression_exn env ctx e0 in
      let aux (elt:p_expression) : t_expression0 =
        let t_elt = type_expression_exn env ctx elt in
        match Local.get_stype ctx t_elt.exp_typ te0.exp_typ with
        | Some _ -> t_elt
        | None -> unexpected_type_exn ctx elt.exp_loc t_elt.exp_typ te0.exp_typ
      in
      let tlst = List.map aux lst in
      mk_expr e.exp_loc (T_Power te0.exp_typ) (Extension (te0,tlst))
    end

  | Comprehension (ids,p) ->
    let (ctx,tids) = declare_nelist ctx ids in
    let tp = type_predicate_exn env ctx p in
    mk_expr e.exp_loc (T_Power (ids_to_product ctx ids)) (Comprehension (tids,tp))

  | Binder (bi,ids,p,e0) ->
    begin
      match bi with
      | Sum | Prod ->
        let (ctx,tids) = declare_nelist ctx ids in
        let tp = type_predicate_exn env ctx p in
        let te = type_expression_exn env ctx e0 in
        begin match Local.get_stype ctx te.exp_typ t_int with
          | Some _ -> mk_expr e.exp_loc t_int (Binder (bi,tids,tp,te)) 
          | None -> unexpected_type_exn ctx e0.exp_loc te.exp_typ t_int
        end
      | Q_Union | Q_Intersection ->
        let (ctx,tids) = declare_nelist ctx ids in
        let tp = type_predicate_exn env ctx p in
        let te = type_expression_exn env ctx e0 in
        let ty_exp = T_Power (Local.new_meta ctx) in
        begin match Local.get_stype ctx te.exp_typ ty_exp with
          | Some ty -> mk_expr e.exp_loc ty (Binder (bi,tids,tp,te))
          | _ -> unexpected_type_exn ctx e0.exp_loc te.exp_typ ty_exp
        end
      | Lambda ->
        begin
          let (ctx,tids) = declare_nelist ctx ids in
          let tp = type_predicate_exn env ctx p in
          let te = type_expression_exn env ctx e0 in
          mk_expr e.exp_loc (T_Power (T_Product (ids_to_product ctx ids,te.exp_typ)))
            (Binder (bi,tids,tp,te))
        end
    end

  | Record ((hd,tl)) ->
    let aux (id,e) = (id,type_expression_exn env ctx e) in
    let thd = aux hd in
    let ttl = List.map aux tl in
    let ty = T_Record (List.map (fun (id,e) -> (id.lid_str,e.exp_typ)) (thd::ttl)) in
    mk_expr e.exp_loc ty (Record (thd,ttl))

  | Record_Type ((hd,tl)) ->
    let aux (id,e) = (id,type_expression_exn env ctx e) in
    let get_type (id,te) =
      let ty_exp = T_Power (Local.new_meta ctx) in
      match Local.get_stype ctx te.exp_typ ty_exp with
      | Some (T_Power ty) -> (id.lid_str,ty)
      | _ -> unexpected_type_exn ctx e.exp_loc te.exp_typ ty_exp
    in
    let thd = aux hd in
    let ttl = List.map aux tl in
    let ty = T_Power (T_Record (List.map get_type (thd::ttl))) in
    mk_expr e.exp_loc ty (Record_Type (thd,ttl))

  | Record_Field_Access (e0,fd) ->
    let te = type_expression_exn env ctx e0 in
    begin match Local.normalize ctx te.exp_typ with
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

and type_predicate_exn (env:Global.t) (ctx:Local.t) (p:p_predicate) : t_predicate0 =
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
          match Local.get_stype ctx te1.exp_typ te2.exp_typ with
          | Some _ -> mk_pred p.prd_loc (Binary_Pred (bop,te1,te2))
          | None -> unexpected_type_exn ctx e2.exp_loc te1.exp_typ te2.exp_typ
        end
      | Membership | Non_Membership ->
        begin
          let te1 = type_expression_exn env ctx e1 in
          let te2 = type_expression_exn env ctx e2 in
          begin
            match Local.get_stype ctx (T_Power te1.exp_typ) te2.exp_typ with
            | Some _ -> mk_pred p.prd_loc (Binary_Pred (bop,te1,te2))
            | None -> unexpected_type_exn ctx e2.exp_loc te2.exp_typ (T_Power te1.exp_typ)
          end
        end
      | Inclusion _ ->
        begin
          let ty0 = T_Power (Local.new_meta ctx) in
          let te1 = type_expression_exn env ctx e1 in
          let te2 = type_expression_exn env ctx e2 in
          begin match Local.get_stype ctx te1.exp_typ ty0 with
            | Some ty1_bis ->
              begin match Local.get_stype ctx ty1_bis te2.exp_typ with
                | Some _ -> mk_pred p.prd_loc (Binary_Pred (bop,te1,te2))
                | None -> unexpected_type_exn ctx e2.exp_loc te2.exp_typ ty1_bis
              end
            | None -> unexpected_type_exn ctx e1.exp_loc te1.exp_typ ty0
          end
        end
      | Inequality _ ->
        begin
          let te1 = type_expression_exn env ctx e1 in
          let te2 = type_expression_exn env ctx e2 in
          begin match Local.get_stype ctx te1.exp_typ t_int with
            | Some _ ->
              begin match Local.get_stype ctx te2.exp_typ t_int with
                | Some _ -> mk_pred p.prd_loc (Binary_Pred (bop,te1,te2))
                | None -> unexpected_type_exn ctx e2.exp_loc te2.exp_typ t_int
              end
            | None -> unexpected_type_exn ctx e1.exp_loc te1.exp_typ t_int
          end
        end
    end

  | Universal_Q (ids,p) ->
    let (ctx,tids) = declare_nelist ctx ids in
    mk_pred p.prd_loc (Universal_Q (tids,type_predicate_exn env ctx p))

  | Existential_Q (ids,p) ->
    let (ctx,tids) = declare_nelist ctx ids in
    mk_pred p.prd_loc (Existential_Q (tids,type_predicate_exn env ctx p))

let type_var_exn (env:Global.t) (ctx:Local.t) (v:p_var) : t_var0 =
  match get_ident_type env ctx v.var_id with
  | None -> Error.raise_exn v.var_loc ("Unknown identifier '"^v.var_id^"'.")
  | Some var_typ -> { var_loc=v.var_loc; var_id=v.var_id; var_typ }

let rec type_substitution_exn (env:Global.t) (ctx:Local.t) (s0:p_substitution) : t_substitution0 =
  match s0.sub_desc with
  | Skip -> mk_subst s0.sub_loc Skip

  | Pre (p,s) -> mk_subst s0.sub_loc (Pre (type_predicate_exn env ctx p,type_substitution_exn env ctx s))

  | Assert (p,s) -> mk_subst s0.sub_loc (Assert(type_predicate_exn env ctx p, type_substitution_exn env ctx s))

  | Affectation ((x,xlst),e) ->
    let rec mk_tuple (x:p_var) : p_var list -> p_expression = function
        | [] -> mk_expr x.var_loc () (Ident x.var_id)
        | hd::tl ->
          mk_expr x.var_loc ()
            (Couple (Comma false,mk_expr x.var_loc () (Ident x.var_id),mk_tuple hd tl))
    in
    let tuple = mk_tuple x xlst in
    let ttuple = type_expression_exn env ctx tuple in
    let te = type_expression_exn env ctx e in
    let () = match Local.get_stype ctx te.exp_typ ttuple.exp_typ with
      | None -> unexpected_type_exn ctx e.exp_loc te.exp_typ ttuple.exp_typ
      | Some _ -> ()
    in
    let tlst = (type_var_exn env ctx x,List.map (type_var_exn env ctx) xlst) in
    mk_subst s0.sub_loc (Affectation (tlst,te))

  | Function_Affectation (f,(hd,tl),e) -> 
    let rec mk_app (lc:Utils.loc) f = function
      | [] -> f
      | x::tl -> mk_app lc (mk_expr lc () (Application (f,x))) tl
    in
    let lhs = mk_app f.var_loc (mk_expr f.var_loc () (Ident f.var_id)) (hd::tl) in
    let tlhs = type_expression_exn env ctx lhs in
    let te = type_expression_exn env ctx e in
    let () = match Local.get_stype ctx te.exp_typ tlhs.exp_typ with
      | None -> unexpected_type_exn ctx e.exp_loc te.exp_typ tlhs.exp_typ
      | Some _ -> ()
    in
    let tf = type_var_exn env ctx f in
    let tlst = (type_expression_exn env ctx hd,List.map (type_expression_exn env ctx) tl) in
    mk_subst s0.sub_loc (Function_Affectation (tf,tlst,te))

  | Record_Affectation (rc,fd,e) ->
    let rf_access = mk_expr rc.var_loc ()
        (Record_Field_Access (mk_expr rc.var_loc () (Ident rc.var_id),fd)) in
    let trf_access = type_expression_exn env ctx rf_access in
    let te = type_expression_exn env ctx e in
    let () = match Local.get_stype ctx te.exp_typ trf_access.exp_typ with
      | None -> unexpected_type_exn ctx e.exp_loc te.exp_typ trf_access.exp_typ 
      | Some _ -> ()
    in
    mk_subst s0.sub_loc (Record_Affectation (type_var_exn env ctx rc,fd,te))

  | Choice ((s,slst)) ->
   mk_subst s0.sub_loc (Choice (type_substitution_exn env ctx s,List.map (type_substitution_exn env ctx) slst))

  | IfThenElse ((ps,pslst),s_else) ->
    let aux (p,s) = (type_predicate_exn env ctx p, type_substitution_exn env ctx s) in
    let tps = (aux ps, List.map aux pslst) in
    let t_else = match s_else with
      | None -> None
      | Some s -> Some (type_substitution_exn env ctx s)
    in
    mk_subst s0.sub_loc (IfThenElse (tps,t_else))

  | Select ((ps,pslst),s_else) ->
    let aux (p,s) = (type_predicate_exn env ctx p, type_substitution_exn env ctx s) in
    let tps = (aux ps, List.map aux pslst) in
    let t_else = match s_else with
      | None -> None
      | Some s -> Some (type_substitution_exn env ctx s)
    in
    mk_subst s0.sub_loc (Select (tps,t_else))

  | Case (e,(hd,tl),c_else) ->
    let te = type_expression_exn env ctx e in
    let aux (elt,s) =
      let telt =  type_expression_exn env ctx elt in
      match Local.get_stype ctx te.exp_typ telt.exp_typ with
      | None -> unexpected_type_exn ctx e.exp_loc telt.exp_typ te.exp_typ 
      | Some _ -> (telt,type_substitution_exn env ctx s)
    in
    let t_else = match c_else with
      | None -> None
      | Some s -> Some (type_substitution_exn env ctx s)
    in
    mk_subst s0.sub_loc (Case(te,(aux hd,List.map aux tl),t_else))

  | Any (ids,p,s) ->
    let (ctx,tids) = declare_nelist ctx ids in
    let tp = type_predicate_exn env ctx p in
    let ts = type_substitution_exn env ctx s in
    mk_subst s0.sub_loc (Any (tids,tp,ts))

  | Let (ids,(hd,tl),s) ->
    let (ctx,tids) = declare_nelist ctx ids in
    let aux (v,e) =
      let te = type_expression_exn env ctx e in
      match Local.get ctx v.var_id with
      | None -> Error.raise_exn v.var_loc ("Unknown symbol '"^v.var_id^"'.")
      | Some ty_exp ->
        begin match Local.get_stype ctx te.exp_typ ty_exp with
          | None -> unexpected_type_exn ctx e.exp_loc te.exp_typ ty_exp
          | Some var_typ -> ({var_loc=v.var_loc;var_typ;var_id=v.var_id},te)
        end
    in
    mk_subst s0.sub_loc (Let (tids,(aux hd,List.map aux tl),type_substitution_exn env ctx s))

  | BecomesElt ((x,xlst),e) ->
    let rec mk_tuple (x:p_var) : p_var list -> p_expression = function
      | [] -> mk_expr x.var_loc () (Ident x.var_id)
      | hd::tl -> mk_expr x.var_loc () (Couple (Comma false,mk_expr x.var_loc () (Ident x.var_id),mk_tuple hd tl))
    in
    let tuple = mk_tuple x xlst in
    let ttuple = type_expression_exn env ctx tuple in
    let ty_exp = T_Power ttuple.exp_typ in
    let te = type_expression_exn env ctx e in
    let () = match Local.get_stype ctx te.exp_typ ty_exp with
      | None -> unexpected_type_exn ctx e.exp_loc te.exp_typ ty_exp
      | Some _ -> ()
    in
    let tlst = (type_var_exn env ctx x,List.map (type_var_exn env ctx) xlst) in
    mk_subst s0.sub_loc (BecomesElt (tlst,te))

  | BecomesSuch ((x,xlst),p) ->
    let tlst = (type_var_exn env ctx x,List.map (type_var_exn env ctx) xlst) in
    mk_subst s0.sub_loc (BecomesSuch (tlst,type_predicate_exn env ctx p))

  | Var (vars,s) ->
    let (ctx,tvars) = declare_nelist ctx vars in
   mk_subst s0.sub_loc (Var(tvars,type_substitution_exn env ctx s))

  | CallUp (ids,op,params) ->
    begin match Global.get_operation env op.lid_str with
      | None -> Error.raise_exn op.lid_loc ("Unknown operation '"^op.lid_str^"'.")
      | Some (ope,_) ->
        let tids = List.map (type_var_exn env ctx) ids in
        let tparams = List.map (type_expression_exn env ctx) params in
        let aux te (_,ty_arg) =
          let ty_exp = to_open ty_arg in
          match Local.get_stype ctx te.exp_typ ty_exp with
          | None -> unexpected_type_exn ctx te.exp_loc te.exp_typ ty_exp
          | Some _ -> ()
        in
        begin try
            let () = List.iter2 (fun v -> aux (mk_expr v.var_loc v.var_typ (Ident v.var_id))) tids ope.args_out in
            let () = List.iter2 aux tparams ope.args_in in
            mk_subst s0.sub_loc (CallUp (tids,op,tparams))
          with Invalid_argument _ ->
            Error.raise_exn op.lid_loc ("Incorrect number of in/out parameters.")
        end
    end

  | While (p,s,inv,var) ->
    let tp = type_predicate_exn env ctx p in
    let ts = type_substitution_exn env ctx s in
    let t_inv = type_predicate_exn env ctx inv in
    let t_var = type_expression_exn env ctx var in
    let exp = t_int in
    let () = match Local.get_stype ctx t_var.exp_typ exp with
      | None -> unexpected_type_exn ctx var.exp_loc t_var.exp_typ exp
      | Some _ -> ()
    in
    mk_subst s0.sub_loc (While (tp,ts,t_inv,t_var))

  | Sequencement (s1,s2) ->
    mk_subst s0.sub_loc (Sequencement(type_substitution_exn env ctx s1,
                                      type_substitution_exn env ctx s2))

  | Parallel (s1,s2) ->
    mk_subst s0.sub_loc (Parallel(type_substitution_exn env ctx s1,
                                  type_substitution_exn env ctx s2))

(* *****************************************************************************
 * Type Inference (closed types)
 * ************************************************************************** *)

type t_var = (Utils.loc,btype) var 
type t_set = (Utils.loc,btype) set 
type t_expression = (Utils.loc,btype) expression
type t_predicate = (Utils.loc,btype) predicate
type t_substitution = (Utils.loc,btype) substitution
type t_operation = (Utils.loc,btype) operation
type t_component = (Utils.loc,btype) component

let close_exn (lc:loc) (ctx:Local.t) (ty:opn_btype) : btype =
  match Local.close ctx ty with
  | None ->
    Error.raise_exn lc
      ("The type of this expression could not be fully infered. The type infered so far is '"^
       to_string (Local.normalize ctx ty)^"'.")
  | Some ty -> ty

let close_var (ctx:Local.t) (v:t_var0) : t_var =
  match Local.close ctx v.var_typ with
  | None -> Error.raise_exn v.var_loc 
      ("The type of symbol '"^v.var_id^
       "' could not be fully infered. The type infered so far is '"^
       to_string (Local.normalize ctx v.var_typ)^"'.")
  | Some var_typ -> { var_loc=v.var_loc; var_id=v.var_id; var_typ }

let close_var_nlist ctx (hd,tl) = (close_var ctx hd,List.map (close_var ctx) tl)

let mk_expr ctx exp_loc exp_typ exp_desc =
  let exp_typ = close_exn exp_loc ctx exp_typ in
  { exp_loc; exp_typ ; exp_desc }

let mk_pred prd_loc prd_desc = { prd_loc; prd_desc }
let mk_subst sub_loc sub_desc = { sub_loc; sub_desc;  }

let rec close_expr (ctx:Local.t) (e:t_expression0) : t_expression =
  match e.exp_desc with
  | Ident _ | Dollar _ | Builtin _ as d ->
    mk_expr ctx e.exp_loc e.exp_typ d
  | Pbool p -> mk_expr ctx e.exp_loc e.exp_typ (Pbool (close_pred ctx p))
  | Application (e1,e2) -> 
    mk_expr ctx e.exp_loc e.exp_typ
      (Application (close_expr ctx e1,close_expr ctx e2))
  | Couple (cm,e1,e2) -> 
    mk_expr ctx e.exp_loc e.exp_typ
      (Couple (cm,close_expr ctx e1,close_expr ctx e2))
  | Sequence (hd,tl) ->
    mk_expr ctx e.exp_loc e.exp_typ
      (Sequence (close_expr ctx hd,List.map (close_expr ctx) tl))
  | Extension (hd,tl) ->
    mk_expr ctx e.exp_loc e.exp_typ
      (Extension (close_expr ctx hd,List.map (close_expr ctx) tl))
  | Comprehension (xlst,p) ->
    mk_expr ctx e.exp_loc e.exp_typ
      (Comprehension (close_var_nlist ctx xlst,close_pred ctx p))
  | Binder (bi,xlst,p,e0) ->
    mk_expr ctx e.exp_loc e.exp_typ
      (Binder (bi,close_var_nlist ctx xlst,close_pred ctx p,close_expr ctx e0))
  | Record_Field_Access (e0,id) ->
    mk_expr ctx e.exp_loc e.exp_typ
      (Record_Field_Access (close_expr ctx e0,id))
  | Record (hd,tl) ->
    let aux (id,e) = (id,close_expr ctx e) in
    mk_expr ctx e.exp_loc e.exp_typ (Record(aux hd,List.map aux tl))
  | Record_Type (hd,tl) ->
    let aux (id,e) = (id,close_expr ctx e) in
    mk_expr ctx e.exp_loc e.exp_typ (Record_Type(aux hd,List.map aux tl))

and close_pred (ctx:Local.t) (p:t_predicate0) : t_predicate =
  match p.prd_desc with
  | P_Builtin _ as d -> mk_pred p.prd_loc d
  | Binary_Prop (bop,p1,p2) -> mk_pred p.prd_loc (Binary_Prop (bop,close_pred ctx p1,close_pred ctx p2))
  | Binary_Pred (bop,e1,e2) -> mk_pred p.prd_loc (Binary_Pred (bop,close_expr ctx e1,close_expr ctx e2))
  | Negation p0 -> mk_pred p.prd_loc (Negation (close_pred ctx p0))
  | Universal_Q (xlst,p0) -> mk_pred p.prd_loc (Universal_Q (close_var_nlist ctx xlst,close_pred ctx p0)) 
  | Existential_Q (xlst,p0) -> mk_pred p.prd_loc (Existential_Q (close_var_nlist ctx xlst,close_pred ctx p0)) 

let rec close_subst (ctx:Local.t) (s:t_substitution0) : t_substitution =
  match s.sub_desc with
  | Skip -> mk_subst s.sub_loc Skip
  | Affectation (xlst,e) -> mk_subst s.sub_loc (Affectation(close_var_nlist ctx xlst,close_expr ctx e))
  | Function_Affectation (v,(hd,tl),e) ->
    mk_subst s.sub_loc (Function_Affectation (close_var ctx v,(close_expr ctx hd,List.map (close_expr ctx) tl),close_expr ctx e))
  | Record_Affectation (v,id,e) ->
    mk_subst s.sub_loc (Record_Affectation (close_var ctx v,id,close_expr ctx e))
  | Pre (p,s0) -> mk_subst s.sub_loc (Pre(close_pred ctx p,close_subst ctx s0))
  | Assert (p,s0) -> mk_subst s.sub_loc (Assert(close_pred ctx p,close_subst ctx s0))
  | Choice (hd,tl) -> mk_subst s.sub_loc (Choice(close_subst ctx hd,List.map (close_subst ctx) tl))
  | IfThenElse ((hd,tl),opt) ->
    let aux (p,s) = (close_pred ctx p,close_subst ctx s) in
    let topt = match opt with
      | None -> None
      | Some s0 -> Some (close_subst ctx s0)
    in
    mk_subst s.sub_loc (IfThenElse ((aux hd,List.map aux tl),topt))
  | Select ((hd,tl),opt) ->
    let aux (p,s) = (close_pred ctx p,close_subst ctx s) in
    let topt = match opt with
      | None -> None
      | Some s0 -> Some (close_subst ctx s0)
    in
    mk_subst s.sub_loc (Select ((aux hd,List.map aux tl),topt))
  | Case (e,(hd,tl),opt) -> 
    let aux (e,s) = (close_expr ctx e,close_subst ctx s) in
    let topt = match opt with
      | None -> None
      | Some s0 -> Some (close_subst ctx s0)
    in
    mk_subst s.sub_loc (Case (close_expr ctx e,(aux hd,List.map aux tl),topt))
  | Any (xlst,p,s0) ->
    mk_subst s.sub_loc (Any (close_var_nlist ctx xlst,close_pred ctx p,close_subst ctx s0))
  | Let (xlst,(hd,tl),s0) ->
    let aux (v,e) = (close_var ctx v,close_expr ctx e) in
    mk_subst s.sub_loc (Let (close_var_nlist ctx xlst,(aux hd,List.map aux tl),close_subst ctx s0))
  | BecomesElt (xlst,e) -> mk_subst s.sub_loc (BecomesElt (close_var_nlist ctx xlst,close_expr ctx e))
  | BecomesSuch (xlst,p) -> mk_subst s.sub_loc (BecomesSuch (close_var_nlist ctx xlst,close_pred ctx p))
  | Var (xlst,s0) -> mk_subst s.sub_loc (Var (close_var_nlist ctx xlst,close_subst ctx s0))
  | CallUp (args_out,id,args_in) ->
    mk_subst s.sub_loc (CallUp (List.map (close_var ctx) args_out,id,List.map (close_expr ctx) args_in))
  | While (p1,s0,p2,e) -> mk_subst s.sub_loc (While (close_pred ctx p1,close_subst ctx s0,close_pred ctx p2,close_expr ctx e))
  | Sequencement (s1,s2) -> mk_subst s.sub_loc (Sequencement (close_subst ctx s1,close_subst ctx s2))
  | Parallel (s1,s2) -> mk_subst s.sub_loc (Parallel (close_subst ctx s1,close_subst ctx s2))

let type_expression (env:Global.t) (ctx:Local.t) (e:p_expression) : t_expression Error.t_result =
  try Ok (close_expr ctx (type_expression_exn env ctx e))
  with Error.Error err -> Error err

let type_predicate (env:Global.t) (ctx:Local.t) (p:p_predicate) : t_predicate Error.t_result =
  try Ok (close_pred ctx (type_predicate_exn env ctx p))
  with Error.Error err -> Error err

let type_substitution (env:Global.t) (ctx:Local.t) (s:p_substitution) : t_substitution Error.t_result =
  try Ok (close_subst ctx (type_substitution_exn env ctx s))
  with Error.Error err -> Error err

let type_expression2_exn (env:Global.t) (ctx:Local.t) (e:p_expression) : t_expression =
  close_expr ctx (type_expression_exn env ctx e)

let type_predicate2_exn (env:Global.t) (ctx:Local.t) (p:p_predicate) : t_predicate =
  close_pred ctx (type_predicate_exn env ctx p)

let type_substitution2_exn (env:Global.t) (ctx:Local.t) (s:p_substitution) : t_substitution =
  close_subst ctx (type_substitution_exn env ctx s)

(* *****************************************************************************
 * Type Checking for Components
 * ************************************************************************** *)

let clause_some_err str = function
  | None -> None
  | Some (l,_) -> Error.raise_exn l str 

let map_clause f = function
  | None -> None
  | Some (l,x) -> Some(l,f x)

let map_list_clause f = function
  | None -> None
  | Some (l,(hd,tl)) -> Some(l,(f hd,List.map f tl))

let iter_list_clause f = function
  | None -> ()
  | Some (_,(hd,tl)) -> List.iter f (hd::tl)

let fold_list_clause f accu = function
  | None -> accu
  | Some (_,(hd,tl)) -> List.fold_left f accu (hd::tl)

let type_set : p_set -> t_set = function
  | Abstract_Set v -> Abstract_Set { var_loc=v.var_loc; var_id=v.var_id; var_typ=T_Power (T_Atomic v.var_id) }
  | Concrete_Set (v,elts) ->
    Concrete_Set ({ var_loc=v.var_loc; var_id=v.var_id; var_typ=T_Power (T_Atomic v.var_id) },
                  List.map (fun e -> { var_loc=e.var_loc; var_id=e.var_id; var_typ=(T_Atomic v.var_id) }) elts )

let add_new_global_symbol env v kind =
  match Global.get_symbol env v.var_id with
  | Some (_,src) ->
    begin match src with
      | From_Current_Mch _ ->
        Error.raise_exn v.var_loc ("The symbol '"^v.var_id^"' is already declared.")
      | From_Imported_Mch mch | From_Refined_Mch mch | From_Seen_Mch mch ->
        Error.raise_exn v.var_loc ("The symbol '"^v.var_id^"' is already declared in machine '"^mch.lid_str^"'.")
    end
  | None -> Global.add_symbol env v.var_loc v.var_id v.var_typ kind

let declare_set (env:Global.t) (s:p_set) : t_set =
  let ts = type_set s in
  let () = match ts with
    | Abstract_Set v -> add_new_global_symbol env v K_Abstract_Set
    | Concrete_Set (v,elts) ->
      add_new_global_symbol env v K_Concrete_Set;
      List.iter (fun v -> add_new_global_symbol env v K_Enumerate) elts
  in
  ts

let kind_to_string = function
  | K_Abstract_Variable -> "an abstract variable"
  | K_Concrete_Variable -> "a concrete variable"
  | K_Abstract_Constant -> "an abstract constant"
  | K_Concrete_Constant -> "a concrete constant"
  | K_Abstract_Set -> "an abstract set"
  | K_Concrete_Set -> "a concrete set"
  | K_Enumerate -> "an enumerate"

let are_kind_compatible k1 k2 =
  k1 = k2 ||
  match k1, k2 with
  | K_Abstract_Variable, K_Concrete_Variable -> true
  | K_Abstract_Constant, K_Concrete_Constant -> true
  | _, _ -> false

let declare_symb (env:Global.t) (k:t_kind) (ctx:Local.t) (v:p_var) : Local.t =
  match Global.get_symbol env v.var_id with
  | None -> fst (Local.declare ctx v.var_id)
  | Some (ts,src) ->
    let mch = match src with
      | From_Current_Mch _ -> 
        Error.raise_exn v.var_loc ("The symbol '"^v.var_id^"' is already declared.")
      | From_Seen_Mch mch ->
        Error.raise_exn v.var_loc ("The symbol '"^v.var_id^"' is already declared in '"^mch.lid_str^"'.")
      | From_Imported_Mch mch | From_Refined_Mch mch -> mch
    in
    if are_kind_compatible ts.kind k then ctx
    else Error.raise_exn v.var_loc
        ("The symbol '"^v.var_id^"' is declared as "^kind_to_string k^
         " in this machine but it is declared as "^kind_to_string ts.kind^" in '"^mch.lid_str^"'")

let load_seen_mch f env id =
  match f id.lid_str with
  | None -> Error.raise_exn id.lid_loc ("Unknown machine '"^id.lid_str^"'.")
  | Some itf -> Global.load_interface env itf (From_Seen_Mch id)

let type_var_exn (env:Global.t) (ctx:Local.t) (v:p_var) : t_var =
  match Local.get ctx v.var_id with
  | Some var_typ ->
    begin match Local.close ctx var_typ with
      | None ->
        let str = Printf.sprintf "The type of symbol '%s' could not be fully infered. The type infered so far is '%s'."
            v.var_id (to_string (Local.normalize ctx var_typ)) in
        Error.raise_exn v.var_loc str
      | Some var_typ -> { var_loc=v.var_loc; var_id=v.var_id; var_typ }
    end
  | None ->
    begin match Global.get_symbol env v.var_id with
      | Some (s,_) -> { var_loc=v.var_loc; var_id=v.var_id; var_typ=s.typ }
      | None -> assert false
    end

let promote_symb (env:Global.t) (ctx:Local.t) (k:t_kind) (v:t_var) : unit =
  if Global.mem_symbol env v.var_id then Global.redeclare_symbol env v.var_id
  else Global.add_symbol env v.var_loc v.var_id v.var_typ k

let declare_constants (env:Global.t) cconst aconst prop =
  let ctx = Local.create () in
  let ctx = fold_list_clause (declare_symb env K_Concrete_Constant) ctx cconst in
  let ctx = fold_list_clause (declare_symb env K_Abstract_Constant) ctx aconst in
  let t_prop = map_clause (type_predicate2_exn env ctx) prop in
  let t_cconst = map_list_clause (type_var_exn env ctx) cconst in
  let t_aconst = map_list_clause (type_var_exn env ctx) aconst in
  let _ = iter_list_clause (promote_symb env ctx K_Concrete_Constant) t_cconst in
  let _ = iter_list_clause (promote_symb env ctx K_Abstract_Constant) t_aconst in
  (t_cconst,t_aconst,t_prop)

let declare_variables (env:Global.t) cvars avars inv =
  let ctx = Local.create () in
  let ctx = fold_list_clause (declare_symb env K_Concrete_Variable) ctx cvars in
  let ctx = fold_list_clause (declare_symb env K_Abstract_Variable) ctx avars in
  let t_inv = map_clause (type_predicate2_exn env ctx) inv in
  let t_cvars = map_list_clause (type_var_exn env ctx) cvars in
  let t_avars = map_list_clause (type_var_exn env ctx) avars in
  let _ = iter_list_clause (promote_symb env ctx K_Concrete_Variable) t_cvars in
  let _ = iter_list_clause (promote_symb env ctx K_Abstract_Variable) t_avars in
  (t_cvars,t_avars,t_inv)

let declare_list (ctx:Local.t) (lst:p_var list) : Local.t * (Utils.loc,opn_btype) var list =
  let (ctx,tvars) = List.fold_left
      (fun (ctx,tvars) v ->
         let (ctx,var_typ) = Local.declare ctx v.var_id in
         ( ctx, { var_loc=v.var_loc; var_typ; var_id=v.var_id }::tvars )
      ) (ctx,[]) lst
  in
  (ctx,List.rev tvars)

let check_signature op s =
  let rec aux lst1 lst2 =
    match lst1, lst2 with
    | [], [] -> ()
    | v::lst1, (x,_)::lst2 ->
      if ident_eq v.var_id x then aux lst1 lst2
      else Error.raise_exn v.var_loc ("Expecting parameter '"^x^"' instead of '"^v.var_id^"'.")
    | v::_, [] -> Error.raise_exn v.var_loc ("Unexpected parameter '"^v.var_id^"'.")
    | [], (x,_)::_ ->
       Error.raise_exn op.op_name.lid_loc ("Missing parameter '"^x^"'.")
  in
  aux op.op_in s.args_in;
  aux op.op_out s.args_out

let get_operation_context (env:Global.t) (op:p_operation) : Local.t*bool =
  match Global.get_operation env op.op_name.lid_str with
  | None ->
    let ctx = Local.create () in
    let (ctx,op_out) = declare_list ctx op.op_out in
    let (ctx,op_in) = declare_list ctx op.op_in in
    (ctx,true)
  | Some (s,src) ->
    let () = match src with
      | From_Current_Mch is_local ->
        if not is_local then
          Error.raise_exn op.op_name.lid_loc
            ("An operation with the same name is already in the machine.")
      | From_Seen_Mch mch ->
        Error.raise_exn op.op_name.lid_loc
          ("An operation with the same name is already declared in machine '"^mch.lid_str^"'.")
      | From_Refined_Mch _ -> ()
      | From_Imported_Mch _ -> ()
    in
    let ctx = Local.create () in
    let aux ctx (s,ty) = Local.add ctx s (to_open ty) in
    let ctx = List.fold_left aux ctx s.args_in in
    let ctx = List.fold_left aux ctx s.args_out in
    let () = check_signature op s in
    (ctx,false)

let declare_local_operation (env:Global.t) (op:p_operation) : t_operation =
  match Global.get_operation env op.op_name.lid_str with
  | None ->
    let ctx = Local.create () in
    let (ctx,op_out) = declare_list ctx op.op_out in
    let (ctx,op_in) = declare_list ctx op.op_in in
    let body = type_substitution_exn env ctx op.op_body in
    let op_in = List.map (type_var_exn env ctx) op.op_in in
    let op_out = List.map (type_var_exn env ctx) op.op_out in
    let op_body = close_subst ctx body in
    let aux v = (v.var_id, v.var_typ) in
    let args_out = List.map aux op_out in
    let args_in  = List.map aux op_in in
    let () = Global.add_operation env op.op_name.lid_loc op.op_name.lid_str args_in args_out true in
    { op_name=op.op_name; op_in; op_out; op_body }
  | Some (_,src) ->
    begin match src with
      | From_Current_Mch _ ->
        Error.raise_exn op.op_name.lid_loc
          ("An operation with the same name is already in the machine.")
      | From_Seen_Mch mch | From_Refined_Mch mch | From_Imported_Mch mch ->
        Error.raise_exn op.op_name.lid_loc
          ("An operation with the same name is already declared in machine '"^mch.lid_str^"'.")
    end

let declare_operation (env:Global.t) (op:p_operation) : t_operation =
  let (ctx,is_new) = get_operation_context env op in
  let body = type_substitution_exn env ctx op.op_body in
  let op_in = List.map (type_var_exn env ctx) op.op_in in
  let op_out = List.map (type_var_exn env ctx) op.op_out in
  let op_body = close_subst ctx body in
  let aux v = (v.var_id, v.var_typ) in
  let args_out = List.map aux op_out in
  let args_in  = List.map aux op_in in
  ( if is_new then
      Global.add_operation env op.op_name.lid_loc op.op_name.lid_str args_in args_out false
    else
      Global.redefine_operation env op.op_name.lid_str );
  { op_name=op.op_name; op_in; op_out; op_body }

let type_machine_exn (f:string -> MachineInterface.t option) env (mch:_ machine_desc) : (Utils.loc,btype) machine_desc =
  let mch_constraints = clause_some_err "Not implemented: machine with clause CONSTRAINTS." mch.mch_constraints in
  let mch_includes = clause_some_err "Not implemented: clause INCLUDES." mch.mch_includes in
  let mch_promotes = clause_some_err "Not implemented: clause PROMOTES." mch.mch_promotes in
  let mch_extends = clause_some_err "Not implemented: clause EXTENDS." mch.mch_extends in
  let mch_uses = clause_some_err "Not implemented: clause USES." mch.mch_uses in
  let () = iter_list_clause (load_seen_mch f env) mch.mch_sees in
  let mch_sets = map_list_clause (declare_set env) mch.mch_sets in
  let (mch_concrete_constants,mch_abstract_constants,mch_properties) =
    declare_constants env mch.mch_concrete_constants
      mch.mch_abstract_constants mch.mch_properties
  in
  let (mch_concrete_variables,mch_abstract_variables,mch_invariant) =
    declare_variables env mch.mch_concrete_variables
      mch.mch_abstract_variables mch.mch_invariant
  in
  let ctx = Local.create () in
  let mch_assertions = map_list_clause (type_predicate2_exn env ctx) mch.mch_assertions in
  let mch_initialisation = map_clause (type_substitution2_exn env ctx) mch.mch_initialisation in
  let mch_operations = map_list_clause (declare_operation env) mch.mch_operations in
  { mch_constraints; mch_sees=mch.mch_sees; mch_includes; mch_promotes; mch_extends;
    mch_uses; mch_sets; mch_concrete_constants; mch_abstract_constants;
    mch_properties; mch_concrete_variables; mch_abstract_variables;
    mch_invariant; mch_assertions; mch_initialisation; mch_operations }

let load_refines f env mch =
  match f mch.lid_str with
  | None -> Error.raise_exn mch.lid_loc ("Unknown machine '"^mch.lid_str^"'.")
  | Some itf -> Global.load_interface env itf (From_Refined_Mch mch)

let type_refinement_exn (f:string->MachineInterface.t option) env ref : (Utils.loc,btype) refinement_desc =
  let () = load_refines f env ref.ref_refines in
  let ref_includes = clause_some_err "Not implemented: clause INCLUDES." ref.ref_includes in
  let ref_promotes = clause_some_err "Not implemented: clause PROMOTES." ref.ref_promotes in
  let ref_extends = clause_some_err "Not implemented: clause EXTENDS."ref.ref_extends in
  let () = iter_list_clause (load_seen_mch f env) ref.ref_sees in
  let ref_sets = map_list_clause (declare_set env) ref.ref_sets in
  let (ref_concrete_constants,ref_abstract_constants,ref_properties) =
    declare_constants env ref.ref_concrete_constants
      ref.ref_abstract_constants ref.ref_properties
  in
  let (ref_concrete_variables,ref_abstract_variables,ref_invariant) =
    declare_variables env ref.ref_concrete_variables
      ref.ref_abstract_variables ref.ref_invariant
  in
  let ctx = Local.create () in
  let ref_assertions = map_list_clause (type_predicate2_exn env ctx) ref.ref_assertions in
  let ref_initialisation = map_clause (type_substitution2_exn env ctx) ref.ref_initialisation in
  let ref_local_operations = map_list_clause (declare_local_operation env) ref.ref_local_operations in
  let ref_operations = map_list_clause (declare_operation env) ref.ref_operations in
  { ref_refines=ref.ref_refines; ref_sees=ref.ref_sees; ref_includes; ref_promotes;
    ref_extends; ref_sets; ref_concrete_constants; ref_abstract_constants;
    ref_properties; ref_concrete_variables; ref_abstract_variables; ref_invariant;
    ref_assertions; ref_initialisation; ref_operations; ref_local_operations; }

let type_value env (v,e:p_var*p_expression) : t_var*t_expression = (*FIXME*)
  match Global.get_symbol env v.var_id with
  | None -> Error.raise_exn v.var_loc ("Unknown symbol '"^v.var_id^"'.")
  | Some (s,_) ->
    let ctx = Local.create () in
    let te = type_expression2_exn env ctx e in
    if is_equal te.exp_typ s.typ then
      ( {var_loc=v.var_loc;var_id=v.var_id;var_typ=s.typ},te)
    else
  Error.raise_exn e.exp_loc
    ("This expression has type '" ^ to_string te.exp_typ ^
     "' but an expression of type '" ^ to_string s.typ ^"' was expected.")

let load_imported_mch f env mi =
  match mi.mi_params with
  | [] ->
    begin match f mi.mi_mch.lid_str with
      | None -> Error.raise_exn mi.mi_mch.lid_loc ("Unknown machine '"^mi.mi_mch.lid_str^"'.")
      | Some itf -> ( Global.load_interface env itf (From_Imported_Mch mi.mi_mch); {mi_mch=mi.mi_mch;mi_params=[]} )
    end
  | _::_ -> Error.raise_exn mi.mi_mch.lid_loc "Not implemented: importation of machine with parameters."

let promote_op env op_name =
  if Global.promote_operation env op_name.lid_loc op_name.lid_str then ()
  else
    Error.raise_exn op_name.lid_loc "This operation could not be found in any imported machine."

let load_extended_mch f env mi =
  match mi.mi_params with
  | [] ->
    begin match f mi.mi_mch.lid_str with
      | None -> Error.raise_exn mi.mi_mch.lid_loc ("Unknown machine '"^mi.mi_mch.lid_str^"'.")
      | Some itf ->
        let () = Global.load_interface env itf (From_Imported_Mch mi.mi_mch) in
        let op_list = MachineInterface.get_operations itf in
        List.iter (fun (s,_) -> promote_op env { lid_loc=mi.mi_mch.lid_loc; lid_str=s} ) op_list;
        {mi_mch=mi.mi_mch;mi_params=[]}
    end
  | _::_ -> Error.raise_exn mi.mi_mch.lid_loc "Not implemented: extension of machine with parameters."


let type_implementation_exn (f:string -> MachineInterface.t option) env imp : (Utils.loc,btype) implementation_desc =
  let () = load_refines f env imp.imp_refines in
  let () = iter_list_clause (load_seen_mch f env) imp.imp_sees in
  let imp_imports = map_list_clause (load_imported_mch f env) imp.imp_imports in
  let imp_extends = map_list_clause (load_extended_mch f env) imp.imp_extends in
  let () = iter_list_clause (promote_op env) imp.imp_promotes in
  let imp_sets = map_list_clause (declare_set env) imp.imp_sets in
  let (imp_concrete_constants,imp_abstract_constants,imp_properties) =
    declare_constants env imp.imp_concrete_constants
      None imp.imp_properties
  in
  let (imp_concrete_variables,imp_abstract_variables,imp_invariant) =
    declare_variables env imp.imp_concrete_variables None imp.imp_invariant
  in
  let imp_values = map_list_clause (type_value env) imp.imp_values in
  let ctx = Local.create () in
  let imp_assertions = map_list_clause (type_predicate2_exn env ctx) imp.imp_assertions in
  let imp_initialisation = map_clause (type_substitution2_exn env ctx) imp.imp_initialisation in
  let imp_local_operations = map_list_clause (declare_local_operation env) imp.imp_local_operations in
  let imp_operations = map_list_clause (declare_operation env) imp.imp_operations in
  { imp_refines=imp.imp_refines; imp_sees=imp.imp_sees; imp_imports;
    imp_promotes=imp.imp_promotes; imp_extends; imp_sets; imp_concrete_constants;
    imp_properties; imp_values; imp_concrete_variables; imp_invariant;
    imp_assertions; imp_initialisation; imp_operations; imp_local_operations; }

let mk_comp co_loc co_name co_parameters co_desc : t_component = { co_name; co_parameters; co_loc; co_desc }

let type_component (f:string -> MachineInterface.t option) (env:Global.t) (co:p_component) : t_component Error.t_result =
  try
    let params = match co.co_parameters with
      | [] -> []
      | x::_ -> Error.raise_exn x.var_loc "Not implemented: machine with parameters."
    in
    match co.co_desc with
    | Machine mch -> Ok (mk_comp co.co_loc co.co_name params (Machine (type_machine_exn f env mch)))
    | Refinement ref -> Ok (mk_comp co.co_loc co.co_name params (Refinement (type_refinement_exn f env ref)))
    | Implementation imp -> Ok (mk_comp co.co_loc co.co_name params (Implementation (type_implementation_exn f env imp)))
  with
  | Error.Error err -> Error err

let get_interface (f:string -> MachineInterface.t option) (co:p_component) : MachineInterface.t =
  let env = Global.create () in
  let _ = type_component f env co in
  Global.to_interface env
