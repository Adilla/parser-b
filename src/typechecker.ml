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
type t_op  = { loc:loc; args_in: (string*btype) list; args_out: (string*btype) list; is_readonly:bool; }

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
    val add_operation : t -> ident -> t_op -> bool -> unit
    val mem_symbol : t -> ident -> bool
    val mem_operation : t -> ident -> bool
    val redefine_operation : t -> ident -> bool -> unit
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

  let add_operation (env:t) (id:ident) (op:t_op) (is_local:bool) : unit =
    Hashtbl.add env.ops id ( op, From_Current_Mch is_local)

  let mem_operation (env:t)(id:ident) : bool = Hashtbl.mem env.ops id

  let redefine_operation (env:t) (id:ident) (is_ro:bool) : unit =
    if Hashtbl.mem env.ops id then
      let (op,_) = Hashtbl.find env.ops id in
      Hashtbl.replace env.ops id ({op with is_readonly=is_ro},From_Current_Mch false)
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
      | From_Imported_Mch _ ->
        ( Hashtbl.replace env.ops id (op,From_Current_Mch false); true )
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

type visibility = V_Invariant | V_Properties | V_Operations | V_Local_Operations | V_Values | V_Assert
type env = { gl: Global.t; uf:Unif.t; vi:visibility }

(* *****************************************************************************
 * Local contexts 
 * ************************************************************************** *)
 (*TODO write permission*)
module Local :
sig
  type t
  val create : unit -> t
  val add : t -> ident -> opn typ -> t
  val get : t -> ident -> (opn typ) option
  val iter : (string -> opn typ -> unit) -> t -> unit
  val get_vars: t -> ident list

  end = struct

  module M = Map.Make(
    struct
      type t = string
      let compare = String.compare
    end )

  type t = opn typ M.t

  let create () = M.empty

  let add (ctx:t) (id:ident) (ty:opn typ) : t =
    M.add id ty ctx

  let get (ctx:t) (id:ident) : (opn typ) option =
    try Some (M.find id ctx)
    with Not_found -> None

  let iter = M.iter

  let get_vars ctx =
    List.map fst (M.bindings ctx)
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

let declare (uf:Unif.t) (ctx:Local.t) (id:string) : Local.t * opn typ = 
  let mt = Unif.new_meta uf in
  ( Local.add ctx id mt, mt )

let declare_list (uf:Unif.t) (ctx:Local.t) (lst:p_var list) : Local.t * t_var0 list =
  let (ctx,tvars) = List.fold_left
      (fun (ctx,tvars) v ->
         let (ctx,var_typ) = declare uf ctx v.var_id in
         ( ctx, { var_loc=v.var_loc; var_typ; var_id=v.var_id }::tvars )
      ) (ctx,[]) lst
  in
  (ctx,List.rev tvars)

let declare_nelist (uf:Unif.t) (ctx:Local.t) (xlst:p_var Nlist.t) : Local.t * t_var0 Nlist.t =
  let (ctx,lst) = declare_list uf ctx (Nlist.to_list xlst) in
  (ctx,Nlist.from_list_exn lst)

let ids_to_product (ctx:Local.t) (xlst:p_var Nlist.t) : opn_btype =
  let aux pr v =
    match Local.get ctx v.var_id with
    | None -> assert false
    | Some ty -> T_Product (pr,ty)
  in
  match Local.get ctx (Nlist.hd xlst).var_id with
  | None -> assert false
  | Some ty -> List.fold_left aux ty (Nlist.tl xlst)

let get_builtin_type_exn (uf:Unif.t) (lc:Utils.loc) : e_builtin -> opn_btype = function
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

let compatibility_check_exn lc vis kind src =
  match vis, kind, src with
  | V_Properties, (K_Abstract_Variable|K_Concrete_Variable), _ ->
    Error.raise_exn lc "Variables are not visible in the clause PROPERTIES."
  | V_Properties, _, _ -> ()
  | V_Invariant, (K_Abstract_Variable|K_Concrete_Variable), From_Seen_Mch _ ->
    Error.raise_exn lc "Variables from seen machines are not visible in the clauses INVARIANT or ASSERTIONS."
  | V_Invariant, _, _ -> ()
  | V_Operations, K_Abstract_Constant, (From_Refined_Mch _|From_Imported_Mch _) ->
    Error.raise_exn lc "Abstract constants from refined or imported machines are not visible in the clause OPERATIONS or INITIALISATION."
  | V_Operations, K_Abstract_Variable, (From_Refined_Mch _|From_Imported_Mch _) ->
    Error.raise_exn lc "Abstract variables from refined or imported machines are not visible in the clause OPERATIONS or INITIALISATION."
  | V_Operations, _, _ -> ()
  | V_Local_Operations, (K_Abstract_Constant|K_Abstract_Variable), From_Refined_Mch _ ->
    Error.raise_exn lc "Abstract constants or variables from refined machines are not visible in the clause LOCAL_OPERATIONS."
  | V_Local_Operations, _, _ -> ()
  | V_Values, K_Abstract_Constant, _ ->
    Error.raise_exn lc "Abstract constants are not visible in the clause VALUES."
  | V_Values, (K_Abstract_Variable|K_Concrete_Variable), _ ->
    Error.raise_exn lc "Variables are not visible in the clause VALUES."
  | V_Values, _, _ -> ()
  | V_Assert, _, _ -> ()
  
let get_ident_type_exn (env:env) (ctx:Local.t) (lc:loc) (id:ident) : opn_btype =
  match Local.get ctx id with
  | Some ty -> ty
  | None ->
    begin match Global.get_symbol env.gl id with
      | Some (symb,src) ->
        let () = compatibility_check_exn lc env.vi symb.kind src in
        to_open symb.typ
      | None -> Error.raise_exn lc ("Unknown identifier '"^id^"'.")
    end

let unexpected_type_exn (uf:Unif.t) (lc:Utils.loc) (inf:opn_btype) (exp:opn_btype) =
  let str = Printf.sprintf
      "This expression has type '%s' but an expression of type '%s' was expected."
      (to_string (Unif.normalize uf inf)) (to_string (Unif.normalize uf exp))
  in
  Error.raise_exn lc str

type t_int_or_power = C_Int | C_Power

let is_int_or_power_exn l (uf:Unif.t) (arg:t_expression0) : t_int_or_power =
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
  let op_ty_exp = type_of_binary_fun t_int t_int t_int in
  let op_ty_inf = type_of_unary_fun arg.exp_typ (Unif.new_meta uf) in
  match Unif.get_stype uf op_ty_inf op_ty_exp with
  | None -> unexpected_type_exn uf op_lc op_ty_inf op_ty_exp
  | Some _ ->
    let op = mk_expr op_lc op_ty_exp (Builtin Product) in
    mk_expr app_lc t_int (Application (op,arg))

let type_int_difference_exn (app_lc:Utils.loc) (op_lc:Utils.loc) (uf:Unif.t) (arg:t_expression0) : t_expression0 =
  let op_ty_exp = type_of_binary_fun t_int t_int t_int in
  let op_ty_inf = type_of_unary_fun arg.exp_typ (Unif.new_meta uf) in
  match Unif.get_stype uf op_ty_inf op_ty_exp with
  | None -> unexpected_type_exn uf op_lc op_ty_inf op_ty_exp
  | Some _ ->
    let op = mk_expr op_lc op_ty_exp (Builtin Difference) in
    mk_expr app_lc t_int (Application (op,arg))

let type_set_difference_exn (app_lc:Utils.loc) (op_lc:Utils.loc) (uf:Unif.t) (arg:t_expression0) : t_expression0 =
  let mt = Unif.new_meta uf in
  let op_ty_exp = type_of_binary_fun (T_Power mt) (T_Power mt) (T_Power mt) in
  let op_ty_inf = type_of_unary_fun arg.exp_typ (Unif.new_meta uf) in
  match Unif.get_stype uf op_ty_exp op_ty_inf with
  | None -> unexpected_type_exn uf op_lc op_ty_inf op_ty_exp
  | Some _ ->
    let op = mk_expr op_lc op_ty_exp (Builtin Difference) in
    mk_expr app_lc (T_Power mt) (Application (op,arg))

let rec type_expression_exn (env:env) (ctx:Local.t) (e:p_expression) : t_expression0 =
  match e.exp_desc with

  | Ident id | Dollar id as d ->
    let ty = get_ident_type_exn env ctx e.exp_loc id in
    mk_expr e.exp_loc ty d

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
    let (ctx,tids) = declare_nelist env.uf ctx ids in
    let tp = type_predicate_exn env ctx p in
    mk_expr e.exp_loc (T_Power (ids_to_product ctx ids)) (Comprehension (tids,tp))

  | Binder (bi,ids,p,e0) ->
    begin
      match bi with
      | Sum | Prod ->
        let (ctx,tids) = declare_nelist env.uf ctx ids in
        let tp = type_predicate_exn env ctx p in
        let te = type_expression_exn env ctx e0 in
        begin match Unif.get_stype env.uf te.exp_typ t_int with
          | Some _ -> mk_expr e.exp_loc t_int (Binder (bi,tids,tp,te)) 
          | None -> unexpected_type_exn env.uf e0.exp_loc te.exp_typ t_int
        end
      | Q_Union | Q_Intersection ->
        let (ctx,tids) = declare_nelist env.uf ctx ids in
        let tp = type_predicate_exn env ctx p in
        let te = type_expression_exn env ctx e0 in
        let ty_exp = T_Power (Unif.new_meta env.uf) in
        begin match Unif.get_stype env.uf te.exp_typ ty_exp with
          | Some ty -> mk_expr e.exp_loc ty (Binder (bi,tids,tp,te))
          | _ -> unexpected_type_exn env.uf e0.exp_loc te.exp_typ ty_exp
        end
      | Lambda ->
        begin
          let (ctx,tids) = declare_nelist env.uf ctx ids in
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
    let (ctx,tids) = declare_nelist env.uf ctx ids in
    mk_pred p.prd_loc (Universal_Q (tids,type_predicate_exn env ctx p))

  | Existential_Q (ids,p) ->
    let (ctx,tids) = declare_nelist env.uf ctx ids in
    mk_pred p.prd_loc (Existential_Q (tids,type_predicate_exn env ctx p))

let type_var_exn (env:env) (ctx:Local.t) (v:p_var) : t_var0 =
  let var_typ = get_ident_type_exn env ctx v.var_loc v.var_id in
  { var_loc=v.var_loc; var_id=v.var_id; var_typ }

let rec type_substitution_exn (env:env) (ctx:Local.t) (s0:p_substitution) : t_substitution0 =
  match s0.sub_desc with
  | Skip -> mk_subst s0.sub_loc Skip

  | Pre (p,s) ->
    mk_subst s0.sub_loc
      (Pre (type_predicate_exn env ctx p, type_substitution_exn env ctx s))

  | Assert (p,s) ->
    mk_subst s0.sub_loc
      (Assert(type_predicate_exn {env with vi=V_Assert} ctx p,
              type_substitution_exn env ctx s))

  | Affectation (xlst,e) -> (*FIXME write permission*)
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
    let tlst = Nlist.map (type_var_exn env ctx) xlst in
    mk_subst s0.sub_loc (Affectation (tlst,te))

  | Function_Affectation (f,nlst,e) ->  (*FIXME write permission*)
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

  | Record_Affectation (rc,fd,e) -> (*FIXME write permission*)
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
    let (ctx,tids) = declare_nelist env.uf ctx ids in
    let tp = type_predicate_exn env ctx p in
    let ts = type_substitution_exn env ctx s in
    mk_subst s0.sub_loc (Any (tids,tp,ts))

  | Let (ids,nlst,s) ->
    let (ctx,tids) = declare_nelist env.uf ctx ids in
    let aux (v,e) =
      let te = type_expression_exn env ctx e in
      match Local.get ctx v.var_id with
      | None -> Error.raise_exn v.var_loc ("Unknown symbol '"^v.var_id^"'.")
      | Some ty_exp ->
        begin match Unif.get_stype env.uf te.exp_typ ty_exp with
          | None -> unexpected_type_exn env.uf e.exp_loc te.exp_typ ty_exp
          | Some var_typ -> ({var_loc=v.var_loc;var_typ;var_id=v.var_id},te)
        end
    in
    mk_subst s0.sub_loc (Let (tids,Nlist.map aux nlst,type_substitution_exn env ctx s))

  | BecomesElt (xlst,e) -> (*FIXME write permission*)
    let rec mk_tuple (x:p_var) : p_var list -> p_expression = function
      | [] -> mk_expr x.var_loc () (Ident x.var_id)
      | hd::tl -> mk_expr x.var_loc () (Couple (Comma false,mk_expr x.var_loc () (Ident x.var_id),mk_tuple hd tl))
    in
    let tuple = mk_tuple (Nlist.hd xlst) (Nlist.tl xlst) in
    let ttuple = type_expression_exn env ctx tuple in
    let ty_exp = T_Power ttuple.exp_typ in
    let te = type_expression_exn env ctx e in
    let () = match Unif.get_stype env.uf te.exp_typ ty_exp with
      | None -> unexpected_type_exn env.uf e.exp_loc te.exp_typ ty_exp
      | Some _ -> ()
    in
    let tlst = (Nlist.map (type_var_exn env ctx) xlst) in
    mk_subst s0.sub_loc (BecomesElt (tlst,te))

  | BecomesSuch (xlst,p) -> (*FIXME write permission*)
    let tlst = Nlist.map (type_var_exn env ctx) xlst in
    mk_subst s0.sub_loc (BecomesSuch (tlst,type_predicate_exn env ctx p))

  | Var (vars,s) ->
    let (ctx,tvars) = declare_nelist env.uf ctx vars in
    mk_subst s0.sub_loc (Var(tvars,type_substitution_exn env ctx s))

  | CallUp (ids,op,params) -> (*FIXME write permission*)
    begin match Global.get_operation env.gl op.lid_str with
      | None -> Error.raise_exn op.lid_loc ("Unknown operation '"^op.lid_str^"'.")
      | Some (ope,src) ->
        let () = match src with
          | From_Current_Mch false ->
            Error.raise_exn s0.sub_loc
              "This operation is defined in the machine; it cannot be used here."
          | From_Current_Mch true -> ()
          | From_Seen_Mch mch_name ->
            if not ope.is_readonly then
              Error.raise_exn s0.sub_loc
                ("This operation is defined is the seen machine '"^
                 mch_name.lid_str ^"' but it is not read-only.")
          | From_Refined_Mch mch_name ->
            Error.raise_exn s0.sub_loc
              ("This operation is defined in refined machine '"^
               mch_name.lid_str^"'; it cannot be used here.")
          | From_Imported_Mch _ -> ()
        in
        let tids = List.map (type_var_exn env ctx) ids in
        let tparams = List.map (type_expression_exn env ctx) params in
        let aux te (_,ty_arg) =
          let ty_exp = to_open ty_arg in
          match Unif.get_stype env.uf te.exp_typ ty_exp with
          | None -> unexpected_type_exn env.uf te.exp_loc te.exp_typ ty_exp
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
    let t_inv = type_predicate_exn { env with vi=V_Assert } ctx inv in
    let t_var = type_expression_exn { env with vi=V_Assert } ctx var in
    let exp = t_int in
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

let close_exn (lc:loc) (uf:Unif.t) (ty:opn_btype) : btype =
  match Btype.close (Unif.normalize uf ty) with
  | None ->
    Error.raise_exn lc
      ("The type of this expression could not be fully infered. The type infered so far is '"^
       to_string (Unif.normalize uf ty)^"'.")
  | Some ty -> ty

let close_var (uf:Unif.t) (v:t_var0) : t_var =
  match Btype.close (Unif.normalize uf v.var_typ) with
  | None -> Error.raise_exn v.var_loc 
      ("The type of symbol '"^v.var_id^
       "' could not be fully infered. The type infered so far is '"^
       to_string (Unif.normalize uf v.var_typ)^"'.")
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

let type_expression2_exn (env:env) (ctx:Local.t) (e:p_expression) : t_expression =
  close_expr env.uf (type_expression_exn env ctx e)

let type_predicate2_exn (env:env) (ctx:Local.t) (p:p_predicate) : t_predicate =
  close_pred env.uf (type_predicate_exn env ctx p)

let type_substitution2_exn (env:env) (ctx:Local.t) (s:p_substitution) : t_substitution =
  close_subst env.uf (type_substitution_exn env ctx s)

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
  | Some (l,nlst) -> Some(l,Nlist.map f nlst)

let iter_list_clause f = function
  | None -> ()
  | Some (_,nlst) -> List.iter f (Nlist.to_list nlst)

let fold_list_clause f accu = function
  | None -> accu
  | Some (_,nlst) -> List.fold_left f accu (Nlist.to_list nlst)

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

let declare_symb (env:Global.t) (uf:Unif.t) (k:t_kind) (ctx:Local.t) (v:p_var) : Local.t =
  match Global.get_symbol env v.var_id with
  | None -> fst (declare uf ctx v.var_id)
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

let type_var_exn (env:Global.t) (uf:Unif.t) (ctx:Local.t) (v:p_var) : t_var =
  match Local.get ctx v.var_id with
  | Some var_typ ->
    begin match Btype.close (Unif.normalize uf var_typ) with
      | None ->
        let str = Printf.sprintf "The type of symbol '%s' could not be fully infered. The type infered so far is '%s'."
            v.var_id (to_string (Unif.normalize uf var_typ)) in
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

let declare_constants (gl:Global.t) (uf:Unif.t) cconst aconst prop =
  let ctx = Local.create () in
  let ctx = fold_list_clause (declare_symb gl uf K_Concrete_Constant) ctx cconst in
  let ctx = fold_list_clause (declare_symb gl uf K_Abstract_Constant) ctx aconst in
  let t_prop = map_clause (type_predicate2_exn {gl;uf;vi=V_Properties} ctx) prop in
  let t_cconst = map_list_clause (type_var_exn gl uf ctx) cconst in
  let t_aconst = map_list_clause (type_var_exn gl uf ctx) aconst in
  let _ = iter_list_clause (promote_symb gl ctx K_Concrete_Constant) t_cconst in
  let _ = iter_list_clause (promote_symb gl ctx K_Abstract_Constant) t_aconst in
  (t_cconst,t_aconst,t_prop)

let declare_variables (gl:Global.t) (uf:Unif.t) cvars avars inv =
  let ctx = Local.create () in
  let ctx = fold_list_clause (declare_symb gl uf K_Concrete_Variable) ctx cvars in
  let ctx = fold_list_clause (declare_symb gl uf K_Abstract_Variable) ctx avars in
  let t_inv = map_clause (type_predicate2_exn {gl;uf;vi=V_Invariant} ctx) inv in
  let t_cvars = map_list_clause (type_var_exn gl uf ctx) cvars in
  let t_avars = map_list_clause (type_var_exn gl uf ctx) avars in
  let _ = iter_list_clause (promote_symb gl ctx K_Concrete_Variable) t_cvars in
  let _ = iter_list_clause (promote_symb gl ctx K_Abstract_Variable) t_avars in
  (t_cvars,t_avars,t_inv)

let declare_list (uf:Unif.t) (ctx:Local.t) (lst:p_var list) : Local.t * (Utils.loc,opn_btype) var list =
  let (ctx,tvars) = List.fold_left
      (fun (ctx,tvars) v ->
         let (ctx,var_typ) = declare uf ctx v.var_id in
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

let get_operation_context (env:Global.t) (uf:Unif.t) (op:p_operation) : Local.t*bool =
  match Global.get_operation env op.op_name.lid_str with
  | None ->
    let ctx = Local.create () in
    let (ctx,op_out) = declare_list uf ctx op.op_out in
    let (ctx,op_in) = declare_list uf ctx op.op_in in
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
      | From_Imported_Mch _ -> () (*FIXME*)
    in
    let ctx = Local.create () in
    let aux ctx (s,ty) = Local.add ctx s (to_open ty) in
    let ctx = List.fold_left aux ctx s.args_in in
    let ctx = List.fold_left aux ctx s.args_out in
    let () = check_signature op s in
    (ctx,false)

let mem id = List.exists (Syntax.ident_eq id)

let rec is_read_only (gl:Global.t) (ctx:ident list) (s:p_substitution) : bool =
  match s.sub_desc with
  | Skip -> true
  | Affectation (xlst,_) | BecomesElt (xlst,_) | BecomesSuch (xlst,_) ->
    let aux v = mem v.var_id ctx in
    List.for_all aux (Nlist.to_list xlst)
  | Function_Affectation (v,_,_) | Record_Affectation (v,_,_) ->
    mem v.var_id ctx
  | CallUp (args_out,id,args_in) ->
    let aux v = mem v.var_id ctx in
    List.for_all aux args_out &&
    ( match Global.get_operation gl id.lid_str with
      | None -> assert false
      | Some (op,_) -> op.is_readonly )
  | Pre (p,s0) -> is_read_only gl ctx s0
  | Assert (p,s0) -> is_read_only gl ctx s0
  | Choice nlst -> List.for_all (is_read_only gl ctx) (Nlist.to_list nlst)
  | IfThenElse (nlst,opt) | Select (nlst,opt) ->
    let aux (_,s0) = is_read_only gl ctx s0 in
    let opt_ro = match opt with
      | None -> true
      | Some s0 -> is_read_only gl ctx s0
    in
    opt_ro && List.for_all aux (Nlist.to_list nlst)
  | Case (_,nlst,opt) ->
    let aux (_,s0) = is_read_only gl ctx s0 in
    let opt_ro = match opt with
      | None -> true
      | Some s0 -> is_read_only gl ctx s0
    in
    opt_ro && List.for_all aux (Nlist.to_list nlst)
  | Any (xlst,_,s0) | Let (xlst,_,s0) | Var (xlst,s0) ->
    let ctx = List.fold_left (fun ctx v -> v.var_id::ctx) ctx (Nlist.to_list xlst) in
    is_read_only gl ctx s0
  | While (p1,s0,p2,e) -> is_read_only gl ctx s0
  | Sequencement (s1,s2) | Parallel (s1,s2) ->
    is_read_only gl ctx s1 && is_read_only gl ctx s2

let declare_local_operation (gl:Global.t) (uf:Unif.t) (op:p_operation) : t_operation =
  match Global.get_operation gl op.op_name.lid_str with
  | None ->
    let ctx = Local.create () in
    let (ctx,op_out) = declare_list uf ctx op.op_out in
    let (ctx,op_in) = declare_list uf ctx op.op_in in
    let body = type_substitution_exn {gl;uf;vi=V_Local_Operations} ctx op.op_body in
    let op_in = List.map (type_var_exn gl uf ctx) op.op_in in
    let op_out = List.map (type_var_exn gl uf ctx) op.op_out in
    let op_body = close_subst uf body in
    let aux v = (v.var_id, v.var_typ) in
    let args_out = List.map aux op_out in
    let args_in  = List.map aux op_in in
    let loc = op.op_name.lid_loc in
    let is_readonly = is_read_only gl (Local.get_vars ctx) op.op_body in
    let () = Global.add_operation gl op.op_name.lid_str {loc; args_in; args_out; is_readonly} true in
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

let declare_operation (gl:Global.t) (uf:Unif.t) (op:p_operation) : t_operation =
  let (ctx,is_new) = get_operation_context gl uf op in
  let body = type_substitution_exn {gl;uf;vi=V_Operations} ctx op.op_body in (*TODO no op_out in PRE?*)
  let op_in = List.map (type_var_exn gl uf ctx) op.op_in in
  let op_out = List.map (type_var_exn gl uf ctx) op.op_out in
  let op_body = close_subst uf body in
  let aux v = (v.var_id, v.var_typ) in
  let args_out = List.map aux op_out in
  let args_in  = List.map aux op_in in
  let is_readonly = is_read_only gl (Local.get_vars ctx) op.op_body in
  ( if is_new then
      let loc = op.op_name.lid_loc in
      Global.add_operation gl op.op_name.lid_str { loc; args_in; args_out; is_readonly } false
    else
      Global.redefine_operation gl op.op_name.lid_str is_readonly );
  { op_name=op.op_name; op_in; op_out; op_body }

let type_machine_exn (f:string -> MachineInterface.t option) (gl:Global.t) (mch:_ machine_desc) : (Utils.loc,btype) machine_desc =
  let uf = Unif.create () in
  let mch_constraints = clause_some_err "Not implemented: machine with clause CONSTRAINTS." mch.mch_constraints in
  let mch_includes = clause_some_err "Not implemented: clause INCLUDES." mch.mch_includes in
  let mch_promotes = clause_some_err "Not implemented: clause PROMOTES." mch.mch_promotes in
  let mch_extends = clause_some_err "Not implemented: clause EXTENDS." mch.mch_extends in
  let mch_uses = clause_some_err "Not implemented: clause USES." mch.mch_uses in
  let () = iter_list_clause (load_seen_mch f gl) mch.mch_sees in
  let mch_sets = map_list_clause (declare_set gl) mch.mch_sets in
  let (mch_concrete_constants,mch_abstract_constants,mch_properties) =
    declare_constants gl uf mch.mch_concrete_constants
      mch.mch_abstract_constants mch.mch_properties
  in
  let (mch_concrete_variables,mch_abstract_variables,mch_invariant) =
    declare_variables gl uf mch.mch_concrete_variables
      mch.mch_abstract_variables mch.mch_invariant
  in
  let ctx = Local.create () in
  let mch_assertions = map_list_clause (type_predicate2_exn {gl;uf;vi=V_Invariant} ctx) mch.mch_assertions in
  let mch_initialisation = map_clause (type_substitution2_exn {gl;uf;vi=V_Operations} ctx) mch.mch_initialisation in
  let mch_operations = map_list_clause (declare_operation gl uf) mch.mch_operations in
  { mch_constraints; mch_sees=mch.mch_sees; mch_includes; mch_promotes; mch_extends;
    mch_uses; mch_sets; mch_concrete_constants; mch_abstract_constants;
    mch_properties; mch_concrete_variables; mch_abstract_variables;
    mch_invariant; mch_assertions; mch_initialisation; mch_operations }

let load_refines f env mch =
  match f mch.lid_str with
  | None -> Error.raise_exn mch.lid_loc ("Unknown machine '"^mch.lid_str^"'.")
  | Some itf -> Global.load_interface env itf (From_Refined_Mch mch)

let type_refinement_exn (f:string->MachineInterface.t option) (gl:Global.t) ref : (Utils.loc,btype) refinement_desc =
  let uf = Unif.create () in
  let () = load_refines f gl ref.ref_refines in
  let ref_includes = clause_some_err "Not implemented: clause INCLUDES." ref.ref_includes in
  let ref_promotes = clause_some_err "Not implemented: clause PROMOTES." ref.ref_promotes in
  let ref_extends = clause_some_err "Not implemented: clause EXTENDS."ref.ref_extends in
  let () = iter_list_clause (load_seen_mch f gl) ref.ref_sees in
  let ref_sets = map_list_clause (declare_set gl) ref.ref_sets in
  let (ref_concrete_constants,ref_abstract_constants,ref_properties) =
    declare_constants gl uf ref.ref_concrete_constants
      ref.ref_abstract_constants ref.ref_properties
  in
  let (ref_concrete_variables,ref_abstract_variables,ref_invariant) =
    declare_variables gl uf ref.ref_concrete_variables
      ref.ref_abstract_variables ref.ref_invariant
  in
  let ctx = Local.create () in
  let ref_assertions = map_list_clause (type_predicate2_exn {gl;uf;vi=V_Invariant} ctx) ref.ref_assertions in
  let ref_initialisation = map_clause (type_substitution2_exn {gl;uf;vi=V_Operations} ctx) ref.ref_initialisation in
  let ref_local_operations = map_list_clause (declare_local_operation gl uf) ref.ref_local_operations in
  let ref_operations = map_list_clause (declare_operation gl uf) ref.ref_operations in
  { ref_refines=ref.ref_refines; ref_sees=ref.ref_sees; ref_includes; ref_promotes;
    ref_extends; ref_sets; ref_concrete_constants; ref_abstract_constants;
    ref_properties; ref_concrete_variables; ref_abstract_variables; ref_invariant;
    ref_assertions; ref_initialisation; ref_operations; ref_local_operations; }

let type_value (gl:Global.t) (uf:Unif.t) (v,e:p_var*p_expression) : t_var*t_expression =
  match Global.get_symbol gl v.var_id with
  | None -> Error.raise_exn v.var_loc ("Unknown symbol '"^v.var_id^"'.")
  | Some (s,_) ->
    let ctx = Local.create () in
    let te = type_expression2_exn {gl;uf;vi=V_Values} ctx e in
    if Unif.is_equal_modulo_alias uf te.exp_typ s.typ then
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

let is_abstract_set env v =
  match Global.get_symbol env v.var_id with
  | None -> false
  | Some (x,_) -> (x.kind = K_Abstract_Set)

let manage_set_concretisation_exn (gl:Global.t) (uf:Unif.t) (v,e) =
  if is_abstract_set gl v then
    let te = type_expression2_exn {gl;uf;vi=V_Values} (Local.create ()) e in
    match te.exp_typ with
    | T_Power ty ->
      if not (Unif.add_alias uf v.var_id ty) then
        Error.raise_exn v.var_loc "Incorrect abstract set definition."
    | _ ->
      let str = Printf.sprintf
          "This expression has type '%s' but an expression of type '%s' was expected."
          (to_string te.exp_typ) (to_string (T_Power (Unif.new_meta uf)))
      in
      Error.raise_exn e.exp_loc str

let type_implementation_exn (f:string -> MachineInterface.t option) (gl:Global.t) imp : (Utils.loc,btype) implementation_desc =
  let uf = Unif.create () in
  let () = load_refines f gl imp.imp_refines in
  let () = iter_list_clause (load_seen_mch f gl) imp.imp_sees in
  let imp_sets = map_list_clause (declare_set gl) imp.imp_sets in
  let () = iter_list_clause (manage_set_concretisation_exn gl uf) imp.imp_values in
  let imp_imports = map_list_clause (load_imported_mch f gl) imp.imp_imports in
  let imp_extends = map_list_clause (load_extended_mch f gl) imp.imp_extends in
  let () = iter_list_clause (promote_op gl) imp.imp_promotes in
  let (imp_concrete_constants,imp_abstract_constants,imp_properties) =
    declare_constants gl uf imp.imp_concrete_constants
      None imp.imp_properties
  in
  let (imp_concrete_variables,imp_abstract_variables,imp_invariant) =
    declare_variables gl uf imp.imp_concrete_variables None imp.imp_invariant
  in
  let imp_values = map_list_clause (type_value gl uf) imp.imp_values in
  let ctx = Local.create () in
  let imp_assertions = map_list_clause (type_predicate2_exn {gl;uf;vi=V_Invariant} ctx) imp.imp_assertions in
  let imp_initialisation = map_clause (type_substitution2_exn {gl;uf;vi=V_Operations} ctx) imp.imp_initialisation in
  let imp_local_operations = map_list_clause (declare_local_operation gl uf) imp.imp_local_operations in
  let imp_operations = map_list_clause (declare_operation gl uf) imp.imp_operations in
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

let get_interface (f:string -> MachineInterface.t option) (co:p_component) : MachineInterface.t Error.t_result =
  let env = Global.create () in
  match type_component f env co with
  | Ok _ -> Ok (Global.to_interface env)
  | Error err -> Error err
