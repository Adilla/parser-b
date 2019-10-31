module G = Global
val extended_sees: bool ref

type 'mr clause =
  | C_CONSTRAINTS : 'mr clause
  | C_PROPERTIES : 'mr clause
  | C_INVARIANT : 'mr clause
  | C_MCH_OPERATIONS : G.t_mch clause
  | C_REF_OPERATIONS : G.t_ref clause
  | C_IMP_OPERATIONS : G.t_ref clause
  | C_ASSERT : 'mr clause
  | C_LOCAL_OPERATIONS : G.t_ref clause
  | C_VALUES : G.t_ref clause
  | C_MCH_PARAMTERS : G.t_mch clause

(*
val mk_assert_clause: ('mr) clause -> ('mr) clause
   *)

val get_ident_in_clause: ('mr) clause -> 'mr G.t_kind -> 'mr G.t_kind option 
val get_mutable_in_clause: ('mr) clause -> 'mr G.t_kind -> 'mr G.t_kind option 
    (*

val make_mch_prop: G.t_mch G.t_kind -> (G.t_mch,t_mch_prop) t_global_ident option
val make_mch_inv: G.t_mch G.t_kind -> (G.t_mch,t_mch_inv) t_global_ident option
val make_mch_op: G.t_mch G.t_kind -> (G.t_mch,t_mch_op) t_global_ident option
val make_mch_mut: G.t_mch G.t_kind -> (G.t_mch,t_mch_op) t_mutable_ident option
val make_mch_assert: G.t_mch G.t_kind -> (G.t_mch,t_mch_op t_assert) t_global_ident option


val make_ref_prop: G.t_ref G.t_kind -> (G.t_ref,t_ref_prop) t_global_ident option
val make_ref_inv: G.t_ref G.t_kind -> (G.t_ref,t_ref_inv) t_global_ident option
val make_ref_op: G.t_ref G.t_kind -> (G.t_ref,t_ref_op) t_global_ident option
val make_ref_mut: G.t_ref G.t_kind -> (G.t_ref,t_ref_op) t_mutable_ident option
val make_ref_assert: G.t_ref G.t_kind -> (G.t_ref,t_ref_op t_assert) t_global_ident option


val make_imp_prop: G.t_ref G.t_kind -> (G.t_ref,t_imp_prop) t_global_ident option
val make_imp_inv: G.t_ref G.t_kind -> (G.t_ref,t_imp_inv) t_global_ident option
val make_imp_op: G.t_ref G.t_kind -> (G.t_ref,t_imp_op) t_global_ident option
val make_imp_mut: G.t_ref G.t_kind -> (G.t_ref,t_imp_op) t_mutable_ident option
val make_imp_assert: G.t_ref G.t_kind -> (G.t_ref,t_imp_op t_assert) t_global_ident option
val make_imp_val: G.t_ref G.t_kind -> (G.t_ref,t_imp_val) t_global_ident option

val make_imp_lop: G.t_ref G.t_kind -> (G.t_ref,t_imp_lop) t_global_ident option
val make_imp_lmut: G.t_ref G.t_kind -> (G.t_ref,t_imp_lop) t_mutable_ident option
val make_imp_lassert: G.t_ref G.t_kind -> (G.t_ref,t_imp_lop t_assert) t_global_ident option

type t_imp_op_view =
  | IOV_Concrete_Variable of (G.t_ref,G.t_concrete) G.t_decl
  | IOV_Concrete_Constant of (G.t_ref,G.t_concrete) G.t_decl
  | IOV_Abstract_Set of (G.t_ref,G.t_concrete) G.t_decl
  | IOV_Concrete_Set of string list * (G.t_ref,G.t_concrete) G.t_decl
  | IOV_Enumerate of (G.t_ref,G.t_concrete) G.t_decl

type t_imp_val_view =
  | IVV_Concrete_Constant of (G.t_ref,G.t_concrete) G.t_decl
  | IVV_Abstract_Set of (G.t_ref,G.t_concrete) G.t_decl
  | IVV_Concrete_Set of string list * (G.t_ref,G.t_concrete) G.t_decl
  | IVV_Enumerate of (G.t_ref,G.t_concrete) G.t_decl

type t_imp_mut_view =
  | IMV_Concrete_Variable_From_Machine of Utils.loc
  | IMV_Concrete_Variable_Implicitely_Redeclared
  | IMV_Concrete_Variable_Redeclared_By_Machine of Utils.loc

val view_imp_op: (G.t_ref,t_imp_op) t_global_ident -> t_imp_op_view
val view_imp_val: (G.t_ref,t_imp_val) t_global_ident -> t_imp_val_view
val view_imp_mut: (G.t_ref,t_imp_op) t_mutable_ident -> t_imp_mut_view
*)
