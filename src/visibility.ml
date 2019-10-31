module G = Global

let extended_sees = ref false

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

let make_mch_mut: G.t_mch G.t_kind -> (G.t_mch,t_mch_op) t_mutable_ident option = function
  | G.Pack(G.K_Abstract_Variable,G.D_Machine _) as x -> Some x
  | G.Pack(G.K_Concrete_Variable,G.D_Machine _) as x -> Some x
  | _ -> None

let make_ref_mut: G.t_ref G.t_kind -> (G.t_ref,t_ref_op) t_mutable_ident option = function
  | G.Pack(G.K_Abstract_Variable, G.D_Machine _) as x -> Some x
  | G.Pack(G.K_Abstract_Variable, G.D_Redeclared G.By_Machine _) as x -> Some x
  | G.Pack(G.K_Concrete_Variable, G.D_Machine _ ) as x -> Some x
  | G.Pack(G.K_Concrete_Variable, G.D_Redeclared G.By_Machine _) as x -> Some x
  | G.Pack(G.K_Concrete_Variable, G.D_Redeclared G.Implicitely) as x -> Some x
  | _ -> None

let make_imp_mut: G.t_ref G.t_kind -> (G.t_ref,t_imp_op) t_mutable_ident option = function
  | G.Pack(G.K_Concrete_Variable,G.D_Machine _) as x -> Some x
  | G.Pack(G.K_Concrete_Variable,G.D_Redeclared G.Implicitely) as x -> Some x
  | G.Pack(G.K_Concrete_Variable,G.D_Redeclared G.By_Machine _) as x -> Some x
  | _ -> None

let make_imp_val: G.t_ref G.t_kind -> (G.t_ref,t_imp_val) t_global_ident option = function
  | G.Pack(G.K_Concrete_Constant,_) as x -> Some x
  | G.Pack(G.K_Abstract_Set,_) as x -> Some x
  | G.Pack(G.K_Concrete_Set _,_) as x -> Some x
  | G.Pack(G.K_Enumerate,_) as x -> Some x
  | _ -> None

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

let view_imp_op: (G.t_ref,t_imp_val) t_global_ident -> t_imp_op_view = function
  | G.Pack(G.K_Concrete_Variable,d) -> IOV_Concrete_Variable d
  | G.Pack(G.K_Concrete_Constant,d) -> IOV_Concrete_Constant d
  | G.Pack(G.K_Abstract_Set,d) -> IOV_Abstract_Set d
  | G.Pack(G.K_Concrete_Set elts,d) -> IOV_Concrete_Set (elts,d)
  | G.Pack(G.K_Enumerate,d) -> IOV_Enumerate d
  | _ -> assert false

let view_imp_val: (G.t_ref,t_imp_val) t_global_ident -> t_imp_val_view = function
  | G.Pack(G.K_Concrete_Constant,d) -> IVV_Concrete_Constant d
  | G.Pack(G.K_Abstract_Set,d) -> IVV_Abstract_Set d
  | G.Pack(G.K_Concrete_Set elts,d) -> IVV_Concrete_Set (elts,d)
  | G.Pack(G.K_Enumerate,d) -> IVV_Enumerate d
  | _ -> assert false

let view_imp_mut: (G.t_ref,t_imp_val) t_mutable_ident -> t_imp_mut_view = function
  | G.Pack(G.K_Concrete_Variable,G.D_Machine l) -> IMV_Concrete_Variable_From_Machine l
  | G.Pack(G.K_Concrete_Variable,G.D_Redeclared G.Implicitely) -> IMV_Concrete_Variable_Implicitely_Redeclared
  | G.Pack(G.K_Concrete_Variable,G.D_Redeclared G.By_Machine l) -> IMV_Concrete_Variable_Redeclared_By_Machine l
  | _ -> assert false

let make_imp_lop: G.t_ref G.t_kind -> (G.t_ref,t_imp_lop) t_global_ident option = function
  | G.Pack(G.K_Abstract_Variable,G.D_Disappearing) -> None
  | G.Pack(G.K_Abstract_Constant,G.D_Disappearing) -> None
  | x -> Some x

let make_imp_lmut: G.t_ref G.t_kind -> (G.t_ref,t_imp_lop) t_mutable_ident option = function
  | G.Pack(G.K_Concrete_Variable,G.D_Machine _) as x -> Some x
  | G.Pack(G.K_Concrete_Variable,G.D_Redeclared G.By_Machine _) as x-> Some x
  | G.Pack(G.K_Concrete_Variable,G.D_Redeclared G.Implicitely) as x-> Some x
  | G.Pack(G.K_Concrete_Variable,G.D_Included_Or_Imported _) as x-> Some x
  | G.Pack(G.K_Concrete_Variable,G.D_Redeclared G.By_Included_Or_Imported _) as x-> Some x
  | G.Pack(G.K_Abstract_Variable,G.D_Included_Or_Imported _) as x -> Some x
  | G.Pack(G.K_Abstract_Variable,G.D_Redeclared G.By_Included_Or_Imported _) as x -> Some x
  | _ -> None


let make_mch_param: G.t_mch G.t_kind -> (G.t_mch,t_mch_param) t_global_ident option = function (*FIXME*)
  | G.Pack(G.K_Parameter _,_) as x -> Some x
  | G.Pack(G.K_Concrete_Constant,_) as x -> Some x
  | G.Pack(G.K_Abstract_Set,_) as x -> Some x
  | G.Pack(G.K_Concrete_Set _,_) as x -> Some x
  | G.Pack(G.K_Enumerate,_) as x -> Some x
  | _ -> None

let make_ref_param: G.t_ref G.t_kind -> (G.t_ref,t_ref_param) t_global_ident option = function (*FIXME*)
  | G.Pack(G.K_Parameter _,_) as x -> Some x
  | G.Pack(G.K_Concrete_Constant,_) as x -> Some x
  | G.Pack(G.K_Abstract_Set,_) as x -> Some x
  | G.Pack(G.K_Concrete_Set _,_) as x -> Some x
  | G.Pack(G.K_Enumerate,_) as x -> Some x
  | _ -> None

let make_imp_param: G.t_ref G.t_kind -> (G.t_ref,t_imp_param) t_global_ident option = function (*FIXME*)
  | G.Pack(G.K_Parameter _,_) as x -> Some x
  | G.Pack(G.K_Concrete_Constant,_) as x -> Some x
  | G.Pack(G.K_Abstract_Set,_) as x -> Some x
  | G.Pack(G.K_Concrete_Set _,_) as x -> Some x
  | G.Pack(G.K_Enumerate,_) as x -> Some x
  | _ -> None
   *)

let get_ident_in_clause (type mr) (cl:mr clause) (ki:mr G.t_kind) =
  (*FIXME parameters*)
  match cl, ki with
  (* In the CONSTRAINTS clause
   * only parameters are visible *)
  | C_CONSTRAINTS, G.K_Parameter _ -> Some ki
  | C_CONSTRAINTS, _ -> None
  (* In the PROPERTIES clause
   * variables are NOT visible *)
  | C_PROPERTIES, G.K_Abstract_Variable _ -> None
  | C_PROPERTIES, G.K_Concrete_Variable _ -> None
  | C_PROPERTIES, _ -> Some ki
  (* In the INVARIANT clause
   * variables from seen machines are visible only if the option extended_sees is set *)
(*   | C_INVARIANT, G.Pack(G.K_Concrete_Variable,D_Seen _) -> if !extended_sees then Some ki else None *)
  | C_INVARIANT, G.K_Concrete_Variable (D_Seen _) when not !extended_sees -> None
  | C_INVARIANT, G.K_Abstract_Variable (D_Seen _) when not !extended_sees -> None
  | C_INVARIANT, _ -> Some ki
  (* In clause ASSERT
   * everything is visible *)
  | C_ASSERT, _ -> Some ki
  (* In the OPERATIONS clause in an abstract machines
   * everything is visible *)
  | C_MCH_OPERATIONS, _ -> Some ki
  (* In the OPERATIONS clause in a refinement
   * disappearing datas are not visible *)
  | C_REF_OPERATIONS, G.K_Abstract_Constant D_Disappearing -> None
  | C_REF_OPERATIONS, G.K_Abstract_Variable D_Disappearing -> None
  | C_REF_OPERATIONS, _ -> Some ki
  (* In the OPERATIONS clause in an implementation
   * Abstract datas are not visible *)
  | C_IMP_OPERATIONS, G.K_Abstract_Constant _ -> None
  | C_IMP_OPERATIONS, G.K_Abstract_Variable _ -> None
  | C_IMP_OPERATIONS, _ -> Some ki
  (* In the LOCAL_OPERATIONS clause in an implementation
   * disappearing datas are not visible *)
  | C_LOCAL_OPERATIONS, G.K_Abstract_Constant D_Disappearing -> None
  | C_LOCAL_OPERATIONS, G.K_Abstract_Variable D_Disappearing -> None
  | C_LOCAL_OPERATIONS, _ -> Some ki
(* In the VALUES clause
 * Only concrete data are visible (parameters excluded) *)
  | C_VALUES, G.K_Concrete_Constant _ -> Some ki
  | C_VALUES, G.K_Abstract_Set _ -> Some ki
  | C_VALUES, G.K_Concrete_Set _ -> Some ki
  | C_VALUES, G.K_Enumerate _ -> Some ki
  | C_VALUES, _ -> None

(* dans une machine *)
  | C_MCH_PARAMTERS, G.K_Parameter _ -> Some ki
  | C_MCH_PARAMTERS, G.K_Concrete_Constant (D_Machine _|D_Seen _) -> Some ki
  | C_MCH_PARAMTERS, G.K_Concrete_Constant (D_Included_Or_Imported _|D_Used _) -> None
  | C_MCH_PARAMTERS, G.K_Abstract_Constant (D_Machine _|D_Seen _) -> Some ki
  | C_MCH_PARAMTERS, G.K_Abstract_Constant (D_Included_Or_Imported _|D_Used _) -> None
  | C_MCH_PARAMTERS, G.K_Concrete_Set (_,(D_Machine _|D_Seen _)) -> Some ki
  | C_MCH_PARAMTERS, G.K_Concrete_Set (_,(D_Included_Or_Imported _|D_Used _)) -> None
  | C_MCH_PARAMTERS, G.K_Abstract_Set (D_Machine _|D_Seen _) -> Some ki
  | C_MCH_PARAMTERS, G.K_Abstract_Set (D_Included_Or_Imported _|D_Used _) -> None
  | C_MCH_PARAMTERS, G.K_Enumerate (D_Machine _|D_Seen _) -> Some ki
  | C_MCH_PARAMTERS, G.K_Enumerate (D_Included_Or_Imported _|D_Used _) -> None
  | C_MCH_PARAMTERS, G.K_Abstract_Variable _ -> None
  | C_MCH_PARAMTERS, G.K_Concrete_Variable _ -> None


let get_mutable_in_clause (type mr) (cl:mr clause) (ki:mr G.t_kind) : mr G.t_kind option =
  assert false (*FIXME*)
(*
  match cl with
  | C_Mch_Op -> make_mch_mut ki
  | C_Ref_Op -> make_ref_mut ki
  | C_Imp_Op -> make_imp_mut ki
  | C_Imp_Lop -> make_imp_lmut ki
  | _ -> assert false
*)
