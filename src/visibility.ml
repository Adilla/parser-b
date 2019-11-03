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
  | C_MCH_PARAMETERS : G.t_mch clause
  | C_REF_PARAMETERS : G.t_ref clause
  | C_IMP_PARAMETERS : G.t_ref clause

type 'mr mclause =
  | M_MCH_OPERATIONS : G.t_mch mclause
  | M_REF_OPERATIONS : G.t_ref mclause
  | M_IMP_OPERATIONS : G.t_ref mclause
  | M_LOCAL_OPERATIONS : G.t_ref mclause

let get_ident_in_clause (type mr) (cl:mr clause) (ki:mr G.t_kind) =
  (*FIXME parameters*)
  match cl, ki with
  (* In the CONSTRAINTS clause
   * only parameters are visible *)
  | C_CONSTRAINTS, G.K_Parameter _ -> Some ki
  | C_CONSTRAINTS, _ -> None
  (* In the PROPERTIES clause
   * parameters variables are NOT visible *)
  | C_PROPERTIES, G.K_Parameter _ -> None
  | C_PROPERTIES, G.K_Abstract_Variable _ -> None
  | C_PROPERTIES, G.K_Concrete_Variable _ -> None
  | C_PROPERTIES, _ -> Some ki
  (* In the INVARIANT clause
   * variables from seen machines are visible only if the option extended_sees is set *)
  | C_INVARIANT, G.K_Concrete_Variable (G.D_Seen _) when not !extended_sees -> None
  | C_INVARIANT, G.K_Abstract_Variable (G.D_Seen _) when not !extended_sees -> None
  | C_INVARIANT, _ -> Some ki
  (* In clause ASSERT
   * everything is visible *)
  | C_ASSERT, _ -> Some ki
  (* In the OPERATIONS clause in an abstract machines
   * everything is visible *)
  | C_MCH_OPERATIONS, _ -> Some ki
  (* In the OPERATIONS clause in a refinement
   * disappearing datas are not visible *)
  | C_REF_OPERATIONS, G.K_Abstract_Constant G.D_Disappearing -> None
  | C_REF_OPERATIONS, G.K_Abstract_Variable G.D_Disappearing -> None
  | C_REF_OPERATIONS, _ -> Some ki
  (* In the OPERATIONS clause in an implementation
   * Abstract datas are not visible *)
  | C_IMP_OPERATIONS, G.K_Abstract_Constant _ -> None
  | C_IMP_OPERATIONS, G.K_Abstract_Variable _ -> None
  | C_IMP_OPERATIONS, _ -> Some ki
  (* In the LOCAL_OPERATIONS clause in an implementation
   * disappearing datas are not visible *)
  | C_LOCAL_OPERATIONS, G.K_Abstract_Constant G.D_Disappearing -> None
  | C_LOCAL_OPERATIONS, G.K_Abstract_Variable G.D_Disappearing -> None
  | C_LOCAL_OPERATIONS, _ -> Some ki
(* In the VALUES clause
 * Only concrete data are visible (parameters excluded) *)
  | C_VALUES, G.K_Concrete_Constant _ -> Some ki
  | C_VALUES, G.K_Abstract_Set _ -> Some ki
  | C_VALUES, G.K_Concrete_Set _ -> Some ki
  | C_VALUES, G.K_Enumerate _ -> Some ki
  | C_VALUES, _ -> None
(* INCLUDES/EXTENDS parameters in a machine *)
  | C_MCH_PARAMETERS, G.K_Parameter _ -> Some ki
  | C_MCH_PARAMETERS, G.K_Concrete_Constant (G.D_Machine _|G.D_Seen _) -> Some ki
  | C_MCH_PARAMETERS, G.K_Concrete_Constant (G.D_Included_Or_Imported _|G.D_Used _) -> None
  | C_MCH_PARAMETERS, G.K_Abstract_Constant (G.D_Machine _|G.D_Seen _) -> Some ki
  | C_MCH_PARAMETERS, G.K_Abstract_Constant (G.D_Included_Or_Imported _|G.D_Used _) -> None
  | C_MCH_PARAMETERS, G.K_Concrete_Set (_,(G.D_Machine _|G.D_Seen _)) -> Some ki
  | C_MCH_PARAMETERS, G.K_Concrete_Set (_,(G.D_Included_Or_Imported _|G.D_Used _)) -> None
  | C_MCH_PARAMETERS, G.K_Abstract_Set (G.D_Machine _|G.D_Seen _) -> Some ki
  | C_MCH_PARAMETERS, G.K_Abstract_Set (G.D_Included_Or_Imported _|G.D_Used _) -> None
  | C_MCH_PARAMETERS, G.K_Enumerate (G.D_Machine _|G.D_Seen _) -> Some ki
  | C_MCH_PARAMETERS, G.K_Enumerate (G.D_Included_Or_Imported _|G.D_Used _) -> None
  | C_MCH_PARAMETERS, G.K_Abstract_Variable _ -> None
  | C_MCH_PARAMETERS, G.K_Concrete_Variable _ -> None
(* INCLUDES/EXTENDS parameters in a refinemnt *)
  | C_REF_PARAMETERS , _ -> assert false (*FIXME*)
(* INCLUDES/EXTENDS parameters in an implementation *)
  | C_IMP_PARAMETERS , _ -> assert false (*FIXME*)

let get_mutable_in_clause (type mr) (cl:mr mclause) (ki:mr G.t_kind) : mr G.t_kind option =
  match cl, ki with
  | M_MCH_OPERATIONS, G.K_Concrete_Variable (G.D_Machine _) -> Some ki
  | M_MCH_OPERATIONS, G.K_Abstract_Variable (G.D_Machine _) -> Some ki
  | M_MCH_OPERATIONS, _ -> None
  | M_REF_OPERATIONS, G.K_Abstract_Variable (G.D_Machine _) -> Some ki
  | M_REF_OPERATIONS, G.K_Abstract_Variable (G.D_Redeclared G.By_Machine _) -> Some ki
  | M_REF_OPERATIONS, G.K_Concrete_Variable (G.D_Machine _ ) -> Some ki
  | M_REF_OPERATIONS, G.K_Concrete_Variable (G.D_Redeclared G.By_Machine _) -> Some ki
  | M_REF_OPERATIONS, G.K_Concrete_Variable (G.D_Redeclared G.Implicitely) -> Some ki
  | M_REF_OPERATIONS, _ -> None
  | M_IMP_OPERATIONS, G.K_Concrete_Variable (G.D_Machine _) -> Some ki
  | M_IMP_OPERATIONS, G.K_Concrete_Variable (G.D_Redeclared G.Implicitely) -> Some ki
  | M_IMP_OPERATIONS, G.K_Concrete_Variable (G.D_Redeclared G.By_Machine _) -> Some ki
  | M_IMP_OPERATIONS, _ -> None

  | M_LOCAL_OPERATIONS, G.K_Concrete_Variable (G.D_Machine _) -> Some ki
  | M_LOCAL_OPERATIONS, G.K_Concrete_Variable (G.D_Redeclared G.By_Machine _)-> Some ki
  | M_LOCAL_OPERATIONS, G.K_Concrete_Variable (G.D_Redeclared G.Implicitely)-> Some ki
  | M_LOCAL_OPERATIONS, G.K_Concrete_Variable (G.D_Included_Or_Imported _)-> Some ki
  | M_LOCAL_OPERATIONS, G.K_Concrete_Variable (G.D_Redeclared G.By_Included_Or_Imported _)-> Some ki
  | M_LOCAL_OPERATIONS, G.K_Abstract_Variable (G.D_Included_Or_Imported _) -> Some ki
  | M_LOCAL_OPERATIONS, G.K_Abstract_Variable (G.D_Redeclared G.By_Included_Or_Imported _) -> Some ki
  | M_LOCAL_OPERATIONS, _ -> None

let to_clause (type mr) (cl:mr mclause) : mr clause =
  match cl with
  | M_MCH_OPERATIONS -> C_MCH_OPERATIONS
  | M_REF_OPERATIONS -> C_REF_OPERATIONS
  | M_IMP_OPERATIONS -> C_IMP_OPERATIONS
  | M_LOCAL_OPERATIONS -> C_LOCAL_OPERATIONS
