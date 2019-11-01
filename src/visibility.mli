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
  | C_MCH_PARAMETERS : G.t_mch clause
  | C_REF_PARAMETERS : G.t_ref clause
  | C_IMP_PARAMETERS : G.t_ref clause

type 'mr mclause =
  | M_MCH_OPERATIONS : G.t_mch mclause
  | M_REF_OPERATIONS : G.t_ref mclause
  | M_IMP_OPERATIONS : G.t_ref mclause
  | M_LOCAL_OPERATIONS : G.t_ref mclause

val get_ident_in_clause: 'mr clause -> 'mr G.t_kind -> 'mr G.t_kind option 
val get_mutable_in_clause: 'mr mclause -> 'mr G.t_kind -> 'mr G.t_kind option 
val to_clause : 'mr mclause -> 'mr clause
