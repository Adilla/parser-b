open Utils
open Component

type t_kind = Machine | Refinement of u_ident | Implementation of u_ident
type dep_kind = D_Sees | D_Uses | D_Includes | D_Imports
type exp_source = Declared | Included_From of u_ident | Inherited_E
type exp_source_op = Declared_Operation | Promoted_From of u_ident
type vis_source = Seen_From of u_ident | Used_From of u_ident | Imported_From of u_ident | Inherited
type vis_source_op = Op_Seen_From of u_ident | Op_Imported_From of u_ident | Op_Included_From of u_ident | Local_Op

type component_view = {
  name: u_ident;
  parameters: u_ident list;
  component_kind: t_kind;
  dependencies: (u_ident*dep_kind) list;
  refined_by: u_ident list;
  required_by: (u_ident*dep_kind) list;
  (* Sets *)
  exported_sets: (loc set*exp_source) list; 
  visible_sets: (loc set*vis_source) list;
  (* Constants *)
  exported_abstract_constants: (u_ident*exp_source) list;
  exported_concrete_constants: (u_ident*exp_source) list;
  visible_abstract_constants:  (u_ident*vis_source) list;
  visible_concrete_constants:  (u_ident*vis_source) list;
  (* Variables *)
  exported_abstract_variables: (u_ident*exp_source) list; 
  exported_concrete_variables: (u_ident*exp_source) list;
  visible_abstract_variables:  (u_ident*vis_source) list;
  visible_concrete_variables:  (u_ident*vis_source) list;
  (* Operations *)
  exported_operations: (u_ident*exp_source_op) list;
  visible_operations:  (u_ident*vis_source_op) list;
}

val make : (string -> component_view option) -> u_ident list (*refined_by*) ->
  (u_ident*dep_kind) list (*required_by*) -> u_comp -> component_view
