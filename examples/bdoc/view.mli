open Utils
open Component

type t_kind = Machine | Refinement of ident | Implementation of ident
type dep_kind = D_Sees | D_Uses | D_Includes | D_Imports
type exp_source = Declared | Included_From of ident | Inherited_E
type exp_source_op = Declared_Operation | Promoted_From of ident
type vis_source = Seen_From of ident | Used_From of ident | Imported_From of ident | Inherited
type vis_source_op = Op_Seen_From of ident | Op_Imported_From of ident | Op_Included_From of ident | Local_Op

type component_view = {
  name: ident;
  parameters: ident list;
  component_kind: t_kind;
  dependencies: (ident*dep_kind) list;
  refined_by: ident list;
  required_by: (ident*dep_kind) list;
  (* Sets *)
  exported_sets: (set*exp_source) list; 
  visible_sets: (set*vis_source) list;
  (* Constants *)
  exported_abstract_constants: (ident*exp_source) list;
  exported_concrete_constants: (ident*exp_source) list;
  visible_abstract_constants:  (ident*vis_source) list;
  visible_concrete_constants:  (ident*vis_source) list;
  (* Variables *)
  exported_abstract_variables: (ident*exp_source) list; 
  exported_concrete_variables: (ident*exp_source) list;
  visible_abstract_variables:  (ident*vis_source) list;
  visible_concrete_variables:  (ident*vis_source) list;
  (* Operations *)
  exported_operations: (ident*exp_source_op) list;
  visible_operations:  (ident*vis_source_op) list;
}

val make : (string -> component_view option) -> ident list (*refined_by*) ->
  (ident*dep_kind) list (*required_by*) -> component -> component_view
