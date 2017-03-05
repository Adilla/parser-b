open Utils
open Expression
open Substitution

type operation = ident list * ident * ident list * substitution
type machine_instanciation = ident * expression list

type set =
  | Abstract_Set of ident
  | Concrete_Set of ident*ident list

type clause =
  | Constraints of loc * predicate
  | Imports of loc * (ident*expression list) list
  | Sees of loc * ident list
  | Includes of loc * machine_instanciation list
  | Extends of loc * machine_instanciation list
  | Promotes of loc * ident list
  | Uses of loc * ident list
  | Sets of loc * set list
  | Constants of loc * ident list
  | Abstract_constants of loc * ident list
  | Properties of loc * predicate
  | Concrete_variables of loc * ident list
  | Variables of loc * ident list
  | Invariant of loc * predicate
  | Assertions of loc * predicate list
  | Initialization of loc * substitution
  | Operations of loc * operation list
  | Local_Operations of loc * operation list
  | Values of loc * (ident*expression) list

type abstract_machine = {
  name: ident;
  parameters: ident list;
  clause_constraints: (loc*predicate) option;
  clause_sees: (loc*ident list) option;
  clause_includes: (loc*machine_instanciation list) option;
  clause_promotes: (loc*ident list) option;
  clause_extends: (loc*machine_instanciation list) option;
  clause_uses: (loc*ident list) option;
  clause_sets: (loc*set list) option;
  clause_concrete_constants: (loc*ident list) option;
  clause_abstract_constants: (loc*ident list) option;
  clause_properties: (loc*predicate) option;
  clause_concrete_variables: (loc*ident list) option;
  clause_abstract_variables: (loc*ident list) option;
  clause_invariant: (loc*predicate) option;
  clause_assertions: (loc*predicate list) option;
  clause_initialisation: (loc*substitution) option;
  clause_operations: (loc*operation list) option;
}

val mch_eq : abstract_machine -> abstract_machine -> bool

type refinement = {
  name: ident;
  parameters: ident list;
  refines: ident;
  clause_sees: (loc*ident list) option;
  clause_includes: (loc*machine_instanciation list) option;
  clause_promotes: (loc*ident list) option;
  clause_extends: (loc*machine_instanciation list) option;
  clause_sets: (loc*set list) option;
  clause_concrete_constants: (loc*ident list) option;
  clause_abstract_constants: (loc*ident list) option;
  clause_properties: (loc*predicate) option;
  clause_concrete_variables: (loc*ident list) option;
  clause_abstract_variables: (loc*ident list) option;
  clause_invariant: (loc*predicate) option;
  clause_assertions: (loc*predicate list) option;
  clause_initialisation: (loc*substitution) option;
  clause_operations: (loc*operation list) option;
  clause_local_operations: (loc*operation list) option;
}

val ref_eq : refinement -> refinement -> bool

type implementation = {
  name: ident;
  refines: ident;
  parameters: ident list;
  clause_sees: (loc*ident list) option;
  clause_imports: (loc*machine_instanciation list) option;
  clause_promotes: (loc*ident list) option;
  clause_extends_B0: (loc*machine_instanciation list) option;
  clause_sets: (loc*set list) option;
  clause_concrete_constants: (loc*ident list) option;
  clause_properties: (loc*predicate) option;
  clause_values: (loc*(ident*expression) list) option;
  clause_concrete_variables: (loc*ident list) option;
  clause_invariant: (loc*predicate) option;
  clause_assertions: (loc*predicate list) option;
  clause_initialisation_B0: (loc*substitution) option;
  clause_operations_B0: (loc*operation list) option;
  clause_local_operations_B0: (loc*operation list) option;
}

val imp_eq : implementation -> implementation -> bool

type component = 
  | Abstract_machine of abstract_machine
  | Refinement of refinement
  | Implementation of implementation

val component_eq : component -> component -> bool

val mk_machine_exn : ident -> ident list -> clause list -> abstract_machine
val mk_refinement_exn : ident -> ident list -> ident -> clause list -> refinement
val mk_implementation_exn : ident -> ident list -> ident -> clause list -> implementation

val ef_machine : abstract_machine -> Easy_format.t 
val ef_refinement : refinement -> Easy_format.t 
val ef_implementation : implementation -> Easy_format.t 
val ef_component : component -> Easy_format.t 

val norm_mch : abstract_machine -> abstract_machine
val norm_ref : refinement -> refinement
val norm_imp : implementation -> implementation
val norm_component : component -> component
