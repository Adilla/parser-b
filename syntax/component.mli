open Utils
open Expression
open Substitution

type ('lc,'ty) operation = ('ty*'lc ident) list * 'lc ident * ('ty*'lc ident) list * ('lc,'ty) substitution
type ('lc,'ty) machine_instanciation = 'lc ident * ('lc,'ty) expression list

type 'lc set =
  | Abstract_Set of 'lc ident
  | Concrete_Set of 'lc ident * 'lc ident list

type ('lc,'ty) clause =
  | Constraints of 'lc * ('lc,'ty) predicate
  | Imports of 'lc * (('lc,'ty) machine_instanciation) list
  | Sees of 'lc * 'lc ident list
  | Includes of 'lc * ('lc,'ty) machine_instanciation list
  | Extends of 'lc * ('lc,'ty) machine_instanciation list
  | Promotes of 'lc * 'lc ident list
  | Uses of 'lc * 'lc ident list
  | Sets of 'lc * 'lc set list
  | Constants of 'lc * 'lc ident list
  | Abstract_constants of 'lc * 'lc ident list
  | Properties of 'lc * ('lc,'ty) predicate
  | Concrete_variables of 'lc * 'lc ident list
  | Variables of 'lc * 'lc ident list
  | Invariant of 'lc * ('lc,'ty) predicate
  | Assertions of 'lc * ('lc,'ty) predicate list
  | Initialization of 'lc * ('lc,'ty) substitution
  | Operations of 'lc * ('lc,'ty) operation list
  | Local_Operations of 'lc * ('lc,'ty) operation list
  | Values of 'lc * ('lc ident * ('lc,'ty) expression) list


type ('lc,'ty) abstract_machine = {
  name: 'lc ident;
  parameters: 'lc ident list;
  clause_constraints: ('lc * ('lc,'ty) predicate) option;
  clause_sees: ('lc * 'lc ident list) option;
  clause_includes: ('lc * ('lc,'ty) machine_instanciation list) option;
  clause_promotes: ('lc * 'lc ident list) option;
  clause_extends: ('lc * ('lc,'ty) machine_instanciation list) option;
  clause_uses: ('lc * 'lc ident list) option;
  clause_sets: ('lc * 'lc set list) option;
  clause_concrete_constants: ('lc * 'lc ident list) option;
  clause_abstract_constants: ('lc * 'lc ident list) option;
  clause_properties: ('lc * ('lc,'ty) predicate) option;
  clause_concrete_variables: ('lc * 'lc ident list) option;
  clause_abstract_variables: ('lc * 'lc ident list) option;
  clause_invariant: ('lc * ('lc,'ty) predicate) option;
  clause_assertions: ('lc * ('lc,'ty) predicate list) option;
  clause_initialisation: ('lc * ('lc,'ty) substitution) option;
  clause_operations: ('lc * ('lc,'ty) operation list) option;
}

type ('lc,'ty) refinement = {
  name: 'lc ident;
  parameters: 'lc ident list;
  refines: 'lc ident;
  clause_sees: ('lc*'lc ident list) option;
  clause_includes: ('lc*('lc,'ty) machine_instanciation list) option;
  clause_promotes: ('lc*'lc ident list) option;
  clause_extends: ('lc*('lc,'ty) machine_instanciation list) option;
  clause_sets: ('lc*'lc set list) option;
  clause_concrete_constants: ('lc*'lc ident list) option;
  clause_abstract_constants: ('lc*'lc ident list) option;
  clause_properties: ('lc*('lc,'ty) predicate) option;
  clause_concrete_variables: ('lc*'lc ident list) option;
  clause_abstract_variables: ('lc*'lc ident list) option;
  clause_invariant: ('lc*('lc,'ty) predicate) option;
  clause_assertions: ('lc*('lc,'ty) predicate list) option;
  clause_initialisation: ('lc*('lc,'ty) substitution) option;
  clause_operations: ('lc*('lc,'ty) operation list) option;
  clause_local_operations: ('lc*('lc,'ty) operation list) option;
}

type ('lc,'ty) implementation = {
  name: 'lc ident;
  refines: 'lc ident;
  parameters: 'lc ident list;
  clause_sees: ('lc*'lc ident list) option;
  clause_imports: ('lc*('lc,'ty) machine_instanciation list) option;
  clause_promotes: ('lc*'lc ident list) option;
  clause_extends_B0: ('lc*('lc,'ty) machine_instanciation list) option;
  clause_sets: ('lc*'lc set list) option;
  clause_concrete_constants: ('lc*'lc ident list) option;
  clause_properties: ('lc*('lc,'ty) predicate) option;
  clause_values: ('lc*('lc ident*('lc,'ty) expression) list) option;
  clause_concrete_variables: ('lc*'lc ident list) option;
  clause_invariant: ('lc*('lc,'ty) predicate) option;
  clause_assertions: ('lc*('lc,'ty) predicate list) option;
  clause_initialisation_B0: ('lc*('lc,'ty) substitution) option;
  clause_operations_B0: ('lc*('lc,'ty) operation list) option;
  clause_local_operations_B0: ('lc*('lc,'ty) operation list) option;
}

val mch_eq : ('lc,'ty) abstract_machine -> ('lc2,'ty2) abstract_machine -> bool
val ref_eq : ('lc,'ty) refinement -> ('lc2,'ty2) refinement -> bool
val imp_eq : ('lc,'ty) implementation -> ('lc2,'ty2) implementation -> bool

type ('lc,'ty) component = 
  | Abstract_machine of ('lc,'ty) abstract_machine
  | Refinement of ('lc,'ty) refinement
  | Implementation of ('lc,'ty) implementation

val component_eq : ('lc,'ty) component -> ('lc2,'ty2) component -> bool

val mk_machine : 'lc ident -> 'lc ident list -> ('lc,'ty) clause list -> (('lc,'ty) abstract_machine,'lc*string) result
val mk_refinement : 'lc ident -> 'lc ident list -> 'lc ident -> ('lc,'ty) clause list -> (('lc,'ty) refinement,'lc*string) result
val mk_implementation : 'lc ident -> 'lc ident list -> 'lc ident -> ('lc,'ty) clause list -> (('lc,'ty) implementation,'lc*string) result

val clist_of_mch : ('lc,'ty) abstract_machine -> ('lc,'ty) clause list
val clist_of_ref : ('lc,'ty) refinement -> ('lc,'ty) clause list
val clist_of_imp : ('lc,'ty) implementation -> ('lc,'ty) clause list

(* ************* *)

type u_clause = (loc,bool) clause
type u_comp   = (loc,bool) component
type u_operation   = (loc,bool) operation
type u_machine = (loc,bool) abstract_machine
type u_refinement = (loc,bool) refinement
type u_implementation = (loc,bool) implementation

val mk_machine_exn : u_ident -> u_ident list -> u_clause list -> u_machine
val mk_refinement_exn : u_ident -> u_ident list -> u_ident -> u_clause list -> u_refinement
val mk_implementation_exn : u_ident -> u_ident list -> u_ident -> u_clause list -> u_implementation
