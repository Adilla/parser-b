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

type ctype = 
  | Machine
  | Refinement of ident 
  | Implementation of ident

type parsed_component = loc * ctype * ident*ident list*clause list

type specification = {
  loc: loc;
  name: ident;
  parameters: ident list;
  abstraction: ident option;
  clause_sees: (loc*ident list) option;
  clause_sets: (loc*set list) option;
  clause_constants: (loc*ident list) option;
  clause_abstract_constants: (loc*ident list) option;
  clause_properties: (loc*predicate) option;
  clause_concrete_variables: (loc*ident list) option;
  clause_variables: (loc*ident list) option;
  clause_invariant: (loc*predicate) option;
  clause_assertions: (loc*predicate list) option;
  clause_initialization: (loc*substitution) option;
  clause_operations: (loc*operation list) option;
}

type implementation = {
  loc: loc;
  abstraction: ident;
  name: ident;
  parameters: ident list;
  clause_imports: (loc*machine_instanciation list) option;
  clause_sees: (loc*ident list) option;
  clause_promotes: (loc*ident list) option;
  clause_sets: (loc*set list) option;
  clause_constants: (loc*ident list) option;
  clause_properties: (loc*predicate) option;
  clause_concrete_variables: (loc*ident list) option;
  clause_invariant: (loc*predicate) option;
  clause_assertions: (loc*predicate list) option;
  clause_initialization: (loc*substitution) option;
  clause_operations: (loc*operation list) option;
  clause_local_operations: (loc*operation list) option;
  clause_values: (loc*(ident*expression) list) option;
}

type component = 
  | Spec of specification
  | Implem of implementation

val mk_component : parsed_component -> (component,loc*string) result
