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

(* ******** *)

exception Error of loc*string

let check_none lc = function
  | None -> ()
  | Some _ -> raise (Error (lc,"This clause is defined twice."))

let add_clause_spec (co:specification) (cl:clause) : specification =
  match cl with
  | Sees (lc,lst) ->
    ( check_none lc co.clause_sees; { co with clause_sees = Some(lc,lst) } )
  | Sets (lc,lst) ->
    ( check_none lc co.clause_sets; { co with clause_sets = Some(lc,lst) } )
  | Constants (lc,lst) ->
    ( check_none lc co.clause_constants;
      { co with clause_constants = Some(lc,lst) } )
  | Abstract_constants (lc,lst) ->
    ( check_none lc co.clause_abstract_constants;
      { co with clause_abstract_constants = Some(lc,lst) } )
  | Properties (lc,p) ->
    ( check_none lc co.clause_properties;
      { co with clause_properties = Some (lc,p) } )
  | Concrete_variables (lc,lst) ->
    ( check_none lc co.clause_concrete_variables;
      { co with clause_concrete_variables = Some(lc,lst) } )
  | Variables (lc,lst) ->
    ( check_none lc co.clause_variables;
      { co with clause_variables = Some(lc,lst) } )
  | Invariant (lc,p) ->
    ( check_none lc co.clause_invariant;
      { co with clause_invariant = Some (lc,p) } )
  | Assertions (lc,lst) ->
    ( check_none lc co.clause_assertions;
      { co with clause_assertions = Some (lc,lst) } )
  | Initialization (lc,p) ->
    ( check_none lc co.clause_initialization;
      { co with clause_initialization = Some (lc,p) } )
  | Operations (lc,lst) ->
    ( check_none lc co.clause_operations; { co with clause_operations = Some(lc,lst) } )
  | Values (lc,_) ->
    raise (Error (lc, "Clause VALUES not allowed in machines."))
  | Local_Operations (lc,_) ->
    raise (Error (lc, "Clause LOCAL_OPERATIONS not allowed in machines."))
  | Promotes (lc,_) ->
    raise (Error (lc, "Clause PROMOTES not allowed in machines."))
  | Imports (lc,_) ->
    raise (Error (lc, "Clause IMPORTS not allowed in machines."))
  | Constraints (lc,_) ->
    raise (Error (lc, "Not implemented: clause CONSTRAINTS.")) (*TODO*)
  | Includes (lc,_) ->
    raise (Error (lc, "Not implemented: clause INCLUDES.")) (*TODO*)
  | Extends (lc,_) ->
    raise (Error (lc, "Not implemented: clause EXTENDS.")) (*TODO*)
  | Uses (lc,_) ->
    raise (Error (lc, "Not implemented: clause USES.")) (*TODO*)

let add_clause_implem (co:implementation) (cl:clause) : implementation =
  match cl with
  | Sees (lc,lst) ->
    ( check_none lc co.clause_sees; { co with clause_sees = Some(lc,lst) } )
  | Sets (lc,lst) ->
    ( check_none lc co.clause_sets; { co with clause_sets = Some(lc,lst) } )
  | Constants (lc,lst) ->
    ( check_none lc co.clause_constants;
      { co with clause_constants = Some(lc,lst) } )
  | Properties (lc,p) ->
    ( check_none lc co.clause_properties;
      { co with clause_properties = Some (lc,p) } )
  | Concrete_variables (lc,lst) ->
    ( check_none lc co.clause_concrete_variables;
      { co with clause_concrete_variables = Some(lc,lst) } )
  | Invariant (lc,p) ->
    ( check_none lc co.clause_invariant;
      { co with clause_invariant = Some (lc,p) } )
  | Assertions (lc,lst) ->
    ( check_none lc co.clause_assertions;
      { co with clause_assertions = Some (lc,lst) } )
  | Initialization (lc,p) ->
    ( check_none lc co.clause_initialization;
      { co with clause_initialization = Some (lc,p) } )
  | Operations (lc,lst) ->
    ( check_none lc co.clause_operations;
      { co with clause_operations = Some(lc,lst) } )
  | Values (lc,lst) ->
    ( check_none lc co.clause_values;
      { co with clause_values = Some(lc,lst) } )
  | Local_Operations (lc,lst) ->
    ( check_none lc co.clause_local_operations;
      { co with clause_local_operations = Some(lc,lst) } )
  | Promotes (lc,lst) ->
    ( check_none lc co.clause_promotes;
      { co with clause_promotes = Some(lc,lst) } )
  | Imports (lc,lst) ->
    ( check_none lc co.clause_imports;
      { co with clause_imports = Some(lc,lst) } )


  | Abstract_constants (lc,lst) ->
    raise (Error (lc, "Clause ABSTRACT_CONSTANTS not allowed in machines."))
  | Variables (lc,lst) ->
    raise (Error (lc, "Clause VARIABLES not allowed in machines."))

  | Constraints (lc,_) ->
    raise (Error (lc, "Not implemented: clause CONSTRAINTS.")) (*TODO*)
  | Includes (lc,_) ->
    raise (Error (lc, "Not implemented: clause INCLUDES.")) (*TODO*)
  | Extends (lc,_) ->
    raise (Error (lc, "Not implemented: clause EXTENDS.")) (*TODO*)
  | Uses (lc,_) ->
    raise (Error (lc, "Not implemented: clause USES.")) (*TODO*)

let get_loc (cl:clause) =
  match cl with
  | Constraints (lc,_) | Imports (lc,_) | Sees (lc,_) | Includes (lc,_)
  | Extends (lc,_) | Promotes (lc,_) | Uses (lc,_) | Sets (lc,_)
  | Constants (lc,_) | Abstract_constants (lc,_) | Properties (lc,_)
  | Concrete_variables (lc,_) | Variables (lc,_) | Invariant (lc,_)
  | Assertions (lc,_) | Initialization (lc,_) | Operations (lc,_)
  | Values (lc,_) | Local_Operations (lc,_) -> lc

let mk_spec lc name params abs : specification = {
    loc=lc;
    name=name;
    parameters=params;
    abstraction=abs;
    clause_sees=None;
    clause_sets=None;
    clause_constants=None;
    clause_abstract_constants=None;
    clause_properties=None;
    clause_concrete_variables=None;
    clause_variables=None;
    clause_invariant=None;
    clause_assertions=None;
    clause_initialization=None;
    clause_operations=None; }

let mk_implem lc name params abs : implementation = {
    loc=lc;
    name=name;
    parameters=params;
    abstraction=abs;
    clause_sees=None;
    clause_sets=None;
    clause_constants=None;
    clause_properties=None;
    clause_concrete_variables=None;
    clause_invariant=None;
    clause_assertions=None;
    clause_initialization=None;
    clause_operations=None;
    clause_imports=None;
    clause_values=None;
    clause_local_operations=None;
    clause_promotes=None;
}

let mk_component (lc,ki,id,params,clauses:parsed_component) : (component,loc*string) result =
  match ki with
  | Machine ->
    ( try Ok ( Spec (
          List.fold_left add_clause_spec (mk_spec lc id params None) clauses))
      with Error (lc,msg) -> Error (lc,msg) )
  | Refinement m ->
    ( try Ok ( Spec (
          List.fold_left add_clause_spec (mk_spec lc id params (Some m)) clauses))
      with Error (lc,msg) -> Error (lc,msg) )
  | Implementation m ->
    ( try Ok ( Implem (
          List.fold_left add_clause_implem (mk_implem lc id params m) clauses))
      with Error (lc,msg) -> Error (lc,msg) )
