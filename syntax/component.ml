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

(* ******** *)

let get_loc (cl:clause) =
  match cl with
  | Constraints (lc,_) | Imports (lc,_) | Sees (lc,_) | Includes (lc,_)
  | Extends (lc,_) | Promotes (lc,_) | Uses (lc,_) | Sets (lc,_)
  | Constants (lc,_) | Abstract_constants (lc,_) | Properties (lc,_)
  | Concrete_variables (lc,_) | Variables (lc,_) | Invariant (lc,_)
  | Assertions (lc,_) | Initialization (lc,_) | Operations (lc,_)
  | Values (lc,_) | Local_Operations (lc,_) -> lc

(*FIXME use non empty lists *)
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

type component = 
  | Abstract_machine of abstract_machine
  | Refinement of refinement
  | Implementation of implementation

let check_none_exn lc = function
  | None -> ()
  | Some _ -> raise (Utils.Error (lc,"This clause is defined twice."))

let add_clause_mch_exn (co:abstract_machine) (cl:clause) : abstract_machine =
  match cl with
  | Sees (lc,lst) ->
    ( check_none_exn lc co.clause_sees; { co with clause_sees = Some(lc,lst) } )
  | Sets (lc,lst) ->
    ( check_none_exn lc co.clause_sets; { co with clause_sets = Some(lc,lst) } )
  | Constants (lc,lst) ->
    ( check_none_exn lc co.clause_concrete_constants; { co with clause_concrete_constants = Some(lc,lst) } )
  | Abstract_constants (lc,lst) ->
    ( check_none_exn lc co.clause_abstract_constants; { co with clause_abstract_constants = Some(lc,lst) } )
  | Properties (lc,p) ->
    ( check_none_exn lc co.clause_properties; { co with clause_properties = Some (lc,p) } )
  | Concrete_variables (lc,lst) ->
    ( check_none_exn lc co.clause_concrete_variables; { co with clause_concrete_variables = Some(lc,lst) } )
  | Variables (lc,lst) ->
    ( check_none_exn lc co.clause_abstract_variables; { co with clause_abstract_variables = Some(lc,lst) } )
  | Invariant (lc,p) ->
    ( check_none_exn lc co.clause_invariant; { co with clause_invariant = Some (lc,p) } )
  | Assertions (lc,lst) ->
    ( check_none_exn lc co.clause_assertions; { co with clause_assertions = Some (lc,lst) } )
  | Initialization (lc,p) ->
    ( check_none_exn lc co.clause_initialisation; { co with clause_initialisation = Some (lc,p) } )
  | Operations (lc,lst) ->
    ( check_none_exn lc co.clause_operations; { co with clause_operations = Some(lc,lst) } )
  | Values (lc,_) ->
    raise (Utils.Error (lc, "The clause VALUES is not allowed in abstract machines."))
  | Local_Operations (lc,_) ->
    raise (Utils.Error (lc, "The clause LOCAL_OPERATIONS is not allowed in abstract machines."))
  | Promotes (lc,lst) ->
    ( check_none_exn lc co.clause_promotes; { co with clause_promotes = Some(lc,lst) } )
  | Imports (lc,_) ->
    raise (Utils.Error (lc, "The clause IMPORTS is not allowed in abstract machines."))
  | Constraints (lc,lst) ->
    ( check_none_exn lc co.clause_constraints; { co with clause_constraints = Some(lc,lst) } )
  | Includes (lc,lst) ->
    ( check_none_exn lc co.clause_includes; { co with clause_includes = Some(lc,lst) } )
  | Extends (lc,lst) ->
    ( check_none_exn lc co.clause_extends; { co with clause_extends = Some(lc,lst) } )
  | Uses (lc,lst) ->
    ( check_none_exn lc co.clause_uses; { co with clause_uses = Some(lc,lst) } )

let add_clause_ref_exn (co:refinement) (cl:clause) : refinement =
  match cl with
  | Sees (lc,lst) ->
    ( check_none_exn lc co.clause_sees; { co with clause_sees = Some(lc,lst) } )
  | Sets (lc,lst) ->
    ( check_none_exn lc co.clause_sets; { co with clause_sets = Some(lc,lst) } )
  | Constants (lc,lst) ->
    ( check_none_exn lc co.clause_concrete_constants; { co with clause_concrete_constants = Some(lc,lst) } )
  | Properties (lc,p) ->
    ( check_none_exn lc co.clause_properties; { co with clause_properties = Some (lc,p) } )
  | Concrete_variables (lc,lst) ->
    ( check_none_exn lc co.clause_concrete_variables; { co with clause_concrete_variables = Some(lc,lst) } )
  | Invariant (lc,p) ->
    ( check_none_exn lc co.clause_invariant; { co with clause_invariant = Some (lc,p) } )
  | Assertions (lc,lst) ->
    ( check_none_exn lc co.clause_assertions; { co with clause_assertions = Some (lc,lst) } )
  | Initialization (lc,p) ->
    ( check_none_exn lc co.clause_initialisation; { co with clause_initialisation = Some (lc,p) } )
  | Operations (lc,lst) ->
    ( check_none_exn lc co.clause_operations; { co with clause_operations = Some(lc,lst) } )
  | Values (lc,lst) ->
    raise (Utils.Error (lc, "The clause VALUES is not allowed in refinements."))
  | Local_Operations (lc,lst) ->
    ( check_none_exn lc co.clause_local_operations; { co with clause_local_operations = Some(lc,lst) } )
  | Promotes (lc,lst) ->
    ( check_none_exn lc co.clause_promotes; { co with clause_promotes = Some(lc,lst) } )
  | Imports (lc,lst) ->
    raise (Utils.Error (lc, "The clause IMPORTS is not allowed in refinements."))
  | Abstract_constants (lc,lst) ->
    ( check_none_exn lc co.clause_abstract_constants; { co with clause_abstract_constants = Some(lc,lst) } )
  | Variables (lc,lst) ->
    ( check_none_exn lc co.clause_abstract_variables; { co with clause_abstract_variables = Some(lc,lst) } )
  | Constraints (lc,_) ->
    raise (Utils.Error (lc, "The clause CONSTRAINTS is not allowed in refinements."))
  | Includes (lc,lst) ->
    ( check_none_exn lc co.clause_includes; { co with clause_includes = Some(lc,lst) } )
  | Extends (lc,lst) ->
    ( check_none_exn lc co.clause_extends; { co with clause_extends = Some(lc,lst) } )
  | Uses (lc,lst) ->
    raise (Utils.Error (lc, "The clause USES is not allowed in refinements."))

let add_clause_imp_exn (co:implementation) (cl:clause) : implementation =
  match cl with
  | Sees (lc,lst) ->
    ( check_none_exn lc co.clause_sees; { co with clause_sees = Some(lc,lst) } )
  | Sets (lc,lst) ->
    ( check_none_exn lc co.clause_sets; { co with clause_sets = Some(lc,lst) } )
  | Constants (lc,lst) ->
    ( check_none_exn lc co.clause_concrete_constants; { co with clause_concrete_constants = Some(lc,lst) } )
  | Properties (lc,p) ->
    ( check_none_exn lc co.clause_properties; { co with clause_properties = Some (lc,p) } )
  | Concrete_variables (lc,lst) ->
    ( check_none_exn lc co.clause_concrete_variables; { co with clause_concrete_variables = Some(lc,lst) } )
  | Invariant (lc,p) ->
    ( check_none_exn lc co.clause_invariant; { co with clause_invariant = Some (lc,p) } )
  | Assertions (lc,lst) ->
    ( check_none_exn lc co.clause_assertions; { co with clause_assertions = Some (lc,lst) } )
  | Initialization (lc,p) ->
    ( check_none_exn lc co.clause_initialisation_B0; { co with clause_initialisation_B0 = Some (lc,p) } )
  | Operations (lc,lst) ->
    ( check_none_exn lc co.clause_operations_B0; { co with clause_operations_B0 = Some(lc,lst) } )
  | Values (lc,lst) ->
    ( check_none_exn lc co.clause_values; { co with clause_values = Some(lc,lst) } )
  | Local_Operations (lc,lst) ->
    ( check_none_exn lc co.clause_local_operations_B0; { co with clause_local_operations_B0 = Some(lc,lst) } )
  | Promotes (lc,lst) ->
    ( check_none_exn lc co.clause_promotes; { co with clause_promotes = Some(lc,lst) } )
  | Imports (lc,lst) ->
    ( check_none_exn lc co.clause_imports; { co with clause_imports = Some(lc,lst) } )
  | Abstract_constants (lc,lst) ->
    raise (Utils.Error (lc, "The clause ABSTRACT_CONSTANTS is not allowed in implementations."))
  | Variables (lc,lst) ->
    raise (Utils.Error (lc, "The clause VARIABLES is not allowed in implementations."))
  | Constraints (lc,_) ->
    raise (Utils.Error (lc, "The clause CONSTRAINTS is not allowed in implementation."))
  | Includes (lc,lst) ->
    raise (Utils.Error (lc, "The clause INCLUDES is not allowed in implementations."))
  | Extends (lc,lst) ->
    ( check_none_exn lc co.clause_extends_B0; { co with clause_extends_B0 = Some(lc,lst) } )
  | Uses (lc,lst) ->
    raise (Utils.Error (lc, "The clause USES is not allowed in implementations."))

let mk_machine_exn (name:ident) (params:ident list) (clauses:clause list) : abstract_machine =
  let mch:abstract_machine =
    { name=name;
      parameters=params;
      clause_sees=None;
      clause_sets=None;
      clause_uses=None;
      clause_promotes=None;
      clause_includes=None;
      clause_extends=None;
      clause_constraints=None;
      clause_concrete_constants=None;
      clause_abstract_constants=None;
      clause_properties=None;
      clause_concrete_variables=None;
      clause_abstract_variables=None;
      clause_invariant=None;
      clause_assertions=None;
      clause_initialisation=None;
      clause_operations=None; }
  in
  List.fold_left add_clause_mch_exn mch clauses

let mk_refinement_exn (name:ident) (params:ident list) (refines:ident) (clauses:clause list) : refinement =
  let ref:refinement =
    { name=name;
      parameters=params;
      refines=refines;
      clause_sees=None;
      clause_sets=None;
      clause_promotes=None;
      clause_includes=None;
      clause_extends=None;
      clause_concrete_constants=None;
      clause_abstract_constants=None;
      clause_properties=None;
      clause_concrete_variables=None;
      clause_abstract_variables=None;
      clause_invariant=None;
      clause_assertions=None;
      clause_initialisation=None;
      clause_local_operations=None;
      clause_operations=None; }
  in
  List.fold_left add_clause_ref_exn ref clauses

let mk_implementation_exn (name:ident) (params:ident list) (refines:ident) (clauses:clause list) : implementation =
  let imp:implementation =
    { name=name;
      parameters=params;
      refines=refines;
      clause_sees=None;
      clause_sets=None;
      clause_values=None;
      clause_imports=None;
      clause_promotes=None;
      clause_concrete_constants=None;
      clause_properties=None;
      clause_concrete_variables=None;
      clause_invariant=None;
      clause_assertions=None;
      clause_extends_B0=None;
      clause_initialisation_B0=None;
      clause_local_operations_B0=None;
      clause_operations_B0=None; }
  in
  List.fold_left add_clause_imp_exn imp clauses

open Easy_format

let mk_atom s = Easy_format.Atom (s,Easy_format.atom)
let mk_sequence lst =
  List(("","","",{list with align_closing=false;space_after_opening=false;space_before_closing=false}),lst)

let mk_clause (cname:string) (n:Easy_format.t) =
  Label ((mk_atom cname,
          {label with label_break=`Always;space_after_label=false}),n)

let mk_ident_list_comma (lst:ident list) : Easy_format.t =
    let lst = List.map (fun id -> mk_atom (snd id)) lst in
    List (("",",","",{list with align_closing=false;
                                space_after_opening=false;
                                space_before_closing=false})
         ,lst)

let ef_minst (id,args:machine_instanciation) : Easy_format.t =
  match args with
  | [] -> mk_atom (snd id)
  | _::_ ->
    Label ((mk_atom (snd id),label), List(("(",",",")",list),
                                          List.map ef_expr args))

let ef_set (x:set) : Easy_format.t =
  match x with
  | Abstract_Set id -> mk_atom (snd id)
  | Concrete_Set (id,lst) ->
    let enums = List(("{",",","}",list),
                     List.map (fun id -> mk_atom (snd id)) lst) in
    List(("","","",list),[mk_atom (snd id);mk_atom "=";enums])

let ef_operation (out,name,args,body:operation) : Easy_format.t =
  let name_args =
    match args with
    | [] -> mk_atom (snd name)
    | _::_ ->
      let lst = {list with align_closing=false;
                           space_after_opening=false;
                           space_before_closing=false}
      in
      let args = List(("(",",",")",lst),
                      List.map (fun a -> mk_atom (snd a)) args) in
      Label((mk_atom (snd name),label),args)
  in
  let spec = match out with
    | [] -> mk_sequence [name_args; mk_atom "="]
    | _::_ -> mk_sequence [mk_ident_list_comma out; mk_atom "<--"; name_args; mk_atom "="]
  in
  let lbl = {label with label_break=`Always;
                        indent_after_label=0;
                        space_after_label=false } in
  Label((spec, lbl), ef_subst body)

let add lst f = function
  | None -> lst
  | Some x -> (f x)::lst

let ef_op_list lst =
    List(("","\n","",{list with align_closing=false;space_after_opening=false;space_before_closing=false}),
         List.map ef_operation lst)

let ef_pred_list lst =
  List(("",";","",{list with align_closing=false;space_after_opening=false;space_before_closing=false}),
       List.map ef_pred lst)

let ef_set_list lst =
  List(("",",","",{list with align_closing=false;space_after_opening=false;space_before_closing=false}),
       List.map ef_set lst)

let ef_minst_list lst =
  List(("",",","",{list with align_closing=false;space_after_opening=false;space_before_closing=false}),
       List.map ef_minst lst)

let ef_value_list lst =
  let ef (id,e) = mk_sequence [mk_atom (snd id);mk_atom "=";ef_expr e] in
  (List(("",";","",{list with align_closing=false;space_after_opening=false;space_before_closing=false}),
        List.map ef lst))

let mk_machine_name name params =
  match params with
  | [] -> mk_atom (snd name)
  | _::_ -> Label((mk_atom (snd name),{label with space_after_label=false}),
                  List(("(",",",")",list),List.map (fun (_,p) -> mk_atom p) params))

let mk_clause_list lst =
  List (("","\n","",
         {list with indent_body=0;
                    space_after_opening=false;
                    space_after_separator=false;
                    align_closing=false}),
        lst)

let mk_operations (_,lst) = mk_clause "OPERATIONS" (ef_op_list lst)
let mk_local_operations (_,lst) = mk_clause "LOCAL_OPERATIONS" (ef_op_list lst)
let mk_initialisation (_,s) = mk_clause "INITIALISATION" (ef_subst s)
let mk_assertions (_,lst) = mk_clause "ASSERTIONS" (ef_pred_list lst)
let mk_invariant (_,p) = mk_clause "INVARIANT" (ef_pred p)
let mk_variables (_,lst) = mk_clause "VARIABLES" (mk_ident_list_comma lst)
let mk_concrete_variables (_,lst) = mk_clause "CONCRETE_VARIABLES" (mk_ident_list_comma lst)
let mk_properties (_,p) = mk_clause "PROPERTIES" (ef_pred p)
let mk_abstract_constants (_,lst) = mk_clause "ABSTRACT_VARIABLES" (mk_ident_list_comma lst)
let mk_constants (_,lst) = mk_clause "CONSTANTS" (mk_ident_list_comma lst)
let mk_sets (_,lst) = mk_clause "SETS" (ef_set_list lst)
let mk_uses (_,lst) = mk_clause "USES" (mk_ident_list_comma lst)
let mk_extends (_,lst) = mk_clause "EXTENDS" (ef_minst_list lst)
let mk_promotes (_,lst) = mk_clause "PROMOTES" (mk_ident_list_comma lst)
let mk_includes (_,lst) = mk_clause "INCLUDES" (ef_minst_list lst)
let mk_sees (_,lst) = mk_clause "SEES" (mk_ident_list_comma lst)
let mk_constraints (_,p) = mk_clause "CONSTRAINTS" (ef_pred p)
let mk_imports (_,lst) = mk_clause "IMPORTS" (ef_minst_list lst)
let mk_values (_,lst) = mk_clause "VALUES" (ef_value_list lst)

let ef_machine (mch:abstract_machine) : Easy_format.t =
  let machine = mk_clause "MACHINE" (mk_machine_name mch.name mch.parameters) in
  let lst = [mk_atom "END"] in
  let lst = add lst mk_operations mch.clause_operations in
  let lst = add lst mk_initialisation mch.clause_initialisation in
  let lst = add lst mk_assertions mch.clause_assertions in
  let lst = add lst mk_invariant mch.clause_invariant in
  let lst = add lst mk_variables mch.clause_abstract_variables in
  let lst = add lst mk_concrete_variables mch.clause_concrete_variables in
  let lst = add lst mk_properties mch.clause_properties in
  let lst = add lst mk_abstract_constants mch.clause_abstract_constants in
  let lst = add lst mk_constants mch.clause_concrete_constants in
  let lst = add lst mk_sets mch.clause_sets in
  let lst = add lst mk_uses mch.clause_uses in
  let lst = add lst mk_extends mch.clause_extends in
  let lst = add lst mk_promotes mch.clause_promotes in
  let lst = add lst mk_includes mch.clause_includes in
  let lst = add lst mk_sees mch.clause_sees in
  let lst = add lst mk_constraints mch.clause_constraints in
  mk_clause_list (machine::lst)

let ef_refinement (ref:refinement) : Easy_format.t =
  let refinement = mk_clause "REFINEMENT" (mk_machine_name ref.name ref.parameters) in
  let refines = mk_clause "REFINES" (mk_atom (snd ref.refines)) in
  let lst = [mk_atom "END"] in
  let lst = add lst mk_operations ref.clause_operations in
  let lst = add lst mk_local_operations ref.clause_local_operations in
  let lst = add lst mk_initialisation ref.clause_initialisation in
  let lst = add lst mk_assertions ref.clause_assertions in
  let lst = add lst mk_invariant ref.clause_invariant in
  let lst = add lst mk_variables ref.clause_abstract_variables in
  let lst = add lst mk_concrete_variables ref.clause_concrete_variables in
  let lst = add lst mk_properties ref.clause_properties in
  let lst = add lst mk_abstract_constants ref.clause_abstract_constants in
  let lst = add lst mk_constants ref.clause_concrete_constants in
  let lst = add lst mk_sets ref.clause_sets in
  let lst = add lst mk_extends ref.clause_extends in
  let lst = add lst mk_promotes ref.clause_promotes in
  let lst = add lst mk_includes ref.clause_includes in
  let lst = add lst mk_sees ref.clause_sees in
  mk_clause_list (refinement::refines::lst)

let ef_implem (imp:implementation) : Easy_format.t =
  let implementation = mk_clause "IMPLEMENTATION" (mk_machine_name imp.name imp.parameters) in
  let refines = mk_clause "REFINES" (mk_atom (snd imp.refines)) in
  let lst = [mk_atom "END"] in
  let lst = add lst mk_operations imp.clause_operations_B0 in
  let lst = add lst mk_local_operations imp.clause_local_operations_B0 in
  let lst = add lst mk_initialisation imp.clause_initialisation_B0 in
  let lst = add lst mk_assertions imp.clause_assertions in
  let lst = add lst mk_invariant imp.clause_invariant in
  let lst = add lst mk_concrete_variables imp.clause_concrete_variables in
  let lst = add lst mk_values imp.clause_values in
  let lst = add lst mk_properties imp.clause_properties in
  let lst = add lst mk_constants imp.clause_concrete_constants in
  let lst = add lst mk_sets imp.clause_sets in
  let lst = add lst mk_extends imp.clause_extends_B0 in
  let lst = add lst mk_promotes imp.clause_promotes in
  let lst = add lst mk_imports imp.clause_imports in
  let lst = add lst mk_sees imp.clause_sees in
  mk_clause_list (implementation::refines::lst)

let ef_component : component -> Easy_format.t = function
  | Abstract_machine x -> ef_machine x
  | Refinement x -> ef_refinement x
  | Implementation x -> ef_implem x
