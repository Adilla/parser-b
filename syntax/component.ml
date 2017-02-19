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
let mk_label a b = Easy_format.Label ((a,Easy_format.label),b)
let mk_list_1 lst = Easy_format.List (("(",",",")",Easy_format.list),lst)
let mk_list_2 lst = Easy_format.List (("","","",Easy_format.list),lst)

let ef_ident_non_empty_list (x,xlst) =
  let lst = List.map (fun id -> mk_atom (snd id)) (x::xlst) in
  List (("",",","",list),lst)

let ef_ident_non_empty_list_2 (x,xlst) =
  let lst = List.map (fun id -> mk_atom (snd id)) (x::xlst) in
  List (("(",",",")",list),lst)

let ef_machine_inst (id,args:machine_instanciation) : Easy_format.t =
  mk_label (mk_atom (snd id)) (List(("(",",",")",list),List.map ef_expr args))

let ef_set (x:set) : Easy_format.t =
  match x with
  | Abstract_Set id -> mk_atom (snd id)
  | Concrete_Set (id,lst) ->
    let enums = List(("{",",","}",list),
                     List.map (fun id -> mk_atom (snd id)) lst) in
    List(("","","",list),[mk_atom (snd id);mk_atom "=";enums])

let ef_operation (out,name,args,body:operation) : Easy_format.t =
  let spec = match out,args with
    | [],[] -> mk_atom (snd name)
    | [], a::alst ->
      mk_label (mk_atom (snd name)) (ef_ident_non_empty_list_2 (a,alst))
    | o::olst, [] -> 
      List(("","","",list),[ef_ident_non_empty_list (o,olst);mk_atom "<--";mk_atom (snd name)])
    | o::olst, a::alst ->
      let f = mk_label (mk_atom (snd name)) (ef_ident_non_empty_list_2 (a,alst)) in
      List(("","","",list),[ef_ident_non_empty_list (o,olst);mk_atom "<--";f])
  in
  mk_label spec (ef_subst body)

let add lst f = function
  | None -> lst
  | Some x -> (f x)::lst

let mk_op_pred (cname:string) (_,p:loc*predicate) =
  mk_label (mk_atom cname) (ef_pred p)

let mk_op_pred_list (cname:string) (_,lst:loc*predicate list) =
  mk_label (mk_atom cname) (List(("",";","",list),List.map ef_pred lst))

let mk_op_ident_list (cname:string) (_,lst:loc*ident list) =
  mk_label (mk_atom cname) (ef_ident_non_empty_list (List.hd lst,List.tl lst)) (*FIXME*)

let mk_op_minst_list (cname:string) (_,lst:loc*machine_instanciation list) =
  mk_label (mk_atom cname) (List(("",",","",list),List.map ef_machine_inst lst))

let mk_op_sets (_,lst:loc*set list) =
  mk_label (mk_atom "SETS") (List (("",",","",list),List.map ef_set lst))

let mk_op_init (_,s:loc*substitution) =
  mk_label (mk_atom "INITIALISATION") (ef_subst s)

let mk_ops (cname:string) (_,lst:loc*operation list) =
  mk_label (mk_atom cname) (List(("","","",list),List.map ef_operation lst))

let mk_op_values (_,lst:loc*(ident*expression) list) =
  let ef (id,e) = List(("","","",list),[mk_atom (snd id);mk_atom "=";ef_expr e]) in
  mk_label (mk_atom "VALUES") (List(("",";","",list),List.map ef lst))

let ef_machine (mch:abstract_machine) : Easy_format.t =
  let m_name = match mch.parameters with
    | [] -> mk_atom (snd mch.name)
    | _::_ -> mk_label (mk_atom (snd mch.name))
                (List (("(",",",")",list),List.map (fun (_,p) -> mk_atom p) mch.parameters))
  in
  let machine = mk_label (mk_atom "MACHINE") m_name in
  let lst = [mk_atom "END"] in
  let lst = add lst (mk_ops "OPERATIONS") mch.clause_operations in
  let lst = add lst mk_op_init mch.clause_initialisation in
  let lst = add lst (mk_op_pred_list "ASSERTIONS") mch.clause_assertions in
  let lst = add lst (mk_op_pred "INVARIANT") mch.clause_invariant in
  let lst = add lst (mk_op_ident_list "VARIABLES") mch.clause_abstract_variables in
  let lst = add lst (mk_op_ident_list "CONCRETE_VARIABLES") mch.clause_concrete_variables in
  let lst = add lst (mk_op_pred "PROPERTIES") mch.clause_properties in
  let lst = add lst (mk_op_ident_list "ABSTRACT_VARIABLES") mch.clause_abstract_constants in
  let lst = add lst (mk_op_ident_list "CONSTANTS") mch.clause_concrete_constants in
  let lst = add lst mk_op_sets mch.clause_sets in
  let lst = add lst (mk_op_ident_list "USES") mch.clause_uses in
  let lst = add lst (mk_op_minst_list "EXTENDS") mch.clause_extends in
  let lst = add lst (mk_op_ident_list "PROMOTES") mch.clause_promotes in
  let lst = add lst (mk_op_minst_list "INCLUDES") mch.clause_includes in
  let lst = add lst (mk_op_ident_list "SEES") mch.clause_sees in
  let lst = add lst (mk_op_pred "CONSTRAINTS") mch.clause_constraints in
  List (("","","",list),machine::lst)

let ef_refinement (ref:refinement) : Easy_format.t =
  let m_name = match ref.parameters with
    | [] -> mk_atom (snd ref.name)
    | _::_ -> mk_label (mk_atom (snd ref.name))
                (List (("(",",",")",list),List.map (fun (_,p) -> mk_atom p) ref.parameters))
  in
  let refinement = mk_label (mk_atom "REFINEMENT") m_name in
  let refines = mk_label (mk_atom "REFINES") (mk_atom (snd ref.refines)) in
  let lst = [mk_atom "END"] in
  let lst = add lst (mk_ops "OPERATIONS") ref.clause_operations in
  let lst = add lst (mk_ops "LOCAL_OPERATIONS") ref.clause_local_operations in
  let lst = add lst (mk_op_init) ref.clause_initialisation in
  let lst = add lst (mk_op_pred_list "ASSERTIONS") ref.clause_assertions in
  let lst = add lst (mk_op_pred "INVARIANT") ref.clause_invariant in
  let lst = add lst (mk_op_ident_list "VARIABLES") ref.clause_abstract_variables in
  let lst = add lst (mk_op_ident_list "CONCRETE_VARIABLES") ref.clause_concrete_variables in
  let lst = add lst (mk_op_pred "PROPERTIES") ref.clause_properties in
  let lst = add lst (mk_op_ident_list "ABSTRACT_CONSTANTS") ref.clause_abstract_constants in
  let lst = add lst (mk_op_ident_list "CONSTANTS") ref.clause_concrete_constants in
  let lst = add lst (mk_op_sets) ref.clause_sets in
  let lst = add lst (mk_op_minst_list "EXTENDS") ref.clause_extends in
  let lst = add lst (mk_op_ident_list "PROMOTES") ref.clause_promotes in
  let lst = add lst (mk_op_minst_list "INCLUDES") ref.clause_includes in
  let lst = add lst (mk_op_ident_list "SEES") ref.clause_sees in
  List (("","","",list),refinement::refines::lst)

let ef_implem (imp:implementation) : Easy_format.t =
  let m_name = match imp.parameters with
    | [] -> mk_atom (snd imp.name)
    | _::_ -> mk_label (mk_atom (snd imp.name))
                (List (("(",",",")",list),List.map (fun (_,p) -> mk_atom p) imp.parameters))
  in
  let implementation = mk_label (mk_atom "IMPLEMENTATION") m_name in
  let refines = mk_label (mk_atom "REFINES") (mk_atom (snd imp.refines)) in
  let lst = [mk_atom "END"] in
  let lst = add lst (mk_ops "OPERATIONS") imp.clause_operations_B0 in
  let lst = add lst (mk_ops "LOCAL_OPERATIONS") imp.clause_local_operations_B0 in
  let lst = add lst (mk_op_init) imp.clause_initialisation_B0 in
  let lst = add lst (mk_op_pred_list "ASSERTIONS") imp.clause_assertions in
  let lst = add lst (mk_op_pred "INVARIANT") imp.clause_invariant in
  let lst = add lst (mk_op_ident_list "CONCRETE_VARIABLES") imp.clause_concrete_variables in
  let lst = add lst (mk_op_values) imp.clause_values in
  let lst = add lst (mk_op_pred "PROPERTIES") imp.clause_properties in
  let lst = add lst (mk_op_ident_list "CONSTANTS") imp.clause_concrete_constants in
  let lst = add lst (mk_op_sets) imp.clause_sets in
  let lst = add lst (mk_op_minst_list "EXTENDS") imp.clause_extends_B0 in
  let lst = add lst (mk_op_ident_list "PROMOTES") imp.clause_promotes in
  let lst = add lst (mk_op_minst_list "IMPORTS") imp.clause_imports in
  let lst = add lst (mk_op_ident_list "SEES") imp.clause_sees in
  List (("","","",list),implementation::refines::lst)

let ef_component : component -> Easy_format.t = function
  | Abstract_machine x -> ef_machine x
  | Refinement x -> ef_refinement x
  | Implementation x -> ef_implem x
