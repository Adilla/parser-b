open Utils
open Expression
open Substitution

type operation = ident list * ident * ident list * substitution

let op_eq (out1,name1,params1,s1:operation) (out2,name2,params2,s2:operation) : bool =
  ident_list_eq out1 out2 && ident_eq name1 name2 &&
  ident_list_eq params1 params2 && subst_eq s1 s2

let norm_op (out,f,args,s:operation) : operation =
  (out, f, args, norm_subst s)

type machine_instanciation = ident * expression list

let minst_eq (id1,lst1:machine_instanciation) (id2,lst2:machine_instanciation) : bool =
  ident_eq id1 id2 && expr_list_eq lst1 lst2

let norm_minst (id, lst:machine_instanciation) : machine_instanciation =
  (id, List.map norm_expr lst)

type set =
  | Abstract_Set of ident
  | Concrete_Set of ident*ident list

let set_eq (s1:set) (s2:set) : bool =
  match s1, s2 with
  | Abstract_Set id1, Abstract_Set id2 -> ident_eq id1 id2
  | Concrete_Set (id1,lst1), Concrete_Set (id2,lst2) ->
    ident_eq id1 id2 && ident_list_eq lst1 lst2
  | _, _ -> false

let list_eq eq l1 l2 =
  try List.for_all2 eq l1 l2
  with Invalid_argument _ -> false

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

let clause_eq (cl1:clause) (cl2:clause) : bool =
  match cl1, cl2 with
  | Constraints (_,p1), Constraints (_,p2) -> pred_eq p1 p2
  | Imports (_,lst1), Imports (_,lst2) ->
    let aux (i1,x1) (i2,x2) = ident_eq i1 i2 && list_eq expr_eq x1 x2 in
    list_eq aux lst1 lst2
  | Sees (_,lst1), Sees (_,lst2) -> ident_list_eq lst1 lst2
  | Includes (_,lst1), Includes (_,lst2) -> list_eq minst_eq lst1 lst2
  | Extends (_,lst1), Extends (_,lst2) -> list_eq minst_eq lst1 lst2
  | Promotes (_,lst1), Promotes(_,lst2) -> ident_list_eq lst1 lst2
  | Uses (_,lst1), Uses (_,lst2) -> ident_list_eq lst1 lst2
  | Sets (_,lst1), Sets (_,lst2) -> list_eq set_eq lst1 lst2
  | Constants (_,lst1), Constants (_,lst2) -> ident_list_eq lst1 lst2
  | Abstract_constants (_,lst1), Abstract_constants (_,lst2) -> ident_list_eq lst1 lst2
  | Properties (_,p1), Properties(_,p2) -> pred_eq p1 p2
  | Concrete_variables (_,lst1), Concrete_variables (_,lst2) -> ident_list_eq lst1 lst2
  | Variables (_,lst1), Variables (_,lst2) -> ident_list_eq lst1 lst2
  | Invariant (_,p1), Invariant (_,p2) -> pred_eq p1 p2
  | Assertions (_,p1), Assertions (_,p2) -> list_eq pred_eq p1 p2
  | Initialization (_,s1), Initialization (_,s2) -> subst_eq s1 s2
  | Operations (_,lst1), Operations (_,lst2) -> list_eq op_eq lst1 lst2
  | Local_Operations (_,lst1), Local_Operations (_,lst2) -> list_eq op_eq lst1 lst2
  | Values (_,lst1), Values (_,lst2) ->
    let aux (i1,x1) (i2,x2) = ident_eq i1 i2 && expr_eq x1 x2 in
    list_eq aux lst1 lst2
  | _, _ -> false

let norm_clause : clause -> clause = function
  | Constraints (l,p) -> Constraints (l,norm_pred p)
  | Imports (l,lst) ->
    let aux (id,lst) = (id,List.map norm_expr lst) in
    Imports (l,List.map aux lst)
  | Includes (l,lst) -> Includes (l,List.map norm_minst lst)
  | Extends (l,lst) -> Extends (l,List.map norm_minst lst)
  | Properties (l,p) -> Properties (l,norm_pred p)
  | Invariant (l,p) -> Invariant (l,norm_pred p)
  | Assertions (l,lst) -> Assertions (l,List.map norm_pred lst)
  | Initialization (l,s) -> Initialization (l,norm_subst s)
  | Operations (l,lst) -> Operations (l,List.map norm_op lst)
  | Local_Operations (l,lst) -> Local_Operations (l,List.map norm_op lst)
  | Values (l,lst) ->
    let aux (id,e) = (id,norm_expr e) in Values (l,List.map aux lst)
  | Sees _
  | Promotes _
  | Uses _
  | Sets _
  | Constants _
  | Abstract_constants _
  | Concrete_variables _
  | Variables _ as cl -> cl

let get_loc (cl:clause) =
  match cl with
  | Constraints (lc,_) | Imports (lc,_) | Sees (lc,_) | Includes (lc,_)
  | Extends (lc,_) | Promotes (lc,_) | Uses (lc,_) | Sets (lc,_)
  | Constants (lc,_) | Abstract_constants (lc,_) | Properties (lc,_)
  | Concrete_variables (lc,_) | Variables (lc,_) | Invariant (lc,_)
  | Assertions (lc,_) | Initialization (lc,_) | Operations (lc,_)
  | Values (lc,_) | Local_Operations (lc,_) -> lc

(* ******** *)

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

let add lst f = function
  | None -> lst
  | Some x -> (f x)::lst

let clist_of_mch (mch:abstract_machine) : clause list =
  let lst = add []  (fun (l,ops) -> Operations (l,ops)) mch.clause_operations in
  let lst = add lst (fun (l,x) -> Initialization(l,x)) mch.clause_initialisation in
  let lst = add lst (fun (l,x) -> Assertions(l,x)) mch.clause_assertions in
  let lst = add lst (fun (l,x) -> Invariant(l,x)) mch.clause_invariant in
  let lst = add lst (fun (l,x) -> Variables(l,x)) mch.clause_abstract_variables in
  let lst = add lst (fun (l,x) -> Concrete_variables(l,x)) mch.clause_concrete_variables in
  let lst = add lst (fun (l,x) -> Properties(l,x)) mch.clause_properties in
  let lst = add lst (fun (l,x) -> Abstract_constants(l,x)) mch.clause_abstract_constants in
  let lst = add lst (fun (l,x) -> Constants(l,x)) mch.clause_concrete_constants in
  let lst = add lst (fun (l,x) -> Sets(l,x)) mch.clause_sets in
  let lst = add lst (fun (l,x) -> Uses(l,x)) mch.clause_uses in
  let lst = add lst (fun (l,x) -> Extends(l,x)) mch.clause_extends in
  let lst = add lst (fun (l,x) -> Promotes(l,x)) mch.clause_promotes in
  let lst = add lst (fun (l,x) -> Includes(l,x)) mch.clause_includes in
  let lst = add lst (fun (l,x) -> Sees(l,x)) mch.clause_sees in
  let lst = add lst (fun (l,x) -> Constraints(l,x)) mch.clause_constraints in
  lst

let mch_eq mch1 mch2 =
  ident_eq mch1.name mch2.name &&
  ident_list_eq mch1.parameters mch2.parameters &&
  (try List.for_all2 clause_eq (clist_of_mch mch1) (clist_of_mch mch2)
   with Invalid_argument _ -> false)

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

let mch_of_clist (name:ident) (params:ident list) (clauses:clause list)
  : (abstract_machine,string) result =
  try Ok (mk_machine_exn name params clauses)
  with Utils.Error (_,err) -> Error err

let norm_mch (mch: abstract_machine) : abstract_machine =
  mk_machine_exn mch.name mch.parameters
    (List.map norm_clause (clist_of_mch mch))

(* ******** *)

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

let clist_of_ref (ref:refinement) : clause list =
  let lst = add []  (fun (l,x) -> Operations(l,x)) ref.clause_operations in
  let lst = add lst (fun (l,x) -> Local_Operations(l,x)) ref.clause_local_operations in
  let lst = add lst (fun (l,x) -> Initialization(l,x)) ref.clause_initialisation in
  let lst = add lst (fun (l,x) -> Assertions(l,x)) ref.clause_assertions in
  let lst = add lst (fun (l,x) -> Invariant(l,x)) ref.clause_invariant in
  let lst = add lst (fun (l,x) -> Variables(l,x)) ref.clause_abstract_variables in
  let lst = add lst (fun (l,x) -> Concrete_variables(l,x)) ref.clause_concrete_variables in
  let lst = add lst (fun (l,x) -> Properties(l,x)) ref.clause_properties in
  let lst = add lst (fun (l,x) -> Abstract_constants(l,x)) ref.clause_abstract_constants in
  let lst = add lst (fun (l,x) -> Constants(l,x)) ref.clause_concrete_constants in
  let lst = add lst (fun (l,x) -> Sets(l,x)) ref.clause_sets in
  let lst = add lst (fun (l,x) -> Extends(l,x)) ref.clause_extends in
  let lst = add lst (fun (l,x) -> Promotes(l,x)) ref.clause_promotes in
  let lst = add lst (fun (l,x) -> Includes(l,x)) ref.clause_includes in
  let lst = add lst (fun (l,x) -> Sees(l,x)) ref.clause_sees in
  lst

let ref_eq ref1 ref2 =
  ident_eq ref1.name ref2.name &&
  ident_list_eq ref1.parameters ref2.parameters &&
  ident_eq ref1.refines ref2.refines &&
  (try List.for_all2 clause_eq (clist_of_ref ref1) (clist_of_ref ref2)
   with Invalid_argument _ -> false)

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

let ref_of_clist (name:ident) (params:ident list) (refines:ident) (clauses:clause list)
  : (refinement,string) result =
  try Ok (mk_refinement_exn name params refines clauses)
  with Utils.Error (_,err) -> Error err

let norm_ref (ref: refinement) : refinement =
  mk_refinement_exn ref.name ref.parameters ref.refines
    (List.map norm_clause (clist_of_ref ref))

(* ******** *)

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

let clist_of_imp (imp:implementation) : clause list =
  let lst = add []  (fun (l,x) -> Operations(l,x)) imp.clause_operations_B0 in
  let lst = add lst (fun (l,x) -> Local_Operations(l,x)) imp.clause_local_operations_B0 in
  let lst = add lst (fun (l,x) -> Initialization(l,x)) imp.clause_initialisation_B0 in
  let lst = add lst (fun (l,x) -> Assertions(l,x)) imp.clause_assertions in
  let lst = add lst (fun (l,x) -> Invariant(l,x)) imp.clause_invariant in
  let lst = add lst (fun (l,x) -> Concrete_variables(l,x)) imp.clause_concrete_variables in
  let lst = add lst (fun (l,x) -> Values(l,x)) imp.clause_values in
  let lst = add lst (fun (l,x) -> Properties(l,x)) imp.clause_properties in
  let lst = add lst (fun (l,x) -> Constants(l,x)) imp.clause_concrete_constants in
  let lst = add lst (fun (l,x) -> Sets(l,x)) imp.clause_sets in
  let lst = add lst (fun (l,x) -> Extends(l,x)) imp.clause_extends_B0 in
  let lst = add lst (fun (l,x) -> Promotes(l,x)) imp.clause_promotes in
  let lst = add lst (fun (l,x) -> Imports(l,x)) imp.clause_imports in
  let lst = add lst (fun (l,x) -> Sees(l,x)) imp.clause_sees in
  lst

let imp_eq imp1 imp2 =
  ident_eq imp1.name imp2.name &&
  ident_list_eq imp1.parameters imp2.parameters &&
  ident_eq imp1.refines imp2.refines &&
  (try List.for_all2 clause_eq (clist_of_imp imp1) (clist_of_imp imp2)
   with Invalid_argument _ -> false)

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

let imp_of_clist (name:ident) (params:ident list) (refines:ident) (clauses:clause list)
  : (implementation,string) result =
  try Ok (mk_implementation_exn name params refines clauses)
  with Utils.Error (_,err) -> Error err

let norm_imp (imp: implementation) : implementation =
  mk_implementation_exn imp.name imp.parameters imp.refines
    (List.map norm_clause (clist_of_imp imp))

(* ******** *)

type component = 
  | Abstract_machine of abstract_machine
  | Refinement of refinement
  | Implementation of implementation

let component_eq c1 c2 =
  match c1, c2 with
  | Abstract_machine mch1, Abstract_machine mch2 -> mch_eq mch1 mch2
  | Refinement ref1, Refinement ref2 -> ref_eq ref1 ref2
  | Implementation imp1, Implementation imp2 -> imp_eq imp1 imp2
  | _, _ -> false

let norm_component : component -> component = function
  | Abstract_machine x -> Abstract_machine (norm_mch x)
  | Refinement x -> Refinement (norm_ref x)
  | Implementation x -> Implementation (norm_imp x)

(* ******** *)

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
    Label ((mk_atom (snd id),label),
           List(("(",",",")",list),
                List.map (fun e -> ef_expr (add_par e)) args))

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
  Label((spec, lbl), ef_subst (Substitution.add_begin_end_ifn body))

let ef_op_list lst =
  List(("",";\n","",{list with align_closing=false;space_after_opening=false;space_before_closing=false}),
       List.map ef_operation lst)

let ef_pred_list lst =
  List(("",";","",{list with align_closing=false;space_after_opening=false;space_before_closing=false}),
       List.map ef_pred lst)

let ef_set_list lst =
  List(("",";","",{list with align_closing=false;space_after_opening=false;space_before_closing=false}),
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

let ef_clause : clause -> Easy_format.t = function
  | Constraints (_,p) -> mk_clause "CONSTRAINTS" (ef_pred p)
  | Imports (_,lst) -> mk_clause "IMPORTS" (ef_minst_list lst)
  | Includes (_,lst) -> mk_clause "INCLUDES" (ef_minst_list lst)
  | Extends (_,lst) -> mk_clause "EXTENDS" (ef_minst_list lst)
  | Properties (_,p) -> mk_clause "PROPERTIES" (ef_pred p)
  | Invariant (_,p) -> mk_clause "INVARIANT" (ef_pred p)
  | Assertions (_,lst) -> mk_clause "ASSERTIONS" (ef_pred_list lst)
  | Initialization (_,s) -> mk_clause "INITIALISATION" (ef_subst s)
  | Operations (_,lst) -> mk_clause "OPERATIONS" (ef_op_list lst)
  | Local_Operations (_,lst) -> mk_clause "LOCAL_OPERATIONS" (ef_op_list lst)
  | Values (_,lst) -> mk_clause "VALUES" (ef_value_list lst)
  | Sees (_,lst) -> mk_clause "SEES" (mk_ident_list_comma lst)
  | Promotes (_,lst) -> mk_clause "PROMOTES" (mk_ident_list_comma lst)
  | Uses (_,lst) -> mk_clause "USES" (mk_ident_list_comma lst)
  | Sets (_,lst) -> mk_clause "SETS" (ef_set_list lst)
  | Constants (_,lst) -> mk_clause "CONSTANTS" (mk_ident_list_comma lst)
  | Abstract_constants (_,lst) -> mk_clause "ABSTRACT_CONSTANTS" (mk_ident_list_comma lst)
  | Concrete_variables (_,lst) -> mk_clause "CONCRETE_VARIABLES" (mk_ident_list_comma lst)
  | Variables (_,lst) -> mk_clause "VARIABLES" (mk_ident_list_comma lst)

let ef_machine (mch:abstract_machine) : Easy_format.t =
  let machine = mk_clause "MACHINE" (mk_machine_name mch.name mch.parameters) in
  let lst = List.rev_map ef_clause (clist_of_mch mch) in
  let ed = [mk_atom "END"] in
  mk_clause_list (machine::(lst@ed))

let ef_refinement (ref:refinement) : Easy_format.t =
  let refinement = mk_clause "REFINEMENT" (mk_machine_name ref.name ref.parameters) in
  let refines = mk_clause "REFINES" (mk_atom (snd ref.refines)) in
  let lst = List.rev_map ef_clause (clist_of_ref ref) in
  let ed = [mk_atom "END"] in
  mk_clause_list (refinement::refines::(lst@ed))

let ef_implementation (imp:implementation) : Easy_format.t =
  let implementation = mk_clause "IMPLEMENTATION" (mk_machine_name imp.name imp.parameters) in
  let refines = mk_clause "REFINES" (mk_atom (snd imp.refines)) in
  let lst = List.rev_map ef_clause (clist_of_imp imp) in
  let ed = [mk_atom "END"] in
  mk_clause_list (implementation::refines::(lst@ed))

let ef_component : component -> Easy_format.t = function
  | Abstract_machine x -> ef_machine x
  | Refinement x -> ef_refinement x
  | Implementation x -> ef_implementation x
