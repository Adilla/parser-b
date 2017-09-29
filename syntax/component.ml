open Utils
open Expression
open Substitution

type ('lc,'ty) operation = ('ty*'lc ident) list * 'lc ident * ('ty*'lc ident) list * ('lc,'ty) substitution
type u_operation   = (loc,bool) operation

let op_eq (out1,name1,params1,s1:('lc,'ty) operation) (out2,name2,params2,s2:('lc2,'ty2) operation) : bool =
  let aux (_,x1) (_,x2) = ident_eq x1 x2 in 
  let aux lst1 lst2 =
   try List.for_all2 aux lst1 lst2
   with Invalid_argument _ -> false
  in
  aux out1 out2 && ident_eq name1 name2 &&
  aux params1 params2 && subst_eq s1 s2

type ('lc,'ty) machine_instanciation = 'lc ident * ('lc,'ty) expression list

let minst_eq (id1,lst1:('lc,'ty) machine_instanciation) (id2,lst2:('lc2,'ty2) machine_instanciation) : bool =
  ident_eq id1 id2 && expr_list_eq lst1 lst2

type 'lc set =
  | Abstract_Set of 'lc ident
  | Concrete_Set of 'lc ident*'lc ident list

let set_eq (s1:'lc set) (s2:'lc2 set) : bool =
  match s1, s2 with
  | Abstract_Set id1, Abstract_Set id2 -> ident_eq id1 id2
  | Concrete_Set (id1,lst1), Concrete_Set (id2,lst2) ->
    ident_eq id1 id2 && ident_list_eq lst1 lst2
  | _, _ -> false

let list_eq eq l1 l2 =
  try List.for_all2 eq l1 l2
  with Invalid_argument _ -> false

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

let clause_eq : type a b c d. (a,b) clause -> (c,d) clause -> bool = fun cl1 cl2 ->
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

type u_clause = (loc,bool) clause

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

type u_machine = (loc,bool) abstract_machine

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

type u_refinement = (loc,bool) refinement

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

type u_implementation = (loc,bool) implementation

let get_loc (cl:('lc,'ty) clause) : 'lc =
  match cl with
  | Constraints (lc,_) | Imports (lc,_) | Sees (lc,_) | Includes (lc,_)
  | Extends (lc,_) | Promotes (lc,_) | Uses (lc,_) | Sets (lc,_)
  | Constants (lc,_) | Abstract_constants (lc,_) | Properties (lc,_)
  | Concrete_variables (lc,_) | Variables (lc,_) | Invariant (lc,_)
  | Assertions (lc,_) | Initialization (lc,_) | Operations (lc,_)
  | Values (lc,_) | Local_Operations (lc,_) -> lc

(* ******** *)

let add lst f = function
  | None -> lst
  | Some x -> (f x)::lst

let clist_of_mch (mch:('lc,'ty) abstract_machine) : ('lc,'ty) clause list =
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

let mch_eq : type a b c d. (a,b) abstract_machine -> (c,d) abstract_machine -> bool = fun mch1 mch2 ->
  ident_eq mch1.name mch2.name &&
  ident_list_eq mch1.parameters mch2.parameters &&
  (try List.for_all2 clause_eq (clist_of_mch mch1) (clist_of_mch mch2)
   with Invalid_argument _ -> false)

let check_none_exn (lc:'lc) (opt:'a option) (mch:'b) : ('b,'lc*string) result =
  match opt with
  | None -> Ok mch
  | Some _ ->  Error (lc,"This clause is defined twice.")

let add_clause_mch (co:('lc,'ty) abstract_machine) (cl:('lc,'ty) clause) : (('lc,'ty) abstract_machine,'lc*string) result =
  match cl with
  | Sees (lc,lst) -> check_none_exn lc co.clause_sees { co with clause_sees = Some(lc,lst) }
  | Sets (lc,lst) -> check_none_exn lc co.clause_sets { co with clause_sets = Some(lc,lst) }
  | Constants (lc,lst) -> check_none_exn lc co.clause_concrete_constants { co with clause_concrete_constants = Some(lc,lst) }
  | Abstract_constants (lc,lst) -> check_none_exn lc co.clause_abstract_constants { co with clause_abstract_constants = Some(lc,lst) }
  | Properties (lc,p) -> check_none_exn lc co.clause_properties { co with clause_properties = Some (lc,p) }
  | Concrete_variables (lc,lst) -> check_none_exn lc co.clause_concrete_variables { co with clause_concrete_variables = Some(lc,lst) }
  | Variables (lc,lst) -> check_none_exn lc co.clause_abstract_variables { co with clause_abstract_variables = Some(lc,lst) }
  | Invariant (lc,p) -> check_none_exn lc co.clause_invariant { co with clause_invariant = Some (lc,p) }
  | Assertions (lc,lst) -> check_none_exn lc co.clause_assertions { co with clause_assertions = Some (lc,lst) }
  | Initialization (lc,p) -> check_none_exn lc co.clause_initialisation { co with clause_initialisation = Some (lc,p) }
  | Operations (lc,lst) -> check_none_exn lc co.clause_operations { co with clause_operations = Some(lc,lst) }
  | Values (lc,_) -> Error (lc, "The clause VALUES is not allowed in abstract machines.")
  | Local_Operations (lc,_) -> Error (lc, "The clause LOCAL_OPERATIONS is not allowed in abstract machines.")
  | Promotes (lc,lst) -> check_none_exn lc co.clause_promotes { co with clause_promotes = Some(lc,lst) }
  | Imports (lc,_) -> Error (lc, "The clause IMPORTS is not allowed in abstract machines.")
  | Constraints (lc,lst) -> check_none_exn lc co.clause_constraints { co with clause_constraints = Some(lc,lst) }
  | Includes (lc,lst) -> check_none_exn lc co.clause_includes { co with clause_includes = Some(lc,lst) }
  | Extends (lc,lst) -> check_none_exn lc co.clause_extends { co with clause_extends = Some(lc,lst) }
  | Uses (lc,lst) -> check_none_exn lc co.clause_uses { co with clause_uses = Some(lc,lst) }

let mk_machine name params clauses =
  let mch =
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
  Error.fold_left add_clause_mch mch clauses

let mk_machine_exn name params clauses =
  match mk_machine name params clauses with
  | Ok x -> x
  | Error (x,y) -> raise (Error.Error (x,y))

(* ******** *)

let clist_of_ref (ref:('lc,'ty) refinement) : ('lc,'ty) clause list =
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

let ref_eq: type a b c d. (a,b) refinement -> (c,d) refinement -> bool = fun ref1 ref2 ->
  ident_eq ref1.name ref2.name &&
  ident_list_eq ref1.parameters ref2.parameters &&
  ident_eq ref1.refines ref2.refines &&
  (try List.for_all2 clause_eq (clist_of_ref ref1) (clist_of_ref ref2)
   with Invalid_argument _ -> false)

let add_clause_ref (co:('lc,'ty) refinement) (cl:('lc,'ty) clause) : (('lc,'ty) refinement,'lc*string) result =
  match cl with
  | Sees (lc,lst) -> check_none_exn lc co.clause_sees { co with clause_sees = Some(lc,lst) }
  | Sets (lc,lst) -> check_none_exn lc co.clause_sets { co with clause_sets = Some(lc,lst) }
  | Constants (lc,lst) -> check_none_exn lc co.clause_concrete_constants { co with clause_concrete_constants = Some(lc,lst) }
  | Properties (lc,p) -> check_none_exn lc co.clause_properties { co with clause_properties = Some (lc,p) }
  | Concrete_variables (lc,lst) -> check_none_exn lc co.clause_concrete_variables { co with clause_concrete_variables = Some(lc,lst) }
  | Invariant (lc,p) -> check_none_exn lc co.clause_invariant { co with clause_invariant = Some (lc,p) }
  | Assertions (lc,lst) -> check_none_exn lc co.clause_assertions { co with clause_assertions = Some (lc,lst) }
  | Initialization (lc,p) -> check_none_exn lc co.clause_initialisation { co with clause_initialisation = Some (lc,p) }
  | Operations (lc,lst) -> check_none_exn lc co.clause_operations { co with clause_operations = Some(lc,lst) }
  | Values (lc,lst) -> Error (lc, "The clause VALUES is not allowed in refinements.")
  | Local_Operations (lc,lst) -> check_none_exn lc co.clause_local_operations { co with clause_local_operations = Some(lc,lst) }
  | Promotes (lc,lst) -> check_none_exn lc co.clause_promotes { co with clause_promotes = Some(lc,lst) }
  | Imports (lc,lst) -> Error (lc, "The clause IMPORTS is not allowed in refinements.")
  | Abstract_constants (lc,lst) -> check_none_exn lc co.clause_abstract_constants { co with clause_abstract_constants = Some(lc,lst) }
  | Variables (lc,lst) -> check_none_exn lc co.clause_abstract_variables { co with clause_abstract_variables = Some(lc,lst) }
  | Constraints (lc,_) -> Error (lc, "The clause CONSTRAINTS is not allowed in refinements.")
  | Includes (lc,lst) -> check_none_exn lc co.clause_includes { co with clause_includes = Some(lc,lst) }
  | Extends (lc,lst) -> check_none_exn lc co.clause_extends { co with clause_extends = Some(lc,lst) }
  | Uses (lc,lst) -> Error (lc, "The clause USES is not allowed in refinements.")

let mk_refinement name params refines clauses =
  let ref =
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
  Error.fold_left add_clause_ref ref clauses

let mk_refinement_exn name params refines clauses =
  match mk_refinement name params refines clauses with
  | Ok x -> x
  | Error (x,y) -> raise (Error.Error (x,y))

(* ******** *)

let clist_of_imp (imp:('lc,'ty) implementation) : ('lc,'ty) clause list =
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

let imp_eq: type a b c d. (a,b) implementation -> (c,d) implementation -> bool = fun imp1 imp2 ->
  ident_eq imp1.name imp2.name &&
  ident_list_eq imp1.parameters imp2.parameters &&
  ident_eq imp1.refines imp2.refines &&
  (try List.for_all2 clause_eq (clist_of_imp imp1) (clist_of_imp imp2)
   with Invalid_argument _ -> false)

let add_clause_imp (co:('lc,'ty) implementation) (cl:('lc,'ty) clause) : (('lc,'ty) implementation,'lc*string) result =
  match cl with
  | Sees (lc,lst) -> check_none_exn lc co.clause_sees { co with clause_sees = Some(lc,lst) }
  | Sets (lc,lst) -> check_none_exn lc co.clause_sets { co with clause_sets = Some(lc,lst) }
  | Constants (lc,lst) -> check_none_exn lc co.clause_concrete_constants { co with clause_concrete_constants = Some(lc,lst) }
  | Properties (lc,p) -> check_none_exn lc co.clause_properties { co with clause_properties = Some (lc,p) }
  | Concrete_variables (lc,lst) -> check_none_exn lc co.clause_concrete_variables { co with clause_concrete_variables = Some(lc,lst) }
  | Invariant (lc,p) -> check_none_exn lc co.clause_invariant { co with clause_invariant = Some (lc,p) }
  | Assertions (lc,lst) -> check_none_exn lc co.clause_assertions { co with clause_assertions = Some (lc,lst) }
  | Initialization (lc,p) -> check_none_exn lc co.clause_initialisation_B0 { co with clause_initialisation_B0 = Some (lc,p) }
  | Operations (lc,lst) -> check_none_exn lc co.clause_operations_B0 { co with clause_operations_B0 = Some(lc,lst) }
  | Values (lc,lst) -> check_none_exn lc co.clause_values { co with clause_values = Some(lc,lst) }
  | Local_Operations (lc,lst) -> check_none_exn lc co.clause_local_operations_B0 { co with clause_local_operations_B0 = Some(lc,lst) }
  | Promotes (lc,lst) -> check_none_exn lc co.clause_promotes { co with clause_promotes = Some(lc,lst) }
  | Imports (lc,lst) -> check_none_exn lc co.clause_imports { co with clause_imports = Some(lc,lst) }
  | Abstract_constants (lc,lst) -> Error (lc, "The clause ABSTRACT_CONSTANTS is not allowed in implementations.")
  | Variables (lc,lst) -> Error (lc, "The clause VARIABLES is not allowed in implementations.")
  | Constraints (lc,_) -> Error (lc, "The clause CONSTRAINTS is not allowed in implementation.")
  | Includes (lc,lst) -> Error (lc, "The clause INCLUDES is not allowed in implementations.")
  | Extends (lc,lst) -> check_none_exn lc co.clause_extends_B0 { co with clause_extends_B0 = Some(lc,lst) }
  | Uses (lc,lst) -> Error (lc, "The clause USES is not allowed in implementations.")

let mk_implementation name params refines clauses =
  let imp =
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
  Error.fold_left add_clause_imp imp clauses

let mk_implementation_exn name params refines clauses =
  match mk_implementation name params refines clauses with
  | Ok x -> x
  | Error (x,y) -> raise (Error.Error (x,y))

(* ******** *)

type ('lc,'ty) component = 
  | Abstract_machine of ('lc,'ty) abstract_machine
  | Refinement of ('lc,'ty) refinement
  | Implementation of ('lc,'ty) implementation

let component_eq: type a b c d. (a,b) component -> (c,d) component -> bool = fun c1 c2 ->
  match c1, c2 with
  | Abstract_machine mch1, Abstract_machine mch2 -> mch_eq mch1 mch2
  | Refinement ref1, Refinement ref2 -> ref_eq ref1 ref2
  | Implementation imp1, Implementation imp2 -> imp_eq imp1 imp2
  | _, _ -> false

type u_comp = (loc,bool) component
