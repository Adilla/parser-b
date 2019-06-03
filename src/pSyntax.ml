open SyntaxCore

type expression_desc =
  | Ident of string
  | Dollar of string
  | Builtin_0 of e_builtin_0
  | Builtin_1 of e_builtin_1*expression
  | Builtin_2 of e_builtin_2*expression*expression
  | Pbool of predicate
  | Sequence of expression Nlist.t
  | Extension of expression Nlist.t
  | Comprehension of lident Nlist.t * predicate
  | Binder of expr_binder * lident Nlist.t * predicate * expression
  | Record_Field_Access of expression * lident
  | Record of (lident * expression) Nlist.t
  | Record_Type of (lident * expression) Nlist.t

and expression = {
    exp_loc: Utils.loc [@equal fun _ _ -> true];
    exp_par: bool [@equal fun _ _ -> true];
    exp_desc: expression_desc
}

and predicate_desc =
  | P_Builtin of p_builtin
  | Binary_Prop of prop_bop * predicate * predicate
  | Binary_Pred of pred_bop * expression * expression
  | Negation of predicate
  | Universal_Q of lident Nlist.t * predicate
  | Existential_Q of lident Nlist.t * predicate

and predicate = {
  prd_loc: Utils.loc [@equal fun _ _ -> true];
  prd_par: bool [@equal fun _ _ -> true];
  prd_desc: predicate_desc
}
[@@deriving eq]

type lhs =
  | Tuple of lident Nlist.t
  | Function of lident * expression Nlist.t
  | Record of lident * lident
[@@deriving eq]

type substitution_desc =
  | Skip
  | Affectation of lhs * expression
  | Pre of predicate * substitution
  | Assert of predicate * substitution
  | Choice of substitution Nlist.t
  | IfThenElse of (predicate * substitution) Nlist.t * substitution option
  | Select of (predicate * substitution) Nlist.t * substitution option
  | Case of expression * (expression Nlist.t * substitution) Nlist.t * substitution option
  | Any of lident Nlist.t * predicate * substitution
  | Let of lident Nlist.t * (lident * expression) Nlist.t * substitution
  | BecomesElt of lident Nlist.t * expression
  | BecomesSuch of  lident Nlist.t * predicate
  | Var of lident Nlist.t * substitution
  | CallUp of lident list * lident * expression list
  | While of predicate * substitution * predicate * expression
  | Sequencement of substitution * substitution
  | Parallel of substitution * substitution

and substitution = {
  sub_loc: Utils.loc [@equal fun _ _ -> true];
  sub_be: bool [@equal fun _ _ -> true];
  sub_desc: substitution_desc
}
[@@deriving eq]

type operation = {
  op_out: lident list;
  op_name: lident;
  op_in: lident list;
  op_body: substitution
}
[@@deriving eq]

type machine_instanciation = {
  mi_mch: lident;
  mi_params: expression list
}
[@@deriving eq]

type set =
  | Abstract_Set of lident
  | Concrete_Set of lident * lident list
[@@deriving eq]

type machine = {
  mch_constraints: predicate option;
  mch_sees: lident list;
  mch_includes: machine_instanciation list;
  mch_promotes: lident list;
  mch_extends: machine_instanciation list;
  mch_uses: lident list;
  mch_sets: set list;
  mch_concrete_constants: lident list;
  mch_abstract_constants: lident list;
  mch_properties: predicate option;
  mch_concrete_variables: lident list;
  mch_abstract_variables: lident list;
  mch_invariant: predicate option;
  mch_assertions: predicate list;
  mch_initialisation: substitution option;
  mch_operations: operation list;
}
[@@deriving eq]

type refinement = {
  ref_refines: lident;
  ref_sees: lident list;
  ref_includes: machine_instanciation list;
  ref_promotes: lident list;
  ref_extends: machine_instanciation list;
  ref_sets: set list;
  ref_concrete_constants: lident list;
  ref_abstract_constants: lident list;
  ref_properties: predicate option;
  ref_concrete_variables: lident list;
  ref_abstract_variables: lident list;
  ref_invariant: predicate option;
  ref_assertions: predicate list;
  ref_initialisation: substitution option;
  ref_operations: operation list;
  ref_local_operations: operation  list;
}
[@@deriving eq]

type implementation = {
  imp_refines: lident;
  imp_sees: lident list;
  imp_imports: machine_instanciation list;
  imp_promotes: lident list;
  imp_extends: machine_instanciation list;
  imp_sets: set list;
  imp_concrete_constants: lident list;
  imp_properties: predicate option;
  imp_values: (lident*expression) list;
  imp_concrete_variables: lident list;
  imp_invariant: predicate option;
  imp_assertions: predicate list;
  imp_initialisation: substitution option;
  imp_operations: operation list;
  imp_local_operations: operation list;
}
[@@deriving eq]

type component_desc =
  | Machine of machine
  | Refinement of refinement
  | Implementation of implementation
[@@deriving eq]

type component = {
  co_name: lident;
  co_parameters: lident list;
  co_desc: component_desc
}
[@@deriving eq]

type clause =
  | Constraints of predicate
  | Imports of machine_instanciation Nlist.t
  | Sees of lident Nlist.t
  | Includes of machine_instanciation Nlist.t
  | Extends of  machine_instanciation Nlist.t
  | Promotes of lident Nlist.t
  | Uses of lident Nlist.t
  | Sets of set Nlist.t
  | Constants of lident Nlist.t
  | Abstract_constants of lident Nlist.t
  | Properties of predicate
  | Concrete_variables of lident Nlist.t
  | Variables of lident Nlist.t
  | Invariant of predicate
  | Assertions of predicate Nlist.t
  | Initialization of substitution
  | Operations of operation Nlist.t
  | Local_Operations of operation Nlist.t
  | Values of (lident * expression) Nlist.t
  | Refines of lident

let add_opt lst f = function
    | None -> lst
    | Some s -> (f s)::lst

let add_nle lst f = function
  | [] -> lst
  | hd::tl -> (f (Nlist.make hd tl))::lst

let clist_of_mch (mch:machine) : clause list =
  let lst = [] in
  let lst = add_nle lst (fun ops -> Operations ops) mch.mch_operations in
  let lst = add_opt lst (fun x -> Initialization(x)) mch.mch_initialisation in
  let lst = add_nle lst (fun x -> Assertions(x)) mch.mch_assertions in
  let lst = add_opt lst (fun x -> Invariant(x)) mch.mch_invariant in
  let lst = add_nle lst (fun x -> Variables(x)) mch.mch_abstract_variables in
  let lst = add_nle lst (fun x -> Concrete_variables(x)) mch.mch_concrete_variables in
  let lst = add_opt lst (fun x -> Properties(x)) mch.mch_properties in
  let lst = add_nle lst (fun x -> Abstract_constants(x)) mch.mch_abstract_constants in
  let lst = add_nle lst (fun x -> Constants(x)) mch.mch_concrete_constants in
  let lst = add_nle lst (fun x -> Sets(x)) mch.mch_sets in
  let lst = add_nle lst (fun x -> Uses(x)) mch.mch_uses in
  let lst = add_nle lst (fun x -> Extends(x)) mch.mch_extends in
  let lst = add_nle lst (fun x -> Promotes(x)) mch.mch_promotes in
  let lst = add_nle lst (fun x -> Includes(x)) mch.mch_includes in
  let lst = add_nle lst (fun x -> Sees(x)) mch.mch_sees in
  let lst = add_opt lst (fun x -> Constraints(x)) mch.mch_constraints in
  lst

let clist_of_ref (ref:refinement) : clause list =
  let lst = [Refines ref.ref_refines] in
  let lst = add_nle lst (fun x -> Operations(x)) ref.ref_operations in
  let lst = add_nle lst (fun x -> Local_Operations(x)) ref.ref_local_operations in
  let lst = add_opt lst (fun x -> Initialization(x)) ref.ref_initialisation in
  let lst = add_nle lst (fun x -> Assertions(x)) ref.ref_assertions in
  let lst = add_opt lst (fun x -> Invariant(x)) ref.ref_invariant in
  let lst = add_nle lst (fun x -> Variables(x)) ref.ref_abstract_variables in
  let lst = add_nle lst (fun x -> Concrete_variables(x)) ref.ref_concrete_variables in
  let lst = add_opt lst (fun x -> Properties(x)) ref.ref_properties in
  let lst = add_nle lst (fun x -> Abstract_constants(x)) ref.ref_abstract_constants in
  let lst = add_nle lst (fun x -> Constants(x)) ref.ref_concrete_constants in
  let lst = add_nle lst (fun x -> Sets(x)) ref.ref_sets in
  let lst = add_nle lst (fun x -> Extends(x)) ref.ref_extends in
  let lst = add_nle lst (fun x -> Promotes(x)) ref.ref_promotes in
  let lst = add_nle lst (fun x -> Includes(x)) ref.ref_includes in
  let lst = add_nle lst (fun x -> Sees(x)) ref.ref_sees in
  lst

let clist_of_imp (imp:implementation) : clause list =
  let lst = [Refines imp.imp_refines] in
  let lst = add_nle lst (fun x -> Operations(x)) imp.imp_operations in
  let lst = add_nle lst (fun x -> Local_Operations(x)) imp.imp_local_operations in
  let lst = add_opt lst (fun x -> Initialization(x)) imp.imp_initialisation in
  let lst = add_nle lst (fun x -> Assertions(x)) imp.imp_assertions in
  let lst = add_opt lst (fun x -> Invariant(x)) imp.imp_invariant in
  let lst = add_nle lst (fun x -> Concrete_variables(x)) imp.imp_concrete_variables in
  let lst = add_nle lst (fun x -> Values(x)) imp.imp_values in
  let lst = add_opt lst (fun x -> Properties(x)) imp.imp_properties in
  let lst = add_nle lst (fun x -> Constants(x)) imp.imp_concrete_constants in
  let lst = add_nle lst (fun x -> Sets(x)) imp.imp_sets in
  let lst = add_nle lst (fun x -> Extends(x)) imp.imp_extends in
  let lst = add_nle lst (fun x -> Promotes(x)) imp.imp_promotes in
  let lst = add_nle lst (fun x -> Imports(x)) imp.imp_imports in
  let lst = add_nle lst (fun x -> Sees(x)) imp.imp_sees in
  lst

let check_none_exn lc opt : unit =
    match opt with
    | None -> ()
    | Some _ -> Error.raise_exn lc "This clause is defined twice."

  let check_empty_exn lc lst : unit =
    match lst with
    | [] -> ()
    | _ -> Error.raise_exn lc "This clause is defined twice."

let add_clause_mch_exn (co:machine) (loc,cl:Utils.loc*clause) : machine =
  match cl with
  | Sees lst -> ( check_empty_exn loc co.mch_sees; { co with mch_sees = Nlist.to_list lst } )
  | Sets lst -> ( check_empty_exn loc co.mch_sets; { co with mch_sets = Nlist.to_list lst } )
  | Constants lst -> ( check_empty_exn loc co.mch_concrete_constants; { co with mch_concrete_constants = Nlist.to_list lst } )
  | Abstract_constants lst -> ( check_empty_exn loc co.mch_abstract_constants; { co with mch_abstract_constants = Nlist.to_list lst } )
  | Properties p -> ( check_none_exn loc co.mch_properties; { co with mch_properties = Some p } )
  | Concrete_variables lst -> ( check_empty_exn loc co.mch_concrete_variables; { co with mch_concrete_variables = Nlist.to_list lst } )
  | Variables lst -> ( check_empty_exn loc co.mch_abstract_variables; { co with mch_abstract_variables = Nlist.to_list lst } )
  | Invariant p -> ( check_none_exn loc co.mch_invariant; { co with mch_invariant = Some p } )
  | Assertions lst -> ( check_empty_exn loc co.mch_assertions; { co with mch_assertions = Nlist.to_list lst } )
  | Initialization p -> ( check_none_exn loc co.mch_initialisation; { co with mch_initialisation = Some p } )
  | Operations lst -> ( check_empty_exn loc co.mch_operations; { co with mch_operations = Nlist.to_list lst } )
  | Values _ -> Error.raise_exn loc "The clause VALUES is not allowed in abstract machines."
  | Local_Operations _ -> Error.raise_exn loc "The clause LOCAL_OPERATIONS is not allowed in abstract machines."
  | Promotes lst -> ( check_empty_exn loc co.mch_promotes; { co with mch_promotes = Nlist.to_list lst } )
  | Imports _ -> Error.raise_exn loc "The clause IMPORTS is not allowed in abstract machines."
  | Constraints p -> ( check_none_exn loc co.mch_constraints; { co with mch_constraints = Some p } )
  | Includes lst -> ( check_empty_exn loc co.mch_includes; { co with mch_includes = Nlist.to_list lst } )
  | Extends lst -> ( check_empty_exn loc co.mch_extends; { co with mch_extends = Nlist.to_list lst } )
  | Uses lst -> ( check_empty_exn loc co.mch_uses; { co with mch_uses = Nlist.to_list lst } )
  | Refines abs -> Error.raise_exn abs.lid_loc "The clause REFINES is not allowed in abstract machines."

let mk_machine_exn (co_name:lident) (co_parameters:lident list) (clauses:(Utils.loc*clause) list) : component =
  let mch_desc =
    { mch_sees=[];
      mch_sets=[];
      mch_uses=[];
      mch_promotes=[];
      mch_includes=[];
      mch_extends=[];
      mch_constraints=None;
      mch_concrete_constants=[];
      mch_abstract_constants=[];
      mch_properties=None;
      mch_concrete_variables=[];
      mch_abstract_variables=[];
      mch_invariant=None;
      mch_assertions=[];
      mch_initialisation=None;
      mch_operations=[]; }
  in
  { co_name; co_parameters;
    co_desc=Machine (List.fold_left add_clause_mch_exn mch_desc clauses) }

let add_clause_ref_exn (co:refinement) (loc,cl:Utils.loc*clause) : refinement =
  match cl with
  | Sees lst -> ( check_empty_exn loc co.ref_sees; { co with ref_sees = Nlist.to_list lst } )
  | Sets lst -> ( check_empty_exn loc co.ref_sets; { co with ref_sets = Nlist.to_list lst } )
  | Constants lst -> ( check_empty_exn loc co.ref_concrete_constants; { co with ref_concrete_constants = Nlist.to_list lst } )
  | Properties p -> ( check_none_exn loc co.ref_properties; { co with ref_properties = Some p } )
  | Concrete_variables lst -> ( check_empty_exn loc co.ref_concrete_variables; { co with ref_concrete_variables = Nlist.to_list lst } )
  | Invariant p -> ( check_none_exn loc co.ref_invariant; { co with ref_invariant = Some p } )
  | Assertions lst -> ( check_empty_exn loc co.ref_assertions; { co with ref_assertions = Nlist.to_list lst } )
  | Initialization p -> ( check_none_exn loc co.ref_initialisation; { co with ref_initialisation = Some p } )
  | Operations lst -> ( check_empty_exn loc co.ref_operations; { co with ref_operations = Nlist.to_list lst } )
  | Values _ -> Error.raise_exn loc "The clause VALUES is not allowed in refinements."
  | Local_Operations lst -> ( check_empty_exn loc co.ref_local_operations; { co with ref_local_operations = Nlist.to_list lst } )
  | Promotes lst -> ( check_empty_exn loc co.ref_promotes; { co with ref_promotes = Nlist.to_list lst } )
  | Imports _ -> Error.raise_exn loc "The clause IMPORTS is not allowed in refinements."
  | Abstract_constants lst -> ( check_empty_exn loc co.ref_abstract_constants; { co with ref_abstract_constants = Nlist.to_list lst } )
  | Variables lst -> ( check_empty_exn loc co.ref_abstract_variables; { co with ref_abstract_variables = Nlist.to_list lst } )
  | Constraints _ -> Error.raise_exn loc "The clause CONSTRAINTS is not allowed in refinements."
  | Includes lst -> ( check_empty_exn loc co.ref_includes; { co with ref_includes = Nlist.to_list lst } )
  | Extends lst -> ( check_empty_exn loc co.ref_extends; { co with ref_extends = Nlist.to_list lst } )
  | Uses _ -> Error.raise_exn loc "The clause USES is not allowed in refinements."
  | Refines abs ->
    ( if String.equal co.ref_refines.lid_str "" then { co with ref_refines=abs }
      else Error.raise_exn abs.lid_loc "This clause is defined twice." )

let mk_refinement_exn co_name co_parameters clauses =
  let ref_desc =
    { ref_refines={lid_loc=Utils.dloc;lid_str=""};
      ref_sees=[];
      ref_sets=[];
      ref_promotes=[];
      ref_includes=[];
      ref_extends=[];
      ref_concrete_constants=[];
      ref_abstract_constants=[];
      ref_properties=None;
      ref_concrete_variables=[];
      ref_abstract_variables=[];
      ref_invariant=None;
      ref_assertions=[];
      ref_initialisation=None;
      ref_local_operations=[];
      ref_operations=[]; }
  in
  { co_name; co_parameters;
    co_desc=Refinement (List.fold_left add_clause_ref_exn ref_desc clauses) }

let add_clause_imp_exn (co:implementation) (loc,cl:Utils.loc*clause) : implementation =
  match cl with
  | Sees lst -> ( check_empty_exn loc co.imp_sees; { co with imp_sees = Nlist.to_list lst } )
  | Sets lst -> ( check_empty_exn loc co.imp_sets; { co with imp_sets = Nlist.to_list lst } )
  | Constants lst -> ( check_empty_exn loc co.imp_concrete_constants; { co with imp_concrete_constants = Nlist.to_list lst } )
  | Properties p -> ( check_none_exn loc co.imp_properties; { co with imp_properties = Some p } )
  | Concrete_variables lst -> ( check_empty_exn loc co.imp_concrete_variables; { co with imp_concrete_variables = Nlist.to_list lst } )
  | Invariant p -> ( check_none_exn loc co.imp_invariant; { co with imp_invariant = Some p } )
  | Assertions lst -> ( check_empty_exn loc co.imp_assertions; { co with imp_assertions = Nlist.to_list lst } )
  | Initialization p -> ( check_none_exn loc co.imp_initialisation; { co with imp_initialisation = Some p } )
  | Operations lst -> ( check_empty_exn loc co.imp_operations; { co with imp_operations = Nlist.to_list lst } )
  | Values lst -> ( check_empty_exn loc co.imp_values; { co with imp_values = Nlist.to_list lst } )
  | Local_Operations lst -> ( check_empty_exn loc co.imp_local_operations; { co with imp_local_operations = Nlist.to_list lst } )
  | Promotes lst -> ( check_empty_exn loc co.imp_promotes; { co with imp_promotes = Nlist.to_list lst } )
  | Imports lst -> ( check_empty_exn loc co.imp_imports; { co with imp_imports = Nlist.to_list lst } )
  | Abstract_constants _ -> Error.raise_exn loc "The clause ABSTRACT_CONSTANTS is not allowed in implementations."
  | Variables _ -> Error.raise_exn loc "The clause VARIABLES is not allowed in implementations."
  | Constraints _ -> Error.raise_exn loc "The clause CONSTRAINTS is not allowed in implementation."
  | Includes _ -> Error.raise_exn loc "The clause INCLUDES is not allowed in implementations."
  | Extends lst -> ( check_empty_exn loc co.imp_extends; { co with imp_extends = Nlist.to_list lst } )
  | Uses _ -> Error.raise_exn loc "The clause USES is not allowed in implementations."
  | Refines abs ->
    ( if String.equal co.imp_refines.lid_str "" then { co with imp_refines=abs }
      else Error.raise_exn abs.lid_loc "This clause is defined twice." )

let mk_implementation_exn co_name co_parameters clauses =
  let imp_desc =
    { imp_refines={lid_loc=Utils.dloc;lid_str=""};
      imp_sees=[];
      imp_sets=[];
      imp_values=[];
      imp_imports=[];
      imp_promotes=[];
      imp_concrete_constants=[];
      imp_properties=None;
      imp_concrete_variables=[];
      imp_invariant=None;
      imp_assertions=[];
      imp_extends=[];
      imp_initialisation=None;
      imp_local_operations=[];
      imp_operations=[]; }
  in
    { co_name; co_parameters;
      co_desc=Implementation (List.fold_left add_clause_imp_exn imp_desc clauses) }

let get_clauses co =
  match co.co_desc with
  | Machine mch -> clist_of_mch mch
  | Refinement ref -> clist_of_ref ref
  | Implementation imp -> clist_of_imp imp
