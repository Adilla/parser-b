open SyntaxCore

type expression_desc =
  | Ident of string
  | Dollar of string
  | Builtin of e_builtin
  | Pbool of predicate
  | Application of expression * expression
  | Couple of c_or_m  * expression * expression
  | Sequence of expression Nlist.t
  | Extension of expression Nlist.t
  | Comprehension of lident Nlist.t * predicate
  | Binder of expr_binder * lident Nlist.t * predicate * expression
  | Record_Field_Access of expression * lident
  | Record of (lident * expression) Nlist.t
  | Record_Type of (lident * expression) Nlist.t

and expression = {
  exp_loc: Utils.loc;
  exp_par:bool;
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
  prd_loc: Utils.loc;
  prd_par:bool;
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
  sub_loc: Utils.loc;
  sub_be: bool;
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

val clist_of_mch : machine -> clause list
val clist_of_ref : refinement -> clause list
val clist_of_imp : implementation -> clause list

val mk_machine_exn : lident -> lident list -> (Utils.loc*clause) list -> component
val mk_refinement_exn : lident -> lident list -> lident -> (Utils.loc*clause) list -> component
val mk_implementation_exn : lident -> lident list -> lident -> (Utils.loc*clause) list -> component

val get_clauses : component -> clause list
