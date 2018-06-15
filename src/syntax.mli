(** Abstract Syntax Trees*)
type inclusion = Not_Strict | Strict | Non_Inclusion | Non_Strict_Inclusion

type inequality = Smaller_or_Equal | Strictly_Smaller | Greater_or_Equal | Strictly_Greater

type prop_bop = Conjonction | Disjonction | Implication | Equivalence

val prop_bop_to_string : prop_bop -> string

type pred_bop = Equality | Disequality | Membership | Non_Membership
             | Inclusion of inclusion | Inequality of inequality

val pred_bop_to_string : pred_bop -> string

type power_set = Full | Non_Empty | Finite | Finite_Non_Empty

type stype = All_Seq | Non_Empty_Seq | Injective_Seq | Injective_Non_Empty_Seq | Permutations

type ftype =
  | Partial_Functions | Total_Functions | Partial_Injections | Total_Injections
  | Partial_Surjections | Total_Surjections | Bijections

type e_builtin =
  | Integer of int | String of string
  | MaxInt | MinInt | Successor | Predecessor
  | INTEGER | NATURAL | NATURAL1 | INT | NAT | NAT1 | STRINGS | BOOLEANS
  | Empty_Set | Empty_Seq
  | Product | Difference | Addition | Division | Modulo | Power
  | Interval | Union | Intersection | Relations | First_Projection
  | Second_Projection | Composition | Direct_Product | Parallel_Product | Iteration
  | Image | Domain_Restriction | Domain_Soustraction | Codomain_Restriction
  | Codomain_Soustraction | Surcharge | Functions of ftype | Concatenation | Head_Insertion
  | Tail_Insertion | Head_Restriction | Tail_Restriction
  | Cardinal | Power_Set of power_set | Identity_Relation | Inverse_Relation
  | Closure | Transitive_Closure | Domain | Range | Fnc | Rel
  | Sequence_Set of stype | Size | First | Last | Front | Tail | Reverse
  | G_Union | G_Intersection | G_Concatenation | Unary_Minus
  | Max | Min | TRUE | FALSE
  | Tree | Btree | Const | Top | Sons | Prefix | Postfix | SizeT | Mirror
  | Rank | Father | Son | Subtree | Arity | Bin | Left | Right | Infix
  (** Builtin constants*)

val builtin_to_string : e_builtin -> string

val e_builtin_eq: e_builtin -> e_builtin -> bool

type p_builtin = Btrue | Bfalse

val expr_constants : e_builtin list
val expr_infix_ops: e_builtin list
val expr_prefix_postfix_ops: e_builtin list

type ident = string
val ident_eq: ident -> ident -> bool

type 'lc lident = { lid_loc:'lc; lid_str:ident }
val lident_eq: 'lc lident -> 'lc2 lident -> bool

type expr_binder = Sum | Prod | Q_Union | Q_Intersection | Lambda

val binder_to_string : expr_binder -> string

type c_or_m = Maplet | Comma of bool | Infix

type ('lc,'ty) var = {
  var_loc:'lc;
  var_typ:'ty;
  var_id:ident
}

val var_eq : ('lc,'ty) var -> ('lc2,'ty2) var -> bool

type ('lc,'ty) expression_desc =
  | Ident of ident
  | Dollar of ident
  | Builtin of e_builtin
  | Pbool of ('lc,'ty) predicate
  | Application of ('lc,'ty) expression * ('lc,'ty) expression
  | Couple of c_or_m  * ('lc,'ty) expression * ('lc,'ty) expression
  | Sequence of (('lc,'ty) expression) Nlist.t
  | Extension of (('lc,'ty) expression) Nlist.t
  | Comprehension of ('lc,'ty) var Nlist.t * ('lc,'ty) predicate
  | Binder of expr_binder * ('lc,'ty) var Nlist.t * ('lc,'ty) predicate * ('lc,'ty) expression
  | Record_Field_Access of ('lc,'ty) expression * 'lc lident
  | Record of ('lc lident * ('lc,'ty) expression) Nlist.t
  | Record_Type of ('lc lident * ('lc,'ty) expression) Nlist.t

and ('lc,'ty) expression = {
  exp_loc:'lc;
  exp_typ:'ty;
  exp_desc:('lc,'ty) expression_desc
}

and ('lc,'ty) predicate_desc =
  | P_Builtin of p_builtin
  | Binary_Prop of prop_bop * ('lc,'ty) predicate * ('lc,'ty) predicate
  | Binary_Pred of pred_bop * ('lc,'ty) expression * ('lc,'ty) expression
  | Negation of ('lc,'ty) predicate
  | Universal_Q of ('lc,'ty) var Nlist.t * ('lc,'ty) predicate
  | Existential_Q of ('lc,'ty) var Nlist.t * ('lc,'ty) predicate

and ('lc,'ty) predicate = {
  prd_loc:'lc;
  prd_desc:('lc,'ty) predicate_desc
}

val expr_eq : ('lc,'ty) expression -> ('lc2,'ty2) expression -> bool
val pred_eq : ('lc,'ty) predicate -> ('lc2,'ty2) predicate -> bool

type ('lc,'ty) substitution_desc =
  | Skip
  | Affectation of ('lc,'ty) lhs * ('lc,'ty) expression
  | Pre of ('lc,'ty) predicate * ('lc,'ty) substitution
  | Assert of ('lc,'ty) predicate * ('lc,'ty) substitution
  | Choice of ('lc,'ty) substitution Nlist.t
  | IfThenElse of (('lc,'ty) predicate * ('lc,'ty) substitution) Nlist.t * ('lc,'ty) substitution option
  | Select of (('lc,'ty) predicate * ('lc,'ty) substitution) Nlist.t * ('lc,'ty) substitution option
  | Case of ('lc,'ty) expression * (('lc,'ty) expression Nlist.t * ('lc,'ty) substitution) Nlist.t * ('lc,'ty) substitution option
  | Any of ('lc,'ty) var Nlist.t * ('lc,'ty) predicate * ('lc,'ty) substitution
  | Let of ('lc,'ty) var Nlist.t * (('lc,'ty) var * ('lc,'ty) expression) Nlist.t * ('lc,'ty) substitution
  | BecomesElt of ('lc,'ty) var Nlist.t * ('lc,'ty) expression
  | BecomesSuch of ('lc,'ty) var Nlist.t * ('lc,'ty) predicate
  | Var of ('lc,'ty) var Nlist.t * ('lc,'ty) substitution
  | CallUp of ('lc,'ty) var list * 'lc lident * ('lc,'ty) expression list
  | While of ('lc,'ty) predicate * ('lc,'ty) substitution * ('lc,'ty) predicate * ('lc,'ty) expression
  | Sequencement of ('lc,'ty) substitution * ('lc,'ty) substitution
  | Parallel of ('lc,'ty) substitution * ('lc,'ty) substitution

and ('lc,'ty) lhs =
  | Tuple of ('lc,'ty) var Nlist.t
  | Function of ('lc,'ty) var * ('lc,'ty) expression Nlist.t
  | Record of ('lc,'ty) var * 'lc lident

and ('lc,'ty) substitution = {
  sub_loc:'lc;
  sub_desc:('lc,'ty) substitution_desc
}

val subst_eq : ('lc,'ty) substitution -> ('lc2,'ty2) substitution -> bool

type ('lc,'ty) operation =
  { op_out: ('lc,'ty) var list;
    op_name: 'lc lident;
    op_in:('lc,'ty)  var list;
    op_body: ('lc,'ty) substitution
  }

val operation_eq : ('lc,'ty) operation -> ('lc,'ty2) operation -> bool

type ('lc,'ty) machine_instanciation =
  { mi_mch: 'lc lident;
    mi_params: ('lc,'ty) expression list
  }

val minst_eq: ('lc,'ty) machine_instanciation -> ('lc2,'ty2) machine_instanciation -> bool

type ('lc,'ty) set =
  | Abstract_Set of ('lc,'ty) var
  | Concrete_Set of ('lc,'ty) var * ('lc,'ty) var list

val set_eq: ('lc,'ty) set -> ('lc2,'ty2) set -> bool

type ('lc,'ty) clause_desc =
  | Constraints of ('lc,'ty) predicate
  | Imports of (('lc,'ty) machine_instanciation) Nlist.t
  | Sees of 'lc lident Nlist.t
  | Includes of ('lc,'ty) machine_instanciation Nlist.t
  | Extends of ('lc,'ty) machine_instanciation Nlist.t
  | Promotes of 'lc lident Nlist.t
  | Uses of 'lc lident Nlist.t
  | Sets of ('lc,'ty) set Nlist.t
  | Constants of ('lc,'ty) var Nlist.t
  | Abstract_constants of ('lc,'ty) var Nlist.t
  | Properties of ('lc,'ty) predicate
  | Concrete_variables of ('lc,'ty) var Nlist.t
  | Variables of ('lc,'ty) var Nlist.t
  | Invariant of ('lc,'ty) predicate
  | Assertions of ('lc,'ty) predicate Nlist.t
  | Initialization of ('lc,'ty) substitution
  | Operations of ('lc,'ty) operation Nlist.t
  | Local_Operations of ('lc,'ty) operation Nlist.t
  | Values of (('lc,'ty) var * ('lc,'ty) expression) Nlist.t

and ('lc,'ty) clause = {
  cl_loc:'lc;
  cl_desc:('lc,'ty) clause_desc
}

val clause_eq: ('lc,'ty) clause -> ('lc2,'ty2) clause -> bool

type ('lc,'ty) machine_desc = {
  mch_constraints: ('lc * ('lc,'ty) predicate) option;
  mch_sees: ('lc * 'lc lident Nlist.t) option;
  mch_includes: ('lc * ('lc,'ty) machine_instanciation Nlist.t) option;
  mch_promotes: ('lc * 'lc lident Nlist.t) option;
  mch_extends: ('lc * ('lc,'ty) machine_instanciation Nlist.t) option;
  mch_uses: ('lc * 'lc lident Nlist.t) option;
  mch_sets: ('lc * ('lc,'ty) set Nlist.t) option;
  mch_concrete_constants: ('lc * ('lc,'ty) var Nlist.t) option;
  mch_abstract_constants: ('lc * ('lc,'ty) var Nlist.t) option;
  mch_properties: ('lc * ('lc,'ty) predicate) option;
  mch_concrete_variables: ('lc * ('lc,'ty) var Nlist.t) option;
  mch_abstract_variables: ('lc * ('lc,'ty) var Nlist.t) option;
  mch_invariant: ('lc * ('lc,'ty) predicate) option;
  mch_assertions: ('lc * ('lc,'ty) predicate Nlist.t) option;
  mch_initialisation: ('lc * ('lc,'ty) substitution) option;
  mch_operations: ('lc * ('lc,'ty) operation Nlist.t) option;
}

type ('lc,'ty) refinement_desc = {
  ref_refines: 'lc lident;
  ref_sees: ('lc*'lc lident Nlist.t) option;
  ref_includes: ('lc*('lc,'ty) machine_instanciation Nlist.t) option;
  ref_promotes: ('lc*'lc lident Nlist.t) option;
  ref_extends: ('lc*('lc,'ty) machine_instanciation Nlist.t) option;
  ref_sets: ('lc*('lc,'ty) set Nlist.t) option;
  ref_concrete_constants: ('lc*('lc,'ty) var Nlist.t) option;
  ref_abstract_constants: ('lc*('lc,'ty) var Nlist.t) option;
  ref_properties: ('lc*('lc,'ty) predicate) option;
  ref_concrete_variables: ('lc*('lc,'ty) var Nlist.t) option;
  ref_abstract_variables: ('lc*('lc,'ty) var Nlist.t) option;
  ref_invariant: ('lc*('lc,'ty) predicate) option;
  ref_assertions: ('lc*('lc,'ty) predicate Nlist.t) option;
  ref_initialisation: ('lc*('lc,'ty) substitution) option;
  ref_operations: ('lc*('lc,'ty) operation Nlist.t) option;
  ref_local_operations: ('lc*('lc,'ty) operation Nlist.t) option;
}

type ('lc,'ty) implementation_desc = {
  imp_refines: 'lc lident;
  imp_sees: ('lc*'lc lident Nlist.t) option;
  imp_imports: ('lc*('lc,'ty) machine_instanciation Nlist.t) option;
  imp_promotes: ('lc*'lc lident Nlist.t) option;
  imp_extends: ('lc*('lc,'ty) machine_instanciation Nlist.t) option;
  imp_sets: ('lc*('lc,'ty) set Nlist.t) option;
  imp_concrete_constants: ('lc*('lc,'ty) var Nlist.t) option;
  imp_properties: ('lc*('lc,'ty) predicate) option;
  imp_values: ('lc*(('lc,'ty) var*('lc,'ty) expression) Nlist.t) option;
  imp_concrete_variables: ('lc*('lc,'ty) var Nlist.t) option;
  imp_invariant: ('lc*('lc,'ty) predicate) option;
  imp_assertions: ('lc*('lc,'ty) predicate Nlist.t) option;
  imp_initialisation: ('lc*('lc,'ty) substitution) option;
  imp_operations: ('lc*('lc,'ty) operation Nlist.t) option;
  imp_local_operations: ('lc*('lc,'ty) operation Nlist.t) option;
}

type ('lc,'ty) component_desc = 
  | Machine of ('lc,'ty) machine_desc
  | Refinement of ('lc,'ty) refinement_desc
  | Implementation of ('lc,'ty) implementation_desc

type ('lc,'ty) component = {
  co_loc: 'lc;
  co_name: ident;
  co_parameters: ('lc,'ty) var list;
  co_desc: ('lc,'ty) component_desc
}

val component_eq : ('lc,'ty) component -> ('lc2,'ty2) component -> bool
val get_clauses : ('lc,'ty) component -> ('lc,'ty) clause list

val mk_machine : 'lc lident -> ('lc,'ty) var list -> ('lc,'ty) clause list -> (('lc,'ty) component,'lc Error.t) result
val mk_refinement : 'lc lident -> ('lc,'ty) var list -> 'lc lident -> ('lc,'ty) clause list -> (('lc,'ty) component,'lc Error.t) result
val mk_implementation : 'lc lident -> ('lc,'ty) var list -> 'lc lident -> ('lc,'ty) clause list -> (('lc,'ty) component,'lc Error.t) result

type p_var = (Utils.loc,unit) var
type p_lident = Utils.loc lident
type p_expression = (Utils.loc,unit) expression
type p_predicate = (Utils.loc,unit) predicate
type p_substitution = (Utils.loc,unit) substitution
type p_set = (Utils.loc,unit) set
type p_machine_instanciation = (Utils.loc,unit) machine_instanciation
type p_clause = (Utils.loc,unit) clause
type p_operation = (Utils.loc,unit) operation
type p_component = (Utils.loc,unit) component
