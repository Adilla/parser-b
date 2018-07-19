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
  | Integer of Int32.t | String of string
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

type p_builtin = Btrue | Bfalse

val expr_constants : e_builtin list
val expr_infix_ops: e_builtin list
val expr_prefix_postfix_ops: e_builtin list

type expr_binder = Sum | Prod | Q_Union | Q_Intersection | Lambda

val binder_to_string : expr_binder -> string

type c_or_m = Maplet | Comma of bool | Infix

type ('ident,'bvar,'rfield,'expression,'predicate) expression_desc =
  | Ident of 'ident
  | Dollar of 'ident
  | Builtin of e_builtin
  | Pbool of 'predicate
  | Application of 'expression * 'expression
  | Couple of c_or_m  * 'expression * 'expression
  | Sequence of 'expression Nlist.t
  | Extension of 'expression Nlist.t
  | Comprehension of 'bvar Nlist.t * 'predicate
  | Binder of expr_binder * 'bvar Nlist.t * 'predicate * 'expression
  | Record_Field_Access of 'expression * 'rfield
  | Record of ('rfield * 'expression) Nlist.t
  | Record_Type of ('rfield * 'expression) Nlist.t

type ('bvar,'expression,'predicate) predicate_desc =
  | P_Builtin of p_builtin
  | Binary_Prop of prop_bop * 'predicate * 'predicate
  | Binary_Pred of pred_bop * 'expression * 'expression
  | Negation of 'predicate
  | Universal_Q of 'bvar Nlist.t * 'predicate
  | Existential_Q of 'bvar Nlist.t * 'predicate

type ('mut_var,'rfield,'expression) lhs =
  | Tuple of 'mut_var Nlist.t
  | Function of 'mut_var * 'expression Nlist.t
  | Record of 'mut_var * 'rfield

type ('bvar,'op_name,'mut_var,'rfield,'expression,'predicate,'substitution) substitution_desc =
  | Skip
  | Affectation of ('mut_var,'rfield,'expression) lhs * 'expression
  | Pre of 'predicate * 'substitution
  | Assert of 'predicate * 'substitution
  | Choice of 'substitution Nlist.t
  | IfThenElse of ('predicate * 'substitution) Nlist.t * 'substitution option
  | Select of ('predicate * 'substitution) Nlist.t * 'substitution option
  | Case of 'expression * ('expression Nlist.t * 'substitution) Nlist.t * 'substitution option
  | Any of 'bvar Nlist.t * 'predicate * 'substitution
  | Let of 'bvar Nlist.t * ('bvar * 'expression) Nlist.t * 'substitution
  | BecomesElt of 'mut_var Nlist.t * 'expression
  | BecomesSuch of  'mut_var Nlist.t * 'predicate
  | Var of  'bvar Nlist.t * 'substitution
  | CallUp of  'mut_var list * 'op_name * 'expression list
  | While of 'predicate * 'substitution * 'predicate * 'expression
  | Sequencement of 'substitution * 'substitution
  | Parallel of 'substitution * 'substitution

type ('op_name,'arg,'substitution) operation =
  { op_out: 'arg list;
    op_name: 'op_name;
    op_in: 'arg list;
    op_body: 'substitution
  }

type ('mch_name,'expression) machine_instanciation =
  { mi_mch: 'mch_name;
    mi_params: 'expression list
  }

type 'a set =
  | Abstract_Set of 'a
  | Concrete_Set of 'a * 'a list

type ('mch_name,'op_name,'symb,'arg,'expression,'predicate,'substitution) machine_desc = {
  mch_constraints: 'predicate option;
  mch_sees: 'mch_name list;
  mch_includes: ('mch_name,'expression) machine_instanciation list;
  mch_promotes: 'op_name list;
  mch_extends: ('mch_name,'expression) machine_instanciation list;
  mch_uses: 'mch_name list;
  mch_sets: 'symb set list;
  mch_concrete_constants: 'symb list;
  mch_abstract_constants: 'symb list;
  mch_properties: 'predicate option;
  mch_concrete_variables: 'symb list;
  mch_abstract_variables: 'symb list;
  mch_invariant: 'predicate option;
  mch_assertions: 'predicate list;
  mch_initialisation: 'substitution option;
  mch_operations: ('op_name,'arg,'substitution) operation list;
}

type ('mch_name,'op_name,'symb,'arg,'expression,'predicate,'substitution) refinement_desc = {
  ref_refines: 'mch_name;
  ref_sees: 'mch_name list;
  ref_includes: ('mch_name,'expression) machine_instanciation list;
  ref_promotes: 'op_name list;
  ref_extends: ('mch_name,'expression) machine_instanciation list;
  ref_sets: 'symb set list;
  ref_concrete_constants: 'symb list;
  ref_abstract_constants: 'symb list;
  ref_properties: 'predicate option;
  ref_concrete_variables: 'symb list;
  ref_abstract_variables: 'symb list;
  ref_invariant: 'predicate option;
  ref_assertions: 'predicate list;
  ref_initialisation: 'substitution option;
  ref_operations: ('op_name,'arg,'substitution) operation list;
  ref_local_operations: ('op_name,'arg,'substitution) operation  list;
}

type ('mch_name,'op_name,'symb,'arg,'expression,'predicate,'substitution) implementation_desc = {
  imp_refines: 'mch_name;
  imp_sees: 'mch_name list;
  imp_imports: ('mch_name,'expression) machine_instanciation list;
  imp_promotes: 'op_name list;
  imp_extends: ('mch_name,'expression) machine_instanciation list;
  imp_sets: 'symb set list;
  imp_concrete_constants: 'symb list;
  imp_properties: 'predicate option;
  imp_values: ('symb*'expression) list;
  imp_concrete_variables: 'symb list;
  imp_invariant: 'predicate option;
  imp_assertions: 'predicate list;
  imp_initialisation: 'substitution option;
  imp_operations: ('op_name,'arg,'substitution) operation list;
  imp_local_operations: ('op_name,'arg,'substitution) operation list;
}

type ('mch_name,'op_name,'symb,'arg,'expression,'predicate,'substitution) component_desc =
  | Machine of ('mch_name,'op_name,'symb,'arg,'expression,'predicate,'substitution) machine_desc
  | Refinement of ('mch_name,'op_name,'symb,'arg,'expression,'predicate,'substitution) refinement_desc
  | Implementation of ('mch_name,'op_name,'symb,'arg,'expression,'predicate,'substitution) implementation_desc

type ('mch_name,'op_name,'symb,'arg,'expression,'predicate,'substitution) clause_desc =
    | Constraints of 'predicate
    | Imports of ('mch_name,'expression) machine_instanciation Nlist.t
    | Sees of 'symb Nlist.t
    | Includes of ('mch_name,'expression) machine_instanciation Nlist.t
    | Extends of  ('mch_name,'expression) machine_instanciation Nlist.t
    | Promotes of 'symb Nlist.t
    | Uses of 'symb Nlist.t
    | Sets of 'symb set Nlist.t
    | Constants of 'symb Nlist.t
    | Abstract_constants of 'symb Nlist.t
    | Properties of 'predicate
    | Concrete_variables of 'symb Nlist.t
    | Variables of 'symb Nlist.t
    | Invariant of 'predicate
    | Assertions of 'predicate Nlist.t
    | Initialization of 'substitution
    | Operations of ('op_name,'arg,'substitution) operation Nlist.t
    | Local_Operations of ('op_name,'arg,'substitution) operation Nlist.t
    | Values of ('symb * 'expression) Nlist.t

module R : sig
  type ident = string
  type bvar = string
  type rfield = string
  type mch_name = string
  type op_name = string
  type mut_var = string
  type symb = string
  type param = string
  type arg = string

  type expression = {
    exp_desc:(ident,bvar,rfield,expression,predicate) expression_desc
  } (*FIXME unboxed*)

  and predicate = {
    prd_desc: (bvar,expression,predicate) predicate_desc
  } (*FIXME unboxed*)

  type substitution = {
    sub_desc: (bvar,op_name,mut_var,rfield,expression,predicate,substitution) substitution_desc
  } (*FIXME unboxed*)

  type nonrec set = symb set
  type nonrec machine_instanciation = (mch_name,expression) machine_instanciation
  type nonrec operation = (op_name,arg,substitution) operation

  type component = {
    co_name: mch_name;
    co_parameters: param list;
    co_desc: (mch_name,op_name,symb,arg,expression,predicate,substitution) component_desc
  }

  type clause = {
    cl_loc: Utils.loc;
    cl_desc: (mch_name,op_name,symb,arg,expression,predicate,substitution) clause_desc;
  }

  val get_clauses : component -> clause list

end

type lident = { lid_loc:Utils.loc; lid_str:string }

module P : sig

  type ident = string
  type bvar = lident
  type rfield = lident
  type mch_name = lident
  type op_name = lident
  type mut_var = lident
  type symb = lident
  type param = lident
  type arg = lident

  type expression = {
    exp_loc: Utils.loc;
    exp_desc: (ident,bvar,rfield,expression,predicate) expression_desc
  }

  and predicate = {
    prd_loc: Utils.loc;
    prd_desc: (bvar,expression,predicate) predicate_desc
  }

  type substitution = {
    sub_loc: Utils.loc;
    sub_desc: (bvar,op_name,mut_var,rfield,expression,predicate,substitution) substitution_desc
  }

  type nonrec set = symb set
  type nonrec machine_instanciation = (mch_name,expression) machine_instanciation
  type nonrec operation = (op_name,arg,substitution) operation

  type component = {
    co_name: mch_name;
    co_parameters: param list;
    co_desc: (mch_name,op_name,symb,arg,expression,predicate,substitution) component_desc
  }

  type clause = {
    cl_loc: Utils.loc;
    cl_desc: (mch_name,op_name,symb,arg,expression,predicate,substitution) clause_desc;
  }

  val mk_machine_exn : lident -> lident list -> clause list -> component
  val mk_refinement_exn : lident -> lident list -> lident -> clause list -> component
  val mk_implementation_exn : lident -> lident list -> lident -> clause list -> component

  val get_clauses : component -> clause list

  val u_expr: expression -> R.expression
  val u_pred: predicate -> R.predicate
  val u_subst: substitution -> R.substitution
  val u_comp: component -> R.component
end

module T : sig

  type var = {
    var_loc: Utils.loc;
    var_typ: Btype.t;
    var_id: string;
  }

  type ident = string
  type bvar = var
  type rfield = lident
  type mch_name = lident
  type op_name = lident
  type mut_var = var
  type symb = var
  type param = var
  type arg = var

  type expression = {
    exp_loc: Utils.loc;
    exp_typ: Btype.t;
    exp_desc: (ident,bvar,rfield,expression,predicate) expression_desc
  }

  and predicate = {
    prd_loc: Utils.loc;
    prd_desc: (bvar,expression,predicate) predicate_desc
  }

  type substitution = {
    sub_loc: Utils.loc;
    sub_desc: (bvar,op_name,mut_var,rfield,expression,predicate,substitution) substitution_desc
  }

  type nonrec set = symb set
  type nonrec machine_instanciation = (mch_name,expression) machine_instanciation
  type nonrec operation = (op_name,arg,substitution) operation

  type component = {
    co_name: mch_name;
    co_parameters: param list;
    co_desc: (mch_name,op_name,symb,arg,expression,predicate,substitution) component_desc
  }
end
