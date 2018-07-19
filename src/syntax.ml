type inclusion = Not_Strict | Strict | Non_Inclusion | Non_Strict_Inclusion

type inequality = Smaller_or_Equal | Strictly_Smaller | Greater_or_Equal | Strictly_Greater

type prop_bop = Conjonction | Disjonction | Implication | Equivalence

type pred_bop = Equality | Disequality | Membership | Non_Membership
             | Inclusion of inclusion | Inequality of inequality

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

let e_builtin_eq b1 b2 =
  match b1, b2 with
  | Integer i1, Integer i2 -> i1 == i2
  | String s1, String s2 -> String.equal s1 s2
  | _, _ -> b1 = b2

let expr_constants = [
  MaxInt; MinInt; INTEGER; NATURAL; NATURAL1; INT; NAT; NAT1; STRINGS;
  BOOLEANS; Empty_Set; Empty_Seq; TRUE; FALSE ]

let expr_infix_ops =
  [ Product; Difference; Addition; Division; Modulo; Power; Interval; Union;
    Intersection; Relations; Composition; Direct_Product; Parallel_Product;
    Domain_Restriction; Domain_Soustraction; Codomain_Restriction;
    Codomain_Soustraction; Surcharge; Concatenation; Head_Insertion;
    Tail_Insertion; Head_Restriction; Tail_Restriction;
    Functions Partial_Functions; Functions Total_Functions;
    Functions Partial_Injections; Functions Total_Injections;
    Functions Partial_Surjections; Functions Total_Surjections;
    Functions Bijections; Image
  ]

let expr_prefix_postfix_ops =
  [ Unary_Minus; First_Projection; Second_Projection; Iteration; Max; Min;
    Cardinal; Identity_Relation; Closure; Transitive_Closure; Domain; Range; Fnc;
    Rel; Size; First; Last; Front; Tail; Reverse; G_Union; G_Intersection;
    G_Concatenation; Tree; Btree; Const; Top; Sons; Prefix; Postfix; SizeT;
    Mirror; Rank; Father; Son; Subtree; Arity; Bin; Left; Right; Infix;
    Sequence_Set All_Seq; Sequence_Set Non_Empty_Seq; Sequence_Set Injective_Seq;
    Sequence_Set Injective_Non_Empty_Seq; Sequence_Set Permutations; Power_Set Full;
    Power_Set Non_Empty; Power_Set Finite; Power_Set Finite_Non_Empty;
    Inverse_Relation; Successor; Predecessor ]

type p_builtin =
  | Btrue
  | Bfalse

type expr_binder = Sum | Prod | Q_Union | Q_Intersection | Lambda

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

module R = struct
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

  let get_clauses _ = assert false (*FIXME*)

end

let _undecorate_e u_ident u_bvar u_rfield u_expr u_pred : _ expression_desc -> R.expression = function
  | Ident id -> { exp_desc=Ident (u_ident id) }
  | Dollar id -> { exp_desc=Dollar (u_ident id) }
  | Builtin bi -> { exp_desc=Builtin bi }
  | Pbool p -> { exp_desc=Pbool (u_pred p) }
  | Application (e1,e2) -> { exp_desc=Application (u_expr e1,u_expr e2) }
  | Couple (x,e1,e2) -> { exp_desc=Couple(x,u_expr e1,u_expr e2) }
  | Sequence nle -> { exp_desc=Sequence (Nlist.map u_expr nle) }
  | Extension nle -> { exp_desc=Extension (Nlist.map u_expr nle) }
  | Comprehension (xlst,p) -> { exp_desc=Comprehension(Nlist.map u_bvar xlst,u_pred p) }
  | Binder (bi,xlst,p,e) -> { exp_desc=Binder (bi,Nlist.map u_bvar xlst,u_pred p,u_expr e) }
  | Record_Field_Access (e,fd) -> { exp_desc=Record_Field_Access (u_expr e,u_rfield fd) }
  | Record nle ->
    let aux (fd,e) = (u_rfield fd,u_expr e) in
    { exp_desc=Record (Nlist.map aux nle) }
  | Record_Type nle ->
    let aux (fd,e) = (u_rfield fd,u_expr e) in
    { exp_desc=Record_Type (Nlist.map aux nle) }

let _undecorate_p u_bvar u_expr u_pred : _ predicate_desc -> R.predicate = function
  | P_Builtin bi -> { prd_desc=P_Builtin bi }
  | Binary_Prop (op,p1,p2) -> { prd_desc=Binary_Prop (op,u_pred p1,u_pred p2) }
  | Binary_Pred (op,e1,e2) -> { prd_desc=Binary_Pred (op,u_expr e1,u_expr e2) }
  | Negation p -> { prd_desc=Negation (u_pred p) }
  | Universal_Q (xlst,p) -> { prd_desc=Universal_Q (Nlist.map u_bvar xlst,u_pred p) }
  | Existential_Q (xlst,p) -> { prd_desc=Existential_Q (Nlist.map u_bvar xlst,u_pred p) }

type lident = { lid_loc:Utils.loc; lid_str:string }

module P = struct

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

  let check_none_exn lc opt : unit =
    match opt with
    | None -> ()
    | Some _ -> Error.raise_exn lc "This clause is defined twice."

  let check_empty_exn lc lst : unit =
    match lst with
    | [] -> ()
    | _ -> Error.raise_exn lc "This clause is defined twice."

  type nonrec machine_desc = (mch_name,op_name,symb,arg,expression,predicate,substitution) machine_desc

  let add_clause_mch_exn (co:machine_desc) (cl:clause) : machine_desc =
    match cl.cl_desc with
    | Sees lst -> ( check_empty_exn cl.cl_loc co.mch_sees; { co with mch_sees = Nlist.to_list lst } )
    | Sets lst -> ( check_empty_exn cl.cl_loc co.mch_sets; { co with mch_sets = Nlist.to_list lst } )
    | Constants lst -> ( check_empty_exn cl.cl_loc co.mch_concrete_constants; { co with mch_concrete_constants = Nlist.to_list lst } )
    | Abstract_constants lst -> ( check_empty_exn cl.cl_loc co.mch_abstract_constants; { co with mch_abstract_constants = Nlist.to_list lst } )
    | Properties p -> ( check_none_exn cl.cl_loc co.mch_properties; { co with mch_properties = Some p } )
    | Concrete_variables lst -> ( check_empty_exn cl.cl_loc co.mch_concrete_variables; { co with mch_concrete_variables = Nlist.to_list lst } )
    | Variables lst -> ( check_empty_exn cl.cl_loc co.mch_abstract_variables; { co with mch_abstract_variables = Nlist.to_list lst } )
    | Invariant p -> ( check_none_exn cl.cl_loc co.mch_invariant; { co with mch_invariant = Some p } )
    | Assertions lst -> ( check_empty_exn cl.cl_loc co.mch_assertions; { co with mch_assertions = Nlist.to_list lst } )
    | Initialization p -> ( check_none_exn cl.cl_loc co.mch_initialisation; { co with mch_initialisation = Some p } )
    | Operations lst -> ( check_empty_exn cl.cl_loc co.mch_operations; { co with mch_operations = Nlist.to_list lst } )
    | Values _ -> Error.raise_exn cl.cl_loc "The clause VALUES is not allowed in abstract machines."
    | Local_Operations _ -> Error.raise_exn cl.cl_loc "The clause LOCAL_OPERATIONS is not allowed in abstract machines."
    | Promotes lst -> ( check_empty_exn cl.cl_loc co.mch_promotes; { co with mch_promotes = Nlist.to_list lst } )
    | Imports _ -> Error.raise_exn cl.cl_loc "The clause IMPORTS is not allowed in abstract machines."
    | Constraints p -> ( check_none_exn cl.cl_loc co.mch_constraints; { co with mch_constraints = Some p } )
    | Includes lst -> ( check_empty_exn cl.cl_loc co.mch_includes; { co with mch_includes = Nlist.to_list lst } )
    | Extends lst -> ( check_empty_exn cl.cl_loc co.mch_extends; { co with mch_extends = Nlist.to_list lst } )
    | Uses lst -> ( check_empty_exn cl.cl_loc co.mch_uses; { co with mch_uses = Nlist.to_list lst } )

  let mk_machine_exn (co_name:lident) (co_parameters:lident list) (clauses:clause list) : component =
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

  type nonrec refinement_desc = (mch_name,op_name,symb,arg,expression,predicate,substitution) refinement_desc

  let add_clause_ref_exn (co:refinement_desc) (cl:clause) : refinement_desc =
    match cl.cl_desc with
    | Sees lst -> ( check_empty_exn cl.cl_loc co.ref_sees; { co with ref_sees = Nlist.to_list lst } )
    | Sets lst -> ( check_empty_exn cl.cl_loc co.ref_sets; { co with ref_sets = Nlist.to_list lst } )
    | Constants lst -> ( check_empty_exn cl.cl_loc co.ref_concrete_constants; { co with ref_concrete_constants = Nlist.to_list lst } )
    | Properties p -> ( check_none_exn cl.cl_loc co.ref_properties; { co with ref_properties = Some p } )
    | Concrete_variables lst -> ( check_empty_exn cl.cl_loc co.ref_concrete_variables; { co with ref_concrete_variables = Nlist.to_list lst } )
    | Invariant p -> ( check_none_exn cl.cl_loc co.ref_invariant; { co with ref_invariant = Some p } )
    | Assertions lst -> ( check_empty_exn cl.cl_loc co.ref_assertions; { co with ref_assertions = Nlist.to_list lst } )
    | Initialization p -> ( check_none_exn cl.cl_loc co.ref_initialisation; { co with ref_initialisation = Some p } )
    | Operations lst -> ( check_empty_exn cl.cl_loc co.ref_operations; { co with ref_operations = Nlist.to_list lst } )
    | Values lst -> Error.raise_exn cl.cl_loc "The clause VALUES is not allowed in refinements."
    | Local_Operations lst -> ( check_empty_exn cl.cl_loc co.ref_local_operations; { co with ref_local_operations = Nlist.to_list lst } )
    | Promotes lst -> ( check_empty_exn cl.cl_loc co.ref_promotes; { co with ref_promotes = Nlist.to_list lst } )
    | Imports lst -> Error.raise_exn cl.cl_loc "The clause IMPORTS is not allowed in refinements."
    | Abstract_constants lst -> ( check_empty_exn cl.cl_loc co.ref_abstract_constants; { co with ref_abstract_constants = Nlist.to_list lst } )
    | Variables lst -> ( check_empty_exn cl.cl_loc co.ref_abstract_variables; { co with ref_abstract_variables = Nlist.to_list lst } )
    | Constraints _ -> Error.raise_exn cl.cl_loc "The clause CONSTRAINTS is not allowed in refinements."
    | Includes lst -> ( check_empty_exn cl.cl_loc co.ref_includes; { co with ref_includes = Nlist.to_list lst } )
    | Extends lst -> ( check_empty_exn cl.cl_loc co.ref_extends; { co with ref_extends = Nlist.to_list lst } )
    | Uses lst -> Error.raise_exn cl.cl_loc "The clause USES is not allowed in refinements."

  let mk_refinement_exn co_name co_parameters refines clauses =
    let ref_desc =
      { ref_refines=refines;
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

  type nonrec implementation_desc = (mch_name,op_name,symb,arg,expression,predicate,substitution) implementation_desc

  let add_clause_imp_exn (co:implementation_desc) (cl:clause) : implementation_desc =
    match cl.cl_desc with
    | Sees lst -> ( check_empty_exn cl.cl_loc co.imp_sees; { co with imp_sees = Nlist.to_list lst } )
    | Sets lst -> ( check_empty_exn cl.cl_loc co.imp_sets; { co with imp_sets = Nlist.to_list lst } )
    | Constants lst -> ( check_empty_exn cl.cl_loc co.imp_concrete_constants; { co with imp_concrete_constants = Nlist.to_list lst } )
    | Properties p -> ( check_none_exn cl.cl_loc co.imp_properties; { co with imp_properties = Some p } )
    | Concrete_variables lst -> ( check_empty_exn cl.cl_loc co.imp_concrete_variables; { co with imp_concrete_variables = Nlist.to_list lst } )
    | Invariant p -> ( check_none_exn cl.cl_loc co.imp_invariant; { co with imp_invariant = Some p } )
    | Assertions lst -> ( check_empty_exn cl.cl_loc co.imp_assertions; { co with imp_assertions = Nlist.to_list lst } )
    | Initialization p -> ( check_none_exn cl.cl_loc co.imp_initialisation; { co with imp_initialisation = Some p } )
    | Operations lst -> ( check_empty_exn cl.cl_loc co.imp_operations; { co with imp_operations = Nlist.to_list lst } )
    | Values lst -> ( check_empty_exn cl.cl_loc co.imp_values; { co with imp_values = Nlist.to_list lst } )
    | Local_Operations lst -> ( check_empty_exn cl.cl_loc co.imp_local_operations; { co with imp_local_operations = Nlist.to_list lst } )
    | Promotes lst -> ( check_empty_exn cl.cl_loc co.imp_promotes; { co with imp_promotes = Nlist.to_list lst } )
    | Imports lst -> ( check_empty_exn cl.cl_loc co.imp_imports; { co with imp_imports = Nlist.to_list lst } )
    | Abstract_constants _ -> Error.raise_exn cl.cl_loc "The clause ABSTRACT_CONSTANTS is not allowed in implementations."
    | Variables _ -> Error.raise_exn cl.cl_loc "The clause VARIABLES is not allowed in implementations."
    | Constraints _ -> Error.raise_exn cl.cl_loc "The clause CONSTRAINTS is not allowed in implementation."
    | Includes _ -> Error.raise_exn cl.cl_loc "The clause INCLUDES is not allowed in implementations."
    | Extends lst -> ( check_empty_exn cl.cl_loc co.imp_extends; { co with imp_extends = Nlist.to_list lst } )
    | Uses _ -> Error.raise_exn cl.cl_loc "The clause USES is not allowed in implementations."

  let mk_implementation_exn co_name co_parameters refines clauses =
    let imp_desc =
      { imp_refines=refines;
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

  let u_ident x = x
  let u_bvar x = x.lid_str
  let u_rfield x = x.lid_str
  let rec u_expr x = _undecorate_e u_ident u_bvar u_rfield u_expr u_pred x.exp_desc
  and u_pred x = _undecorate_p u_bvar u_expr u_pred x.prd_desc

  let u_subst x = assert false (*FIXME*)
  let u_comp x = assert false (*FIXME*)

  let get_clauses _ = assert false (*FIXME*)

end

module T = struct

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

let pred_bop_to_string : pred_bop -> string = function
  | Equality -> "="
  | Disequality -> "/="
  | Membership -> ":"
  | Non_Membership -> "/:"
  | Inclusion Not_Strict -> "<:"
  | Inclusion Strict -> "<<:"
  | Inclusion Non_Inclusion -> "/<:"
  | Inclusion Non_Strict_Inclusion -> "/<<:"
  | Inequality Smaller_or_Equal -> "<="
  | Inequality Strictly_Smaller -> "<"
  | Inequality Greater_or_Equal -> ">="
  | Inequality Strictly_Greater -> ">"

let prop_bop_to_string : prop_bop -> string = function
  | Conjonction -> "&"
  | Disjonction -> "or"
  | Implication -> "=>"
  | Equivalence -> "<=>"

let builtin_to_string : e_builtin -> string = function
  | Integer i -> Int32.to_string i
  | String s -> "\"" ^ s ^ "\""
  | TRUE -> "TRUE"
  | FALSE -> "FALSE"
  | MaxInt -> "MAXINT"
  | MinInt -> "MININT"
  | INTEGER -> "INTEGER"
  | NATURAL -> "NATURAL"
  | NATURAL1 -> "NATURAL1"
  | INT -> "INT"
  | NAT -> "NAT"
  | NAT1 -> "NAT1"
  | STRINGS -> "STRING"
  | BOOLEANS -> "BOOL"
  | Empty_Set -> "{}"
  | Empty_Seq -> "[]"
  | Successor -> "succ"
  | Predecessor -> "pred"
  | Cardinal -> "card"
  | Power_Set Full -> "POW"
  | Power_Set Non_Empty -> "POW1"
  | Power_Set Finite -> "FIN"
  | Power_Set Finite_Non_Empty -> "FIN1"
  | Identity_Relation -> "id"
  | Closure -> "closure"
  | Transitive_Closure -> "closure1"
  | Domain -> "dom"
  | Range -> "ran"
  | Fnc -> "fnc"
  | Rel -> "rel"
  | Sequence_Set All_Seq -> "seq"
  | Sequence_Set Non_Empty_Seq -> "seq1"
  | Sequence_Set Injective_Seq -> "iseq"
  | Sequence_Set Injective_Non_Empty_Seq -> "iseq1"
  | Sequence_Set Permutations -> "perm"
  | Size -> "size"
  | First -> "first"
  | Last -> "last"
  | Front -> "front"
  | Tail -> "tail"
  | Reverse -> "rev"
  | G_Union -> "union"
  | G_Intersection -> "inter"
  | G_Concatenation -> "conc"
  | Max -> "max"
  | Min -> "min"
  | First_Projection -> "prj1"
  | Second_Projection -> "prj2"
  | Iteration -> "iterate"

  | Image -> ".[.]"
  | Unary_Minus -> "-"
  | Inverse_Relation -> "~"

  | Product -> "*"
  | Difference -> "-"
  | Addition -> "+"
  | Division -> "/"
  | Modulo -> "mod"
  | Power -> "**"
  | Interval -> ".."
  | Union -> "\\/"
  | Intersection -> "/\\"
  | Relations -> "<->"
  | Composition -> ";"
  | Direct_Product -> "><"
  | Parallel_Product -> "||"
  | Domain_Restriction -> "<|"
  | Domain_Soustraction -> "<<|"
  | Codomain_Restriction -> "|>"
  | Codomain_Soustraction -> "|>>"
  | Surcharge -> "<+"
  | Functions Partial_Functions -> "+->"
  | Functions Partial_Injections -> ">+>"
  | Functions Total_Injections -> ">->"
  | Functions Total_Functions -> "-->"
  | Functions Total_Surjections -> "-->>"
  | Functions Partial_Surjections -> "+->>"
  | Functions Bijections -> ">->>"
  | Concatenation -> "^"
  | Head_Insertion -> "->"
  | Tail_Insertion -> "<-"
  | Head_Restriction -> "/|\\"
  | Tail_Restriction -> "\\|/"

  | Tree -> "tree"
  | Btree -> "btree"
  | Const -> "const"
  | Top -> "top"
  | Sons -> "sons"
  | Prefix -> "prefix"
  | Postfix -> "postfix"
  | SizeT -> "sizet"
  | Mirror -> "mirror"
  | Rank -> "rank"
  | Father -> "father"
  | Son -> "son"
  | Subtree -> "subtree"
  | Arity -> "arity"
  | Bin -> "bin"
  | Left -> "left"
  | Right -> "right"
  | Infix -> "infix"

let binder_to_string = function
  | Sum -> "SIGMA"
  | Prod -> "PI"
  | Q_Union -> "UNION"
  | Q_Intersection -> "INTER"
  | Lambda -> "%"
