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

type ident = string
let ident_eq = String.equal

type ('lc,'ty) var = { var_loc:'lc; var_typ:'ty; var_id:ident }
let var_eq (v1:('lc,'ty) var) (v2:('lc2,'ty2) var) : bool = String.equal v1.var_id v2.var_id 

type 'lc lident = { lid_loc:'lc; lid_str:ident }
let lident_eq : type a b. a lident -> b lident -> bool = fun a b -> ident_eq a.lid_str b.lid_str

type ('lc,'ty) expression_desc =
  | Ident of ident
  | Dollar of ident
  | Builtin of e_builtin
  | Pbool of ('lc,'ty) predicate
  | Application of ('lc,'ty) expression * ('lc,'ty) expression
  | Couple of c_or_m  * ('lc,'ty) expression * ('lc,'ty) expression
  | Sequence of (('lc,'ty) expression) Utils.non_empty_list
  | Extension of (('lc,'ty) expression) Utils.non_empty_list
  | Comprehension of ('lc,'ty) var Utils.non_empty_list * ('lc,'ty) predicate
  | Binder of expr_binder * ('lc,'ty) var Utils.non_empty_list * ('lc,'ty) predicate * ('lc,'ty) expression
  | Record_Field_Access of ('lc,'ty) expression * 'lc lident
  | Record of ('lc lident * ('lc,'ty) expression) Utils.non_empty_list
  | Record_Type of ('lc lident * ('lc,'ty) expression) Utils.non_empty_list

and ('lc,'ty) expression = { exp_loc:'lc; exp_typ:'ty; exp_desc:('lc,'ty) expression_desc }

and ('lc,'ty) predicate_desc =
  | P_Builtin of p_builtin
  | Binary_Prop of prop_bop * ('lc,'ty) predicate * ('lc,'ty) predicate
  | Binary_Pred of pred_bop * ('lc,'ty) expression * ('lc,'ty) expression
  | Negation of ('lc,'ty) predicate
  | Universal_Q of ('lc,'ty) var Utils.non_empty_list * ('lc,'ty) predicate
  | Existential_Q of ('lc,'ty) var Utils.non_empty_list * ('lc,'ty) predicate

and ('lc,'ty) predicate = { prd_loc:'lc; prd_desc:('lc,'ty) predicate_desc }

let rec expr_eq : type a b c d. (a,b) expression -> (c,d) expression -> bool = fun e1 e2 ->
  match e1.exp_desc, e2.exp_desc with
  | Ident v1, Ident v2 | Dollar v1, Dollar v2 -> ident_eq v1 v2
  | Builtin b1, Builtin b2 -> e_builtin_eq b1 b2
  | Pbool p1, Pbool p2 -> pred_eq p1 p2
  | Application (f1,a1), Application (f2,a2) -> expr_eq f1 f2 && expr_eq a1 a2
  | Couple (_,x1,y1), Couple (_,x2,y2) -> expr_eq x1 x2 && expr_eq y1 y2
  | Sequence lst1, Sequence lst2 -> Utils.nelist_eq expr_eq lst1 lst2
  | Extension lst1, Extension lst2 -> Utils.nelist_eq expr_eq lst1 lst2
  | Comprehension (lst1,p1), Comprehension(lst2,p2) ->
    Utils.nelist_eq var_eq lst1 lst2 && pred_eq p1 p2
  | Binder (bi1,lst1,p1,e1), Binder (bi2,lst2,p2,e2) ->
    bi1 = bi2 && Utils.nelist_eq var_eq lst1 lst2 && pred_eq p1 p2 && expr_eq e1 e2
  | Record_Field_Access (e1,f1), Record_Field_Access(e2,f2) ->
    expr_eq e1 e2 && ident_eq f1.lid_str f2.lid_str
  | Record lst1, Record lst2
  | Record_Type lst1, Record_Type lst2 ->
    let aux (id1,e1) (id2,e2) = ident_eq id1.lid_str id2.lid_str && expr_eq e1 e2 in
    Utils.nelist_eq aux lst1 lst2
  | _, _ -> false

and pred_eq : type a b c d. (a,b) predicate -> (c,d) predicate -> bool = fun p1 p2 ->
  match p1.prd_desc, p2.prd_desc with
  | P_Builtin b1, P_Builtin b2 -> b1 = b2
  | Binary_Prop (b1,p1,q1), Binary_Prop (b2,p2,q2) ->
    b1 = b2 && pred_eq p1 p2 && pred_eq q1 q2
  | Binary_Pred (b1,p1,q1), Binary_Pred (b2,p2,q2) ->
    b1 = b2 && expr_eq p1 p2 && expr_eq q1 q2
  | Negation p1, Negation p2 -> pred_eq p1 p2
  | Universal_Q (lst1,p1), Universal_Q (lst2,p2) ->
    Utils.nelist_eq var_eq lst1 lst2 && pred_eq p1 p2
  | Existential_Q (lst1,p1), Existential_Q (lst2,p2) ->
    Utils.nelist_eq var_eq lst1 lst2 && pred_eq p1 p2
  | _, _ -> false

type ('lc,'ty) substitution_desc =
  | Skip
  | Affectation of ('lc,'ty) var Utils.non_empty_list * ('lc,'ty) expression
  | Function_Affectation of ('lc,'ty) var * ('lc,'ty) expression Utils.non_empty_list * ('lc,'ty) expression
  | Record_Affectation of ('lc,'ty) var * 'lc lident * ('lc,'ty) expression
  | Pre of ('lc,'ty) predicate * ('lc,'ty) substitution
  | Assert of ('lc,'ty) predicate * ('lc,'ty) substitution
  | Choice of ('lc,'ty) substitution Utils.non_empty_list
  | IfThenElse of (('lc,'ty) predicate * ('lc,'ty) substitution) Utils.non_empty_list * ('lc,'ty) substitution option
  | Select of (('lc,'ty) predicate * ('lc,'ty) substitution) Utils.non_empty_list * ('lc,'ty) substitution option
  | Case of ('lc,'ty) expression * (('lc,'ty) expression * ('lc,'ty) substitution) Utils.non_empty_list * ('lc,'ty) substitution option
  | Any of ('lc,'ty) var Utils.non_empty_list * ('lc,'ty) predicate * ('lc,'ty) substitution
  | Let of ('lc,'ty) var Utils.non_empty_list * (('lc,'ty) var * ('lc,'ty) expression) Utils.non_empty_list * ('lc,'ty) substitution
  | BecomesElt of ('lc,'ty) var Utils.non_empty_list * ('lc,'ty) expression
  | BecomesSuch of ('lc,'ty) var Utils.non_empty_list * ('lc,'ty) predicate
  | Var of ('lc,'ty) var Utils.non_empty_list * ('lc,'ty) substitution
  | CallUp of ('lc,'ty) var list * 'lc lident * ('lc,'ty) expression list
  | While of ('lc,'ty) predicate * ('lc,'ty) substitution * ('lc,'ty) predicate * ('lc,'ty) expression
  | Sequencement of ('lc,'ty) substitution * ('lc,'ty) substitution
  | Parallel of ('lc,'ty) substitution * ('lc,'ty) substitution

and ('lc,'ty) substitution = { sub_loc:'lc; sub_desc:('lc,'ty) substitution_desc }

let rec subst_eq : type a b c d. (a,b) substitution -> (c,d) substitution -> bool = fun s1 s2 ->
  match s1.sub_desc ,s2.sub_desc  with
  | Skip, Skip -> true
  | Affectation (xlst1,e1), Affectation (xlst2,e2) -> Utils.nelist_eq var_eq xlst1 xlst2 && expr_eq e1 e2
  | Function_Affectation (f1,lst1,a1), Function_Affectation (f2,lst2,a2) ->
    var_eq f1 f2 && Utils.nelist_eq expr_eq lst1 lst2 && expr_eq a1 a1
  | Record_Affectation (id1,fd1,e1), Record_Affectation (id2,fd2,e2) ->
    var_eq id1 id2 && ident_eq fd1.lid_str fd2.lid_str && expr_eq e1 e2
  | Pre (p1,s1), Pre (p2,s2) -> pred_eq p1 p2 && subst_eq s1 s2
  | Assert (p1,s1), Assert (p2,s2) -> pred_eq p1 p2 && subst_eq s1 s2
  | Choice lst1, Choice lst2 -> Utils.nelist_eq subst_eq lst1 lst2
  | IfThenElse (lst1,opt1), IfThenElse (lst2,opt2)
  | Select (lst1,opt1), Select (lst2,opt2) ->
    let aux (p1,s1) (p2,s2) = pred_eq p1 p2 && subst_eq s1 s2 in
    Utils.nelist_eq aux lst1 lst2 &&
    begin match opt1, opt2 with
      | None, None -> true
      | Some s1, Some s2 -> subst_eq s1 s2
      | _, _ -> false
    end
  | Case (e1,lst1,opt1), Case (e2,lst2,opt2) ->
    let aux (e1,s1) (e2,s2) = expr_eq e1 e2 && subst_eq s1 s2 in
    expr_eq e1 e2 && Utils.nelist_eq aux lst1 lst2 &&
    begin match opt1, opt2 with
      | None, None -> true
      | Some s1, Some s2 -> subst_eq s1 s2
      | _, _ -> false
    end
  | Any (lst1,p1,s1), Any (lst2,p2,s2) ->
    Utils.nelist_eq var_eq lst1 lst2 && pred_eq p1 p2 && subst_eq s1 s2
  | Let (xlst1,ylst1,s1), Let (xlst2,ylst2,s2) ->
    let aux (v1,e1) (v2,e2) = var_eq v1 v2 && expr_eq e1 e2 in
    Utils.nelist_eq var_eq xlst1 xlst2 &&
    Utils.nelist_eq aux ylst1 ylst2 && subst_eq s1 s2
  | BecomesElt (lst1,e1), BecomesElt (lst2,e2) -> Utils.nelist_eq var_eq lst1 lst2 && expr_eq e1 e2
  | BecomesSuch (lst1,p1), BecomesSuch (lst2,p2) -> Utils.nelist_eq var_eq lst1 lst2 && pred_eq p1 p2
  | Var (lst1,s1), Var (lst2,s2) -> Utils.nelist_eq var_eq lst1 lst2 && subst_eq s1 s2
  | CallUp (xlst1,op1,elst1), CallUp (xlst2,op2,elst2) ->
    Utils.list_eq var_eq xlst1 xlst2 && lident_eq op1 op2 && Utils.list_eq expr_eq elst1 elst2
  | While (p1,s1,q1,e1), While (p2,s2,q2,e2) ->
    pred_eq p1 p2 && subst_eq s1 s2 && pred_eq q1 q2 && expr_eq e1 e2
  | Sequencement (s1,r1), Sequencement (s2,r2) ->
    subst_eq s1 s2 && subst_eq r1 r2
  | Parallel (s1,r1), Parallel (s2,r2) ->
    subst_eq s1 s2 && subst_eq r1 r2
  | _, _ -> false

type ('lc,'ty) operation =
  { op_out:('lc,'ty) var list;
    op_name:'lc lident;
    op_in:('lc,'ty)  var list;
    op_body: ('lc,'ty) substitution }

type ('lc,'ty) machine_instanciation =
  { mi_mch: 'lc lident;
    mi_params: ('lc,'ty) expression list }

type ('lc,'ty) set =
  | Abstract_Set of ('lc,'ty) var
  | Concrete_Set of ('lc,'ty) var * ('lc,'ty) var list

type ('lc,'ty) clause_desc =
  | Constraints of ('lc,'ty) predicate
  | Imports of (('lc,'ty) machine_instanciation) Utils.non_empty_list
  | Sees of 'lc lident Utils.non_empty_list
  | Includes of ('lc,'ty) machine_instanciation Utils.non_empty_list
  | Extends of ('lc,'ty) machine_instanciation Utils.non_empty_list
  | Promotes of 'lc lident Utils.non_empty_list
  | Uses of 'lc lident Utils.non_empty_list
  | Sets of ('lc,'ty) set Utils.non_empty_list
  | Constants of ('lc,'ty) var Utils.non_empty_list
  | Abstract_constants of ('lc,'ty) var Utils.non_empty_list
  | Properties of ('lc,'ty) predicate
  | Concrete_variables of ('lc,'ty) var Utils.non_empty_list
  | Variables of ('lc,'ty) var Utils.non_empty_list
  | Invariant of ('lc,'ty) predicate
  | Assertions of ('lc,'ty) predicate Utils.non_empty_list
  | Initialization of ('lc,'ty) substitution
  | Operations of ('lc,'ty) operation Utils.non_empty_list
  | Local_Operations of ('lc,'ty) operation Utils.non_empty_list
  | Values of (('lc,'ty) var * ('lc,'ty) expression) Utils.non_empty_list

and ('lc,'ty) clause = { cl_loc:'lc; cl_desc:('lc,'ty) clause_desc }

type ('lc,'ty) machine_desc = {
  mch_constraints: ('lc * ('lc,'ty) predicate) option;
  mch_sees: ('lc * 'lc lident Utils.non_empty_list) option;
  mch_includes: ('lc * ('lc,'ty) machine_instanciation Utils.non_empty_list) option;
  mch_promotes: ('lc * 'lc lident Utils.non_empty_list) option;
  mch_extends: ('lc * ('lc,'ty) machine_instanciation Utils.non_empty_list) option;
  mch_uses: ('lc * 'lc lident Utils.non_empty_list) option;
  mch_sets: ('lc * ('lc,'ty) set Utils.non_empty_list) option;
  mch_concrete_constants: ('lc * ('lc,'ty) var Utils.non_empty_list) option;
  mch_abstract_constants: ('lc * ('lc,'ty) var Utils.non_empty_list) option;
  mch_properties: ('lc * ('lc,'ty) predicate) option;
  mch_concrete_variables: ('lc * ('lc,'ty) var Utils.non_empty_list) option;
  mch_abstract_variables: ('lc * ('lc,'ty) var Utils.non_empty_list) option;
  mch_invariant: ('lc * ('lc,'ty) predicate) option;
  mch_assertions: ('lc * ('lc,'ty) predicate Utils.non_empty_list) option;
  mch_initialisation: ('lc * ('lc,'ty) substitution) option;
  mch_operations: ('lc * ('lc,'ty) operation Utils.non_empty_list) option;
}

type ('lc,'ty) refinement_desc = {
  ref_refines: 'lc lident;
  ref_sees: ('lc*'lc lident Utils.non_empty_list) option;
  ref_includes: ('lc*('lc,'ty) machine_instanciation Utils.non_empty_list) option;
  ref_promotes: ('lc*'lc lident Utils.non_empty_list) option;
  ref_extends: ('lc*('lc,'ty) machine_instanciation Utils.non_empty_list) option;
  ref_sets: ('lc*('lc,'ty) set Utils.non_empty_list) option;
  ref_concrete_constants: ('lc*('lc,'ty) var Utils.non_empty_list) option;
  ref_abstract_constants: ('lc*('lc,'ty) var Utils.non_empty_list) option;
  ref_properties: ('lc*('lc,'ty) predicate) option;
  ref_concrete_variables: ('lc*('lc,'ty) var Utils.non_empty_list) option;
  ref_abstract_variables: ('lc*('lc,'ty) var Utils.non_empty_list) option;
  ref_invariant: ('lc*('lc,'ty) predicate) option;
  ref_assertions: ('lc*('lc,'ty) predicate Utils.non_empty_list) option;
  ref_initialisation: ('lc*('lc,'ty) substitution) option;
  ref_operations: ('lc*('lc,'ty) operation Utils.non_empty_list) option;
  ref_local_operations: ('lc*('lc,'ty) operation Utils.non_empty_list) option;
}

type ('lc,'ty) implementation_desc = {
  imp_refines: 'lc lident;
  imp_sees: ('lc*'lc lident Utils.non_empty_list) option;
  imp_imports: ('lc*('lc,'ty) machine_instanciation Utils.non_empty_list) option;
  imp_promotes: ('lc*'lc lident Utils.non_empty_list) option;
  imp_extends: ('lc*('lc,'ty) machine_instanciation Utils.non_empty_list) option;
  imp_sets: ('lc*('lc,'ty) set Utils.non_empty_list) option;
  imp_concrete_constants: ('lc*('lc,'ty) var Utils.non_empty_list) option;
  imp_properties: ('lc*('lc,'ty) predicate) option;
  imp_values: ('lc*(('lc,'ty) var*('lc,'ty) expression) Utils.non_empty_list) option;
  imp_concrete_variables: ('lc*('lc,'ty) var Utils.non_empty_list) option;
  imp_invariant: ('lc*('lc,'ty) predicate) option;
  imp_assertions: ('lc*('lc,'ty) predicate Utils.non_empty_list) option;
  imp_initialisation: ('lc*('lc,'ty) substitution) option;
  imp_operations: ('lc*('lc,'ty) operation Utils.non_empty_list) option;
  imp_local_operations: ('lc*('lc,'ty) operation Utils.non_empty_list) option;
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

let operation_eq : type a b c d. (a,b) operation -> (c,d) operation -> bool = fun op1 op2 ->
  Utils.list_eq var_eq op1.op_out op2.op_out && lident_eq op1.op_name op2.op_name &&
  Utils.list_eq var_eq op1.op_in op2.op_in && subst_eq op1.op_body op2.op_body

let minst_eq (mi1:('lc,'ty) machine_instanciation) (mi2:('lc2,'ty2) machine_instanciation) : bool =
  lident_eq mi1.mi_mch mi2.mi_mch && Utils.list_eq expr_eq mi1.mi_params mi2.mi_params 

let set_eq (s1:('lc,'ty) set) (s2:('lc2,'ty2) set) : bool =
  match s1, s2 with
  | Abstract_Set id1, Abstract_Set id2 -> var_eq id1 id2
  | Concrete_Set (id1,lst1), Concrete_Set (id2,lst2) ->
    var_eq id1 id2 && Utils.list_eq var_eq lst1 lst2
  | _, _ -> false

let clause_eq : type a b c d. (a,b) clause -> (c,d) clause -> bool = fun cl1 cl2 ->
  match cl1.cl_desc, cl2.cl_desc with
  | Constraints p1, Constraints p2 -> pred_eq p1 p2
  | Imports lst1, Imports lst2 -> Utils.nelist_eq minst_eq lst1 lst2
  | Sees lst1, Sees lst2 -> Utils.nelist_eq lident_eq lst1 lst2
  | Includes lst1, Includes lst2 -> Utils.nelist_eq minst_eq lst1 lst2
  | Extends lst1, Extends lst2 -> Utils.nelist_eq minst_eq lst1 lst2
  | Promotes lst1, Promotes lst2 -> Utils.nelist_eq lident_eq lst1 lst2
  | Uses lst1, Uses lst2 -> Utils.nelist_eq lident_eq lst1 lst2
  | Sets lst1, Sets lst2 -> Utils.nelist_eq set_eq lst1 lst2
  | Constants lst1, Constants lst2 -> Utils.nelist_eq var_eq lst1 lst2
  | Abstract_constants lst1, Abstract_constants lst2 -> Utils.nelist_eq var_eq lst1 lst2
  | Properties p1, Properties p2 -> pred_eq p1 p2
  | Concrete_variables lst1, Concrete_variables lst2 -> Utils.nelist_eq var_eq lst1 lst2
  | Variables lst1, Variables lst2 -> Utils.nelist_eq var_eq lst1 lst2
  | Invariant p1, Invariant p2 -> pred_eq p1 p2
  | Assertions p1, Assertions p2 -> Utils.nelist_eq pred_eq p1 p2
  | Initialization s1, Initialization s2 -> subst_eq s1 s2
  | Operations lst1, Operations lst2 -> Utils.nelist_eq operation_eq lst1 lst2
  | Local_Operations lst1, Local_Operations lst2 -> Utils.nelist_eq operation_eq lst1 lst2
  | Values lst1, Values lst2 ->
    let aux (i1,x1) (i2,x2) = var_eq i1 i2 && expr_eq x1 x2 in
    Utils.nelist_eq aux lst1 lst2
  | _, _ -> false

(* ******** *)

let add lst f = function
  | None -> lst
  | Some (l,x) -> { cl_loc=l; cl_desc=f x}::lst

let clist_of_mch (mch:('lc,'ty) machine_desc) : ('lc,'ty) clause list =
  let lst = add []  (fun ops -> Operations ops) mch.mch_operations in
  let lst = add lst (fun x -> Initialization(x)) mch.mch_initialisation in
  let lst = add lst (fun x -> Assertions(x)) mch.mch_assertions in
  let lst = add lst (fun x -> Invariant(x)) mch.mch_invariant in
  let lst = add lst (fun x -> Variables(x)) mch.mch_abstract_variables in
  let lst = add lst (fun x -> Concrete_variables(x)) mch.mch_concrete_variables in
  let lst = add lst (fun x -> Properties(x)) mch.mch_properties in
  let lst = add lst (fun x -> Abstract_constants(x)) mch.mch_abstract_constants in
  let lst = add lst (fun x -> Constants(x)) mch.mch_concrete_constants in
  let lst = add lst (fun x -> Sets(x)) mch.mch_sets in
  let lst = add lst (fun x -> Uses(x)) mch.mch_uses in
  let lst = add lst (fun x -> Extends(x)) mch.mch_extends in
  let lst = add lst (fun x -> Promotes(x)) mch.mch_promotes in
  let lst = add lst (fun x -> Includes(x)) mch.mch_includes in
  let lst = add lst (fun x -> Sees(x)) mch.mch_sees in
  let lst = add lst (fun x -> Constraints(x)) mch.mch_constraints in
  lst

let check_none_exn : type a b c. a -> b option -> c -> (c,a Error.t) result =
  fun lc opt res ->
    match opt with
    | None -> Ok res
    | Some _ -> Error { Error.err_loc=lc; err_txt="This clause is defined twice." }

let add_clause_mch (co:('lc,'ty) machine_desc) (cl:('lc,'ty) clause) : (('lc,'ty) machine_desc,'lc Error.t) result =
  match cl.cl_desc with
  | Sees lst -> check_none_exn cl.cl_loc co.mch_sees { co with mch_sees = Some(cl.cl_loc,lst) }
  | Sets lst -> check_none_exn cl.cl_loc co.mch_sets { co with mch_sets = Some(cl.cl_loc,lst) }
  | Constants lst -> check_none_exn cl.cl_loc co.mch_concrete_constants { co with mch_concrete_constants = Some(cl.cl_loc,lst) }
  | Abstract_constants lst -> check_none_exn cl.cl_loc co.mch_abstract_constants { co with mch_abstract_constants = Some(cl.cl_loc,lst) }
  | Properties p -> check_none_exn cl.cl_loc co.mch_properties { co with mch_properties = Some (cl.cl_loc,p) }
  | Concrete_variables lst -> check_none_exn cl.cl_loc co.mch_concrete_variables { co with mch_concrete_variables = Some(cl.cl_loc,lst) }
  | Variables lst -> check_none_exn cl.cl_loc co.mch_abstract_variables { co with mch_abstract_variables = Some(cl.cl_loc,lst) }
  | Invariant p -> check_none_exn cl.cl_loc co.mch_invariant { co with mch_invariant = Some (cl.cl_loc,p) }
  | Assertions lst -> check_none_exn cl.cl_loc co.mch_assertions { co with mch_assertions = Some (cl.cl_loc,lst) }
  | Initialization p -> check_none_exn cl.cl_loc co.mch_initialisation { co with mch_initialisation = Some (cl.cl_loc,p) }
  | Operations lst -> check_none_exn cl.cl_loc co.mch_operations { co with mch_operations = Some(cl.cl_loc,lst) }
  | Values _ -> Error { Error.err_loc=cl.cl_loc; err_txt="The clause VALUES is not allowed in abstract machines." }
  | Local_Operations _ -> Error { Error.err_loc=cl.cl_loc; err_txt="The clause LOCAL_OPERATIONS is not allowed in abstract machines." }
  | Promotes lst -> check_none_exn cl.cl_loc co.mch_promotes { co with mch_promotes = Some(cl.cl_loc,lst) }
  | Imports _ -> Error { Error.err_loc=cl.cl_loc; err_txt="The clause IMPORTS is not allowed in abstract machines."}
  | Constraints lst -> check_none_exn cl.cl_loc co.mch_constraints { co with mch_constraints = Some(cl.cl_loc,lst) }
  | Includes lst -> check_none_exn cl.cl_loc co.mch_includes { co with mch_includes = Some(cl.cl_loc,lst) }
  | Extends lst -> check_none_exn cl.cl_loc co.mch_extends { co with mch_extends = Some(cl.cl_loc,lst) }
  | Uses lst -> check_none_exn cl.cl_loc co.mch_uses { co with mch_uses = Some(cl.cl_loc,lst) }

let mk_machine : type a b. a lident -> (a,b) var list -> (a,b) clause list -> ((a,b) component,a Error.t) result =
  fun name co_parameters clauses ->
    let mch_desc =
      { mch_sees=None;
        mch_sets=None;
        mch_uses=None;
        mch_promotes=None;
        mch_includes=None;
        mch_extends=None;
        mch_constraints=None;
        mch_concrete_constants=None;
        mch_abstract_constants=None;
        mch_properties=None;
        mch_concrete_variables=None;
        mch_abstract_variables=None;
        mch_invariant=None;
        mch_assertions=None;
        mch_initialisation=None;
        mch_operations=None; }
    in
    match Error.fold_left add_clause_mch mch_desc clauses with
    | Ok mch_desc -> Ok { co_loc=name.lid_loc; co_name=name.lid_str;
                          co_parameters; co_desc=Machine mch_desc }
    | Error err -> Error err

(* ******** *)

let clist_of_ref (ref:('lc,'ty) refinement_desc) : ('lc,'ty) clause list =
  let lst = add []  (fun x -> Operations(x)) ref.ref_operations in
  let lst = add lst (fun x -> Local_Operations(x)) ref.ref_local_operations in
  let lst = add lst (fun x -> Initialization(x)) ref.ref_initialisation in
  let lst = add lst (fun x -> Assertions(x)) ref.ref_assertions in
  let lst = add lst (fun x -> Invariant(x)) ref.ref_invariant in
  let lst = add lst (fun x -> Variables(x)) ref.ref_abstract_variables in
  let lst = add lst (fun x -> Concrete_variables(x)) ref.ref_concrete_variables in
  let lst = add lst (fun x -> Properties(x)) ref.ref_properties in
  let lst = add lst (fun x -> Abstract_constants(x)) ref.ref_abstract_constants in
  let lst = add lst (fun x -> Constants(x)) ref.ref_concrete_constants in
  let lst = add lst (fun x -> Sets(x)) ref.ref_sets in
  let lst = add lst (fun x -> Extends(x)) ref.ref_extends in
  let lst = add lst (fun x -> Promotes(x)) ref.ref_promotes in
  let lst = add lst (fun x -> Includes(x)) ref.ref_includes in
  let lst = add lst (fun x -> Sees(x)) ref.ref_sees in
  lst

let add_clause_ref co cl =
  match cl.cl_desc with
  | Sees lst -> check_none_exn cl.cl_loc co.ref_sees { co with ref_sees = Some(cl.cl_loc,lst) }
  | Sets lst -> check_none_exn cl.cl_loc co.ref_sets { co with ref_sets = Some(cl.cl_loc,lst) }
  | Constants lst -> check_none_exn cl.cl_loc co.ref_concrete_constants { co with ref_concrete_constants = Some(cl.cl_loc,lst) }
  | Properties p -> check_none_exn cl.cl_loc co.ref_properties { co with ref_properties = Some (cl.cl_loc,p) }
  | Concrete_variables lst -> check_none_exn cl.cl_loc co.ref_concrete_variables { co with ref_concrete_variables = Some(cl.cl_loc,lst) }
  | Invariant p -> check_none_exn cl.cl_loc co.ref_invariant { co with ref_invariant = Some (cl.cl_loc,p) }
  | Assertions lst -> check_none_exn cl.cl_loc co.ref_assertions { co with ref_assertions = Some (cl.cl_loc,lst) }
  | Initialization p -> check_none_exn cl.cl_loc co.ref_initialisation { co with ref_initialisation = Some (cl.cl_loc,p) }
  | Operations lst -> check_none_exn cl.cl_loc co.ref_operations { co with ref_operations = Some(cl.cl_loc,lst) }
  | Values lst -> Error { Error.err_loc=cl.cl_loc; err_txt="The clause VALUES is not allowed in refinements."}
  | Local_Operations lst -> check_none_exn cl.cl_loc co.ref_local_operations { co with ref_local_operations = Some(cl.cl_loc,lst) }
  | Promotes lst -> check_none_exn cl.cl_loc co.ref_promotes { co with ref_promotes = Some(cl.cl_loc,lst) }
  | Imports lst -> Error { Error.err_loc=cl.cl_loc; err_txt="The clause IMPORTS is not allowed in refinements."}
  | Abstract_constants lst -> check_none_exn cl.cl_loc co.ref_abstract_constants { co with ref_abstract_constants = Some(cl.cl_loc,lst) }
  | Variables lst -> check_none_exn cl.cl_loc co.ref_abstract_variables { co with ref_abstract_variables = Some(cl.cl_loc,lst) }
  | Constraints _ -> Error { Error.err_loc=cl.cl_loc; err_txt="The clause CONSTRAINTS is not allowed in refinements."}
  | Includes lst -> check_none_exn cl.cl_loc co.ref_includes { co with ref_includes = Some(cl.cl_loc,lst) }
  | Extends lst -> check_none_exn cl.cl_loc co.ref_extends { co with ref_extends = Some(cl.cl_loc,lst) }
  | Uses lst -> Error { Error.err_loc=cl.cl_loc; err_txt="The clause USES is not allowed in refinements."}

let mk_refinement name co_parameters refines clauses =
  let ref_desc =
    { ref_refines=refines;
      ref_sees=None;
      ref_sets=None;
      ref_promotes=None;
      ref_includes=None;
      ref_extends=None;
      ref_concrete_constants=None;
      ref_abstract_constants=None;
      ref_properties=None;
      ref_concrete_variables=None;
      ref_abstract_variables=None;
      ref_invariant=None;
      ref_assertions=None;
      ref_initialisation=None;
      ref_local_operations=None;
      ref_operations=None; }
  in
  match Error.fold_left add_clause_ref ref_desc clauses with
  | Ok ref_desc -> Ok { co_loc=name.lid_loc; co_name=name.lid_str;
                        co_parameters; co_desc=Refinement ref_desc }
  | Error _ as err -> err

(* ******** *)

let clist_of_imp (imp:('lc,'ty) implementation_desc) : ('lc,'ty) clause list =
  let lst = add []  (fun x -> Operations(x)) imp.imp_operations in
  let lst = add lst (fun x -> Local_Operations(x)) imp.imp_local_operations in
  let lst = add lst (fun x -> Initialization(x)) imp.imp_initialisation in
  let lst = add lst (fun x -> Assertions(x)) imp.imp_assertions in
  let lst = add lst (fun x -> Invariant(x)) imp.imp_invariant in
  let lst = add lst (fun x -> Concrete_variables(x)) imp.imp_concrete_variables in
  let lst = add lst (fun x -> Values(x)) imp.imp_values in
  let lst = add lst (fun x -> Properties(x)) imp.imp_properties in
  let lst = add lst (fun x -> Constants(x)) imp.imp_concrete_constants in
  let lst = add lst (fun x -> Sets(x)) imp.imp_sets in
  let lst = add lst (fun x -> Extends(x)) imp.imp_extends in
  let lst = add lst (fun x -> Promotes(x)) imp.imp_promotes in
  let lst = add lst (fun x -> Imports(x)) imp.imp_imports in
  let lst = add lst (fun x -> Sees(x)) imp.imp_sees in
  lst

let add_clause_imp co cl =
  match cl.cl_desc with
  | Sees lst -> check_none_exn cl.cl_loc co.imp_sees { co with imp_sees = Some(cl.cl_loc,lst) }
  | Sets lst -> check_none_exn cl.cl_loc co.imp_sets { co with imp_sets = Some(cl.cl_loc,lst) }
  | Constants lst -> check_none_exn cl.cl_loc co.imp_concrete_constants { co with imp_concrete_constants = Some(cl.cl_loc,lst) }
  | Properties p -> check_none_exn cl.cl_loc co.imp_properties { co with imp_properties = Some (cl.cl_loc,p) }
  | Concrete_variables lst -> check_none_exn cl.cl_loc co.imp_concrete_variables { co with imp_concrete_variables = Some(cl.cl_loc,lst) }
  | Invariant p -> check_none_exn cl.cl_loc co.imp_invariant { co with imp_invariant = Some (cl.cl_loc,p) }
  | Assertions lst -> check_none_exn cl.cl_loc co.imp_assertions { co with imp_assertions = Some (cl.cl_loc,lst) }
  | Initialization p -> check_none_exn cl.cl_loc co.imp_initialisation { co with imp_initialisation = Some (cl.cl_loc,p) }
  | Operations lst -> check_none_exn cl.cl_loc co.imp_operations { co with imp_operations = Some(cl.cl_loc,lst) }
  | Values lst -> check_none_exn cl.cl_loc co.imp_values { co with imp_values = Some(cl.cl_loc,lst) }
  | Local_Operations lst -> check_none_exn cl.cl_loc co.imp_local_operations { co with imp_local_operations = Some(cl.cl_loc,lst) }
  | Promotes lst -> check_none_exn cl.cl_loc co.imp_promotes { co with imp_promotes = Some(cl.cl_loc,lst) }
  | Imports lst -> check_none_exn cl.cl_loc co.imp_imports { co with imp_imports = Some(cl.cl_loc,lst) }
  | Abstract_constants lst -> Error { Error.err_loc=cl.cl_loc; err_txt="The clause ABSTRACT_CONSTANTS is not allowed in implementations."}
  | Variables lst -> Error { Error.err_loc=cl.cl_loc; err_txt="The clause VARIABLES is not allowed in implementations."}
  | Constraints _ -> Error { Error.err_loc=cl.cl_loc; err_txt="The clause CONSTRAINTS is not allowed in implementation."}
  | Includes lst -> Error { Error.err_loc=cl.cl_loc; err_txt="The clause INCLUDES is not allowed in implementations."}
  | Extends lst -> check_none_exn cl.cl_loc co.imp_extends { co with imp_extends = Some(cl.cl_loc,lst) }
  | Uses lst -> Error { Error.err_loc=cl.cl_loc; err_txt="The clause USES is not allowed in implementations."}

let mk_implementation name co_parameters refines clauses =
  let imp_desc =
    { imp_refines=refines;
      imp_sees=None;
      imp_sets=None;
      imp_values=None;
      imp_imports=None;
      imp_promotes=None;
      imp_concrete_constants=None;
      imp_properties=None;
      imp_concrete_variables=None;
      imp_invariant=None;
      imp_assertions=None;
      imp_extends=None;
      imp_initialisation=None;
      imp_local_operations=None;
      imp_operations=None; }
  in
  match Error.fold_left add_clause_imp imp_desc clauses with
  | Ok imp_desc -> Ok { co_loc=name.lid_loc; co_name=name.lid_str;
                        co_parameters; co_desc=Implementation imp_desc }
  | Error _ as err -> err

(* ******** *)

let component_eq: type a b c d. (a,b) component -> (c,d) component -> bool = fun c1 c2 ->
  ident_eq c1.co_name c2.co_name && Utils.list_eq var_eq c1.co_parameters c2.co_parameters &&
  match c1.co_desc, c2.co_desc with
  | Machine mch1, Machine mch2 -> Utils.list_eq clause_eq (clist_of_mch mch1) (clist_of_mch mch2)
  | Refinement ref1, Refinement ref2 -> Utils.list_eq clause_eq (clist_of_ref ref1) (clist_of_ref ref2)
  | Implementation imp1, Implementation imp2 -> Utils.list_eq clause_eq (clist_of_imp imp1) (clist_of_imp imp2)
  | _, _ -> false

let get_clauses co =
  match co.co_desc with
  | Machine mch -> clist_of_mch mch
  | Refinement ref -> clist_of_ref ref
  | Implementation imp -> clist_of_imp imp

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
