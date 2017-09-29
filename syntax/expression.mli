open Utils

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

val builtin_to_string : e_builtin -> string

val expr_constants : e_builtin list
val expr_infix_ops: e_builtin list
val expr_prefix_postfix_ops: e_builtin list

type p_builtin =
  | Btrue
  | Bfalse

type expr_binder = Sum | Prod | Q_Union | Q_Intersection | Lambda

val binder_to_string : expr_binder -> string

type c_or_m = Maplet | Comma | Infix

val pred_bop_to_string : pred_bop -> string
val prop_bop_to_string : prop_bop -> string

type ('lc,'ty) expression =
  | Ident of 'ty * 'lc ident
  | Dollar of 'ty * 'lc ident
  | Builtin of 'lc * 'ty * e_builtin
  | Pbool of 'lc * 'ty * ('lc,'ty) predicate
  | Application of 'lc * 'ty * ('lc,'ty) expression * ('lc,'ty) expression
  | Couple of 'lc * 'ty * c_or_m  * ('lc,'ty) expression * ('lc,'ty) expression
  | Sequence of 'lc * 'ty * (('lc,'ty) expression) non_empty_list
  | Extension of 'lc * 'ty * (('lc,'ty) expression) non_empty_list
  | Comprehension of 'lc * 'ty * ('ty*'lc ident) non_empty_list * ('lc,'ty) predicate
  | Binder of 'lc * 'ty * expr_binder * ('ty*'lc ident) non_empty_list * ('lc,'ty) predicate * ('lc,'ty) expression
  | Record_Field_Access of 'lc * 'ty * ('lc,'ty) expression * 'lc ident
  | Record of 'lc * 'ty * ('lc ident * ('lc,'ty) expression) non_empty_list
  | Record_Type of 'lc * 'ty * ('lc ident * ('lc,'ty) expression) non_empty_list

and ('lc,'ty) predicate =
  | P_Builtin of 'lc * p_builtin
  | Binary_Prop of 'lc * prop_bop * ('lc,'ty) predicate * ('lc,'ty) predicate
  | Binary_Pred of 'lc * pred_bop * ('lc,'ty) expression * ('lc,'ty) expression
  | Negation of 'lc * ('lc,'ty) predicate
  | Universal_Q of 'lc * ('ty*'lc ident) non_empty_list * ('lc,'ty) predicate
  | Existential_Q of 'lc * ('ty*'lc ident) non_empty_list * ('lc,'ty) predicate

type u_expr = (loc,bool) expression
type u_pred = (loc,bool) predicate

val expr_loc : u_expr -> loc
val pred_loc : u_pred -> loc

val expr_eq : ('lc,'ty) expression -> ('lc2,'ty2) expression -> bool
val expr_list_eq : ('lc,'ty) expression list -> ('lc2,'ty2) expression list -> bool
val pred_eq : ('lc,'ty) predicate -> ('lc2,'ty2) predicate -> bool

val ident_eq : 'lc ident -> 'lc2 ident -> bool
val ident_list_eq : 'lc ident list -> 'lc2 ident list -> bool
val ident_nelist_eq : 'lc ident non_empty_list -> 'lc2 ident non_empty_list -> bool
