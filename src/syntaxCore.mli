(** Abstract Syntax Trees*)
type inclusion = Not_Strict | Strict | Non_Inclusion | Non_Strict_Inclusion
[@@deriving eq]

type inequality = Smaller_or_Equal | Strictly_Smaller | Greater_or_Equal | Strictly_Greater
[@@deriving eq]

type prop_bop = Conjonction | Disjonction | Implication | Equivalence
[@@deriving eq]

val prop_bop_to_string : prop_bop -> string

type pred_bop = Equality | Disequality | Membership | Non_Membership
             | Inclusion of inclusion | Inequality of inequality
[@@deriving eq]

val pred_bop_to_string : pred_bop -> string

type power_set = Full | Non_Empty | Finite | Finite_Non_Empty
[@@deriving eq]

type stype = All_Seq | Non_Empty_Seq | Injective_Seq | Injective_Non_Empty_Seq | Permutations
[@@deriving eq]

type ftype =
  | Partial_Functions | Total_Functions | Partial_Injections | Total_Injections
  | Partial_Surjections | Total_Surjections | Bijections
[@@deriving eq]

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
[@@deriving eq]

  (** Builtin constants*)

val builtin_to_string : e_builtin -> string

type p_builtin = Btrue | Bfalse
[@@deriving eq]

val expr_constants : e_builtin list
val expr_infix_ops: e_builtin list
val expr_prefix_postfix_ops: e_builtin list

type expr_binder = Sum | Prod | Q_Union | Q_Intersection | Lambda
[@@deriving eq]

val binder_to_string : expr_binder -> string

type c_or_m = Maplet | Comma | Infix
[@@deriving eq]

type lident = { lid_loc:Utils.loc; lid_str:string }
[@@deriving eq]
