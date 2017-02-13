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

type p_builtin =
  | Btrue
  | Bfalse

type expr_binder = Sum | Prod | Q_Union | Q_Intersection | Lambda

type c_or_m = Maplet | Comma | Infix

type expression =
  | Ident of ident
  | Dollar of ident
  | Builtin of loc*e_builtin
  | Pbool of loc*predicate
  | Parentheses of loc*expression
  | Application of loc*expression*expression
  | Couple of loc*c_or_m*expression*expression
  | Sequence of loc*expression non_empty_list
  | Extension of loc*expression non_empty_list
  | Comprehension of loc*ident non_empty_list * predicate
  | Binder of loc*expr_binder*ident non_empty_list*predicate*expression
  | Record_Field_Access of loc*expression*ident
  | Record of loc*(ident option*expression) non_empty_list
  | Record_Type of loc*(ident*expression) non_empty_list

and predicate =
  | P_Ident of ident
  | P_Builtin of loc*p_builtin
  | Binary_Prop of loc*prop_bop*predicate*predicate
  | Binary_Pred of loc*pred_bop*expression*expression
  | Negation of loc*predicate
  | Pparentheses of loc*predicate
  | Universal_Q of loc*ident non_empty_list*predicate
  | Existential_Q of loc*ident non_empty_list*predicate

val expr_loc : expression -> loc
val pred_loc : predicate -> loc

val pp_expr : expression -> unit
val pp_pred : predicate -> unit
