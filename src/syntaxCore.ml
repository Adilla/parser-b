type inclusion = Not_Strict | Strict | Non_Inclusion | Non_Strict_Inclusion
[@@deriving eq]

type inequality = Smaller_or_Equal | Strictly_Smaller | Greater_or_Equal | Strictly_Greater
[@@deriving eq]

type prop_bop = Conjonction | Disjonction | Implication | Equivalence
[@@deriving eq]

type pred_bop = Equality | Disequality | Membership | Non_Membership
             | Inclusion of inclusion | Inequality of inequality
[@@deriving eq]

type power_set = Full | Non_Empty | Finite | Finite_Non_Empty
[@@deriving eq]

type stype = All_Seq | Non_Empty_Seq | Injective_Seq | Injective_Non_Empty_Seq | Permutations
[@@deriving eq]

type ftype =
  | Partial_Functions | Total_Functions | Partial_Injections | Total_Injections
  | Partial_Surjections | Total_Surjections | Bijections
[@@deriving eq]

type e_builtin_0 =
  | Integer of Int32.t | String of string | MaxInt | MinInt | INTEGER | NATURAL
  | NATURAL1 | INT | NAT | NAT1 | STRINGS | BOOLEANS | Empty_Set | Empty_Seq
  | TRUE | FALSE
[@@deriving eq]

type e_builtin_1 =
  | Successor | Predecessor
  | Cardinal | Power_Set of power_set | Identity_Relation | Inverse_Relation
  | Closure | Transitive_Closure | Domain | Range | Fnc | Rel
  | Sequence_Set of stype | Size | First | Last | Front | Tail | Reverse
  | G_Union | G_Intersection | G_Concatenation | Unary_Minus | Max | Min
  | Tree | Btree | Const | Top | Sons | Prefix | Postfix | SizeT | Mirror
  | Rank | Father | Son | Subtree | Arity | Bin | Left | Right | Infix
[@@deriving eq]

type t_couple = Mapplet | Comma
[@@deriving eq]

type e_builtin_2 =
  | Product | Difference | Addition | Division | Modulo | Power | Interval
  | Union | Intersection | Relations | First_Projection | Second_Projection
  | Composition | Direct_Product | Parallel_Product | Iteration | Image
  | Domain_Restriction | Domain_Soustraction | Codomain_Restriction
  | Codomain_Soustraction | Surcharge | Functions of ftype | Concatenation | Head_Insertion
  | Tail_Insertion | Head_Restriction | Tail_Restriction | Application | Couple of t_couple
[@@deriving eq]

type p_builtin = Btrue | Bfalse
[@@deriving eq]

type expr_binder = Sum | Prod | Q_Union | Q_Intersection | Lambda
[@@deriving eq]

type c_or_m = Maplet | Comma | Infix
[@@deriving eq]

type lident = { lid_loc:Utils.loc; lid_str:string }
let equal_lident x y = String.equal x.lid_str y.lid_str

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

let builtin0_to_string : e_builtin_0 -> string = function
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

let builtin1_to_string : e_builtin_1 -> string = function
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
  | Unary_Minus -> "-"
  | Inverse_Relation -> "~"
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

let builtin2_to_string : e_builtin_2 -> string = function
  | First_Projection -> "prj1"
  | Second_Projection -> "prj2"
  | Iteration -> "iterate"
  | Image -> ".[.]"
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
  | Application -> ".(.)"
  | Couple Mapplet -> "|->"
  | Couple Comma -> ","
  
let binder_to_string = function
  | Sum -> "SIGMA"
  | Prod -> "PI"
  | Q_Union -> "UNION"
  | Q_Intersection -> "INTER"
  | Lambda -> "%"
