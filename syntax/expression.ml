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

let builtin_eq b1 b2 =
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
    Inverse_Relation ]

type p_builtin =
  | Btrue
  | Bfalse

type expr_binder = Sum | Prod | Q_Union | Q_Intersection | Lambda

type c_or_m = Maplet | Comma | Infix

type 'lc ident = 'lc * string
type u_ident = loc ident

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

let expr_loc : u_expr -> loc = function
  | Ident (_,(l,_)) | Dollar (_,(l,_)) | Builtin (l,_,_) | Pbool (l,_,_)
  | Application (l,_,_,_) | Couple (l,_,_,_,_) | Sequence (l,_,_) | Extension (l,_,_)
  | Comprehension (l,_,_,_) | Binder (l,_,_,_,_,_) | Record_Field_Access (l,_,_,_)
  | Record_Type (l,_,_) | Record (l,_,_) -> l

let pred_loc : u_pred -> loc = function
  | P_Builtin (l,_) | Binary_Prop (l,_,_,_) | Binary_Pred (l,_,_,_)
  | Negation (l,_) | Universal_Q (l,_,_)
  | Existential_Q (l,_,_) -> l

let ident_eq (_,s1:'lc ident) (_,s2:'lc2 ident) : bool = String.equal s1 s2

let ident_list_eq (lst1:'lc ident list) (lst2:'lc2 ident list) : bool =
  try List.for_all2 ident_eq lst1 lst2
  with Invalid_argument _ -> false

let ident_nelist_eq (hd1,tl1:'lc ident non_empty_list) (hd2,tl2:'lc2 ident non_empty_list) : bool =
  ident_list_eq (hd1::tl1) (hd2::tl2)

let aux_list_eq (hd1,tl1:('ty*'lc ident) non_empty_list) (hd2,tl2:('ty2*'lc2 ident) non_empty_list) : bool =
  try List.for_all2 (fun (_,s1) (_,s2) -> ident_eq s1 s2) (hd1::tl1) (hd2::tl2)
  with Invalid_argument _ -> false

let rec expr_eq : type a b c d. (a,b) expression -> (c,d) expression -> bool = fun e1 e2 ->
  match e1, e2 with
  | Ident (_,v1), Ident (_,v2) | Dollar (_,v1), Dollar (_,v2) -> ident_eq v1 v2
  | Builtin (_,_,b1), Builtin (_,_,b2) -> builtin_eq b1 b2
  | Pbool (_,_,p1), Pbool (_,_,p2) -> pred_eq p1 p2
  | Application (_,_,f1,a1), Application (_,_,f2,a2) -> expr_eq f1 f2 && expr_eq a1 a2
  | Couple (_,_,_,x1,y1), Couple (_,_,_,x2,y2) -> expr_eq x1 x2 && expr_eq y1 y2
  | Sequence (_,_,(hd1,tl1)), Sequence (_,_,(hd2,tl2)) ->
    expr_list_eq (hd1::tl1) (hd2::tl2)
  | Extension (_,_,(hd1,tl1)), Extension (_,_,(hd2,tl2)) ->
    expr_list_eq (hd1::tl1) (hd2::tl2)
  | Comprehension (_,_,lst1,p1), Comprehension(_,_,lst2,p2) ->
    aux_list_eq lst1 lst2 && pred_eq p1 p2
  | Binder (_,_,bi1,lst1,p1,e1), Binder (_,_,bi2,lst2,p2,e2) ->
    bi1 = bi2 && aux_list_eq lst1 lst2 && pred_eq p1 p2 && expr_eq e1 e2
  | Record_Field_Access (_,_,e1,f1), Record_Field_Access(_,_,e2,f2) ->
    expr_eq e1 e2 && ident_eq f1 f2
  | Record (_,_,(hd1,tl1)), Record (_,_,(hd2,tl2))
  | Record_Type (_,_,(hd1,tl1)), Record_Type(_,_,(hd2,tl2)) ->
    begin
      let aux (id1,e1) (id2,e2) = ident_eq id1 id2 && expr_eq e1 e2 in
      try List.for_all2 aux (hd1::tl1) (hd2::tl2)
      with Invalid_argument _ -> false
    end
  | _, _ -> false

and expr_list_eq : type a b c d. ((a,b) expression list) -> ((c,d) expression list) -> bool = fun l1 l2 ->
  try List.for_all2 expr_eq l1 l2
  with Invalid_argument _ -> false

and pred_eq : type a b c d. (a,b) predicate -> (c,d) predicate -> bool = fun p1 p2 ->
  match p1, p2 with
  | P_Builtin (_,b1), P_Builtin (_,b2) -> b1 = b2
  | Binary_Prop (_,b1,p1,q1), Binary_Prop (_,b2,p2,q2) ->
    b1 = b2 && pred_eq p1 p2 && pred_eq q1 q2
  | Binary_Pred (_,b1,p1,q1), Binary_Pred (_,b2,p2,q2) ->
    b1 = b2 && expr_eq p1 p2 && expr_eq q1 q2
  | Negation (_,p1), Negation (_,p2) -> pred_eq p1 p2
  | Universal_Q (_,lst1,p1), Universal_Q (_,lst2,p2) ->
    aux_list_eq lst1 lst2 && pred_eq p1 p2
  | Existential_Q (_,lst1,p1), Existential_Q (_,lst2,p2) ->
    aux_list_eq lst1 lst2 && pred_eq p1 p2
  | _, _ -> false

(* Print *)

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
  | Integer i -> string_of_int i
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
  | Successor -> "Succ"
  | Predecessor -> "Pred"
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

  | Image -> "image"
  | Unary_Minus -> "unary_minus"
  | Inverse_Relation -> "inverse"

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

let rec mk_conjonction l p1 p2 =
  match p1 with
  | Binary_Prop (l2,Conjonction,q1,q2) ->
    mk_conjonction l2 q1 (mk_conjonction l q2 p2)
  | _ -> Binary_Prop (l,Conjonction,p1,p2)

let rec mk_disjunction l p1 p2 =
  match p1 with
  | Binary_Prop (l2,Disjonction,q1,q2) ->
    mk_disjunction l2 q1 (mk_disjunction l q2 p2)
  | _ -> Binary_Prop (l,Disjonction,p1,p2)
