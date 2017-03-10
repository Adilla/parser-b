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

type expression =
  | Ident of ident (* Free or Bound variable *)
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
  | Record of loc*(ident*expression) non_empty_list
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

let expr_loc : expression -> loc = function
  | Ident id | Dollar id -> fst id
  | Builtin (l,_) | Pbool (l,_) | Parentheses (l,_) | Application (l,_,_)
  | Couple (l,_,_,_) | Sequence (l,_) | Extension (l,_) | Comprehension (l,_,_)
  | Binder (l,_,_,_,_) | Record_Field_Access (l,_,_) | Record_Type (l,_)
  | Record (l,_) -> l

let pred_loc : predicate -> loc = function
  | P_Ident id -> fst id
  | P_Builtin (l,_) | Binary_Prop (l,_,_,_) | Binary_Pred (l,_,_,_)
  | Negation (l,_) | Pparentheses (l,_) | Universal_Q (l,_,_)
  | Existential_Q (l,_,_) -> l

let rec expr_eq e1 e2 : bool =
  match e1, e2 with
  | Parentheses (_,e), _ -> expr_eq e e2
  | _, Parentheses (_,e) -> expr_eq e1 e
  | Ident id1, Ident id2 -> ident_eq id1 id2
  | Dollar id1, Dollar id2 -> ident_eq id1 id2
  | Builtin (_,b1), Builtin (_,b2) -> builtin_eq b1 b2
  | Pbool (_,p1), Pbool (_,p2) -> pred_eq p1 p2
  | Application (_,f1,a1), Application (_,f2,a2) ->
    expr_eq f1 f2 && expr_eq a1 a2
  | Couple (_,_,x1,y1), Couple (_,_,x2,y2) -> expr_eq x1 x2 && expr_eq y1 y2
  | Sequence (_,(hd1,tl1)), Sequence (_,(hd2,tl2)) ->
    expr_list_eq (hd1::tl1) (hd2::tl2)
  | Extension (_,(hd1,tl1)), Extension (_,(hd2,tl2)) ->
    expr_list_eq (hd1::tl1) (hd2::tl2)
  | Comprehension (_,(x1,lst1),p1), Comprehension(_,(x2,lst2),p2) ->
    ident_list_eq (x1::lst1) (x2::lst2) && pred_eq p1 p2
  | Binder (_,bi1,(x1,lst1),p1,e1), Binder (_,bi2,(x2,lst2),p2,e2) ->
    bi1 = bi2 && ident_list_eq (x1::lst1) (x2::lst2) &&
    pred_eq p1 p2 && expr_eq e1 e2
  | Record_Field_Access (_,e1,id1), Record_Field_Access(_,e2,id2) ->
    expr_eq e1 e2 && ident_eq id1 id2
  | Record (_,(hd1,tl1)), Record (_,(hd2,tl2))
  | Record_Type (_,(hd1,tl1)), Record_Type(_,(hd2,tl2)) ->
    begin
      let aux (id1,e1) (id2,e2) = ident_eq id1 id2 && expr_eq e1 e2 in
      try List.for_all2 aux (hd1::tl1) (hd2::tl2)
      with Invalid_argument _ -> false
    end
  | _, _ -> false

and expr_list_eq l1 l2 =
  try List.for_all2 expr_eq l1 l2
  with Invalid_argument _ -> false

and pred_eq p1 p2 : bool =
  match p1, p2 with
  | Pparentheses (_,p), _ -> pred_eq p p2
  | _, Pparentheses (_,p) -> pred_eq p1 p
  | P_Ident id1, P_Ident id2 -> ident_eq id1 id2
  | P_Builtin (_,b1), P_Builtin (_,b2) -> b1 = b2
  | Binary_Prop (_,b1,p1,q1), Binary_Prop (_,b2,p2,q2) ->
    b1 = b2 && pred_eq p1 p2 && pred_eq q1 q2
  | Binary_Pred (_,b1,p1,q1), Binary_Pred (_,b2,p2,q2) ->
    b1 = b2 && expr_eq p1 p2 && expr_eq q1 q2
  | Negation (_,p1), Negation (_,p2) -> pred_eq p1 p2
  | Universal_Q (_,(x1,lst1),p1), Universal_Q (_,(x2,lst2),p2) ->
    ident_list_eq (x1::lst1) (x2::lst2) && pred_eq p1 p2
  | Existential_Q (_,(x1,lst1),p1), Existential_Q (_,(x2,lst2),p2) ->
    ident_list_eq (x1::lst1) (x2::lst2) && pred_eq p1 p2
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

(* Remove parentheses *)
let rec norm_expr : expression -> expression = function
  | Ident _ | Dollar _ | Builtin _ as e -> e
  | Pbool (l,p) -> Pbool (l,norm_pred p)
  | Parentheses (_,e) -> norm_expr e
  | Application (l,f,a) -> Application (l,norm_expr f,norm_expr a)
  | Comprehension (l,xlst,p) -> Comprehension (l,xlst,norm_pred p)
  | Binder (l,bi,xlst,p,e) -> Binder (l,bi,xlst,norm_pred p,norm_expr e)
  | Sequence (l,(e,lst)) -> Sequence (l,(norm_expr e,List.map norm_expr lst))
  | Extension (l,(e,lst)) -> Extension (l,(norm_expr e,List.map norm_expr lst))
  | Couple (l,cm,e1,e2) -> Couple (l,cm,norm_expr e1,norm_expr e2)
  | Record_Field_Access (l,e,id) -> Record_Field_Access (l,norm_expr e,id)
  | Record (l,(f,lst)) ->
    let aux (id,e) = (id,norm_expr e) in
    Record (l,(aux f,List.map aux lst))
  | Record_Type (l,(f,lst)) ->
    let aux (id,e) = (id,norm_expr e) in
    Record_Type (l,(aux f,List.map aux lst))

(* Remove parentheses and flatten conjonctions and disjonctions *)
and norm_pred : predicate -> predicate = function
  | P_Ident _ | P_Builtin _ as p -> p
  | Binary_Prop (l,Conjonction,p1,p2) ->
    mk_conjonction l (norm_pred p1) (norm_pred p2)
  | Binary_Prop (l,Disjonction,p1,p2) ->
    mk_disjunction l (norm_pred p1) (norm_pred p2)
  | Binary_Prop (l,bop,p1,p2) -> Binary_Prop (l,bop,norm_pred p1,norm_pred p2)
  | Binary_Pred (l,bop,e1,e2) -> Binary_Pred (l,bop,norm_expr e1,norm_expr e2)
  | Negation (l,p) -> Negation (l,norm_pred p)
  | Pparentheses (_,p) -> norm_pred p
  | Universal_Q (l,xlst,p) -> Universal_Q (l,xlst,norm_pred p)
  | Existential_Q (l,xlst,p) -> Existential_Q (l,xlst,norm_pred p)
