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

(* Print *)

let rec expr_eq e1 e2 : bool =
  match e1, e2 with
  | Parentheses (_,e), _ -> expr_eq e e2
  | _, Parentheses (_,e) -> expr_eq e1 e
  | Ident id1, Ident id2 -> ident_eq id1 id2
  | Dollar id1, Dollar id2 -> ident_eq id1 id2
  | Builtin (_,b1), Builtin (_,b2) -> b1 = b2 (*sufficient?*)
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
  | Record (_,(hd1,tl1)), Record (_,(hd2,tl2)) ->
    begin
      let aux (opt1,e1) (opt2,e2) =
        expr_eq e1 e2 && (match opt1, opt2 with
            | None, None -> true
            | Some id1, Some id2 -> ident_eq id1 id2
            | _, _ -> false )
      in
      try List.for_all2 aux (hd1::tl1) (hd2::tl2)
      with Invalid_argument _ -> false
    end
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

open Easy_format

let mk_atom s = Atom (s,atom)
let mk_label a b = Label ((a,{label with space_after_label=false}),b)

let add_par : expression -> expression = function
  | Application (_,_,Couple(_,Infix,_,_)) | Couple _  | Record_Field_Access _ as e -> Parentheses (dloc,e)
  | Ident _ | Dollar _ | Pbool _ | Builtin _ | Parentheses _ | Comprehension _
  | Binder _ | Sequence _ | Extension _ | Record _
  | Record_Type _ | Application _ as e -> e

let add_par_p : predicate -> predicate = function
  | P_Ident _ | P_Builtin _ | Negation _ | Pparentheses _
  | Universal_Q _ | Existential_Q _ | Binary_Pred _ as p -> p
  | Binary_Prop _ as p -> Pparentheses (dloc,p)

let list_1 =
  { list with space_after_opening=false;
              space_before_closing=false;
              align_closing=false }

let mk_ident_list_comma (lst:ident list) : Easy_format.t =
    let lst = List.map (fun id -> mk_atom (snd id)) lst in
    List (("",",","",list_1), lst)

let rec get_and_list = function
  | Binary_Prop (_,Conjonction,p1,p2) -> (get_and_list p1)@(get_and_list p2)
  | s -> [s]

let rec get_or_list = function
  | Binary_Prop (_,Disjonction,p1,p2) -> (get_or_list p1)@(get_or_list p2)
  | s -> [s]

let rec ef_expr : expression -> Easy_format.t = function
  | Ident id -> mk_atom (snd id)
  | Dollar id -> mk_atom (snd id ^ "$0")
  | Builtin (_,bi) -> mk_atom (builtin_to_string bi)
  | Pbool (_,p) ->
    mk_label (mk_atom "bool") (List(("(","",")",list_1),[ef_pred p]))
  | Parentheses (_,e) -> List(("(","",")",list_1),[ef_expr e])
  | Application (_,Builtin (_,Inverse_Relation),e) ->
    mk_label (ef_expr (add_par e)) (mk_atom "~")
  | Application (_,Builtin (_,Unary_Minus),e) ->
    mk_label (mk_atom "-") (ef_expr (add_par e))
  | Application (_,Builtin (_,Image),Couple(_,_,e1,e2)) ->
    mk_label (ef_expr (add_par e1)) (List (("[","","]",list_1),[ef_expr e2]))
  | Application (_,Builtin (_,(Composition|Parallel_Product as bop)),Couple(_,Infix,e1,e2)) -> (*FIXME*)
    List(("(",builtin_to_string bop,")",{ list_1 with space_before_separator=true }),
         [ef_expr (add_par e1); ef_expr (add_par e2)])
  | Application (_,Builtin (_,bop),Couple(_,Infix,e1,e2)) ->
    List(("",builtin_to_string bop,"",{ list_1 with space_before_separator=true }),
         [ef_expr (add_par e1); ef_expr (add_par e2)])
  | Application (_,f,a) ->
    mk_label (ef_expr (add_par f)) (List (("(","",")",list_1),[ef_expr a]))
  | Comprehension (_,(x,xlst),p) ->
    List(("{","","}",{ list with align_closing=false}),
         [mk_ident_list_comma (x::xlst);mk_atom "|";ef_pred p])
  | Binder (l,bi,(x,xlst),p,e) ->
    let lst = List.map (fun (_,id) -> mk_atom id) (x::xlst) in
    let x = mk_label (mk_atom (binder_to_string bi)) (List(("(",",",")",list_1), lst)) in
    let y = List(("(","|",")",list_1), [ef_pred p;ef_expr e]) in
    List (("",".","",list_1),[x;y])
  | Sequence (_,(e,lst)) ->
    let lst = List.map (fun e -> ef_expr (add_par e)) (e::lst) in
    List (("[",",","]",list_1),lst)
  | Extension (_,(e,lst)) ->
    let lst = List.map (fun e -> ef_expr (add_par e)) (e::lst) in
    List (("{",",","}",list_1),lst)
  | Couple (_,Infix,e1,e2) -> assert false
  | Couple (_,Maplet,e1,e2) ->
    List(("","","",list_1),[ef_expr (add_par e1);mk_atom "|->";ef_expr (add_par e2)])
  | Couple (_,Comma,e1,e2) ->
    List(("","","",list_1),[ef_expr (add_par e1);mk_atom ",";ef_expr (add_par e2)])
  | Record_Field_Access (_,e,id) ->
    mk_label (ef_expr (add_par e)) (mk_atom ("'" ^ snd id))
  | Record (_,(f,lst)) ->
    let flst = List.map ef_rec_field (f::lst) in
    let lst = List (("(",",",")",list_1),flst) in
    mk_label (mk_atom "rec") lst
  | Record_Type (_,(f,lst)) ->
    let flst = List.map ef_struct_field (f::lst) in
    let lst = List (("(",",",")",list_1),flst) in
    mk_label (mk_atom "struct") lst

and ef_struct_field (id,e:ident*expression) : Easy_format.t =
  List(("",":","",list_1), [mk_atom (snd id);ef_expr (add_par e)])

and ef_rec_field (opt,e:ident option*expression) : Easy_format.t =
  match opt with
  | None -> ef_expr (add_par e)
  | Some id -> ef_struct_field (id,e)

and ef_pred : predicate -> Easy_format.t = function
  | P_Ident id -> mk_atom (snd id)
  | P_Builtin (_,Btrue) -> mk_atom "btrue"
  | P_Builtin (_,Bfalse) -> mk_atom "bfalse"
  | Binary_Prop (_,Conjonction,_,_) as p ->
    let pars = List.map (fun p -> ef_pred (add_par_p p)) (get_and_list p) in
    List (("","&","",{ list_1 with space_before_separator=true}),pars)
  | Binary_Prop (_,Disjonction,_,_) as p ->
    let pars = List.map (fun p -> ef_pred (add_par_p p)) (get_or_list p) in
    List (("","or","",{ list_1 with space_before_separator=true}),pars)
  | Binary_Prop (_,bop,p1,p2) ->
    List(("",prop_bop_to_string bop,"",{list_1 with space_before_separator=true}),
         [ef_pred (add_par_p p1); ef_pred (add_par_p p2)])
  | Binary_Pred (_,bop,e1,e2) ->
    List (("",pred_bop_to_string bop,"",{list_1 with space_before_separator=true}),
          [ef_expr e1; ef_expr e2])
  | Negation (_,p) ->
    mk_label (mk_atom "not") (List(("(","",")",list_1),[ef_pred p]))
  | Pparentheses (_,p) -> List(("(","",")",list_1),[ef_pred p])
  | Universal_Q (_,(x,xlst),p) ->
    let lst = List.map (fun (_,id) -> mk_atom id) (x::xlst) in
    let x = mk_label (mk_atom "!") (List(("(",",",")",list_1),lst)) in
    let y = List(("(","",")",list_1), [ef_pred p]) in
    List(("",".","",{list_1 with space_after_separator=false}),[x;y])
  | Existential_Q (_,(x,xlst),p) ->
    let lst = List.map (fun (_,id) -> mk_atom id) (x::xlst) in
    let x = mk_label (mk_atom "#") (List(("(",",",")",list_1),lst)) in
    let y = List(("(","",")",list_1), [ef_pred p]) in
    List(("",".","",{list_1 with space_after_separator=false}),[x;y])
