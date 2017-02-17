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

open Format

let ofst = 2

let pred_bop_to_string : pred_bop -> string = function
  | Equality -> "="
  | Disequality -> "\\="
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
  | String s -> s
  | TRUE -> "TRUE"
  | FALSE -> "FALSE"
  | MaxInt -> "MaxInt"
  | MinInt -> "MinInt"
  | INTEGER -> "INTEGER"
  | NATURAL -> "NATURAL"
  | NATURAL1 -> "NATURAL1"
  | INT -> "INT"
  | NAT -> "NAT"
  | NAT1 -> "NAT1"
  | STRINGS -> "STRING"
  | BOOLEANS -> "BOOLEAN"
  | Empty_Set -> "[]"
  | Empty_Seq -> "{}"
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
  | Functions Partial_Injections -> ">+->"
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

let pp_ident (out:formatter) (id:ident) : unit =
  fprintf out "%s" (snd id)

let pp_ident_list : formatter -> ident list -> unit =
  pp_print_list ~pp_sep:(fun out () -> fprintf out ",") pp_ident

let rec pp_expr (out:formatter) : expression -> unit = function
  | Ident id -> pp_ident out id
  | Dollar id -> fprintf out "%s$" (snd id)
  | Builtin (_,bi) -> fprintf out "%s" (builtin_to_string bi)
  | Pbool (_,p) -> fprintf out "@[pbool(%a)@]" pp_pred p
  | Parentheses (_,e) -> fprintf out "@[(%a)@]" pp_expr e
  | Application (_,Builtin (_,Inverse_Relation),e) ->
    fprintf out "@[%a~@]" pp_expr_wp e
  | Application (_,Builtin (_,Unary_Minus),e) ->
    fprintf out "@[-%a@]" pp_expr_wp e
  | Application (_,Builtin (_,Image),Couple(_,_,e1,e2)) ->
    fprintf out "@[%a[%a]@]" pp_expr_wp e1 pp_expr e2
  | Application (_,Builtin (_,bop),Couple(_,Infix,e1,e2)) ->
    fprintf out "@[%a@ %s@ %a@]" pp_expr_wp e1 (builtin_to_string bop) pp_expr_wp e2
  | Application (_,f,a) ->
    fprintf out "@[%a(%a)@]" pp_expr_wp f pp_expr a
  | Comprehension (l,(e,lst),p) ->
    fprintf out "@[{@ %a@ |@ %a@ }@]" pp_ident_list (e::lst) pp_pred p
  | Binder (l,bi,(x,lst),p,e) ->
    fprintf out "@[%s( %a ).(@ %a@ |@ %a@ )@]" (binder_to_string bi)
      pp_ident_list (x::lst) pp_pred p pp_expr e
  | Sequence (_,(e,lst)) -> fprintf out "@[[@ %a@ ]@]" pp_expr_list (e::lst)
  | Extension (_,(e,lst)) -> fprintf out "@[{@ %a@ }@]" pp_expr_list (e::lst)
  | Couple (_,Infix,e1,e2) -> assert false
  | Couple (_,Maplet,e1,e2) ->
    fprintf out "@[%a@ |->@ %a@]" pp_expr_wp e1 pp_expr_wp e2
  | Couple (_,Comma,e1,e2) ->
    fprintf out "@[%a,@ %a@]" pp_expr_wp e1 pp_expr_wp e2
  | Record_Field_Access (_,e,id) ->
    fprintf out "@[%a'%a@]" pp_expr_wp e pp_ident id
  | Record (_,(f,lst)) ->
    fprintf out "@[rec(%a)@]" pp_rec_field_list (f::lst)
  | Record_Type (_,(f,lst)) ->
    fprintf out "@[struct(%a)@]" pp_struct_field_list (f::lst)

and pp_struct_field_list (out:formatter) (lst:(ident*expression) list) : unit =
  let pp (out:formatter) (id,e:ident*expression) : unit =
    fprintf out "%a: %a" pp_ident id pp_expr_wp e
  in
  pp_print_list ~pp_sep:(fun out () -> fprintf out ",@,") pp out lst

and pp_rec_field_list (out:formatter) (lst:(ident option*expression) list) : unit =
  let pp (out:formatter) (opt,e:ident option*expression) : unit =
    match opt with
    | None -> pp_expr_wp out e
    | Some id -> fprintf out "%a: %a" pp_ident id pp_expr_wp e
  in
  pp_print_list ~pp_sep:(fun out () -> fprintf out ",@,") pp out lst

and pp_expr_list (out:formatter) (lst:expression list) : unit =
  pp_print_list ~pp_sep:(fun out () -> fprintf out ",@ ") pp_expr_wp out lst

and pp_expr_wp (out:formatter) : expression -> unit = function
  | Application (_,_,Couple(_,Infix,_,_)) | Couple _ | Record_Field_Access _ as e ->
    fprintf out "@[(%a)@]" pp_expr e
  | Ident _ | Dollar _ | Pbool _ | Builtin _ | Parentheses _ | Comprehension _
  | Binder _ | Sequence _ | Extension _ | Record _ | Record_Type _ | Application _ as e ->
    pp_expr out e

and pp_pred (out:formatter) : predicate -> unit =
  function
  | P_Ident id -> fprintf out "%s" (snd id)
  | P_Builtin (_,Btrue) -> fprintf out "btrue"
  | P_Builtin (_,Bfalse) -> fprintf out "bfalse"
  | Binary_Prop (_,bop,p1,p2) ->
    fprintf out "@[%a@ %s@ %a@]" pp_pred_wp p1 (prop_bop_to_string bop) pp_pred_wp p2
  | Binary_Pred (_,bop,e1,e2) ->
    fprintf out "@[%a@ %s@ %a@]" pp_expr_wp e1 (pred_bop_to_string bop) pp_expr_wp e2
  | Negation (_,p) -> fprintf out "@[not(%a)@]" pp_pred p
  | Pparentheses (_,p) -> fprintf out "@[(%a)@]" pp_pred p
  | Universal_Q (_,(x,lst),p) ->
    fprintf out "@[!(%a).(@,%a)@]" pp_ident_list (x::lst) pp_pred p
  | Existential_Q (_,(x,lst),p) ->
    fprintf out "@[#(%a).(@,%a)@]" pp_ident_list (x::lst) pp_pred p

and pp_pred_wp (out:formatter) : predicate -> unit = function
  | P_Ident _ | P_Builtin _ | Negation _ | Pparentheses _
  | Universal_Q _ | Existential_Q _ as p -> pp_pred out p
  | Binary_Prop _ | Binary_Pred _ as p -> fprintf out "@[(%a)@]" pp_pred p
    
and pp_field (out:formatter) (id,e:ident*expression) : unit =
  fprintf out "%s:@ %a" (snd id) pp_expr e

and pp_rec_field (out:formatter) (opt,e:ident option*expression) : unit =
  match opt with
  | Some id -> pp_field out (id,e)
  | None -> pp_expr out e
