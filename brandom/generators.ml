open Blib.SyntaxCore
open Blib.PSyntax
open QCheck
module Nlist = Blib.Nlist
module Utils = Blib.Utils

let pred_op_list =
  [ Equality; Disequality; Membership; Non_Membership;
    Inclusion Not_Strict; Inclusion Strict; Inclusion Non_Inclusion;
    Inclusion Non_Strict_Inclusion;
    Inequality Smaller_or_Equal;
    Inequality Strictly_Smaller;
    Inequality Greater_or_Equal;
    Inequality Strictly_Greater ]

let char_list : char list =
  let rec aux n =
    if n <= 97 then [97]
    else n::(aux (n-1)) 
  in
  List.map Char.chr (aux 122)

let gen_string : string Gen.t = fun rd ->
  "str_" ^ (Gen.string_size ~gen:(Gen.oneofl char_list) (Gen.return 3) rd)

let gen_lident : lident Gen.t = fun rd ->
  { lid_str="id_" ^ (Gen.string_size ~gen:(Gen.oneofl char_list) (Gen.return 3) rd);
    lid_loc=Blib.Utils.dloc }

let gen_ren_ident : ren_ident Gen.t =
  Gen.map (fun (r_prefix,r_str) -> 
      { r_prefix; r_loc=Utils.dloc; r_str }
    ) (Gen.pair (Gen.opt gen_string) gen_string)

let small_nat = Gen.map Int64.of_int Gen.small_nat

let gen_e_constant_bi : e_builtin_0 Gen.t =
  Gen.oneof [
    Gen.map (fun i -> Integer i) small_nat;
    Gen.map (fun s -> String s) gen_string;
    Gen.return MaxInt;
    Gen.return MinInt;
    Gen.return INTEGER;
    Gen.return NATURAL;
    Gen.return NATURAL1;
    Gen.return INT;
    Gen.return NAT;
    Gen.return NAT1;
    Gen.return STRINGS;
    Gen.return BOOLEANS;
    Gen.return Empty_Set;
    Gen.return Empty_Seq;
    Gen.return TRUE;
    Gen.return FALSE;
    Gen.return Successor;
    Gen.return Predecessor; 
  ]

let gen_builtin_1 : e_builtin_1 Gen.t =
  Gen.oneofl
    [ Cardinal; Power_Set Full; Power_Set Non_Empty;
      Power_Set Finite; Power_Set Finite_Non_Empty; Identity_Relation;
      Inverse_Relation; Closure; Transitive_Closure; Domain; Range; Fnc; Rel;
      Sequence_Set All_Seq; Sequence_Set Non_Empty_Seq; Sequence_Set Injective_Seq;
      Sequence_Set Injective_Non_Empty_Seq; Sequence_Set Permutations; Size;
      First; Last; Front; Tail; Reverse; G_Union; G_Intersection; G_Concatenation;
      Unary_Minus; Max; Min; Tree; Btree; Const; Top; Sons; Prefix; Postfix; SizeT;
      Mirror; Rank; Father; Son; Subtree; Arity; Bin; Left; Right; Infix ]

let gen_builtin_2 : e_builtin_2 Gen.t =
  Gen.oneofl
    [ Product; Difference; Addition; Division; Modulo; Power; Interval; Union;
      Intersection; Relations; First_Projection; Second_Projection; Composition;
      Direct_Product; Parallel_Product; Iteration; Image; Domain_Restriction;
      Domain_Soustraction; Codomain_Restriction; Codomain_Soustraction; Surcharge;
      Functions Partial_Functions; Functions Total_Functions; Functions Partial_Injections;
      Functions Total_Injections; Functions Partial_Surjections; Functions Total_Surjections;
      Functions Bijections; Concatenation; Head_Insertion; Tail_Insertion;
      Head_Restriction; Tail_Restriction; Application; Couple Comma; Couple Mapplet ]

let split_int n rd =
  let k = Random.State.int rd (n + 1) in
  (k, n - k)

let split_int_into_nel n rd =
  let rec aux n k = (*returns a list of size (k+1) which sum is n *)
    if k == 0 then [n]
    else
      let (p,q) = split_int n rd in
      p::(aux q (k-1))
  in
  let s = Gen.int_bound 7 rd in
  let lst = Gen.shuffle_l (aux n s) rd in
  Nlist.from_list_exn lst

let sized_pair (split:(int*int) Gen.sized) (gen1:'a Gen.sized) (gen2:'b Gen.sized)
  : ('a*'b) Gen.sized = fun n rd ->
  let f1, f2 = split n rd in (gen1 f1 rd, gen2 f2 rd)

let sized_nel (split:int Nlist.t Gen.sized) (gen:'a Gen.sized)
  : 'a Nlist.t Gen.sized = fun n rd ->
  let flst = split n rd in
  Nlist.make (gen (Nlist.hd flst) rd) (List.map (fun f -> gen f rd) (Nlist.tl flst))

let gen_nel (gen:'a Gen.t) : ('a Nlist.t) Gen.t = fun rd ->
  Nlist.make (gen rd) (Gen.small_list gen rd)

let mk_expr exp_desc = { exp_desc; exp_loc=Utils.dloc; exp_par=false }
let mk_pred prd_desc = { prd_desc; prd_loc=Utils.dloc; prd_par=false }
let mk_subst sub_desc = { sub_desc; sub_loc=Utils.dloc; sub_be=false }

let rec sized_expr : expression Gen.sized = fun n ->
  if n <= 0 then
    Gen.oneof
      [ Gen.map (fun (p,id) -> mk_expr (Ident (p,id))) (Gen.pair (Gen.opt gen_string) gen_string);
        Gen.map (fun (p,id) -> mk_expr (Dollar (p,id))) (Gen.pair (Gen.opt gen_string) gen_string);
        Gen.map (fun bi -> mk_expr (Builtin_0 bi)) gen_e_constant_bi ]

  else
    Gen.oneof
      [ Gen.map (fun p -> mk_expr (Pbool p)) (sized_pred (n-1));
        Gen.map (fun (bi,e) -> mk_expr (Builtin_1(bi,e)))
          (Gen.pair gen_builtin_1 (sized_expr (n-1)));
        Gen.map (fun (bi,(e1,e2)) -> mk_expr (Builtin_2(bi,e1,e2)))
          (Gen.pair gen_builtin_2
             (sized_pair split_int sized_expr sized_expr (n-1)));
        Gen.map (fun lst -> mk_expr (Sequence lst))
          (sized_nel split_int_into_nel sized_expr (n-1));
        Gen.map (fun lst -> mk_expr (Extension lst))
          (sized_nel split_int_into_nel sized_expr (n-1));
        Gen.map (fun (nel,p) -> mk_expr (Comprehension (nel,p)))
          (Gen.pair
             (gen_nel gen_lident)
             (sized_pred (n-1)) );
        Gen.map (fun (bi,nel,(p,e)) -> mk_expr (Binder (bi,nel,p,e)))
          (Gen.triple
             (Gen.oneofl [Sum;Prod;Q_Union;Q_Intersection;Lambda])
             (gen_nel gen_lident)
             (sized_pair split_int sized_pred sized_expr (n-1)) );
        Gen.map (fun (e,id) -> mk_expr (Record_Field_Access (e,id)))
          (Gen.pair (sized_expr (n-1)) gen_lident);
        Gen.map (fun nel -> mk_expr (Record nel))
          (sized_nel split_int_into_nel
             (fun fuel -> Gen.pair
                 gen_lident
                 (sized_expr fuel) ) (n-1));
        Gen.map (fun nel -> mk_expr (Record_Type nel))
          (sized_nel split_int_into_nel
             (fun fuel -> Gen.pair
                 gen_lident
                 (sized_expr fuel) ) (n-1));
      ]

and sized_pred : predicate Gen.sized = fun n ->
  if n <= 0 then
    Gen.map (fun (op,(p,q)) -> mk_pred (Binary_Pred (op,p,q)))
      (Gen.pair (Gen.oneofl pred_op_list)
         (sized_pair split_int sized_expr sized_expr 0))
(*
    Gen.oneof
      [ Gen.map (fun id -> P_Ident id) gen_ident;
        Gen.return (P_Builtin (dloc,Btrue));
        Gen.return (P_Builtin (dloc,Bfalse)) ]
*)
  else
    Gen.oneof
      [ Gen.map (fun p -> mk_pred (Negation p)) (sized_pred (n-1));
        Gen.map (fun (op,(p,q)) -> mk_pred (Binary_Prop (op,p,q)))
          (Gen.pair (Gen.oneofl [Conjonction; Disjonction; Implication; Equivalence])
             (sized_pair split_int sized_pred sized_pred (n-1)));
        Gen.map (fun (op,(p,q)) -> mk_pred (Binary_Pred (op,p,q)))
          (Gen.pair (Gen.oneofl pred_op_list)
             (sized_pair split_int sized_expr sized_expr (n-1)));
        Gen.map (fun (nel,p) -> mk_pred (Universal_Q (nel,p)))
          (Gen.pair
             (gen_nel gen_lident)
             (sized_pred (n-1)) );
        Gen.map (fun (nel,p) -> mk_pred (Existential_Q (nel,p)))
          (Gen.pair
             (gen_nel gen_lident)
             (sized_pred (n-1)) );
      ]

let gen_expr : expression Gen.t = sized_expr 7
let gen_pred : predicate Gen.t = sized_pred 7

let sized_nel_and_opt (gen1:'a Gen.sized) (gen2:'b Gen.sized) :
  ('a Nlist.t * 'b option) Gen.sized = fun n rd ->
  if Gen.bool rd then
    let rec aux n k = (*returns a list of size (k+2) which sum is n *)
      if k == 0 then
        let (x,y) = split_int n rd in
        [x;y]
      else
        let (p,q) = split_int n rd in
        p::(aux q (k-1))
    in
    let s = Gen.int_bound 7 rd in
    match Gen.shuffle_l (aux n s) rd with
    | x::y::z -> Nlist.make (gen1 y rd) (List.map (fun f -> gen1 f rd) z),Some (gen2 x rd)
    | _ -> assert false
  else
    Gen.pair
      (sized_nel split_int_into_nel gen1 n)
      (Gen.return None) rd

let rec sized_subst : substitution Gen.sized = fun n rd ->
  if n <= 0 then
    Gen.oneof [
      Gen.return (mk_subst Skip);
      Gen.map (fun (id_nel,e_nel) -> mk_subst (Affectation (Tuple id_nel,e_nel)))
        (Gen.pair (gen_nel gen_ren_ident) gen_expr) ;
      Gen.map (fun (id,e_nel,e)-> mk_subst (Affectation(Function(id,e_nel),e)))
        (Gen.triple gen_ren_ident (gen_nel gen_expr) gen_expr);
      Gen.map (fun (id,fd,e) -> mk_subst (Affectation (Record(id,fd),e)))
        (Gen.triple gen_ren_ident gen_lident gen_expr);
      Gen.map (fun (id_nel,e) -> mk_subst (BecomesElt (id_nel,e)))
        (Gen.pair (gen_nel gen_ren_ident) gen_expr);
      Gen.map (fun (id_nel,p) -> mk_subst (BecomesSuch (id_nel,p)))
        (Gen.pair (gen_nel gen_ren_ident) gen_pred);
      Gen.map (fun (id_lst,id,e_lst) -> mk_subst (CallUp (id_lst,id,e_lst)))
        (Gen.triple (Gen.small_list gen_ren_ident) gen_ren_ident (Gen.small_list gen_expr))
    ] rd
  else
    Gen.oneof [
      Gen.map (fun (p,s) -> mk_subst (Pre (p,s)))
        (Gen.pair gen_pred (sized_subst (n-1)));
      Gen.map (fun (p,s) -> mk_subst (Assert (p,s)))
        (Gen.pair gen_pred (sized_subst (n-1)));
      Gen.map (fun (s_nle) -> mk_subst (Choice (s_nle)))
        (sized_nel split_int_into_nel sized_subst (n-1));
      Gen.map (fun (ps_nel,s_opt) -> mk_subst (IfThenElse (ps_nel,s_opt)))
        (sized_nel_and_opt
           (fun fuel -> Gen.pair gen_pred (sized_subst fuel))
           sized_subst (n-1));
      Gen.map (fun (ps_nel,s_opt) -> mk_subst (Select (ps_nel,s_opt)))
        (sized_nel_and_opt
           (fun fuel -> Gen.pair gen_pred (sized_subst fuel))
           sized_subst (n-1));
      Gen.map (fun (e,(es_nel,s_opt)) -> mk_subst (Case (e,es_nel,s_opt)))
        (Gen.pair gen_expr
           (sized_nel_and_opt
              (fun fuel -> Gen.pair (fun rd -> Nlist.make1 (gen_expr rd)) (sized_subst fuel))
              sized_subst (n-1)) );
      Gen.map (fun (id_nel,p,s) -> mk_subst (Any (id_nel,p,s)))
        (Gen.triple (gen_nel gen_lident) gen_pred (sized_subst (n-1)));
      Gen.map (fun (id_nel,ie_nel,s) -> mk_subst (Let (id_nel,ie_nel,s)))
        (Gen.triple (gen_nel gen_lident)
           (gen_nel (Gen.pair gen_lident gen_expr))
           (sized_subst (n-1)));
      Gen.map (fun (id_nel,s) -> mk_subst (Var (id_nel,s)))
        (Gen.pair (gen_nel gen_lident) (sized_subst (n-1)));
      Gen.map (fun (p,s,q,e) -> mk_subst (While (p,s,q,e)))
        (Gen.quad gen_pred (sized_subst (n-1)) gen_pred gen_expr);
      Gen.map (fun (s1,s2) -> mk_subst (Sequencement (s1,s2)))
        (sized_pair split_int sized_subst sized_subst (n-1));
      Gen.map (fun (s1,s2) -> mk_subst (Parallel (s1,s2)))
        (sized_pair split_int sized_subst sized_subst (n-1));
    ] rd

let gen_subst : substitution Gen.t = sized_subst 7

let gen_minst : machine_instanciation Gen.t = fun rd ->
  { mi_mch=gen_ren_ident rd;
    mi_params=Gen.list_size (Gen.oneofl [0;1;2;3]) gen_expr rd }

let gen_set : set Gen.t =
  Gen.oneof [ Gen.map (fun id -> Abstract_Set id) gen_lident;
              Gen.map (fun (id,lst) -> Concrete_Set (id,lst)) 
                (Gen.pair gen_lident (Gen.list_size (Gen.int_range 1 9) gen_lident)) ]

let gen_op : operation Gen.t = fun rd ->
  { op_out = Gen.small_list gen_lident rd;
    op_name = gen_lident rd;
    op_in = Gen.small_list gen_lident rd;
    op_body = gen_subst rd }

let gen_machine : machine Gen.t = fun rd -> {
    mch_parameters = (Gen.small_list gen_lident) rd;
    mch_constraints = Gen.opt gen_pred rd;
    mch_sees = Gen.small_list gen_ren_ident rd;
    mch_includes = Gen.small_list gen_minst rd;
    mch_promotes = Gen.small_list gen_lident rd;
    mch_extends = Gen.small_list gen_minst rd;
    mch_uses = Gen.small_list gen_ren_ident rd;
    mch_sets = Gen.small_list gen_set rd;
    mch_concrete_constants = Gen.small_list gen_lident rd;
    mch_abstract_constants = Gen.small_list gen_lident rd;
    mch_properties = Gen.opt gen_pred rd;
    mch_concrete_variables = Gen.small_list gen_lident rd;
    mch_abstract_variables = Gen.small_list gen_lident rd;
    mch_invariant = Gen.opt gen_pred rd;
    mch_assertions = Gen.small_list gen_pred rd;
    mch_initialisation = Gen.opt gen_subst rd;
    mch_operations = Gen.small_list gen_op rd;
  }

let gen_refinement : refinement Gen.t = fun rd -> {
    ref_parameters = (Gen.small_list gen_lident) rd;
    ref_refines = gen_lident rd;
    ref_sees = Gen.small_list gen_ren_ident rd;
    ref_includes = Gen.small_list gen_minst rd;
    ref_promotes = Gen.small_list gen_lident rd;
    ref_extends = Gen.small_list gen_minst rd;
    ref_sets = Gen.small_list gen_set rd;
    ref_concrete_constants = Gen.small_list gen_lident rd;
    ref_abstract_constants = Gen.small_list gen_lident rd;
    ref_properties = Gen.opt gen_pred rd;
    ref_concrete_variables = Gen.small_list gen_lident rd;
    ref_abstract_variables = Gen.small_list gen_lident rd;
    ref_invariant = Gen.opt gen_pred rd;
    ref_assertions = Gen.small_list gen_pred rd;
    ref_initialisation = Gen.opt gen_subst rd;
    ref_operations = Gen.small_list gen_op rd;
  }

let gen_implementation : implementation Gen.t = fun rd -> {
    imp_parameters = (Gen.small_list gen_lident) rd;
    imp_refines = gen_lident rd;
    imp_sees = Gen.small_list gen_ren_ident rd;
    imp_imports = Gen.small_list gen_minst rd;
    imp_promotes = Gen.small_list gen_lident rd;
    imp_extends = Gen.small_list gen_minst rd;
    imp_sets = Gen.small_list gen_set rd;
    imp_concrete_constants = Gen.small_list gen_lident rd;
    imp_properties = Gen.opt gen_pred rd;
    imp_values = Gen.small_list (Gen.pair gen_lident gen_expr) rd;
    imp_concrete_variables = Gen.small_list gen_lident rd;
    imp_invariant = Gen.opt gen_pred rd;
    imp_assertions = Gen.small_list gen_pred rd;
    imp_initialisation = Gen.opt gen_subst rd;
    imp_operations = Gen.small_list gen_op rd;
    imp_local_operations = Gen.small_list gen_op rd;
  }

let mk_mch (co_name,desc) = { co_name; co_desc=Machine desc }
let mk_ref (co_name,desc) = { co_name; co_desc=Refinement desc }
let mk_imp (co_name,desc) = { co_name; co_desc=Implementation desc }

let gen_machine : component Gen.t =
  Gen.map mk_mch (Gen.pair gen_lident gen_machine)

let gen_refinement : component Gen.t =
  Gen.map mk_ref (Gen.pair gen_lident gen_refinement)

let gen_implementation : component Gen.t =
  Gen.map mk_imp (Gen.pair gen_lident gen_implementation)

let gen_component : component Gen.t =
  Gen.oneof [ gen_machine; gen_refinement; gen_implementation ]
