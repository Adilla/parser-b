open Utils
open Expression
open Substitution
open Component
open QCheck

let pred_op_list =
  [ Equality; Disequality; Membership; Non_Membership;
    Inclusion Not_Strict; Inclusion Strict; Inclusion Non_Inclusion;
    Inclusion Non_Strict_Inclusion;
    Inequality Smaller_or_Equal;
    Inequality Strictly_Smaller;
    Inequality Greater_or_Equal;
    Inequality Strictly_Greater ]

let gen_e_constant_bi : e_builtin Gen.t = fun random ->
  match Gen.int_bound 43 random with
  | 0 -> Integer (Gen.small_nat random)
  | 1 -> String (Gen.string random)
  | _ -> Gen.oneofl Expression.expr_constants random

let gen_ident : ident Gen.t = fun rd -> (dloc,Gen.string rd)

let split_int n rd =
  let k = Random.State.int rd (n + 1) in
  (k, n - k)

let gen_ident_nel rd =
  (gen_ident rd, Gen.small_list gen_ident rd)

let split_int_into_nel n rd =
  let rec aux n k = (*returns a list of size (k+1) which sum is n *)
    if k == 0 then [n]
    else
      let (p,q) = split_int n rd in
      p::(aux q (k-1))
  in
  let s = Gen.int_bound 7 rd in
  let lst = Gen.shuffle_l (aux n s) rd in
  (List.hd lst, List.tl lst)

let sized_pair (split:(int*int) Gen.sized) (gen1:'a Gen.sized) (gen2:'b Gen.sized)
  : ('a*'b) Gen.sized = fun n rd ->
  let f1, f2 = split n rd in (gen1 f1 rd, gen2 f2 rd)

let sized_nel (split:int non_empty_list Gen.sized) (gen:'a Gen.sized)
  : 'a non_empty_list Gen.sized = fun n rd ->
  let (f,flst) = split n rd in
  (gen f rd, List.map (fun f -> gen f rd) flst)

let rec sized_expr : expression Gen.sized = fun n ->
  if n <= 0 then
    Gen.oneof
      [ Gen.map (fun id -> Ident id) gen_ident;
        Gen.map (fun id -> Dollar id) gen_ident;
        Gen.map (fun bi -> Builtin (dloc,bi)) gen_e_constant_bi ]

  else
    Gen.oneof
      [ Gen.map (fun p -> Pbool (dloc,p)) (sized_pred (n-1));
        Gen.map (fun e -> Parentheses (dloc,e)) (sized_expr (n-1));
        Gen.map (fun (e1,e2) -> Application (dloc,e1,e2))
          (sized_pair split_int sized_expr sized_expr (n-1));
        Gen.map (fun (bi,e2) -> Application (dloc,Builtin (dloc,bi),e2))
          (Gen.pair
             (Gen.oneofl Expression.expr_prefix_postfix_ops) 
             (sized_expr (n-1)));
        Gen.map (fun (bi,(e1,e2)) ->
            Application (dloc,Builtin(dloc,bi),Couple(dloc,Infix,e1,e2)))
          (Gen.pair
             (Gen.oneofl Expression.expr_infix_ops)
             (sized_pair split_int sized_expr sized_expr (n-1)));
        Gen.map (fun (x,(e1,e2)) -> Couple(dloc,x,e1,e2))
          (Gen.pair
             (Gen.oneofl [Comma;Maplet])
             (sized_pair split_int sized_expr sized_expr (n-1))
          );
        Gen.map (fun lst -> Sequence (dloc,lst))
          (sized_nel split_int_into_nel sized_expr (n-1));
        Gen.map (fun lst -> Extension (dloc,lst))
          (sized_nel split_int_into_nel sized_expr (n-1));
        Gen.map (fun (nel,p) -> Comprehension (dloc,nel,p))
          (Gen.pair
             gen_ident_nel
             (sized_pred (n-1)) );
        Gen.map (fun (bi,nel,(p,e)) -> Binder (dloc,bi,nel,p,e))
          (Gen.triple
             (Gen.oneofl [Sum;Prod;Q_Union;Q_Intersection;Lambda])
             gen_ident_nel
             (sized_pair split_int sized_pred sized_expr (n-1)) );
        Gen.map (fun (e,id) -> Record_Field_Access (dloc,e,id))
          (Gen.pair (sized_expr (n-1)) gen_ident);
        Gen.map (fun nel -> Record (dloc,nel))
          (sized_nel split_int_into_nel
             (fun fuel -> Gen.pair
                 (Gen.opt gen_ident)
                 (sized_expr fuel) ) (n-1));
        Gen.map (fun nel -> Record_Type (dloc,nel))
          (sized_nel split_int_into_nel
             (fun fuel -> Gen.pair
                 gen_ident
                 (sized_expr fuel) ) (n-1));
      ]

and sized_pred : predicate Gen.sized = fun n ->
  if n <= 0 then
    Gen.oneof
      [ Gen.map (fun id -> P_Ident id) gen_ident;
        Gen.return (P_Builtin (dloc,Btrue));
        Gen.return (P_Builtin (dloc,Bfalse)) ]
  else
    Gen.oneof
      [ Gen.map (fun p -> Negation (dloc,p)) (sized_pred (n-1));
        Gen.map (fun p -> Pparentheses (dloc,p)) (sized_pred (n-1));
        Gen.map (fun (op,(p,q)) -> Binary_Prop (dloc,op,p,q))
          (Gen.pair (Gen.oneofl [Conjonction; Disjonction; Implication; Equivalence])
             (sized_pair split_int sized_pred sized_pred (n-1)));
        Gen.map (fun (op,(p,q)) -> Binary_Pred (dloc,op,p,q))
          (Gen.pair (Gen.oneofl pred_op_list)
             (sized_pair split_int sized_expr sized_expr (n-1)));
        Gen.map (fun (nel,p) -> Universal_Q (dloc,nel,p))
          (Gen.pair
             gen_ident_nel
             (sized_pred (n-1)) );
        Gen.map (fun (nel,p) -> Existential_Q (dloc,nel,p))
          (Gen.pair
             gen_ident_nel
             (sized_pred (n-1)) );
      ]

let gen_expr : expression Gen.t = Gen.sized sized_expr
let gen_pred : predicate Gen.t = Gen.sized sized_pred

let gen_nel (gen:'a Gen.t) : ('a non_empty_list) Gen.t = fun rd ->
  (gen rd, Gen.small_list gen rd)

let sized_nel_and_opt (gen1:'a Gen.sized) (gen2:'b Gen.sized) :
  ('a non_empty_list * 'b option) Gen.sized = fun n rd ->
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
    | x::y::z -> ((gen1 y rd,List.map (fun f -> gen1 f rd) z),Some (gen2 x rd))
    | _ -> assert false
  else
    Gen.pair
      (sized_nel split_int_into_nel gen1 n)
      (Gen.return None) rd

let rec sized_subst : substitution Gen.sized = fun n rd ->
  if n <= 0 then
    Gen.oneof [
      Gen.return Skip;
      Gen.map (fun (id_nel,e_nel) -> Affectation (id_nel,e_nel))
        (Gen.pair gen_ident_nel (gen_nel gen_expr)) ;
      Gen.map (fun (id,e_nel,e)-> Function_Affectation(id,e_nel,e))
        (Gen.triple gen_ident (gen_nel gen_expr) gen_expr);
      Gen.map (fun (id,fd,e) -> Record_Affectation (id,fd,e))
        (Gen.triple gen_ident gen_ident gen_expr);
      Gen.map (fun (id_nel,e) -> BecomesElt (id_nel,e))
        (Gen.pair gen_ident_nel gen_expr);
      Gen.map (fun (id_nel,p) -> BecomesSuch (id_nel,p))
        (Gen.pair gen_ident_nel gen_pred);
      Gen.map (fun (id_lst,id,e_lst) -> CallUp (id_lst,id,e_lst))
        (Gen.triple (Gen.small_list gen_ident) gen_ident (Gen.small_list gen_expr))
    ] rd
  else
    Gen.oneof [
      Gen.map (fun s -> BeginEnd s) (sized_subst (n-1));
      Gen.map (fun (p,s) -> Pre (p,s))
        (Gen.pair gen_pred (sized_subst (n-1)));
      Gen.map (fun (p,s) -> Assert (p,s))
        (Gen.pair gen_pred (sized_subst (n-1)));
      Gen.map (fun (s_nle) -> Choice s_nle)
        (sized_nel split_int_into_nel sized_subst (n-1));
      Gen.map (fun (ps_nel,s_opt) -> IfThenElse (ps_nel,s_opt))
        (sized_nel_and_opt
           (fun fuel -> Gen.pair gen_pred (sized_subst fuel))
           sized_subst (n-1));
      Gen.map (fun (ps_nel,s_opt) -> Select (ps_nel,s_opt))
        (sized_nel_and_opt
           (fun fuel -> Gen.pair gen_pred (sized_subst fuel))
           sized_subst (n-1));
      Gen.map (fun (e,(es_nel,s_opt)) -> Case (e,es_nel,s_opt))
        (Gen.pair gen_expr
           (sized_nel_and_opt
              (fun fuel -> Gen.pair gen_expr (sized_subst fuel))
              sized_subst (n-1)) );
      Gen.map (fun (id_nel,p,s) -> Any (id_nel,p,s))
        (Gen.triple gen_ident_nel gen_pred (sized_subst (n-1)));
      Gen.map (fun (id_nel,ie_nel,s) -> Let (id_nel,ie_nel,s))
        (Gen.triple gen_ident_nel
           (gen_nel (Gen.pair gen_ident gen_expr))
           (sized_subst (n-1)));
      Gen.map (fun (id_nel,s) -> Var (id_nel,s))
        (Gen.pair gen_ident_nel (sized_subst (n-1)));
      Gen.map (fun (p,s,q,e) -> While (p,s,q,e))
        (Gen.quad gen_pred (sized_subst (n-1)) gen_pred gen_expr);
      Gen.map (fun (s1,s2) -> Sequencement (s1,s2))
        (sized_pair split_int sized_subst sized_subst (n-1));
      Gen.map (fun (s1,s2) -> Parallel (s1,s2))
        (sized_pair split_int sized_subst sized_subst (n-1));
    ] rd

let gen_subst : substitution Gen.t = Gen.sized sized_subst

let gen_minst : machine_instanciation Gen.t =
  Gen.pair gen_ident (Gen.small_list gen_expr)

let gen_set : set Gen.t =
  Gen.oneof [ Gen.map (fun id -> Abstract_Set id) gen_ident;
              Gen.map (fun (id,lst) -> Concrete_Set (id,lst)) 
                (Gen.pair gen_ident (Gen.small_list gen_ident)) ]

let gen_op : operation Gen.t =
  Gen.quad (Gen.small_list gen_ident) gen_ident
    (Gen.small_list gen_ident) gen_subst

let gen_machine : abstract_machine Gen.t = fun rd -> {
    name = gen_ident rd;
    parameters = Gen.small_list gen_ident rd;
    clause_constraints =
      Gen.opt (Gen.map (fun p -> (dloc,p)) gen_pred) rd;
    clause_sees =
      Gen.opt (Gen.map (fun lst -> (dloc,lst)) (Gen.small_list gen_ident)) rd;
    clause_includes =
      Gen.opt (Gen.map (fun lst -> (dloc,lst)) (Gen.small_list gen_minst)) rd;
    clause_promotes =
      Gen.opt (Gen.map (fun lst -> (dloc,lst)) (Gen.small_list gen_ident)) rd;
    clause_extends =
      Gen.opt (Gen.map (fun lst -> (dloc,lst)) (Gen.small_list gen_minst)) rd;
    clause_uses =
      Gen.opt (Gen.map (fun lst -> (dloc,lst)) (Gen.small_list gen_ident)) rd;
    clause_sets =
      Gen.opt (Gen.map (fun lst -> (dloc,lst)) (Gen.small_list gen_set)) rd;
    clause_concrete_constants =
      Gen.opt (Gen.map (fun lst -> (dloc,lst)) (Gen.small_list gen_ident)) rd;
    clause_abstract_constants =
      Gen.opt (Gen.map (fun lst -> (dloc,lst)) (Gen.small_list gen_ident)) rd;
    clause_properties =
      Gen.opt (Gen.map (fun p -> (dloc,p)) gen_pred) rd;
    clause_concrete_variables =
      Gen.opt (Gen.map (fun lst -> (dloc,lst)) (Gen.small_list gen_ident)) rd;
    clause_abstract_variables =
      Gen.opt (Gen.map (fun lst -> (dloc,lst)) (Gen.small_list gen_ident)) rd;
    clause_invariant =
      Gen.opt (Gen.map (fun p -> (dloc,p)) gen_pred) rd;
    clause_assertions =
      Gen.opt (Gen.map (fun p -> (dloc,p)) (Gen.small_list gen_pred)) rd;
    clause_initialisation =
      Gen.opt (Gen.map (fun s -> (dloc,s)) gen_subst) rd;
    clause_operations =
      Gen.opt (Gen.map (fun op -> (dloc,op)) (Gen.small_list gen_op)) rd;
}

let gen_refinement : refinement Gen.t = fun rd -> {
  name = gen_ident rd;
  parameters = Gen.small_list gen_ident rd;
  refines = gen_ident rd;
  clause_sees =
    Gen.opt (Gen.map (fun lst -> (dloc,lst)) (Gen.small_list gen_ident)) rd;
  clause_includes =
    Gen.opt (Gen.map (fun lst -> (dloc,lst)) (Gen.small_list gen_minst)) rd;
  clause_promotes =
    Gen.opt (Gen.map (fun lst -> (dloc,lst)) (Gen.small_list gen_ident)) rd;
  clause_extends =
    Gen.opt (Gen.map (fun lst -> (dloc,lst)) (Gen.small_list gen_minst)) rd;
  clause_sets =
    Gen.opt (Gen.map (fun lst -> (dloc,lst)) (Gen.small_list gen_set)) rd;
  clause_concrete_constants =
    Gen.opt (Gen.map (fun lst -> (dloc,lst)) (Gen.small_list gen_ident)) rd;
  clause_abstract_constants =
    Gen.opt (Gen.map (fun lst -> (dloc,lst)) (Gen.small_list gen_ident)) rd;
  clause_properties =
    Gen.opt (Gen.map (fun p -> (dloc,p)) gen_pred) rd;
  clause_concrete_variables =
    Gen.opt (Gen.map (fun lst -> (dloc,lst)) (Gen.small_list gen_ident)) rd;
  clause_abstract_variables =
    Gen.opt (Gen.map (fun lst -> (dloc,lst)) (Gen.small_list gen_ident)) rd;
  clause_invariant =
    Gen.opt (Gen.map (fun p -> (dloc,p)) gen_pred) rd;
  clause_assertions =
    Gen.opt (Gen.map (fun p -> (dloc,p)) (Gen.small_list gen_pred)) rd;
  clause_initialisation =
    Gen.opt (Gen.map (fun s -> (dloc,s)) gen_subst) rd;
  clause_operations =
    Gen.opt (Gen.map (fun op -> (dloc,op)) (Gen.small_list gen_op)) rd;
  clause_local_operations =
    Gen.opt (Gen.map (fun op -> (dloc,op)) (Gen.small_list gen_op)) rd;
}

let gen_implementation : implementation Gen.t = fun rd -> {
  name = gen_ident rd;
  parameters = Gen.small_list gen_ident rd;
  refines = gen_ident rd;
  clause_sees =
    Gen.opt (Gen.map (fun lst -> (dloc,lst)) (Gen.small_list gen_ident)) rd;
  clause_imports =
    Gen.opt (Gen.map (fun lst -> (dloc,lst)) (Gen.small_list gen_minst)) rd;
  clause_promotes =
    Gen.opt (Gen.map (fun lst -> (dloc,lst)) (Gen.small_list gen_ident)) rd;
  clause_extends_B0 =
    Gen.opt (Gen.map (fun lst -> (dloc,lst)) (Gen.small_list gen_minst)) rd;
  clause_sets =
    Gen.opt (Gen.map (fun lst -> (dloc,lst)) (Gen.small_list gen_set)) rd;
  clause_concrete_constants =
    Gen.opt (Gen.map (fun lst -> (dloc,lst)) (Gen.small_list gen_ident)) rd;
  clause_properties =
    Gen.opt (Gen.map (fun p -> (dloc,p)) gen_pred) rd;
  clause_values =
    Gen.opt (Gen.map (fun lst -> (dloc,lst)) (Gen.small_list (Gen.pair gen_ident gen_expr))) rd;
  clause_concrete_variables =
    Gen.opt (Gen.map (fun lst -> (dloc,lst)) (Gen.small_list gen_ident)) rd;
  clause_invariant =
    Gen.opt (Gen.map (fun p -> (dloc,p)) gen_pred) rd;
  clause_assertions =
    Gen.opt (Gen.map (fun p -> (dloc,p)) (Gen.small_list gen_pred)) rd;
  clause_initialisation_B0 =
    Gen.opt (Gen.map (fun s -> (dloc,s)) gen_subst) rd;
  clause_operations_B0 =
    Gen.opt (Gen.map (fun op -> (dloc,op)) (Gen.small_list gen_op)) rd;
  clause_local_operations_B0 =
    Gen.opt (Gen.map (fun op -> (dloc,op)) (Gen.small_list gen_op)) rd;
  }

let gen_component : component Gen.t =
  Gen.oneof [
    (Gen.map (fun x -> Abstract_machine x) gen_machine);
    (Gen.map (fun x -> Refinement x) gen_refinement);
    (Gen.map (fun x -> Implementation x) gen_implementation)
  ]
