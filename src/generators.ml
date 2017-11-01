open Utils
open Syntax
open QCheck

type var = (unit,unit) Syntax.var
type expression = (unit,unit) Syntax.expression
type predicate = (unit,unit) Syntax.predicate
type substitution = (unit,unit) Syntax.substitution
type component = (unit,unit) Syntax.component
type set = (unit,unit) Syntax.set
type machine_instanciation = (unit,unit) Syntax.machine_instanciation
type operation = (unit,unit) Syntax.operation

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

let gen_e_constant_bi : e_builtin Gen.t = fun random ->
  match Gen.int_bound 43 random with
  | 0 -> Integer (Gen.small_nat random)
  | 1 -> String (gen_string random)
  | _ -> Gen.oneofl Syntax.expr_constants random

let gen_ident : ident Gen.t = gen_string

let gen_rfield : unit lident Gen.t = fun rd -> { lid_loc=(); lid_str=gen_ident rd }
let gen_op_name : unit lident Gen.t = fun rd -> { lid_loc=(); lid_str=gen_ident rd }
let gen_mch_name : unit lident Gen.t = fun rd -> { lid_loc=(); lid_str=gen_ident rd }
let gen_var : var Gen.t = fun rd -> { var_loc=(); var_typ=(); var_id=gen_ident rd }

let split_int n rd =
  let k = Random.State.int rd (n + 1) in
  (k, n - k)

let gen_var_nelist rd : var Nlist.t =
  Nlist.make (gen_var rd) (Gen.small_list gen_var rd)

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

let mk_expr exp_desc = { exp_loc=(); exp_typ=(); exp_desc }
let mk_pred prd_desc = { prd_loc=(); prd_desc }
let mk_subst sub_desc : substitution = { sub_loc=(); sub_desc }

let rec sized_expr : expression Gen.sized = fun n ->
  if n <= 0 then
    Gen.oneof
      [ Gen.map (fun id -> mk_expr (Ident id)) gen_ident;
        Gen.map (fun id -> mk_expr (Dollar id)) gen_ident;
        Gen.map (fun bi -> mk_expr (Builtin bi)) gen_e_constant_bi ]

  else
    Gen.oneof
      [ Gen.map (fun p -> mk_expr (Pbool p)) (sized_pred (n-1));
        Gen.map (fun (e1,e2) -> mk_expr (Application (e1,e2)))
          (sized_pair split_int sized_expr sized_expr (n-1));
        Gen.map (fun (bi,e2) -> mk_expr (Application (mk_expr (Builtin bi),e2)))
          (Gen.pair
             (Gen.oneofl expr_prefix_postfix_ops) 
             (sized_expr (n-1)));
        Gen.map (fun (bi,(e1,e2)) ->
            mk_expr (Application (mk_expr (Builtin bi),mk_expr (Couple(Infix,e1,e2)))))
          (Gen.pair
             (Gen.oneofl expr_infix_ops)
             (sized_pair split_int sized_expr sized_expr (n-1)));
        Gen.map (fun (x,(e1,e2)) -> mk_expr (Couple(x,e1,e2)))
          (Gen.pair
             (Gen.oneofl [Comma false;Maplet])
             (sized_pair split_int sized_expr sized_expr (n-1))
          );
        Gen.map (fun lst -> mk_expr (Sequence lst))
          (sized_nel split_int_into_nel sized_expr (n-1));
        Gen.map (fun lst -> mk_expr (Extension lst))
          (sized_nel split_int_into_nel sized_expr (n-1));
        Gen.map (fun (nel,p) -> mk_expr (Comprehension (nel,p)))
          (Gen.pair
             gen_var_nelist
             (sized_pred (n-1)) );
        Gen.map (fun (bi,nel,(p,e)) -> mk_expr (Binder (bi,nel,p,e)))
          (Gen.triple
             (Gen.oneofl [Sum;Prod;Q_Union;Q_Intersection;Lambda])
             gen_var_nelist
             (sized_pair split_int sized_pred sized_expr (n-1)) );
        Gen.map (fun (e,id) -> mk_expr (Record_Field_Access (e,id)))
          (Gen.pair (sized_expr (n-1)) gen_rfield);
        Gen.map (fun nel -> mk_expr (Record nel))
          (sized_nel split_int_into_nel
             (fun fuel -> Gen.pair
                 gen_rfield
                 (sized_expr fuel) ) (n-1));
        Gen.map (fun nel -> mk_expr (Record_Type nel))
          (sized_nel split_int_into_nel
             (fun fuel -> Gen.pair
                 gen_rfield
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
             gen_var_nelist
             (sized_pred (n-1)) );
        Gen.map (fun (nel,p) -> mk_pred (Existential_Q (nel,p)))
          (Gen.pair
             gen_var_nelist
             (sized_pred (n-1)) );
      ]

let gen_expr : expression Gen.t = sized_expr 7
let gen_pred : predicate Gen.t = sized_pred 7

let gen_nel (gen:'a Gen.t) : ('a Nlist.t) Gen.t = fun rd ->
  Nlist.make (gen rd) (Gen.small_list gen rd)

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
      Gen.map (fun (id_nel,e_nel) -> mk_subst (Affectation (id_nel,e_nel)))
        (Gen.pair gen_var_nelist gen_expr) ;
      Gen.map (fun (id,e_nel,e)-> mk_subst (Function_Affectation(id,e_nel,e)))
        (Gen.triple gen_var (gen_nel gen_expr) gen_expr);
      Gen.map (fun (id,fd,e) -> mk_subst (Record_Affectation (id,fd,e)))
        (Gen.triple gen_var gen_rfield gen_expr);
      Gen.map (fun (id_nel,e) -> mk_subst (BecomesElt (id_nel,e)))
        (Gen.pair gen_var_nelist gen_expr);
      Gen.map (fun (id_nel,p) -> mk_subst (BecomesSuch (id_nel,p)))
        (Gen.pair gen_var_nelist gen_pred);
      Gen.map (fun (id_lst,id,e_lst) -> mk_subst (CallUp (id_lst,id,e_lst)))
        (Gen.triple (Gen.small_list gen_var) gen_op_name (Gen.small_list gen_expr))
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
              (fun fuel -> Gen.pair gen_expr (sized_subst fuel))
              sized_subst (n-1)) );
      Gen.map (fun (id_nel,p,s) -> mk_subst (Any (id_nel,p,s)))
        (Gen.triple gen_var_nelist gen_pred (sized_subst (n-1)));
      Gen.map (fun (id_nel,ie_nel,s) -> mk_subst (Let (id_nel,ie_nel,s)))
        (Gen.triple gen_var_nelist
           (gen_nel (Gen.pair gen_var gen_expr))
           (sized_subst (n-1)));
      Gen.map (fun (id_nel,s) -> mk_subst (Var (id_nel,s)))
        (Gen.pair gen_var_nelist (sized_subst (n-1)));
      Gen.map (fun (p,s,q,e) -> mk_subst (While (p,s,q,e)))
        (Gen.quad gen_pred (sized_subst (n-1)) gen_pred gen_expr);
      Gen.map (fun (s1,s2) -> mk_subst (Sequencement (s1,s2)))
        (sized_pair split_int sized_subst sized_subst (n-1));
      Gen.map (fun (s1,s2) -> mk_subst (Parallel (s1,s2)))
        (sized_pair split_int sized_subst sized_subst (n-1));
    ] rd

let gen_subst : substitution Gen.t = sized_subst 7

let gen_minst : machine_instanciation Gen.t = fun rd ->
  { mi_mch=gen_mch_name rd;
    mi_params=Gen.list_size (Gen.oneofl [0;1;2;3]) gen_expr rd }


let small_list (gen:'a Gen.t) : ('a list) Gen.t =
  Gen.list_size Gen.(1 -- 10) gen

let small_nelist (gen:'a Gen.t) : ('a Nlist.t) Gen.t = fun rd ->
  let lst = small_list gen rd in
  Nlist.from_list_exn lst

let gen_set : set Gen.t =
  Gen.oneof [ Gen.map (fun id -> Abstract_Set id) gen_var;
              Gen.map (fun (id,lst) -> Concrete_Set (id,lst)) 
                (Gen.pair gen_var (small_list gen_var)) ]

let gen_op : operation Gen.t = fun rd ->
  { op_out = Gen.small_list gen_var rd;
    op_name = gen_op_name rd;
    op_in = Gen.small_list gen_var rd;
    op_body = gen_subst rd }

let gen_machine : (unit,unit) machine_desc Gen.t = fun rd -> {
    mch_constraints =
      Gen.opt (Gen.map (fun p -> ((),p)) gen_pred) rd;
    mch_sees =
      Gen.opt (Gen.map (fun lst -> ((),lst)) (small_nelist gen_mch_name)) rd;
    mch_includes =
      Gen.opt (Gen.map (fun lst -> ((),lst)) (small_nelist gen_minst)) rd;
    mch_promotes =
      Gen.opt (Gen.map (fun lst -> ((),lst)) (small_nelist gen_op_name)) rd;
    mch_extends =
      Gen.opt (Gen.map (fun lst -> ((),lst)) (small_nelist gen_minst)) rd;
    mch_uses =
      Gen.opt (Gen.map (fun lst -> ((),lst)) (small_nelist gen_mch_name)) rd;
    mch_sets =
      Gen.opt (Gen.map (fun lst -> ((),lst)) (small_nelist gen_set)) rd;
    mch_concrete_constants =
      Gen.opt (Gen.map (fun lst -> ((),lst)) (small_nelist gen_var)) rd;
    mch_abstract_constants =
      Gen.opt (Gen.map (fun lst -> ((),lst)) (small_nelist gen_var)) rd;
    mch_properties =
      Gen.opt (Gen.map (fun p -> ((),p)) gen_pred) rd;
    mch_concrete_variables =
      Gen.opt (Gen.map (fun lst -> ((),lst)) (small_nelist gen_var)) rd;
    mch_abstract_variables =
      Gen.opt (Gen.map (fun lst -> ((),lst)) (small_nelist gen_var)) rd;
    mch_invariant =
      Gen.opt (Gen.map (fun p -> ((),p)) gen_pred) rd;
    mch_assertions =
      Gen.opt (Gen.map (fun p -> ((),p)) (small_nelist gen_pred)) rd;
    mch_initialisation =
      Gen.opt (Gen.map (fun s -> ((),s)) gen_subst) rd;
    mch_operations =
      Gen.opt (Gen.map (fun op -> ((),op)) (small_nelist gen_op)) rd;
}

let gen_refinement : (unit,unit) refinement_desc Gen.t = fun rd -> {
  ref_refines = gen_mch_name rd;
  ref_sees =
    Gen.opt (Gen.map (fun lst -> ((),lst)) (small_nelist gen_mch_name)) rd;
  ref_includes =
    Gen.opt (Gen.map (fun lst -> ((),lst)) (small_nelist gen_minst)) rd;
  ref_promotes =
    Gen.opt (Gen.map (fun lst -> ((),lst)) (small_nelist gen_op_name)) rd;
  ref_extends =
    Gen.opt (Gen.map (fun lst -> ((),lst)) (small_nelist gen_minst)) rd;
  ref_sets =
    Gen.opt (Gen.map (fun lst -> ((),lst)) (small_nelist gen_set)) rd;
  ref_concrete_constants =
    Gen.opt (Gen.map (fun lst -> ((),lst)) (small_nelist gen_var)) rd;
  ref_abstract_constants =
    Gen.opt (Gen.map (fun lst -> ((),lst)) (small_nelist gen_var)) rd;
  ref_properties =
    Gen.opt (Gen.map (fun p -> ((),p)) gen_pred) rd;
  ref_concrete_variables =
    Gen.opt (Gen.map (fun lst -> ((),lst)) (small_nelist gen_var)) rd;
  ref_abstract_variables =
    Gen.opt (Gen.map (fun lst -> ((),lst)) (small_nelist gen_var)) rd;
  ref_invariant =
    Gen.opt (Gen.map (fun p -> ((),p)) gen_pred) rd;
  ref_assertions =
    Gen.opt (Gen.map (fun p -> ((),p)) (small_nelist gen_pred)) rd;
  ref_initialisation =
    Gen.opt (Gen.map (fun s -> ((),s)) gen_subst) rd;
  ref_operations =
    Gen.opt (Gen.map (fun op -> ((),op)) (small_nelist gen_op)) rd;
  ref_local_operations =
    Gen.opt (Gen.map (fun op -> ((),op)) (small_nelist gen_op)) rd;
}

let gen_implementation : (unit,unit) implementation_desc Gen.t = fun rd -> {
  imp_refines = gen_mch_name rd;
  imp_sees =
    Gen.opt (Gen.map (fun lst -> ((),lst)) (small_nelist gen_mch_name)) rd;
  imp_imports =
    Gen.opt (Gen.map (fun lst -> ((),lst)) (small_nelist gen_minst)) rd;
  imp_promotes =
    Gen.opt (Gen.map (fun lst -> ((),lst)) (small_nelist gen_op_name)) rd;
  imp_extends =
    Gen.opt (Gen.map (fun lst -> ((),lst)) (small_nelist gen_minst)) rd;
  imp_sets =
    Gen.opt (Gen.map (fun lst -> ((),lst)) (small_nelist gen_set)) rd;
  imp_concrete_constants =
    Gen.opt (Gen.map (fun lst -> ((),lst)) (small_nelist gen_var)) rd;
  imp_properties =
    Gen.opt (Gen.map (fun p -> ((),p)) gen_pred) rd;
  imp_values =
    Gen.opt (Gen.map (fun lst -> ((),lst)) (small_nelist (Gen.pair gen_var gen_expr))) rd;
  imp_concrete_variables =
    Gen.opt (Gen.map (fun lst -> ((),lst)) (small_nelist gen_var)) rd;
  imp_invariant =
    Gen.opt (Gen.map (fun p -> ((),p)) gen_pred) rd;
  imp_assertions =
    Gen.opt (Gen.map (fun p -> ((),p)) (small_nelist gen_pred)) rd;
  imp_initialisation =
    Gen.opt (Gen.map (fun s -> ((),s)) gen_subst) rd;
  imp_operations =
    Gen.opt (Gen.map (fun op -> ((),op)) (small_nelist gen_op)) rd;
  imp_local_operations =
    Gen.opt (Gen.map (fun op -> ((),op)) (small_nelist gen_op)) rd;
  }

let mk_mch (co_name,co_parameters,desc) = { co_loc=(); co_name; co_parameters; co_desc=Machine desc }
let mk_ref (co_name,co_parameters,desc) = { co_loc=(); co_name; co_parameters; co_desc=Refinement desc }
let mk_imp (co_name,co_parameters,desc) = { co_loc=(); co_name; co_parameters; co_desc=Implementation desc }

let gen_machine : component Gen.t =
  Gen.map mk_mch (Gen.triple gen_ident (Gen.small_list gen_var) gen_machine)

let gen_refinement : component Gen.t =
  Gen.map mk_ref (Gen.triple gen_ident (Gen.small_list gen_var) gen_refinement)

let gen_implementation : component Gen.t =
  Gen.map mk_imp (Gen.triple gen_ident (Gen.small_list gen_var) gen_implementation)

let gen_component : component Gen.t =
  Gen.oneof [ gen_machine; gen_refinement; gen_implementation ]
