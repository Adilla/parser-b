open Utils
open Expression

type substitution =
  | Skip
  | BeginEnd of substitution
  | Affectation of ident non_empty_list * expression non_empty_list (** id1, ..., idn := e1, ..., en *)
  | Function_Affectation of ident * expression non_empty_list * expression
  | Record_Affectation of ident * ident * expression (** record'field := e *)
  | Pre of predicate * substitution
  | Assert of predicate * substitution
  | Choice of substitution non_empty_list
  | IfThenElse of (predicate*substitution) non_empty_list * substitution option
  | Select of (predicate*substitution) non_empty_list * substitution option
  | Case of expression*(expression*substitution) non_empty_list * substitution option
  | Any of ident non_empty_list * predicate * substitution
  | Let of ident non_empty_list * (ident*expression) non_empty_list * substitution
  | BecomesElt of ident non_empty_list * expression
  | BecomesSuch of ident non_empty_list * predicate
  | Var of ident non_empty_list * substitution
  | CallUp of ident list * ident * expression list
  | While of predicate * substitution * predicate * expression
  | Sequencement of substitution * substitution
  | Parallel of substitution * substitution

let rec subst_eq s1 s2 : bool =
  match s1,s2 with
  | BeginEnd s, _ -> subst_eq s s2
  | _, BeginEnd s -> subst_eq s1 s
  | Skip, Skip -> true
  | Affectation ((x1,xlst1),(e1,elst1)), Affectation ((x2,xlst2),(e2,elst2)) ->
    ident_list_eq (x1::xlst1) (x2::xlst2) &&
    expr_list_eq (e1::elst1) (e2::elst2)
  | Function_Affectation (f1,(e1,elst1),a1), Function_Affectation (f2,(e2,elst2),a2) ->
    ident_eq f1 f2 && expr_list_eq (e1::elst1) (e2::elst2) && expr_eq a1 a1
  | Record_Affectation (id1,fd1,e1), Record_Affectation (id2,fd2,e2) ->
    ident_eq id1 id2 && ident_eq fd1 fd2 && expr_eq e1 e2
  | Pre (p1,s1), Pre (p2,s2) -> pred_eq p1 p2 && subst_eq s1 s2
  | Assert (p1,s1), Assert (p2,s2) -> pred_eq p1 p2 && subst_eq s1 s2
  | Choice (s1,slst1), Choice (s2,slst2) -> subst_list_eq (s1::slst1) (s2::slst2)
  | IfThenElse ((y1,ylst1),opt1), IfThenElse ((y2,ylst2),opt2) ->
    let aux l1 l2 =
      try List.for_all2
            (fun (p1,s1) (p2,s2) -> pred_eq p1 p2 && subst_eq s1 s2)
            l1 l2
      with Invalid_argument _ -> false
    in
    aux (y1::ylst1) (y2::ylst2) &&
    ( match opt1, opt2 with
      | None, None -> true
      | Some s1, Some s2 -> subst_eq s1 s2
      | _, _ -> false )
  | Select ((y1,ylst1),opt1), Select ((y2,ylst2),opt2) ->
    let aux l1 l2 =
      try List.for_all2
            (fun (p1,s1) (p2,s2) -> pred_eq p1 p2 && subst_eq s1 s2)
            l1 l2
      with Invalid_argument _ -> false
    in
    aux (y1::ylst1) (y2::ylst2) &&
    ( match opt1, opt2 with
      | None, None -> true
      | Some s1, Some s2 -> subst_eq s1 s2
      | _, _ -> false )
  | Case (e1,(y1,ylst1),opt1), Case (e2,(y2,ylst2),opt2) ->
    let aux l1 l2 =
      try List.for_all2
            (fun (e1,s1) (e2,s2) -> expr_eq e1 e2 && subst_eq s1 s2)
            l1 l2
      with Invalid_argument _ -> false
    in
    expr_eq e1 e2 && aux (y1::ylst1) (y2::ylst2) &&
    ( match opt1, opt2 with
      | None, None -> true
      | Some s1, Some s2 -> subst_eq s1 s2
      | _, _ -> false )
  | Any ((x1,xlst1),p1,s1), Any ((x2,xlst2),p2,s2) ->
    ident_list_eq (x1::xlst1) (x2::xlst2) && pred_eq p1 p2 && subst_eq s1 s2
  | Let ((x1,xlst1),(y1,ylst1),s1), Let ((x2,xlst2),(y2,ylst2),s2) ->
    let aux l1 l2 =
      try List.for_all2
            (fun (id1,e1) (id2,e2) -> ident_eq id1 id2 && expr_eq e1 e2)
            l1 l2
      with Invalid_argument _ -> false
    in
    ident_list_eq (x1::xlst1) (x2::xlst2) && aux (y1::ylst1) (y2::ylst2) && subst_eq s1 s2
  | BecomesElt ((x1,xlst1),e1), BecomesElt ((x2,xlst2),e2) ->
    ident_list_eq (x1::xlst1) (x2::xlst2) && expr_eq e1 e2
  | BecomesSuch ((x1,xlst1),p1), BecomesSuch ((x2,xlst2),p2) ->
    ident_list_eq (x1::xlst1) (x2::xlst2) && pred_eq p1 p2
  | Var ((x1,xlst1),s1), Var ((x2,xlst2),s2) ->
    ident_list_eq (x1::xlst1) (x2::xlst2) && subst_eq s1 s2
  | CallUp (xlst1,op1,elst1), CallUp (xlst2,op2,elst2) ->
    ident_list_eq xlst1 xlst2 && ident_eq op1 op2 && expr_list_eq elst1 elst2
  | While (p1,s1,q1,e1), While (p2,s2,q2,e2) ->
    pred_eq p1 p2 && subst_eq s1 s2 && pred_eq q1 q2 && expr_eq e1 e2
  | Sequencement (s1,r1), Sequencement (s2,r2) ->
    subst_eq s1 s2 && subst_eq r1 r2
  | Parallel (s1,r1), Parallel (s2,r2) ->
    subst_eq s1 s2 && subst_eq r1 r2
  | _, _ -> false

and subst_list_eq l1 l2 : bool =
  try List.for_all2 subst_eq l1 l2
  with Invalid_argument _ -> false

let rec mk_sequence s1 s2 =
  match s1 with
  | Sequencement (r1,r2) -> mk_sequence r1 (mk_sequence r2 s2)
  | _ -> Sequencement (s1,s2)

let rec mk_parallel s1 s2 =
  match s1 with
  | Parallel (r1,r2) -> mk_parallel r1 (mk_parallel r2 s2)
  | _ -> Parallel (s1,s2)

let map_opt f = function
  | None -> None
  | Some x -> Some (f x)

let rec norm_subst : substitution -> substitution = function
  | Skip as s -> s
  | BeginEnd s -> norm_subst s
  | Affectation (xlst,(e,elst)) ->
    Affectation (xlst,(norm_expr e,List.map norm_expr elst))
  | Function_Affectation (id,(a,alst),e) ->
    Function_Affectation (id,(norm_expr a,List.map norm_expr alst),norm_expr e)
  | Record_Affectation (id,fd,e) -> Record_Affectation (id,fd,norm_expr e)
  | Pre (p,s) -> Pre (norm_pred p,norm_subst s)
  | Assert (p,s) -> Assert (norm_pred p,norm_subst s)
  | Choice ((s,slst)) -> Choice (norm_subst s,List.map norm_subst slst)
  | IfThenElse ((ps,pslst),opt) ->
    let aux (p,s) = (norm_pred p,norm_subst s) in
    IfThenElse ((aux ps,List.map aux pslst),map_opt norm_subst opt)
  | Select ((ps,pslst),opt) ->
    let aux (p,s) = (norm_pred p,norm_subst s) in
    Select ((aux ps,List.map aux pslst),map_opt norm_subst opt)
  | Case (c,(es,eslst),opt) ->
    let aux (e,s) = (norm_expr e,norm_subst s) in
    Case (norm_expr c,(aux es,List.map aux eslst),map_opt norm_subst opt)
  | Any (xlst,p,s) -> Any (xlst,norm_pred p,norm_subst s)
  | Let (xlst,(ie,ielst),s) ->
    let aux (id,e) = (id,norm_expr e) in
    Let (xlst,(aux ie,List.map aux ielst),norm_subst s)
  | BecomesElt (xlst,e) -> BecomesElt (xlst,norm_expr e)
  | BecomesSuch (xlst,p) -> BecomesSuch (xlst,norm_pred p)
  | Var (xlst,s) -> Var (xlst,norm_subst s)
  | CallUp (xlst,f,args) -> CallUp (xlst,f,List.map norm_expr args)
  | While (p,s,q,e) -> While (norm_pred p,norm_subst s,norm_pred q,norm_expr e)
  | Sequencement (s1,s2) -> mk_sequence (norm_subst s1) (norm_subst s2)
  | Parallel (s1,s2) -> mk_parallel (norm_subst s1) (norm_subst s2)
