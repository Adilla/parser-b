open Utils
open Expression

type ('lc,'ty) substitution =
  | Skip of 'lc
  | Affectation of 'lc * 'lc ident non_empty_list * ('lc,'ty) expression
  | Function_Affectation of 'lc * 'lc ident * ('lc,'ty) expression non_empty_list * ('lc,'ty) expression
  | Record_Affectation of 'lc * 'lc ident * 'lc ident * ('lc,'ty) expression (** record'field := e *)
  | Pre of 'lc * ('lc,'ty) predicate * ('lc,'ty) substitution
  | Assert of 'lc * ('lc,'ty) predicate * ('lc,'ty) substitution
  | Choice of 'lc * ('lc,'ty) substitution non_empty_list
  | IfThenElse of 'lc * (('lc,'ty) predicate * ('lc,'ty) substitution) non_empty_list * ('lc,'ty) substitution option
  | Select of 'lc * (('lc,'ty) predicate * ('lc,'ty) substitution) non_empty_list * ('lc,'ty) substitution option
  | Case of 'lc * ('lc,'ty) expression * (('lc,'ty) expression * ('lc,'ty) substitution) non_empty_list * ('lc,'ty) substitution option
  | Any of 'lc * 'lc ident non_empty_list * ('lc,'ty) predicate * ('lc,'ty) substitution
  | Let of 'lc * 'lc ident non_empty_list * ('lc ident * ('lc,'ty) expression) non_empty_list * ('lc,'ty) substitution
  | BecomesElt of 'lc * 'lc ident non_empty_list * ('lc,'ty) expression
  | BecomesSuch of 'lc * 'lc ident non_empty_list * ('lc,'ty) predicate
  | Var of 'lc * 'lc ident non_empty_list * ('lc,'ty) substitution
  | CallUp of 'lc * 'lc ident list * 'lc ident * ('lc,'ty) expression list
  | While of 'lc * ('lc,'ty) predicate * ('lc,'ty) substitution * ('lc,'ty) predicate * ('lc,'ty) expression
  | Sequencement of 'lc * ('lc,'ty) substitution * ('lc,'ty) substitution
  | Parallel of 'lc * ('lc,'ty) substitution * ('lc,'ty) substitution

type u_subst = (loc,bool) substitution

let rec subst_eq : type a b c d. (a,b) substitution -> (c,d) substitution -> bool = fun s1 s2 ->
  match s1,s2 with
  | Skip _, Skip _ -> true
  | Affectation (_,(hd1,tl1),e1), Affectation (_,(hd2,tl2),e2) ->
     begin
       try List.for_all2 ident_eq (hd1::tl1) (hd2::tl2) && expr_eq e1 e2
       with Invalid_argument _ -> false
     end
  | Function_Affectation (_,f1,(e1,elst1),a1), Function_Affectation (_,f2,(e2,elst2),a2) ->
    begin
      try
        ident_eq f1 f2 && expr_eq e1 e2 && expr_eq a1 a1 &&
        List.for_all2 expr_eq elst1 elst2
      with Invalid_argument _ -> false
    end
  | Record_Affectation (_,id1,fd1,e1), Record_Affectation (_,id2,fd2,e2) ->
    ident_eq id1 id2 && ident_eq fd1 fd2 && expr_eq e1 e2
  | Pre (_,p1,s1), Pre (_,p2,s2) -> pred_eq p1 p2 && subst_eq s1 s2
  | Assert (_,p1,s1), Assert (_,p2,s2) -> pred_eq p1 p2 && subst_eq s1 s2
  | Choice (_,(s1,slst1)), Choice (_,(s2,slst2)) -> subst_list_eq (s1::slst1) (s2::slst2)
  | IfThenElse (_,(y1,ylst1),opt1), IfThenElse (_,(y2,ylst2),opt2) ->
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
  | Select (_,(y1,ylst1),opt1), Select (_,(y2,ylst2),opt2) ->
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
  | Case (_,e1,(y1,ylst1),opt1), Case (_,e2,(y2,ylst2),opt2) ->
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
  | Any (_,lst1,p1,s1), Any (_,lst2,p2,s2) ->
    ident_nelist_eq lst1 lst2 && pred_eq p1 p2 && subst_eq s1 s2
  | Let (_,(x1,xlst1),(y1,ylst1),s1), Let (_,(x2,xlst2),(y2,ylst2),s2) ->
    let aux l1 l2 =
      try List.for_all2
            (fun (id1,e1) (id2,e2) -> ident_eq id1 id2 && expr_eq e1 e2)
            l1 l2
      with Invalid_argument _ -> false
    in
    ident_list_eq (x1::xlst1) (x2::xlst2) && aux (y1::ylst1) (y2::ylst2) && subst_eq s1 s2
  | BecomesElt (_,lst1,e1), BecomesElt (_,lst2,e2) -> ident_nelist_eq lst1 lst2 && expr_eq e1 e2
  | BecomesSuch (_,lst1,p1), BecomesSuch (_,lst2,p2) -> ident_nelist_eq lst1 lst2 && pred_eq p1 p2
  | Var (_,lst1,s1), Var (_,lst2,s2) -> ident_nelist_eq lst1 lst2 && subst_eq s1 s2
  | CallUp (_,xlst1,op1,elst1), CallUp (_,xlst2,op2,elst2) ->
    ident_list_eq xlst1 xlst2 && ident_eq op1 op2 && expr_list_eq elst1 elst2
  | While (_,p1,s1,q1,e1), While (_,p2,s2,q2,e2) ->
    pred_eq p1 p2 && subst_eq s1 s2 && pred_eq q1 q2 && expr_eq e1 e2
  | Sequencement (_,s1,r1), Sequencement (_,s2,r2) ->
    subst_eq s1 s2 && subst_eq r1 r2
  | Parallel (_,s1,r1), Parallel (_,s2,r2) ->
    subst_eq s1 s2 && subst_eq r1 r2
  | _, _ -> false

and subst_list_eq : type a b c d. (a,b) substitution list -> (c,d) substitution list -> bool = fun l1 l2 ->
  try List.for_all2 subst_eq l1 l2
  with Invalid_argument _ -> false
