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


open Format

(*
  BEGIN s=substitution END { s }
| SKIP { Skip }
| ids=ident_lst_comma AFFECTATION e=expression { Affectation ((List.hd ids,List.tl ids),expr_to_nonempty_list e) }
| id=IDENT LPAR e1=expression RPAR lst=list(LPAR e=expression RPAR {e}) AFFECTATION e2=expression
     { Function_Affectation (($startpos(id),id),(e1,lst),e2) }
| id=IDENT SQUOTE fi=IDENT AFFECTATION e=expression { Record_Affectation (($startpos(id),id),($startpos(fi),fi),e) }
| PRE p=predicate THEN s=substitution END { Pre (p,s) }
| ASSERT p=predicate THEN s=substitution END { Assert (p,s) }
| CHOICE lst=separated_nonempty_list(CASE_OR,substitution) END { Choice (List.hd lst,List.tl lst) }
| IF p=predicate THEN s=substitution ei=elsif* e=option(els) END { IfThenElse (((p,s),ei),e) }
| SELECT p=predicate THEN s=substitution w=whn* e=option(els) END { Select (((p,s),w),e) }
| CASE exp=expression OF
        EITHER e=expression THEN s=substitution
        ors=case_or+
        opt=option(els)
  END END { Case (exp,((e,s),ors),opt) }
| ANY ids=ident_lst_comma WHERE p=predicate THEN s=substitution END { Any ((List.hd ids,List.tl ids),p,s) }
| LET ids=ident_lst_comma BE eqs=separated_nonempty_list(AND,id_eq_expr) IN s=substitution END { Let ((List.hd ids,List.tl ids),(List.hd eqs,List.tl eqs),s) }
| ids=ident_lst_comma BECOMES_ELT e=expression { BecomesElt ((List.hd ids,List.tl ids),e) }
| ids=ident_lst_comma MEMBER_OF LPAR p=predicate RPAR { BecomesSuch ((List.hd ids,List.tl ids),p) }
| VAR ids=ident_lst_comma IN s=substitution END { Var ((List.hd ids,List.tl ids),s) }
| c=callup_subst { c }
| WHILE cond=predicate DO s=substitution INVARIANT inv=predicate VARIANT var=expression END { While (cond,s,inv,var) }
| s1=substitution SEMICOLON s2=substitution { Sequencement (s1,s2) }
| s1=substitution PARALLEL s2=substitution { Parallel (s1,s2) }
   *)

let rec pp_subst (out:formatter) : substitution -> unit = function
  | Skip -> fprintf out "skip"
  | BeginEnd s -> fprintf out "BEGIN %a END" pp_subst s
  | Affectation ((x,xlst),(e,elst)) -> assert false (*FIXME ids := elst *)
  | Function_Affectation (id,(a,alst),e) -> assert false (*FIXME id(lst)(lst) := e*)
  | Record_Affectation (id,fd,e) -> assert false (*FIXME id'fd := e *)
  | Pre (p,s) -> assert false (*FIXME PRE THEN END *)
  | Assert (p,s) -> assert false (*FIXME ASSERT THEN END *)
  | Choice ((s,slst)) -> assert false (*FIXME CHOICE s (OR s)* END *)
  | IfThenElse ((ps,pslst),opt) -> assert false (*FIXME IF c THEN s (ELSIF c THEN s) ELSE s END *)
  | Select ((ps,pslst),opt) -> assert false (*FIXME SELECT p THEN s (WHEN p THEN s)* ELSE s END*)
  | Case (e,(es,eslst),opt) -> assert false (*FIXME CASE e OF EITHER e THEN s (OR e THEN s) ELSE END END*)
  | Any ((x,xlst),p,s) -> assert false (*FIXME ANY WHERE THEN END*)
  | Let ((x,xlst),(ie,ielst),s) -> assert false (*FIXME LET ids BE id=e (AND id=e)* IN s END *)
  | BecomesElt ((x,xlst),e) -> assert false (*FIXME :: *)
  | BecomesSuch ((x,lst),p) -> assert false (*FIXME :( ) *)
  | Var ((x,xlst),s) -> assert false (*FIXME VAR IN END*)
  | CallUp (ids,f,lst) -> assert false (*FIXME lst <-- Op(lst) *)
  | While (p,s,q,e) -> assert false (*FIXME WHILE DO INVARIANT VARIANT END*)
  | Sequencement (s1,s2) -> assert false (*FIXME ; *)
  | Parallel (s1,s2) -> assert false (*FIXME || *)

