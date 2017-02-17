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

let rec pp_subst (out:formatter) : substitution -> unit = function
  | Skip -> fprintf out "skip"
  | BeginEnd s -> fprintf out "BEGIN %a END" pp_subst s
  | Affectation ((x,xlst),(e,elst)) ->
    fprintf out "%a := %a" pp_ident_list (x::xlst) pp_expr_list (e::elst)
  | Function_Affectation (id,(a,alst),e) ->
    let pp_aux out e = fprintf out "(%a)" pp_expr e in
    fprintf out "%a%a := %a" pp_ident id (pp_print_list pp_aux) (a::alst) pp_expr e
  | Record_Affectation (id,fd,e) ->
    fprintf out "%a'%a := %a" pp_ident id pp_ident fd pp_expr e
  | Pre (p,s) -> fprintf out "PRE %a THEN %a END" pp_pred p pp_subst s
  | Assert (p,s) -> fprintf out "ASSERT %a THEN %a END" pp_pred p pp_subst s
  | Choice ((s,slst)) ->
    let pp_sep out () = fprintf out " OR " in
    fprintf out "CHOICE %a END" (pp_print_list ~pp_sep pp_subst) (s::slst)
  | IfThenElse ((ps,pslst),opt) ->
    let pp_sep out () = fprintf out " ELSIF " in
    let pp_elsif out (p,s) = fprintf out "%a THEN %a" pp_pred p pp_subst s in
    fprintf out "IF %a %a END" (pp_print_list ~pp_sep pp_elsif) (ps::pslst) pp_else opt
  | Select ((ps,pslst),opt) ->
    let pp_sep out () = fprintf out " WHEN " in
    let pp_when out (p,s) = fprintf out "%a THEN %a" pp_pred p pp_subst s in
    fprintf out "SELECT %a %a END" (pp_print_list ~pp_sep pp_when) (ps::pslst) pp_else opt
  | Case (e,(es,eslst),opt) ->
    let pp_sep out () = fprintf out " OR " in
    let pp_or out (e,s) = fprintf out "%a THEN %a" pp_expr e pp_subst s in
    fprintf out "CASE %a OF EITHER %a %a END END"
      pp_expr e (pp_print_list ~pp_sep pp_or) (es::eslst) pp_else opt
  | Any ((x,xlst),p,s) ->
    fprintf out "ANY %a WHERE %a THEN %a END" pp_ident_list (x::xlst) pp_pred p pp_subst s
  | Let ((x,xlst),(ie,ielst),s) ->
    let pp_sep out () = fprintf out " AND " in
    let pp_eq out (id,e) = fprintf out "%a = %a" pp_ident id pp_expr e in
    fprintf out "LET %a BE %a IN %a END" pp_ident_list (x::xlst)
      (pp_print_list ~pp_sep pp_eq) (ie::ielst) pp_subst s
  | BecomesElt ((x,xlst),e) ->
    fprintf out "%a :: %a" pp_ident_list (x::xlst) pp_expr e
  | BecomesSuch ((x,xlst),p) ->
    fprintf out "%a :( %a )" pp_ident_list (x::xlst) pp_pred p
  | Var ((x,xlst),s) ->
    fprintf out "VAR %a IN %a END" pp_ident_list (x::xlst) pp_subst s
  | CallUp (ids,f,lst) ->
    fprintf out "%a <-- %a(%a)" pp_ident_list ids pp_ident f pp_expr_list lst
  | While (p,s,q,e) ->
    fprintf out "WHILE %a DO %a INVARIANT %a VARIANT %a END"
      pp_pred p pp_subst s pp_pred q pp_expr e
  | Sequencement (s1,s2) -> fprintf out "%a; %a" pp_subst s1 pp_subst s2
  | Parallel (s1,s2) -> fprintf out "%a || %a" pp_subst s1 pp_subst s2

and pp_else (out:formatter) (opt:substitution option) : unit =
  match opt with
  | None -> ()
  | Some s -> fprintf out "ELSE %a" pp_subst s
