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

open Easy_format

let mk_atom s = Atom (s,atom)
let mk_label a b = Label ((a,label),b)
let mk_list_1 lst = List (("(",",",")",list),lst)
let mk_list_2 lst = List (("","","",list),lst)

let ef_ident_non_empty_list (x,xlst) =
  let lst = List.map (fun id -> mk_atom (snd id)) (x::xlst) in
  List (("",",","",list),lst)

let ef_ident_non_empty_list_2 (x,xlst) =
  let lst = List.map (fun id -> mk_atom (snd id)) (x::xlst) in
  List (("(",",",")",list),lst)

let rec ef_subst : substitution -> Easy_format.t = function
  | Skip -> mk_atom "skip"
  | BeginEnd s -> List (("BEGIN","","END",list),[ef_subst s])
  | Affectation (xlst,(e,elst)) ->
    let lst = List(("",",","",list),List.map ef_expr (e::elst)) in
    List(("","","",list),[ef_ident_non_empty_list xlst;mk_atom ":=";lst])
  | Function_Affectation (id,(a,alst),e) ->
    let lst_args = List(("(",",",")",list),List.map ef_expr (a::alst)) in
    let lf = mk_label (mk_atom (snd id)) lst_args in
    List(("","","",list),[lf;mk_atom ":=";ef_expr e])
  | Record_Affectation (id,fd,e) ->
    let lf = List(("","'","",list),[mk_atom (snd id);mk_atom (snd fd)]) in
    List(("","","",list),[lf;mk_atom ":=";ef_expr e])
  | Pre (p,s) ->
    List(("","","",list),
         [mk_atom "PRE";ef_pred p;mk_atom "THEN";ef_subst s;mk_atom "END"])
  | Assert (p,s) ->
    List(("","","",list),
         [mk_atom "ASSERT";ef_pred p;mk_atom "THEN";ef_subst s;mk_atom "END"])
  | Choice ((s,slst)) ->
    List(("CHOICE","OR","END",list), List.map ef_subst (s::slst))
  | IfThenElse ((ps,pslst),opt) ->
    let ef_if (p,s) =
      mk_label
        (List (("","","",list), [mk_atom "IF"; ef_pred p;mk_atom "THEN"]))
        (ef_subst s)
    in
    let ef_elsif (p,s) =
      mk_label
        (List (("","","",list), [mk_atom "ELSIF"; ef_pred p;mk_atom "THEN"]))
        (ef_subst s)
    in
    let ef_else = match opt with
      | None -> []
      | Some s -> [mk_label (mk_atom "ELSE") (ef_subst s)]
    in
    List(("","","",list), (ef_if ps::(List.map ef_elsif pslst))@ef_else@[mk_atom "END"])
  | Select ((ps,pslst),opt) ->
    let ef_ps (p,s) =
      List (("","","",list), [ef_pred p;mk_atom "THEN";ef_subst s])
    in
    let ef_when (p,s) =
      List (("","","",list), [mk_atom "WHEN";ef_pred p;mk_atom "THEN";ef_subst s])
    in
    let ef_else = match opt with
      | None -> []
      | Some s -> [mk_label (mk_atom "ELSE") (ef_subst s)]
    in
    List (("SELECT","","END",list),ef_ps ps::(List.map ef_when pslst)@ef_else)
  | Case (e,(es,eslst),opt) ->
    let ef_either (e,s) =
      mk_label
        (List (("","","",list), [mk_atom "EITHER";ef_expr e;mk_atom "THEN"]))
        (ef_subst s)
    in
    let ef_or (e,s) =
      mk_label
        (List (("","","",list), [mk_atom "OR";ef_expr e;mk_atom "THEN"]))
        (ef_subst s)
    in
    let ef_else =
      match opt with
      | None -> []
      | Some s -> [mk_label (mk_atom "ELSE") (ef_subst s)]
    in
    let lst = (ef_either es)::(List.map ef_or eslst)@ef_else@[mk_atom "END"] in
    let cs = mk_label
        (List (("","","",list),[mk_atom "CASE";ef_expr e;mk_atom "OF"]))
        (List (("","","",list),lst))
    in
    List (("","","",list),[cs;mk_atom "END"])
  | Any (xlst,p,s) ->
    List(("","","",list), [mk_atom "ANY";
                           ef_ident_non_empty_list xlst;
                           mk_atom "WHERE";
                           ef_pred p;
                           mk_atom "THEH";
                           ef_subst s;
                           mk_atom "END"])
  | Let (xlst,(ie,ielst),s) ->
    let ef_eq (id,e) = List (("","=","",list),[mk_atom (snd id);ef_expr e]) in
    List(("","","",list), [mk_atom "LET";
                           ef_ident_non_empty_list xlst;
                           mk_atom "BE";
                           List (("","AND","",list),List.map ef_eq (ie::ielst));
                           mk_atom "IN";
                           ef_subst s;
                           mk_atom "END"])
  | BecomesElt (xlst,e) ->
    List(("","","",list),[ef_ident_non_empty_list xlst;
                          mk_atom "::";
                          ef_expr e])
  | BecomesSuch (xlst,p) ->
    mk_label (ef_ident_non_empty_list xlst) (List((":(","",")",list),[ef_pred p]))
  | Var (xlst,s) ->
    List(("","","",list), [mk_atom "VAR";
                           ef_ident_non_empty_list xlst;
                           mk_atom "IN";
                           ef_subst s;
                           mk_atom "END"])
  | CallUp ([],f,lst) ->
    mk_label (mk_atom (snd f)) (List(("(",",",")",list),List.map ef_expr lst))
  | CallUp ((x::xlst),f,lst) ->
    let lf = ef_ident_non_empty_list (x,xlst) in
    let rg = mk_label (mk_atom (snd f)) (List(("(",",",")",list),List.map ef_expr lst)) in
    List(("","<--","",list),[lf;rg])
  | While (p,s,q,e) ->
    List(("","","",list),[mk_atom "WHILE";
                          ef_pred p;
                          mk_atom "DO";
                          ef_subst s;
                          mk_atom "INVARIANT";
                          ef_pred q;
                          mk_atom "VARIANT";
                          ef_expr e;
                          mk_atom "END"])
  | Sequencement (s1,s2) -> List (("",";","",list),[ef_subst s1;ef_subst s2]) (*FIXME parenthesese*)
  | Parallel (s1,s2) -> List (("","||","",list),[ef_subst s1;ef_subst s2])
