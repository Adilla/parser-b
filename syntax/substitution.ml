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

let mk_sequence lst =
  List(("","","",{list with align_closing=false;space_after_opening=false;space_before_closing=false}),lst)

let mk_sequence_nl lst =
  List(("","","",{list with space_after_opening=false;
                            space_before_closing=false;
                            align_closing=false;
                            wrap_body=`Force_breaks;
                 }),lst)

let mk_ident_list_comma (lst:ident list) : Easy_format.t =
    let lst = List.map (fun id -> mk_atom (snd id)) lst in
    List (("",",","",{list with align_closing=false;
                                space_after_opening=false;
                                space_before_closing=false})
         ,lst)

let rec get_seq_list = function
  | Sequencement (s1,s2) -> (get_seq_list s1)@(get_seq_list s2)
  | s -> [s]

let rec get_par_list = function
  | Parallel (s1,s2) -> (get_par_list s1)@(get_par_list s2)
  | s -> [s]

let rec ef_subst : substitution -> Easy_format.t = function
  | Skip -> mk_atom "skip"
  | BeginEnd s -> List (("BEGIN","","END",list),[ef_subst s])

  | Affectation ((x,xlst),(e,elst)) ->
    let lst = List(("",",","",{list with space_after_opening=false;
                                         align_closing=false;
                                         space_before_closing=false}),
                   List.map ef_expr (e::elst)) in
    mk_sequence [mk_ident_list_comma (x::xlst);mk_atom ":=";lst]

  | Function_Affectation (id,(a,alst),e) ->
    let lst_args = List(("(",",",")",list),List.map ef_expr (a::alst)) in
    let lf = Label ((mk_atom (snd id),label), lst_args) in
    mk_sequence [lf;mk_atom ":=";ef_expr e]

  | Record_Affectation (id,fd,e) ->
    let lf = mk_atom (snd id ^ "'" ^ snd fd) in
    mk_sequence [lf;mk_atom ":=";ef_expr e]

  | Pre (p,s) ->
    let lb = {label with label_break=`Always} in
    mk_sequence_nl [
      Label((mk_atom "PRE",lb),ef_pred p);
      Label((mk_atom "THEN",lb),ef_subst s);
      mk_atom "END"]

  | Assert (p,s) ->
    let lb = {label with label_break=`Always} in
    mk_sequence_nl [ Label((mk_atom "ASSERT",lb),ef_pred p);
                     Label((mk_atom "THEN",lb),ef_subst s);
                     mk_atom "END"]

  | Choice ((s,slst)) ->
    let lb = {label with label_break=`Always} in
    mk_sequence_nl (
      Label((mk_atom "CHOICE",lb),ef_subst s)::
      (List.map (fun s -> Label((mk_atom "OR",lb),ef_subst s)) slst)
      @[mk_atom "END"])

  | IfThenElse (((p,s),pslst),opt) ->
    let lb = {label with label_break=`Always;space_after_label=false} in
    let clst =
      [ [Label((mk_atom "IF",lb),ef_pred p);
         Label((mk_atom "THEN",lb),ef_subst s)] ]
      @
      (List.map (fun (p,s) ->
           [Label((mk_atom "ELSIF",lb),ef_pred p);
            Label((mk_atom "THEN",lb),ef_subst s)]
         ) pslst)
      @ (match opt with
          | None -> []
          | Some s -> [[Label((mk_atom "ELSE",lb),ef_subst s)]] )
      @ [[mk_atom "END"]]
    in
    mk_sequence_nl (List.concat clst)

  | Select (((p,s),pslst),opt) ->
    let lb = {label with label_break=`Always} in
    let clst =
      [ [Label((mk_atom "SELECT",lb),ef_pred p);
         Label((mk_atom "THEN",lb),ef_subst s)] ]
      @
      (List.map (fun (p,s) ->
           [Label((mk_atom "WHEN",lb),ef_pred p);
            Label((mk_atom "THEN",lb),ef_subst s)]
         ) pslst)
      @ (match opt with
          | None -> []
          | Some s -> [[Label((mk_atom "ELSE",lb),ef_subst s)]] )
      @ [[mk_atom "END"]]
    in
    mk_sequence_nl (List.concat clst)

  | Case (c,((e,s),eslst),opt) ->
    let lb = {label with label_break=`Always} in
    let clst =
      [ [Label((mk_atom "EITHER",lb),ef_expr e);
         Label((mk_atom "THEN",lb),ef_subst s)] ]
      @
      (List.map (fun (e,s) ->
           [Label((mk_atom "OR",lb),ef_expr e);
            Label((mk_atom "THEN",lb),ef_subst s)]
         ) eslst)
      @ (match opt with
          | None -> []
          | Some s -> [[Label((mk_atom "ELSE",lb),ef_subst s)]] )
      @ [[mk_atom "END"]]
    in
    let clst = List.concat clst in
    let case_c = Label ((mk_atom "CASE",label),ef_expr c) in
    mk_sequence_nl [ Label((case_c,lb),mk_sequence_nl clst);
                     mk_atom "END" ]

  | Any ((x,xlst),p,s) ->
    let lb = { label with label_break=`Always } in
    mk_sequence_nl [ Label((mk_atom "ANY",lb),mk_ident_list_comma (x::xlst));
                     Label((mk_atom "WHERE",lb),ef_pred p);
                     Label((mk_atom "THEN",lb),ef_subst s);
                     mk_atom "END"]

  | Let ((x,xlst),(ie,ielst),s) ->
    let lb = { label with label_break=`Always } in
    let lst = { list with space_after_opening=false; space_before_closing=false; align_closing=false; } in
    let ef_eq (id,e) = mk_sequence [mk_atom (snd id);mk_atom "=";ef_expr e] in
    let defs = List(("","AND","",lst),List.map ef_eq (ie::ielst)) in
    mk_sequence_nl [ Label((mk_atom "LET",lb),mk_ident_list_comma (x::xlst));
                     Label((mk_atom "BE",lb),defs);
                     Label((mk_atom "IN",lb),ef_subst s);
                     mk_atom "END"]

  | BecomesElt ((x,xlst),e) ->
    mk_sequence [mk_ident_list_comma (x::xlst); mk_atom "::"; ef_expr e]

  | BecomesSuch ((x,xlst),p) ->
    Label((mk_ident_list_comma (x::xlst),label),
          List((":(","",")",list),[ef_pred p]))

  | Var ((x,xlst),s) ->
    let lb = {label with label_break=`Always} in
    mk_sequence_nl [ Label((mk_atom "VAR",lb), mk_ident_list_comma (x::xlst));
                     Label((mk_atom "IN",lb), ef_subst s);
                     mk_atom "END"]

  | CallUp (xlst,f,args) ->
    let lst = {list with align_closing=false;
                         space_after_opening=false;
                         space_before_closing=false}
    in
    let lb = {label with space_after_label=false} in
    begin match xlst, args with
      | [], [] -> mk_atom (snd f)
      | [], _::_ ->
        let args = List(("(",",",")",lst), List.map ef_expr args) in
        Label((mk_atom (snd f),lb),args)

      | _::_, [] ->
        mk_sequence [mk_ident_list_comma xlst;
                     mk_atom "<--";
                     mk_atom (snd f)]

      | _::_, _::_ ->
        let args = List(("(",",",")",lst), List.map ef_expr args) in
        mk_sequence [mk_ident_list_comma xlst;
                     mk_atom "<--";
                     Label((mk_atom (snd f),lb),args)]
    end

  | While (p,s,q,e) ->
    let lb = {label with label_break=`Always} in
    mk_sequence_nl [ Label((mk_atom "WHILE",lb), ef_pred p);
                     Label((mk_atom "DO",lb), ef_subst s);
                     Label((mk_atom "INVARIANT",lb), ef_pred q);
                     Label((mk_atom "VARIANT",lb), ef_expr e);
                     mk_atom "END"]

  | Sequencement _ as s ->
    let seqs = List.map ef_subst (get_seq_list s) in
    let lst = { list with space_after_opening=false; align_closing=false; space_before_closing=false; indent_body=0; wrap_body=`Force_breaks; space_before_separator=true } in
    List (("",";","",lst),seqs) (*FIXME parenthesese*)

  | Parallel _ as s ->
    let pars = List.map ef_subst (get_par_list s) in
    let lst = { list with space_after_opening=false; align_closing=false; space_before_closing=false; indent_body=0; wrap_body=`Force_breaks; space_before_separator=true } in
    List (("","||","",lst),pars) (*FIXME parenthesese*)

