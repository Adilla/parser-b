open Utils
open Easy_format
open Expression

let mk_atom s = Atom (s,atom)
let mk_label a b = Label ((a,{label with space_after_label=false}),b)

let add_par : expression -> expression = function
  | Application _ | Couple _  | Record_Field_Access _ as e -> Parentheses (dloc,e)
  | Ident _ | Dollar _ | Pbool _ | Builtin _ | Parentheses _ | Comprehension _
  | Binder _ | Sequence _ | Extension _ | Record _ | Record_Type _ as e -> e

let add_par_p : predicate -> predicate = function
  | P_Ident _ | P_Builtin _ | Negation _ | Pparentheses _
  | Universal_Q _ | Existential_Q _ | Binary_Pred _ as p -> p
  | Binary_Prop _ as p -> Pparentheses (dloc,p)

let list_1 =
  { list with space_after_opening=false;
              space_before_closing=false;
              align_closing=false }

let mk_ident_list_comma (lst:ident list) : Easy_format.t =
    let lst = List.map (fun id -> mk_atom (snd id)) lst in
    List (("",",","",list_1), lst)

let rec get_and_list = function
  | Binary_Prop (_,Conjonction,p1,p2) -> (get_and_list p1)@(get_and_list p2)
  | s -> [s]

let rec get_or_list = function
  | Binary_Prop (_,Disjonction,p1,p2) -> (get_or_list p1)@(get_or_list p2)
  | s -> [s]

let rec ef_expr : expression -> Easy_format.t = function
  | Ident id -> mk_atom (snd id)
  | Dollar id -> mk_atom (snd id ^ "$0")
  | Builtin (_,bi) -> mk_atom (builtin_to_string bi)
  | Pbool (_,p) ->
    mk_label (mk_atom "bool") (List(("(","",")",list_1),[ef_pred p]))
  | Parentheses (_,e) -> List(("(","",")",list_1),[ef_expr e])
  | Application (_,Builtin (_,Inverse_Relation),e) ->
    mk_label (ef_expr (add_par e)) (mk_atom "~")
  | Application (_,Builtin (_,Unary_Minus),e) ->
    mk_label (mk_atom "-") (ef_expr (add_par e))
  | Application (_,Builtin (_,Image),Couple(_,_,e1,e2)) ->
    mk_label (ef_expr (add_par e1)) (List (("[","","]",list_1),[ef_expr e2]))
  | Application (_,Builtin (_,(Composition|Parallel_Product as bop)),Couple(_,Infix,e1,e2)) ->
    List(("(",builtin_to_string bop,")",{ list_1 with space_before_separator=true }),
         [ef_expr (add_par e1); ef_expr (add_par e2)])
  | Application (_,Builtin (_,bop),Couple(_,Infix,e1,e2)) ->
    List(("",builtin_to_string bop,"",{ list_1 with space_before_separator=true }),
         [ef_expr (add_par e1); ef_expr (add_par e2)])
  | Application (_,f,a) ->
    mk_label (ef_expr (add_par f)) (List (("(","",")",list_1),[ef_expr a]))
  | Comprehension (_,(x,xlst),p) ->
    List(("{","","}",{ list with align_closing=false}),
         [mk_ident_list_comma (x::xlst);mk_atom "|";ef_pred p])
  | Binder (l,bi,(x,xlst),p,e) ->
    let lst = List.map (fun (_,id) -> mk_atom id) (x::xlst) in
    let x = mk_label (mk_atom (binder_to_string bi)) (List(("(",",",")",list_1), lst)) in
    let y = List(("(","|",")",list_1), [ef_pred p;ef_expr e]) in
    List (("",".","",list_1),[x;y])
  | Sequence (_,(e,lst)) ->
    let lst = List.map (fun e -> ef_expr (add_par e)) (e::lst) in
    List (("[",",","]",list_1),lst)
  | Extension (_,(e,lst)) ->
    let lst = List.map (fun e -> ef_expr (add_par e)) (e::lst) in
    List (("{",",","}",list_1),lst)
  | Couple (_,Infix,e1,e2) -> assert false
  | Couple (_,Maplet,e1,e2) ->
    List(("","","",list_1),[ef_expr (add_par e1);mk_atom "|->";ef_expr (add_par e2)])
  | Couple (_,Comma,e1,e2) ->
    List(("","","",list_1),[ef_expr (add_par e1);mk_atom ",";ef_expr (add_par e2)])
  | Record_Field_Access (_,e,id) ->
    mk_label (ef_expr (add_par e)) (mk_atom ("'" ^ snd id))
  | Record (_,(f,lst)) ->
    let flst = List.map ef_struct_field (f::lst) in
    let lst = List (("(",",",")",list_1),flst) in
    mk_label (mk_atom "rec") lst
  | Record_Type (_,(f,lst)) ->
    let flst = List.map ef_struct_field (f::lst) in
    let lst = List (("(",",",")",list_1),flst) in
    mk_label (mk_atom "struct") lst

and ef_struct_field (id,e:ident*expression) : Easy_format.t =
  List(("",":","",list_1), [mk_atom (snd id);ef_expr (add_par e)])

and ef_pred : predicate -> Easy_format.t = function
  | P_Ident id -> mk_atom (snd id)
  | P_Builtin (_,Btrue) -> mk_atom "btrue"
  | P_Builtin (_,Bfalse) -> mk_atom "bfalse"
  | Binary_Prop (_,Conjonction,_,_) as p ->
    let pars = List.map (fun p -> ef_pred (add_par_p p)) (get_and_list p) in
    List (("","&","",{ list_1 with space_before_separator=true}),pars)
  | Binary_Prop (_,Disjonction,_,_) as p ->
    let pars = List.map (fun p -> ef_pred (add_par_p p)) (get_or_list p) in
    List (("","or","",{ list_1 with space_before_separator=true}),pars)
  | Binary_Prop (_,bop,p1,p2) ->
    List(("",prop_bop_to_string bop,"",{list_1 with space_before_separator=true}),
         [ef_pred (add_par_p p1); ef_pred (add_par_p p2)])
  | Binary_Pred (_,bop,e1,e2) ->
    List (("",pred_bop_to_string bop,"",{list_1 with space_before_separator=true}),
          [ef_expr e1; ef_expr e2])
  | Negation (_,p) ->
    mk_label (mk_atom "not") (List(("(","",")",list_1),[ef_pred p]))
  | Pparentheses (_,p) -> List(("(","",")",list_1),[ef_pred p])
  | Universal_Q (_,(x,xlst),p) ->
    let lst = List.map (fun (_,id) -> mk_atom id) (x::xlst) in
    let x = mk_label (mk_atom "!") (List(("(",",",")",list_1),lst)) in
    let y = List(("(","",")",list_1), [ef_pred p]) in
    List(("",".","",{list_1 with space_after_separator=false}),[x;y])
  | Existential_Q (_,(x,xlst),p) ->
    let lst = List.map (fun (_,id) -> mk_atom id) (x::xlst) in
    let x = mk_label (mk_atom "#") (List(("(",",",")",list_1),lst)) in
    let y = List(("(","",")",list_1), [ef_pred p]) in
    List(("",".","",{list_1 with space_after_separator=false}),[x;y])

(* let mk_atom s = Atom (s,atom) *)
open Substitution

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

let add_be = function
  | Skip | BeginEnd _ | Affectation _ | Function_Affectation _
  | Record_Affectation _ | Pre _ | Assert _ | Choice _ | IfThenElse _
  | Select _ | Case _ | Any _ | Let _ | BecomesElt _ | BecomesSuch _
  | Var _ | CallUp _ | While _ as s -> s
  | Sequencement _ | Parallel _ as s -> BeginEnd s

let add_begin_end_ifn = add_be

let rec ef_subst : substitution -> Easy_format.t = function
  | Skip -> mk_atom "skip"
  | BeginEnd s -> List (("BEGIN","","END",{list with stick_to_label=false;}),[ef_subst s])

  | Affectation ((x,xlst),(e,elst)) ->
    let lst = List(("",",","",{list with space_after_opening=false;
                                         align_closing=false;
                                         space_before_closing=false}),
                   List.map (fun e -> ef_expr (add_par e)) (e::elst)) in
    mk_sequence [mk_ident_list_comma (x::xlst);mk_atom ":=";lst]

  | Function_Affectation (id,(a,alst),e) ->
    let aux (e:expression) = List(("(","",")",list),[ef_expr e]) in
    let lst_args = List(("","","",list),List.map aux (a::alst)) in
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
    let lb = {label with label_break=`Always_rec; space_after_label=false} in
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
      [ [ (mk_atom "OF");
          Label((mk_atom "EITHER",lb),ef_expr e);
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
    let defs = List(("","&","",lst),List.map ef_eq (ie::ielst)) in
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
        let args = List(("(",",",")",lst), List.map (fun e -> ef_expr (add_par e)) args) in
        Label((mk_atom (snd f),lb),args)

      | _::_, [] ->
        mk_sequence [mk_ident_list_comma xlst;
                     mk_atom "<--";
                     mk_atom (snd f)]

      | _::_, _::_ ->
        let args = List(("(",",",")",lst), List.map (fun e -> ef_expr (add_par e)) args) in
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
    let seqs = List.map (fun s -> ef_subst (add_be s)) (get_seq_list s) in
    let lst = { list with space_after_opening=false; align_closing=false;
                          space_before_closing=false; indent_body=0;
                          wrap_body=`Force_breaks; space_before_separator=true } in
    List (("",";","",lst),seqs)

  | Parallel _ as s ->
    let pars = List.map (fun s -> ef_subst (add_be s)) (get_par_list s) in
    let lst = { list with space_after_opening=false; align_closing=false;
                          space_before_closing=false; indent_body=0;
                          wrap_body=`Force_breaks; space_before_separator=true } in
    List (("","||","",lst),pars)

open Component

(* let mk_atom s = Easy_format.Atom (s,Easy_format.atom) *)
let mk_sequence lst =
  List(("","","",{list with align_closing=false;space_after_opening=false;space_before_closing=false}),lst)

let mk_clause (cname:string) (n:Easy_format.t) =
  Label ((mk_atom cname,
          {label with label_break=`Always;space_after_label=false}),n)

let mk_ident_list_comma (lst:ident list) : Easy_format.t =
    let lst = List.map (fun id -> mk_atom (snd id)) lst in
    List (("",",","",{list with align_closing=false;
                                space_after_opening=false;
                                space_before_closing=false})
         ,lst)

let ef_minst (id,args:machine_instanciation) : Easy_format.t =
  match args with
  | [] -> mk_atom (snd id)
  | _::_ ->
    Label ((mk_atom (snd id),label),
           List(("(",",",")",list),
                List.map (fun e -> ef_expr (add_par e)) args))

let ef_set (x:set) : Easy_format.t =
  match x with
  | Abstract_Set id -> mk_atom (snd id)
  | Concrete_Set (id,lst) ->
    let enums = List(("{",",","}",list),
                     List.map (fun id -> mk_atom (snd id)) lst) in
    List(("","","",list),[mk_atom (snd id);mk_atom "=";enums])

let ef_operation (out,name,args,body:operation) : Easy_format.t =
  let name_args =
    match args with
    | [] -> mk_atom (snd name)
    | _::_ ->
      let lst = {list with align_closing=false;
                           space_after_opening=false;
                           space_before_closing=false}
      in
      let args = List(("(",",",")",lst),
                      List.map (fun a -> mk_atom (snd a)) args) in
      Label((mk_atom (snd name),label),args)
  in
  let spec = match out with
    | [] -> mk_sequence [name_args; mk_atom "="]
    | _::_ -> mk_sequence [mk_ident_list_comma out; mk_atom "<--"; name_args; mk_atom "="]
  in
  let lbl = {label with label_break=`Always;
                        indent_after_label=0;
                        space_after_label=false } in
  Label((spec, lbl), ef_subst (add_begin_end_ifn body))

let ef_op_list lst =
  List(("",";\n","",{list with align_closing=false;space_after_opening=false;space_before_closing=false}),
       List.map ef_operation lst)

let ef_pred_list lst =
  List(("",";","",{list with align_closing=false;space_after_opening=false;space_before_closing=false}),
       List.map ef_pred lst)

let ef_set_list lst =
  List(("",";","",{list with align_closing=false;space_after_opening=false;space_before_closing=false}),
       List.map ef_set lst)

let ef_minst_list lst =
  List(("",",","",{list with align_closing=false;space_after_opening=false;space_before_closing=false}),
       List.map ef_minst lst)

let ef_value_list lst =
  let ef (id,e) = mk_sequence [mk_atom (snd id);mk_atom "=";ef_expr e] in
  (List(("",";","",{list with align_closing=false;space_after_opening=false;space_before_closing=false}),
        List.map ef lst))

let mk_machine_name name params =
  match params with
  | [] -> mk_atom (snd name)
  | _::_ -> Label((mk_atom (snd name),{label with space_after_label=false}),
                  List(("(",",",")",list),List.map (fun (_,p) -> mk_atom p) params))

let mk_clause_list lst =
  List (("","\n","",
         {list with indent_body=0;
                    space_after_opening=false;
                    space_after_separator=false;
                    align_closing=false}),
        lst)

let ef_clause : clause -> Easy_format.t = function
  | Constraints (_,p) -> mk_clause "CONSTRAINTS" (ef_pred p)
  | Imports (_,lst) -> mk_clause "IMPORTS" (ef_minst_list lst)
  | Includes (_,lst) -> mk_clause "INCLUDES" (ef_minst_list lst)
  | Extends (_,lst) -> mk_clause "EXTENDS" (ef_minst_list lst)
  | Properties (_,p) -> mk_clause "PROPERTIES" (ef_pred p)
  | Invariant (_,p) -> mk_clause "INVARIANT" (ef_pred p)
  | Assertions (_,lst) -> mk_clause "ASSERTIONS" (ef_pred_list lst)
  | Initialization (_,s) -> mk_clause "INITIALISATION" (ef_subst s)
  | Operations (_,lst) -> mk_clause "OPERATIONS" (ef_op_list lst)
  | Local_Operations (_,lst) -> mk_clause "LOCAL_OPERATIONS" (ef_op_list lst)
  | Values (_,lst) -> mk_clause "VALUES" (ef_value_list lst)
  | Sees (_,lst) -> mk_clause "SEES" (mk_ident_list_comma lst)
  | Promotes (_,lst) -> mk_clause "PROMOTES" (mk_ident_list_comma lst)
  | Uses (_,lst) -> mk_clause "USES" (mk_ident_list_comma lst)
  | Sets (_,lst) -> mk_clause "SETS" (ef_set_list lst)
  | Constants (_,lst) -> mk_clause "CONSTANTS" (mk_ident_list_comma lst)
  | Abstract_constants (_,lst) -> mk_clause "ABSTRACT_CONSTANTS" (mk_ident_list_comma lst)
  | Concrete_variables (_,lst) -> mk_clause "CONCRETE_VARIABLES" (mk_ident_list_comma lst)
  | Variables (_,lst) -> mk_clause "VARIABLES" (mk_ident_list_comma lst)

let ef_machine (mch:abstract_machine) : Easy_format.t =
  let machine = mk_clause "MACHINE" (mk_machine_name mch.name mch.parameters) in
  let lst = List.rev_map ef_clause (clist_of_mch mch) in
  let ed = [mk_atom "END"] in
  mk_clause_list (machine::(lst@ed))

let ef_refinement (ref:refinement) : Easy_format.t =
  let refinement = mk_clause "REFINEMENT" (mk_machine_name ref.name ref.parameters) in
  let refines = mk_clause "REFINES" (mk_atom (snd ref.refines)) in
  let lst = List.rev_map ef_clause (clist_of_ref ref) in
  let ed = [mk_atom "END"] in
  mk_clause_list (refinement::refines::(lst@ed))

let ef_implementation (imp:implementation) : Easy_format.t =
  let implementation = mk_clause "IMPLEMENTATION" (mk_machine_name imp.name imp.parameters) in
  let refines = mk_clause "REFINES" (mk_atom (snd imp.refines)) in
  let lst = List.rev_map ef_clause (clist_of_imp imp) in
  let ed = [mk_atom "END"] in
  mk_clause_list (implementation::refines::(lst@ed))

let ef_component : component -> Easy_format.t = function
  | Abstract_machine x -> ef_machine x
  | Refinement x -> ef_refinement x
  | Implementation x -> ef_implementation x
