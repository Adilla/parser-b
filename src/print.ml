open Easy_format
open SyntaxCore
open PSyntax

let mk_atom (s:string) : Easy_format.t = Atom (s,atom)
let mk_label (a:Easy_format.t) (b:Easy_format.t) : Easy_format.t =
  Label ((a,{label with space_after_label=false}),b)

let list_1 =
  { list with space_after_opening=false;
              space_before_closing=false;
              align_closing=false }

let rec get_and_list p =
  match p.prd_desc with
  | Binary_Prop (Conjonction,p1,p2) -> (get_and_list p1)@[p2]
  | _ -> [p]

let rec get_or_list p =
  match p.prd_desc with
  | Binary_Prop (Disjonction,p1,p2) -> (get_or_list p1)@[p2]
  | _ -> [p]

let mk_atom_from_lident (s:lident) : Easy_format.t = Atom (s.lid_str,atom)

let mk_string_list_comma (lst:lident list) : Easy_format.t =
  List (("",",","",list_1), List.map mk_atom_from_lident lst)

let mk_string_nelist_comma (nlst:lident Nlist.t) : Easy_format.t =
  mk_string_list_comma (Nlist.to_list nlst)

let mk_string_nelist_comma_par (nlst:lident Nlist.t) : Easy_format.t =
  List (("(",",",")",list_1), List.map mk_atom_from_lident (Nlist.to_list nlst))

let rec ef_expr : expression -> Easy_format.t = fun e ->
  match e.exp_desc with
  | Ident id -> mk_atom id
  | Dollar id -> mk_atom ( id ^ "$0")
  | Builtin bi -> mk_atom (builtin_to_string bi)
  | Pbool p -> mk_label (mk_atom "bool") (List(("(","",")",list_1),[ef_pred p]))
  | Application (f,a) ->
    begin
      match f.exp_desc, a.exp_desc with
      | Builtin Inverse_Relation, _ -> mk_label (ef_expr_wp a) (mk_atom "~")
      | Builtin Unary_Minus, _ -> mk_label (mk_atom "-") (ef_expr_wp a)
      | Builtin Image, Couple(_,e1,e2) -> mk_label (ef_expr_wp e1) (List (("[","","]",list_1),[ef_expr e2]))
      | Builtin (Composition|Parallel_Product as bop), Couple(_,e1,e2) ->
        List(("(",builtin_to_string bop,")",{ list_1 with space_before_separator=true }),
             [ef_expr_wp e1; ef_expr_wp e2])
      | Builtin bop, Couple(Infix,e1,e2) ->
        List(("",builtin_to_string bop,"",{ list_1 with space_before_separator=true }),
             [ef_expr_wp e1; ef_expr_wp e2])
      | _ -> mk_label (ef_expr_wp f) (List (("(","",")",list_1),[ef_expr a]))
    end
  | Comprehension (xlst,p) ->
    List(("{","","}",{ list with align_closing=false}),
         [mk_string_nelist_comma xlst;mk_atom "|";ef_pred p])
  | Binder (bi,xlst,p,e) ->
    let lst = mk_string_nelist_comma_par xlst in
    let x = mk_label (mk_atom (binder_to_string bi)) lst in
    let y = List(("(","|",")",list_1), [ef_pred p;ef_expr e]) in
    List (("",".","",list_1),[x;y])
  | Sequence nlst ->
    let lst = List.map (fun e -> ef_expr_wp e) (Nlist.to_list nlst) in
    List (("[",",","]",list_1),lst)
  | Extension nlst ->
    let lst = List.map (fun e -> ef_expr_wp e) (Nlist.to_list nlst) in
    List (("{",",","}",list_1),lst)
  | Couple (Infix,_,_) -> assert false
  | Couple (Maplet,e1,e2) ->
    List(("","","",list_1),[ef_expr_wp e1;mk_atom "|->";ef_expr_wp e2])
  | Couple (Comma,e1,e2) ->
    List(("","","",list_1),[ef_expr_wp e1;mk_atom ",";ef_expr_wp e2])
  | Record_Field_Access (e,fd) ->
    mk_label (ef_expr_wp e) (mk_atom ("'" ^ fd.lid_str))
  | Record nlst ->
    let flst = List.map ef_struct_field (Nlist.to_list nlst) in
    let lst = List (("(",",",")",list_1),flst) in
    mk_label (mk_atom "rec") lst
  | Record_Type nlst ->
    let flst = List.map ef_struct_field (Nlist.to_list nlst) in
    let lst = List (("(",",",")",list_1),flst) in
    mk_label (mk_atom "struct") lst

and ef_expr_wp e =
  match e.exp_desc with
  | Application _ | Couple _ -> List(("(","",")",list_1),[ef_expr e])
  | Record_Field_Access _ | Ident _ | Dollar _ | Pbool _ | Builtin _
  | Comprehension _ | Binder _ | Sequence _ | Extension _ | Record _
  | Record_Type _ -> ef_expr e

and ef_struct_field (rf,e:lident*expression) : Easy_format.t =
  List(("",":","",list_1), [mk_atom_from_lident rf;ef_expr_wp e])

and ef_pred : predicate -> Easy_format.t = fun p ->
  match p.prd_desc with
  | P_Builtin Btrue -> mk_atom "btrue"
  | P_Builtin Bfalse -> mk_atom "bfalse"
  | Binary_Prop (Conjonction,_,_) ->
    let pars = List.map (fun p -> ef_pred_wp p) (get_and_list p) in
    List (("","&","",{ list_1 with space_before_separator=true}),pars)
  | Binary_Prop (Disjonction,_,_) ->
    let pars = List.map (fun p -> ef_pred_wp p) (get_or_list p) in
    List (("","or","",{ list_1 with space_before_separator=true}),pars)
  | Binary_Prop (bop,p1,p2) ->
    List(("",prop_bop_to_string bop,"",{list_1 with space_before_separator=true}),
         [ef_pred_wp p1; ef_pred_wp p2])
  | Binary_Pred (bop,e1,e2) ->
    List (("",pred_bop_to_string bop,"",{list_1 with space_before_separator=true}),
          [ef_expr e1; ef_expr e2])
  | Negation p ->
    mk_label (mk_atom "not") (List(("(","",")",list_1),[ef_pred p]))
  | Universal_Q (xlst,p) ->
    let lst = mk_string_nelist_comma_par xlst in
    let x = mk_label (mk_atom "!") lst in
    let y = List(("(","",")",list_1), [ef_pred p]) in
    List(("",".","",{list_1 with space_after_separator=false}),[x;y])
  | Existential_Q (xlst,p) ->
    let lst = mk_string_nelist_comma_par xlst in
    let x = mk_label (mk_atom "#") lst in
    let y = List(("(","",")",list_1), [ef_pred p]) in
    List(("",".","",{list_1 with space_after_separator=false}),[x;y])

and ef_pred_wp p =
  match p.prd_desc with
  | P_Builtin _ | Negation _ | Universal_Q _ | Existential_Q _
  | Binary_Pred _ -> ef_pred p
  | Binary_Prop _ -> List(("(","",")",list_1),[ef_pred p])

let mk_sequence lst =
  List(("","","",{list with align_closing=false;space_after_opening=false;space_before_closing=false}),lst)

let mk_sequence_nl lst =
  List(("","","",{list with space_after_opening=false;
                            space_before_closing=false;
                            align_closing=false;
                            wrap_body=`Force_breaks;
                 }),lst)

let rec get_seq_list (s:substitution) =
  match s.sub_desc with
  | Sequencement (s1,s2) -> (get_seq_list s1)@[s2]
  | _ -> [s]

let rec get_par_list (s:substitution) =
  match s.sub_desc with
  | Parallel (s1,s2) -> (get_par_list s1)@[s2]
  | _ -> [s]

let mk_expr_nelist_comma (lst:expression Nlist.t) : Easy_format.t =
  let lst = List.map ef_expr_wp (Nlist.to_list lst) in
  List (("",",","",list_1), lst)

let rec ef_subst : substitution -> Easy_format.t = fun s ->
  match s.sub_desc with
  | Skip -> mk_atom "skip"

  | Affectation (Tuple xlst,e) ->
    mk_sequence [mk_string_nelist_comma xlst;mk_atom ":=";ef_expr e]

  | Affectation (Function(f,alst),e) ->
    let aux e = List(("(","",")",list),[ef_expr e]) in
    let lst_args = List(("","","",list),List.map aux (Nlist.to_list alst)) in
    let lf = Label ((mk_atom_from_lident f,label), lst_args) in
    mk_sequence [lf;mk_atom ":=";ef_expr e]

  | Affectation (Record(rf,fd),e) ->
    let lf = mk_atom (rf.lid_str ^ "'" ^ fd.lid_str) in
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

  | Choice slst ->
    let lb = {label with label_break=`Always} in
    mk_sequence_nl (
      Label((mk_atom "CHOICE",lb),ef_subst (Nlist.hd slst))::
      (List.map (fun s -> Label((mk_atom "OR",lb),ef_subst s)) (Nlist.tl slst))
      @[mk_atom "END"])

  | IfThenElse (pslst,opt) ->
    let lb = {label with label_break=`Always_rec; space_after_label=false} in
    let (p,s) = Nlist.hd pslst in
    let clst =
      [ [Label((mk_atom "IF",lb),ef_pred p);
         Label((mk_atom "THEN",lb),ef_subst s)] ]
      @
      (List.map (fun (p,s) ->
           [Label((mk_atom "ELSIF",lb),ef_pred p);
            Label((mk_atom "THEN",lb),ef_subst s)]
         ) (Nlist.tl pslst))
      @ (match opt with
          | None -> []
          | Some s -> [[Label((mk_atom "ELSE",lb),ef_subst s)]] )
      @ [[mk_atom "END"]]
    in
    mk_sequence_nl (List.concat clst)

  | Select (pslst,opt) ->
    let lb = {label with label_break=`Always} in
    let (p,s) = Nlist.hd pslst in
    let clst =
      [ [Label((mk_atom "SELECT",lb),ef_pred p);
         Label((mk_atom "THEN",lb),ef_subst s)] ]
      @
      (List.map (fun (p,s) ->
           [Label((mk_atom "WHEN",lb),ef_pred p);
            Label((mk_atom "THEN",lb),ef_subst s)]
         ) (Nlist.tl pslst))
      @ (match opt with
          | None -> []
          | Some s -> [[Label((mk_atom "ELSE",lb),ef_subst s)]] )
      @ [[mk_atom "END"]]
    in
    mk_sequence_nl (List.concat clst)

  | Case (c,eslst,opt) ->
    let lb = {label with label_break=`Always} in
    let (e,s) = Nlist.hd eslst in
    let clst =
      [ [ (mk_atom "OF");
          Label((mk_atom "EITHER",lb),mk_expr_nelist_comma e);
          Label((mk_atom "THEN",lb),ef_subst s)] ]
      @
      (List.map (fun (lst,s) ->
           [Label((mk_atom "OR",lb),mk_expr_nelist_comma lst);
            Label((mk_atom "THEN",lb),ef_subst s)]
         ) (Nlist.tl eslst))
      @ (match opt with
          | None -> []
          | Some s -> [[Label((mk_atom "ELSE",lb),ef_subst s)]] )
      @ [[mk_atom "END"]]
    in
    let clst = List.concat clst in
    let case_c = Label ((mk_atom "CASE",label),ef_expr c) in
    mk_sequence_nl [ Label((case_c,lb),mk_sequence_nl clst);
                     mk_atom "END" ]

  | Any (xlst,p,s) ->
    let lb = { label with label_break=`Always } in
    mk_sequence_nl [ Label((mk_atom "ANY",lb),mk_string_nelist_comma xlst);
                     Label((mk_atom "WHERE",lb),ef_pred p);
                     Label((mk_atom "THEN",lb),ef_subst s);
                     mk_atom "END"]

  | Let (xlst,ielst,s) ->
    let lb = { label with label_break=`Always } in
    let lst = { list with space_after_opening=false; space_before_closing=false; align_closing=false; } in
    let ef_eq (v,e) = mk_sequence [mk_atom_from_lident v;mk_atom "=";ef_expr e] in
    let defs = List(("","&","",lst),List.map ef_eq (Nlist.to_list ielst)) in
    mk_sequence_nl [ Label((mk_atom "LET",lb),mk_string_nelist_comma xlst);
                     Label((mk_atom "BE",lb),defs);
                     Label((mk_atom "IN",lb),ef_subst s);
                     mk_atom "END"]

  | BecomesElt (xlst,e) ->
    mk_sequence [mk_string_nelist_comma xlst; mk_atom "::"; ef_expr e]

  | BecomesSuch (xlst,p) ->
    Label((mk_string_nelist_comma xlst,label),
          List((":(","",")",list),[ef_pred p]))

  | Var (xlst,s) ->
    let lb = {label with label_break=`Always} in
    mk_sequence_nl [ Label((mk_atom "VAR",lb), mk_string_nelist_comma xlst);
                     Label((mk_atom "IN",lb), ef_subst s);
                     mk_atom "END"]

  | CallUp (xlst,f,args) ->
    let lst = {list with align_closing=false;
                         space_after_opening=false;
                         space_before_closing=false}
    in
    let lb = {label with space_after_label=false} in
    begin match xlst, args with
      | [], [] -> mk_atom_from_lident f
      | [], _::_ ->
        let args = List(("(",",",")",lst), List.map (fun e -> ef_expr_wp e) args) in
        Label((mk_atom_from_lident f,lb),args)

      | _::_, [] ->
        mk_sequence [mk_string_list_comma xlst;
                     mk_atom "<--";
                     mk_atom_from_lident f]

      | _::_, _::_ ->
        let args = List(("(",",",")",lst), List.map (fun e -> ef_expr_wp e) args) in
        mk_sequence [mk_string_list_comma xlst;
                     mk_atom "<--";
                     Label((mk_atom_from_lident f,lb),args)]
    end

  | While (p,s,q,e) ->
    let lb = {label with label_break=`Always} in
    mk_sequence_nl [ Label((mk_atom "WHILE",lb), ef_pred p);
                     Label((mk_atom "DO",lb), ef_subst s);
                     Label((mk_atom "INVARIANT",lb), ef_pred q);
                     Label((mk_atom "VARIANT",lb), ef_expr e);
                     mk_atom "END"]

  | Sequencement _ ->
    let seqs = List.map (fun s -> ef_subst_wbe s) (get_seq_list s) in
    let lst = { list with space_after_opening=false; align_closing=false;
                          space_before_closing=false; indent_body=0;
                          wrap_body=`Force_breaks; space_before_separator=true } in
    List (("",";","",lst),seqs)

  | Parallel _ ->
    let pars = List.map (fun s -> ef_subst_wbe s) (get_par_list s) in
    let lst = { list with space_after_opening=false; align_closing=false;
                          space_before_closing=false; indent_body=0;
                          wrap_body=`Force_breaks; space_before_separator=true } in
    List (("","||","",lst),pars)

and ef_subst_wbe s =
  match s.sub_desc with
  | Skip |  Affectation _
  | Pre _ | Assert _ | Choice _ | IfThenElse _ | Select _ | Case _ | Any _
  | Let _ | BecomesElt _ | BecomesSuch _ | Var _ | CallUp _ | While _ -> ef_subst s
  | Sequencement _ | Parallel _ ->
    List (("BEGIN","","END",{list with stick_to_label=false;}),[ef_subst s])

let mk_sequence lst =
  List(("","","",{list with align_closing=false;space_after_opening=false;space_before_closing=false}),lst)

let mk_clause (cname:string) (n:Easy_format.t) =
  Label ((mk_atom cname,
          {label with label_break=`Always;space_after_label=false}),n)

let ef_minst (mi:machine_instanciation) : Easy_format.t =
  match mi.mi_params with
  | [] -> mk_atom_from_lident mi.mi_mch
  | _::_ ->
    Label ((mk_atom_from_lident mi.mi_mch,label),
           List(("(",",",")",list),
                List.map (fun e -> ef_expr_wp e) mi.mi_params))

let ef_set (x:set) : Easy_format.t =
  match x with
  | Abstract_Set s -> mk_atom_from_lident s
  | Concrete_Set (s,lst) ->
    let enums = List(("{",",","}",list),
                     List.map mk_atom_from_lident lst) in
    List(("","","",list),[mk_atom_from_lident s;mk_atom "=";enums])

let ef_operation (op:operation) : Easy_format.t =
  let name_args =
    match op.op_in with
    | [] -> mk_atom_from_lident op.op_name
    | _::_ ->
      let lst = {list with align_closing=false;
                           space_after_opening=false;
                           space_before_closing=false}
      in
      let args = List(("(",",",")",lst),
                      List.map mk_atom_from_lident op.op_in) in
      Label((mk_atom_from_lident op.op_name,label),args)
  in
  let spec = match op.op_out with
    | [] -> mk_sequence [name_args; mk_atom "="]
    | _::_ -> mk_sequence [mk_string_list_comma op.op_out; mk_atom "<--"; name_args; mk_atom "="]
  in
  let lbl = {label with label_break=`Always;
                        indent_after_label=0;
                        space_after_label=false } in
  Label((spec, lbl), ef_subst_wbe op.op_body)

let ef_op_nelist nlst =
  List(("",";\n","",{list with align_closing=false;space_after_opening=false;space_before_closing=false}),
       List.map ef_operation (Nlist.to_list nlst))

let ef_pred_nelist nlst =
  List(("",";","",{list with align_closing=false;space_after_opening=false;space_before_closing=false}),
       List.map ef_pred (Nlist.to_list nlst))

let ef_set_nelist nlst =
  List(("",";","",{list with align_closing=false;space_after_opening=false;space_before_closing=false}),
       List.map ef_set (Nlist.to_list nlst))

let ef_minst_nelist nlst =
  List(("",",","",{list with align_closing=false;space_after_opening=false;space_before_closing=false}),
       List.map ef_minst (Nlist.to_list nlst))

let ef_value_nelist nlst =
  let ef (v,e) = mk_sequence [mk_atom_from_lident v;mk_atom "=";ef_expr e] in
  (List(("",";","",{list with align_closing=false;space_after_opening=false;space_before_closing=false}),
        List.map ef (Nlist.to_list nlst)))

let mk_machine_name (name:lident) (params:lident list) : Easy_format.t =
  match params with
  | [] -> mk_atom_from_lident name
  | _::_ -> Label((mk_atom_from_lident name,{label with space_after_label=false}),
                  List(("(",",",")",list),List.map mk_atom_from_lident params))

let mk_clause_list lst =
  List (("","\n","",
         {list with indent_body=0;
                    space_after_opening=false;
                    space_after_separator=false;
                    align_closing=false}),
        lst)

let mk_mch_name_nelist_comma nlst : Easy_format.t =
  let lst = List.map  mk_atom_from_lident (Nlist.to_list nlst) in
  List (("",",","",list_1), lst)

let mk_op_name_nelist_comma nlst : Easy_format.t =
  let lst = List.map mk_atom_from_lident (Nlist.to_list nlst) in
  List (("",",","",list_1), lst)

let ef_clause : clause -> Easy_format.t = fun c ->
  match c with
  | Constraints p -> mk_clause "CONSTRAINTS" (ef_pred p)
  | Imports lst -> mk_clause "IMPORTS" (ef_minst_nelist lst)
  | Includes lst -> mk_clause "INCLUDES" (ef_minst_nelist lst)
  | Extends lst -> mk_clause "EXTENDS" (ef_minst_nelist lst)
  | Properties p -> mk_clause "PROPERTIES" (ef_pred p)
  | Invariant p -> mk_clause "INVARIANT" (ef_pred p)
  | Assertions lst -> mk_clause "ASSERTIONS" (ef_pred_nelist lst)
  | Initialization s -> mk_clause "INITIALISATION" (ef_subst s)
  | Operations lst -> mk_clause "OPERATIONS" (ef_op_nelist lst)
  | Local_Operations lst -> mk_clause "LOCAL_OPERATIONS" (ef_op_nelist lst)
  | Values lst -> mk_clause "VALUES" (ef_value_nelist lst)
  | Sees lst -> mk_clause "SEES" (mk_mch_name_nelist_comma lst)
  | Promotes lst -> mk_clause "PROMOTES" (mk_op_name_nelist_comma lst)
  | Uses lst -> mk_clause "USES" (mk_mch_name_nelist_comma lst)
  | Sets lst -> mk_clause "SETS" (ef_set_nelist lst)
  | Constants lst -> mk_clause "CONSTANTS" (mk_string_nelist_comma lst)
  | Abstract_constants lst -> mk_clause "ABSTRACT_CONSTANTS" (mk_string_nelist_comma lst)
  | Concrete_variables lst -> mk_clause "CONCRETE_VARIABLES" (mk_string_nelist_comma lst)
  | Variables lst -> mk_clause "VARIABLES" (mk_string_nelist_comma lst)

let ef_machine name params clauses =
  let machine = mk_clause "MACHINE" (mk_machine_name name params) in
  let lst = List.map ef_clause clauses in
  let ed = [mk_atom "END"] in
  mk_clause_list (machine::(lst@ed))

let ef_refinement name refines params clauses =
  let refinement = mk_clause "REFINEMENT" (mk_machine_name name params) in
  let refines = mk_clause "REFINES" (mk_atom_from_lident refines) in
  let lst = List.map ef_clause clauses in
  let ed = [mk_atom "END"] in
  mk_clause_list (refinement::refines::(lst@ed))

let ef_implementation name refines params clauses =
  let implementation = mk_clause "IMPLEMENTATION" (mk_machine_name name params) in
  let refines = mk_clause "REFINES" (mk_atom_from_lident refines) in
  let lst = List.map ef_clause clauses in
  let ed = [mk_atom "END"] in
  mk_clause_list (implementation::refines::(lst@ed))

let ef_component co =
  match co.co_desc with
  | Machine _ -> ef_machine co.co_name co.co_parameters (get_clauses co)
  | Refinement x -> ef_refinement co.co_name x.ref_refines co.co_parameters (get_clauses co)
  | Implementation x -> ef_implementation co.co_name x.imp_refines co.co_parameters (get_clauses co)

let expression_to_format = ef_expr
let predicate_to_format = ef_pred
let substitution_to_format = ef_subst
(* let machine_to_format = ef_machine *)
(* let refinement_to_format = ef_refinement *)
(* let implementation_to_format = ef_implementation *)
let component_to_format = ef_component

let print_expression out e = Easy_format.Pretty.to_channel out (ef_expr e)
let print_predicate out p = Easy_format.Pretty.to_channel out (ef_pred p)
let print_substitution out s = Easy_format.Pretty.to_channel out (ef_subst s)
let print_component out c = Easy_format.Pretty.to_channel out (ef_component c)
