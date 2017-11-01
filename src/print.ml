open Utils
open Easy_format
open Syntax

let pred_bop_to_string : pred_bop -> string = function
  | Equality -> "="
  | Disequality -> "/="
  | Membership -> ":"
  | Non_Membership -> "/:"
  | Inclusion Not_Strict -> "<:"
  | Inclusion Strict -> "<<:"
  | Inclusion Non_Inclusion -> "/<:"
  | Inclusion Non_Strict_Inclusion -> "/<<:"
  | Inequality Smaller_or_Equal -> "<="
  | Inequality Strictly_Smaller -> "<"
  | Inequality Greater_or_Equal -> ">="
  | Inequality Strictly_Greater -> ">"

let prop_bop_to_string : prop_bop -> string = function
  | Conjonction -> "&"
  | Disjonction -> "or"
  | Implication -> "=>"
  | Equivalence -> "<=>"

let builtin_to_string : e_builtin -> string = function
  | Integer i -> string_of_int i
  | String s -> "\"" ^ s ^ "\""
  | TRUE -> "TRUE"
  | FALSE -> "FALSE"
  | MaxInt -> "MAXINT"
  | MinInt -> "MININT"
  | INTEGER -> "INTEGER"
  | NATURAL -> "NATURAL"
  | NATURAL1 -> "NATURAL1"
  | INT -> "INT"
  | NAT -> "NAT"
  | NAT1 -> "NAT1"
  | STRINGS -> "STRING"
  | BOOLEANS -> "BOOL"
  | Empty_Set -> "{}"
  | Empty_Seq -> "[]"
  | Successor -> "succ"
  | Predecessor -> "pred"
  | Cardinal -> "card"
  | Power_Set Full -> "POW"
  | Power_Set Non_Empty -> "POW1"
  | Power_Set Finite -> "FIN"
  | Power_Set Finite_Non_Empty -> "FIN1"
  | Identity_Relation -> "id"
  | Closure -> "closure"
  | Transitive_Closure -> "closure1"
  | Domain -> "dom"
  | Range -> "ran"
  | Fnc -> "fnc"
  | Rel -> "rel"
  | Sequence_Set All_Seq -> "seq"
  | Sequence_Set Non_Empty_Seq -> "seq1"
  | Sequence_Set Injective_Seq -> "iseq"
  | Sequence_Set Injective_Non_Empty_Seq -> "iseq1"
  | Sequence_Set Permutations -> "perm"
  | Size -> "size"
  | First -> "first"
  | Last -> "last"
  | Front -> "front"
  | Tail -> "tail"
  | Reverse -> "rev"
  | G_Union -> "union"
  | G_Intersection -> "inter"
  | G_Concatenation -> "conc"
  | Max -> "max"
  | Min -> "min"
  | First_Projection -> "prj1"
  | Second_Projection -> "prj2"
  | Iteration -> "iterate"

  | Image -> "image"
  | Unary_Minus -> "unary_minus"
  | Inverse_Relation -> "inverse"

  | Product -> "*"
  | Difference -> "-"
  | Addition -> "+"
  | Division -> "/"
  | Modulo -> "mod"
  | Power -> "**"
  | Interval -> ".."
  | Union -> "\\/"
  | Intersection -> "/\\"
  | Relations -> "<->"
  | Composition -> ";"
  | Direct_Product -> "><"
  | Parallel_Product -> "||"
  | Domain_Restriction -> "<|"
  | Domain_Soustraction -> "<<|"
  | Codomain_Restriction -> "|>"
  | Codomain_Soustraction -> "|>>"
  | Surcharge -> "<+"
  | Functions Partial_Functions -> "+->"
  | Functions Partial_Injections -> ">+>"
  | Functions Total_Injections -> ">->"
  | Functions Total_Functions -> "-->"
  | Functions Total_Surjections -> "-->>"
  | Functions Partial_Surjections -> "+->>"
  | Functions Bijections -> ">->>"
  | Concatenation -> "^"
  | Head_Insertion -> "->"
  | Tail_Insertion -> "<-"
  | Head_Restriction -> "/|\\"
  | Tail_Restriction -> "\\|/"

  | Tree -> "tree"
  | Btree -> "btree"
  | Const -> "const"
  | Top -> "top"
  | Sons -> "sons"
  | Prefix -> "prefix"
  | Postfix -> "postfix"
  | SizeT -> "sizet"
  | Mirror -> "mirror"
  | Rank -> "rank"
  | Father -> "father"
  | Son -> "son"
  | Subtree -> "subtree"
  | Arity -> "arity"
  | Bin -> "bin"
  | Left -> "left"
  | Right -> "right"
  | Infix -> "infix"

let binder_to_string = function
  | Sum -> "SIGMA"
  | Prod -> "PI"
  | Q_Union -> "UNION"
  | Q_Intersection -> "INTER"
  | Lambda -> "%"

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

let mk_var_list_comma (lst:_ var list) : Easy_format.t =
  let lst = List.map (fun (v:_ var) -> mk_atom v.var_id) lst in
  List (("",",","",list_1), lst)

let mk_var_nelist_comma (nlst:_ var Nlist.t) : Easy_format.t =
  mk_var_list_comma (Nlist.to_list nlst)

let mk_var_nelist_comma2 (nlst:_ var Nlist.t) : Easy_format.t =
  let lst = List.map (fun (v:_ var) -> mk_atom v.var_id) (Nlist.to_list nlst) in
  List (("(",",",")",list_1), lst)

let rec ef_expr : ('lc,'ty) expression -> Easy_format.t = fun e ->
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
         [mk_var_nelist_comma xlst;mk_atom "|";ef_pred p])
  | Binder (bi,xlst,p,e) ->
    let lst = mk_var_nelist_comma2 xlst in
    let x = mk_label (mk_atom (binder_to_string bi)) lst in
    let y = List(("(","|",")",list_1), [ef_pred p;ef_expr e]) in
    List (("",".","",list_1),[x;y])
  | Sequence nlst ->
    let lst = List.map (fun e -> ef_expr_wp e) (Nlist.to_list nlst) in
    List (("[",",","]",list_1),lst)
  | Extension nlst ->
    let lst = List.map (fun e -> ef_expr_wp e) (Nlist.to_list nlst) in
    List (("{",",","}",list_1),lst)
  | Couple (Infix,e1,e2) -> assert false
  | Couple (Maplet,e1,e2) ->
    List(("","","",list_1),[ef_expr_wp e1;mk_atom "|->";ef_expr_wp e2])
  | Couple (Comma _,e1,e2) ->
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

and ef_struct_field (rf,e:'lc lident*('lc,'ty) expression) : Easy_format.t =
  List(("",":","",list_1), [mk_atom rf.lid_str;ef_expr_wp e])

and ef_pred : ('lc,'ty) predicate -> Easy_format.t = fun p ->
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
    let lst = mk_var_nelist_comma2 xlst in
    let x = mk_label (mk_atom "!") lst in
    let y = List(("(","",")",list_1), [ef_pred p]) in
    List(("",".","",{list_1 with space_after_separator=false}),[x;y])
  | Existential_Q (xlst,p) ->
    let lst = mk_var_nelist_comma2 xlst in
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

let rec get_seq_list (s:_ substitution) =
  match s.sub_desc with
  | Sequencement (s1,s2) -> (get_seq_list s1)@[s2]
  | _ -> [s]

let rec get_par_list (s:_ substitution) =
  match s.sub_desc with
  | Parallel (s1,s2) -> (get_par_list s1)@[s2]
  | _ -> [s]

let rec ef_subst : ('lc,'ty) substitution -> Easy_format.t = fun s ->
  match s.sub_desc with
  | Skip -> mk_atom "skip"

  | Affectation (xlst,e) ->
    mk_sequence [mk_var_nelist_comma xlst;mk_atom ":=";ef_expr e]

  | Function_Affectation (f,alst,e) ->
    let aux e = List(("(","",")",list),[ef_expr e]) in
    let lst_args = List(("","","",list),List.map aux (Nlist.to_list alst)) in
    let lf = Label ((mk_atom f.var_id,label), lst_args) in
    mk_sequence [lf;mk_atom ":=";ef_expr e]

  | Record_Affectation (rf,fd,e) ->
    let lf = mk_atom (rf.var_id ^ "'" ^ fd.lid_str) in
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
          Label((mk_atom "EITHER",lb),ef_expr e);
          Label((mk_atom "THEN",lb),ef_subst s)] ]
      @
      (List.map (fun (e,s) ->
           [Label((mk_atom "OR",lb),ef_expr e);
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
    mk_sequence_nl [ Label((mk_atom "ANY",lb),mk_var_nelist_comma xlst);
                     Label((mk_atom "WHERE",lb),ef_pred p);
                     Label((mk_atom "THEN",lb),ef_subst s);
                     mk_atom "END"]

  | Let (xlst,ielst,s) ->
    let lb = { label with label_break=`Always } in
    let lst = { list with space_after_opening=false; space_before_closing=false; align_closing=false; } in
    let ef_eq (v,e:_ var*_ expression) = mk_sequence [mk_atom v.var_id;mk_atom "=";ef_expr e] in
    let defs = List(("","&","",lst),List.map ef_eq (Nlist.to_list ielst)) in
    mk_sequence_nl [ Label((mk_atom "LET",lb),mk_var_nelist_comma xlst);
                     Label((mk_atom "BE",lb),defs);
                     Label((mk_atom "IN",lb),ef_subst s);
                     mk_atom "END"]

  | BecomesElt (xlst,e) ->
    mk_sequence [mk_var_nelist_comma xlst; mk_atom "::"; ef_expr e]

  | BecomesSuch (xlst,p) ->
    Label((mk_var_nelist_comma xlst,label),
          List((":(","",")",list),[ef_pred p]))

  | Var (xlst,s) ->
    let lb = {label with label_break=`Always} in
    mk_sequence_nl [ Label((mk_atom "VAR",lb), mk_var_nelist_comma xlst);
                     Label((mk_atom "IN",lb), ef_subst s);
                     mk_atom "END"]

  | CallUp (xlst,f,args) ->
    let lst = {list with align_closing=false;
                         space_after_opening=false;
                         space_before_closing=false}
    in
    let lb = {label with space_after_label=false} in
    begin match xlst, args with
      | [], [] -> mk_atom f.lid_str
      | [], _::_ ->
        let args = List(("(",",",")",lst), List.map (fun e -> ef_expr_wp e) args) in
        Label((mk_atom f.lid_str,lb),args)

      | _::_, [] ->
        mk_sequence [mk_var_list_comma xlst;
                     mk_atom "<--";
                     mk_atom f.lid_str]

      | _::_, _::_ ->
        let args = List(("(",",",")",lst), List.map (fun e -> ef_expr_wp e) args) in
        mk_sequence [mk_var_list_comma xlst;
                     mk_atom "<--";
                     Label((mk_atom f.lid_str,lb),args)]
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
  | Skip |  Affectation _ | Function_Affectation _ | Record_Affectation _
  | Pre _ | Assert _ | Choice _ | IfThenElse _ | Select _ | Case _ | Any _
  | Let _ | BecomesElt _ | BecomesSuch _ | Var _ | CallUp _ | While _ -> ef_subst s
  | Sequencement _ | Parallel _ ->
    List (("BEGIN","","END",{list with stick_to_label=false;}),[ef_subst s])

let mk_sequence lst =
  List(("","","",{list with align_closing=false;space_after_opening=false;space_before_closing=false}),lst)

let mk_clause (cname:string) (n:Easy_format.t) =
  Label ((mk_atom cname,
          {label with label_break=`Always;space_after_label=false}),n)

let ef_minst (mi:_ machine_instanciation) : Easy_format.t =
  match mi.mi_params with
  | [] -> mk_atom mi.mi_mch.lid_str
  | _::_ ->
    Label ((mk_atom mi.mi_mch.lid_str,label),
           List(("(",",",")",list),
                List.map (fun e -> ef_expr_wp e) mi.mi_params))

let ef_set (x:_ set) : Easy_format.t =
  match x with
  | Abstract_Set s -> mk_atom s.var_id
  | Concrete_Set (s,lst) ->
    let enums = List(("{",",","}",list),
                     List.map (fun (e:_ var) -> mk_atom e.var_id) lst) in
    List(("","","",list),[mk_atom s.var_id;mk_atom "=";enums])

let ef_operation (op:_ operation) : Easy_format.t =
  let name_args =
    match op.op_in with
    | [] -> mk_atom op.op_name.lid_str
    | _::_ ->
      let lst = {list with align_closing=false;
                           space_after_opening=false;
                           space_before_closing=false}
      in
      let args = List(("(",",",")",lst),
                      List.map (fun (v:_ var) -> mk_atom v.var_id) op.op_in) in
      Label((mk_atom op.op_name.lid_str,label),args)
  in
  let spec = match op.op_out with
    | [] -> mk_sequence [name_args; mk_atom "="]
    | _::_ -> mk_sequence [mk_var_list_comma op.op_out; mk_atom "<--"; name_args; mk_atom "="]
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
  let ef (v,e:_ var*_ expression) = mk_sequence [mk_atom v.var_id;mk_atom "=";ef_expr e] in
  (List(("",";","",{list with align_closing=false;space_after_opening=false;space_before_closing=false}),
        List.map ef (Nlist.to_list nlst)))

let mk_machine_name (name:ident) (params:_ var list) : Easy_format.t =
  match params with
  | [] -> mk_atom name
  | _::_ -> Label((mk_atom name,{label with space_after_label=false}),
                  List(("(",",",")",list),List.map (fun (v: _ var) -> mk_atom v.var_id) params))

let mk_clause_list lst =
  List (("","\n","",
         {list with indent_body=0;
                    space_after_opening=false;
                    space_after_separator=false;
                    align_closing=false}),
        lst)

let mk_mch_name_nelist_comma nlst : Easy_format.t =
  let lst = List.map (fun (v:_ lident) -> mk_atom v.lid_str) (Nlist.to_list nlst) in
  List (("",",","",list_1), lst)

let mk_op_name_nelist_comma nlst : Easy_format.t =
  let lst = List.map (fun (v:_ lident) -> mk_atom v.lid_str) (Nlist.to_list nlst) in
  List (("",",","",list_1), lst)

let ef_clause : ('lc,'ty) clause -> Easy_format.t = fun c ->
  match c.cl_desc with
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
  | Constants lst -> mk_clause "CONSTANTS" (mk_var_nelist_comma lst)
  | Abstract_constants lst -> mk_clause "ABSTRACT_CONSTANTS" (mk_var_nelist_comma lst)
  | Concrete_variables lst -> mk_clause "CONCRETE_VARIABLES" (mk_var_nelist_comma lst)
  | Variables lst -> mk_clause "VARIABLES" (mk_var_nelist_comma lst)

let ef_machine name params clauses =
  let machine = mk_clause "MACHINE" (mk_machine_name name params) in
  let lst = List.map ef_clause clauses in
  let ed = [mk_atom "END"] in
  mk_clause_list (machine::(lst@ed))

let ef_refinement name refines params clauses =
  let refinement = mk_clause "REFINEMENT" (mk_machine_name name params) in
  let refines = mk_clause "REFINES" (mk_atom refines.lid_str) in
  let lst = List.map ef_clause clauses in
  let ed = [mk_atom "END"] in
  mk_clause_list (refinement::refines::(lst@ed))

let ef_implementation name refines params clauses =
  let implementation = mk_clause "IMPLEMENTATION" (mk_machine_name name params) in
  let refines = mk_clause "REFINES" (mk_atom refines.lid_str) in
  let lst = List.map ef_clause clauses in
  let ed = [mk_atom "END"] in
  mk_clause_list (implementation::refines::(lst@ed))

let ef_component co =
  match co.co_desc with
  | Machine x -> ef_machine co.co_name co.co_parameters (get_clauses co)
  | Refinement x -> ef_refinement co.co_name x.ref_refines co.co_parameters (get_clauses co)
  | Implementation x -> ef_implementation co.co_name x.imp_refines co.co_parameters (get_clauses co)

let expression_to_format = ef_expr
let predicate_to_format = ef_pred
let substitution_to_format = ef_subst
let machine_to_format = ef_machine
let refinement_to_format = ef_refinement
let implementation_to_format = ef_implementation
let component_to_format = ef_component

let print_expression out e = Easy_format.Pretty.to_channel out (ef_expr e)
let print_predicate out p = Easy_format.Pretty.to_channel out (ef_pred p)
let print_substitution out s = Easy_format.Pretty.to_channel out (ef_subst s)
let print_component out c = Easy_format.Pretty.to_channel out (ef_component c)
