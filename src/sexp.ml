open Syntax

type t =
  | Atom of string
  | List of t list

let rec to_easy_format : t -> Easy_format.t = function
  | Atom s -> Easy_format.(Atom (s,atom))
  | List lst ->
    let list = Easy_format.({ list with space_after_opening=false;
                                        space_before_closing=false;
                                        align_closing=false; })
    in
    Easy_format.List ( ("(","",")",list), List.map to_easy_format lst)

let sexp_to_string (x:t) : string =
  Easy_format.Pretty.to_string (to_easy_format x)

let sexp_to_channel (out:out_channel) (x:t) : unit =
  Easy_format.Pretty.to_channel out (to_easy_format x)

let sexp_of_var (v:_ var) : t = Atom ("ident_" ^ v.var_id)
let sexp_of_record_field (v:_ lident) : t = Atom ("ident_" ^ v.lid_str)
let sexp_of_op_name (v:_ lident) : t = Atom ("ident_" ^ v.lid_str)
let sexp_of_mch_name (v:_ lident) : t = Atom ("ident_" ^ v.lid_str)

let rec sexp_of_expr : ('lc,'ty) expression -> t = fun e ->
  match e.exp_desc with
  | Ident id ->  Atom ("ident_" ^ id)
  | Dollar id -> Atom ("ident_" ^ id ^ "$0")
  | Builtin bi -> Atom (Print.builtin_to_string bi)
  | Pbool p -> List [Atom "bool"; (sexp_of_pred p)]
  | Application (e1,e2) -> List [Atom "App";sexp_of_expr e1;sexp_of_expr e2]
  | Couple (cm,e1,e2) ->
    let atm = match cm with
      | Maplet -> "Pair_maplet"
      | Comma _ -> "Pair_comma"
      | Infix -> "Pair_infix"
    in
    List [Atom atm;sexp_of_expr e1;sexp_of_expr e2]
  | Sequence ((e,elst)) ->
    List ((Atom "Seq")::(sexp_of_expr e)::(List.map sexp_of_expr elst))
  | Extension ((e,elst)) ->
    List ((Atom "Ext")::(sexp_of_expr e)::(List.map sexp_of_expr elst))
  | Comprehension ((id,idlst),p) ->
    let ids = List ((sexp_of_var id)::(List.map sexp_of_var idlst)) in
    List [Atom "Compr";ids;sexp_of_pred p]
  | Binder (bi,(x,xlst),p,e) ->
    let ids = List ((sexp_of_var x)::(List.map sexp_of_var xlst)) in
    List [Atom (Print.binder_to_string bi);ids;sexp_of_pred p;sexp_of_expr e]
  | Record_Field_Access (e,id) ->
    List [Atom "Record_Field_Access";sexp_of_expr e;sexp_of_record_field id]
  | Record ((x,xlst)) ->
    let aux (id,e) = List [sexp_of_record_field id;sexp_of_expr e] in
    List ((Atom "Rec")::(aux x)::(List.map aux xlst))
  | Record_Type ((x,xlst)) ->
    let aux (id,e) = List [sexp_of_record_field id;sexp_of_expr e] in
    List ((Atom "Rec")::(aux x)::(List.map aux xlst))

and sexp_of_pred : ('lc,'ty) predicate -> t = fun p ->
  match p.prd_desc with
  | P_Builtin Btrue -> Atom "btrue"
  | P_Builtin Bfalse -> Atom "bfalse"
  | Binary_Prop (bop,p,q) ->
    List [Atom (Print.prop_bop_to_string bop);sexp_of_pred p;sexp_of_pred q]
  | Binary_Pred (bop,e1,e2) ->
    List [Atom (Print.pred_bop_to_string bop);sexp_of_expr e1;sexp_of_expr e2]
  | Negation p -> List [Atom "Neg";sexp_of_pred p]
  | Universal_Q ((x,xlst),p) ->
    let ids = List ((sexp_of_var x)::(List.map sexp_of_var xlst)) in
    List [Atom "!";ids;sexp_of_pred p]
  | Existential_Q ((x,xlst),p) ->
    let ids = List ((sexp_of_var x)::(List.map sexp_of_var xlst)) in
    List [Atom "#";ids;sexp_of_pred p]

let rec sexp_of_subst : ('lc,'ty) substitution -> t = fun s ->
  match s.sub_desc with
  | Skip -> Atom "SKIP"
  | Affectation ((x,xlst),e) ->
    let ids = List ((sexp_of_var x)::(List.map sexp_of_var xlst)) in
    let exprs = List [sexp_of_expr e] in
    List [Atom "AFF1";ids;exprs]
  | Function_Affectation (f,(e,elst),a) ->
    let exprs = List ((sexp_of_expr e)::(List.map sexp_of_expr elst)) in
    List [Atom "AFF2";sexp_of_var f;exprs;sexp_of_expr a]
  | Record_Affectation (id,fd,e) ->
    List [Atom "AFF3";sexp_of_var id;sexp_of_record_field fd;sexp_of_expr e]
  | Pre (p,s) -> List [Atom "PRE"; sexp_of_pred p; sexp_of_subst s]
  | Assert (p,s) -> List [Atom "ASSERT"; sexp_of_pred p; sexp_of_subst s]
  | Choice ((s,slst)) ->
    List ( (Atom "CHOICE")::(sexp_of_subst s)::(List.map sexp_of_subst slst))
  | IfThenElse ((y,ylst),s_opt) ->
    let aux (p,s) = List [sexp_of_pred p;sexp_of_subst s] in
    let yy = List ((aux y)::(List.map aux ylst)) in
    begin match s_opt with
      | None -> List [Atom "IF";yy]
      | Some els -> List [Atom "IF";yy;sexp_of_subst els]
    end
  | Select ((y,ylst),s_opt) ->
   let aux (p,s) = List [sexp_of_pred p;sexp_of_subst s] in
    let yy = List ((aux y)::(List.map aux ylst)) in
    begin match s_opt with
      | None -> List [Atom "SELECT";yy]
      | Some els -> List [Atom "SELECT";yy;sexp_of_subst els]
    end
  | Case (e,(y,ylst),s_opt) ->
    let aux (e,s) = List [sexp_of_expr e;sexp_of_subst s] in
    let yy = List ((aux y)::(List.map aux ylst)) in
    begin match s_opt with
      | None -> List [Atom "SELECT";sexp_of_expr e;yy]
      | Some els -> List [Atom "SELECT";sexp_of_expr e;yy;sexp_of_subst els]
    end
  | Any ((x,xlst),p,s) ->
    let ids = List ((sexp_of_var x)::(List.map sexp_of_var xlst)) in
    List [Atom "ANY";ids;sexp_of_pred p;sexp_of_subst s]
  | Let ((x,xlst),(y,ylst),s) ->
    let ids = List ((sexp_of_var x)::(List.map sexp_of_var xlst)) in
    let aux (id,e) = List [sexp_of_var id;sexp_of_expr e] in
    let exprs = List ((aux y)::(List.map aux ylst)) in
    List [Atom "LET";ids;exprs;sexp_of_subst s]
  | BecomesElt ((x,xlst),e) ->
    let ids = List ((sexp_of_var x)::(List.map sexp_of_var xlst)) in
    List [Atom "BECOMESELT";ids;sexp_of_expr e]
  | BecomesSuch ((x,xlst),p) ->
    let ids = List ((sexp_of_var x)::(List.map sexp_of_var xlst)) in
    List [Atom "BECOMESSUCH";ids;sexp_of_pred p]
  | Var ((x,xlst),s) ->
    let ids = List ((sexp_of_var x)::(List.map sexp_of_var xlst)) in
    List [Atom "VAR";ids;sexp_of_subst s]
  | CallUp (outs,f,args) ->
    List ( ((Atom "CALL")::(List.map sexp_of_var outs))@
          ((sexp_of_op_name f)::(List.map sexp_of_expr args)) )
  | While (p,s,q,e) ->
    List [Atom "WHILE";sexp_of_pred p;sexp_of_subst s;
          sexp_of_pred q;sexp_of_expr e]
  | Sequencement (s1,s2) ->
    List [Atom "SEQ";sexp_of_subst s1;sexp_of_subst s2]
  | Parallel (s1,s2) ->
    List [Atom "PAR";sexp_of_subst s1;sexp_of_subst s2]


let add lst f = function
  | None -> lst
  | Some x -> (f x)::lst

let sexp_of_set : _ set -> t = function
  | Abstract_Set v -> sexp_of_var v
  | Concrete_Set (v,lst) ->
    List[sexp_of_var v;List (List.map sexp_of_var lst)]

let sexp_of_minst (mi:_ machine_instanciation) : t =
  List [Atom (mi.mi_mch.lid_str); List (List.map sexp_of_expr mi.mi_params)]

let sexp_of_op (op:_ operation) : t =
  List [ List (List.map sexp_of_var op.op_in);
         sexp_of_op_name op.op_name;
         List (List.map sexp_of_var op.op_out);
         sexp_of_subst op.op_body ]

let sexp_of_clause : ('lc,'ty) clause -> t = fun c ->
  match c.cl_desc with
  | Constraints p -> List [Atom "CONSTRAINTS";sexp_of_pred p]
  | Imports (hd,tl) -> List ( (Atom "IMPORTS")::(List.map sexp_of_minst (hd::tl)) )
  | Includes (hd,tl) -> List ( (Atom "INCLUDES")::(List.map sexp_of_minst (hd::tl)) )
  | Extends (hd,tl) -> List ( (Atom "EXTENDS")::(List.map sexp_of_minst (hd::tl)) )
  | Properties p -> List [Atom "PROPERTIES";sexp_of_pred p]
  | Invariant p -> List [Atom "INVARIANT";sexp_of_pred p]
  | Assertions (hd,tl) -> List ((Atom "ASSERTIONS")::(List.map sexp_of_pred (hd::tl)))
  | Initialization s -> List [Atom "INITIALISATION";sexp_of_subst s]
  | Operations (hd,tl) -> List ( (Atom "OPERATIONS")::(List.map sexp_of_op (hd::tl)) )
  | Local_Operations (hd,tl) -> List ( (Atom "LOCAL_OPERATIONS")::(List.map sexp_of_op (hd::tl)) )
  | Values (hd,tl) -> let aux (id,e) = List [sexp_of_var id;sexp_of_expr e] in
    List ( (Atom "VALUES")::(List.map aux (hd::tl)) )
  | Sees (hd,tl) -> List ( (Atom "SEES")::(List.map sexp_of_mch_name (hd::tl)) )
  | Promotes (hd,tl) -> List ( (Atom "PROMOTES")::(List.map sexp_of_op_name (hd::tl)) )
  | Uses (hd,tl) -> List ( (Atom "USES")::(List.map sexp_of_mch_name (hd::tl)) )
  | Sets (hd,tl) -> List ( (Atom "SETS")::(List.map sexp_of_set (hd::tl)) )
  | Constants (hd,tl) -> List ( (Atom "CONSTANTS")::(List.map sexp_of_var (hd::tl)) )
  | Abstract_constants (hd,tl) -> List ( (Atom "ABSTRACT_CONSTANTS")::(List.map sexp_of_var (hd::tl)) )
  | Concrete_variables (hd,tl) -> List ( (Atom "CONCRETE_VARIABLES")::(List.map sexp_of_var (hd::tl)) )
  | Variables (hd,tl) -> List ( (Atom "VARIABLES")::(List.map sexp_of_var (hd::tl)) )

let sexp_of_mch name parameters clauses : t =
  List ( (Atom "MACHINE")::(Atom ("ident_" ^ name))::
         (List (List.map sexp_of_var parameters))::
         (List.map sexp_of_clause clauses) )

let sexp_of_ref name refines parameters clauses : t =
  List ( (Atom "REFINEMENT")::(Atom ("ident_" ^ name))::
         (List (List.map sexp_of_var parameters))::
         (List [Atom "REFINES";sexp_of_mch_name refines])::
         (List.map sexp_of_clause clauses) )

let sexp_of_imp name refines parameters clauses : t =
  List ( (Atom "IMPLEMENTATION")::(Atom ("ident_"^name))::
         (List (List.map sexp_of_var parameters))::
         (List [Atom "REFINES";sexp_of_mch_name refines])::
         (List.map sexp_of_clause clauses) )

let sexp_of_component co =
  match co.co_desc with
  | Machine mch -> sexp_of_mch co.co_name co.co_parameters (get_clauses co)
  | Refinement ref -> sexp_of_ref co.co_name ref.ref_refines co.co_parameters (get_clauses co)
  | Implementation imp -> sexp_of_imp co.co_name imp.imp_refines co.co_parameters (get_clauses co)
