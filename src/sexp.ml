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
  | Builtin bi -> Atom (builtin_to_string bi)
  | Pbool p -> List [Atom "bool"; (sexp_of_pred p)]
  | Application (e1,e2) -> List [Atom "App";sexp_of_expr e1;sexp_of_expr e2]
  | Couple (cm,e1,e2) ->
    let atm = match cm with
      | Maplet -> "Pair_maplet"
      | Comma _ -> "Pair_comma"
      | Infix -> "Pair_infix"
    in
    List [Atom atm;sexp_of_expr e1;sexp_of_expr e2]
  | Sequence elst ->
    List ((Atom "Seq")::(List.map sexp_of_expr (Nlist.to_list elst)))
  | Extension elst ->
    List ((Atom "Ext")::(List.map sexp_of_expr (Nlist.to_list elst)))
  | Comprehension (ids,p) ->
    let ids = List (List.map sexp_of_var (Nlist.to_list ids)) in
    List [Atom "Compr";ids;sexp_of_pred p]
  | Binder (bi,xlst,p,e) ->
    let ids = List (List.map sexp_of_var (Nlist.to_list xlst)) in
    List [Atom (binder_to_string bi);ids;sexp_of_pred p;sexp_of_expr e]
  | Record_Field_Access (e,id) ->
    List [Atom "Record_Field_Access";sexp_of_expr e;sexp_of_record_field id]
  | Record nlst ->
    let aux (id,e) = List [sexp_of_record_field id;sexp_of_expr e] in
    List ((Atom "Rec")::(List.map aux (Nlist.to_list nlst)))
  | Record_Type nlst ->
    let aux (id,e) = List [sexp_of_record_field id;sexp_of_expr e] in
    List ((Atom "Rec")::(List.map aux (Nlist.to_list nlst)))

and sexp_of_pred : ('lc,'ty) predicate -> t = fun p ->
  match p.prd_desc with
  | P_Builtin Btrue -> Atom "btrue"
  | P_Builtin Bfalse -> Atom "bfalse"
  | Binary_Prop (bop,p,q) ->
    List [Atom (prop_bop_to_string bop);sexp_of_pred p;sexp_of_pred q]
  | Binary_Pred (bop,e1,e2) ->
    List [Atom (pred_bop_to_string bop);sexp_of_expr e1;sexp_of_expr e2]
  | Negation p -> List [Atom "Neg";sexp_of_pred p]
  | Universal_Q (xlst,p) ->
    let ids = List (List.map sexp_of_var (Nlist.to_list xlst)) in
    List [Atom "!";ids;sexp_of_pred p]
  | Existential_Q (xlst,p) ->
    let ids = List (List.map sexp_of_var (Nlist.to_list xlst)) in
    List [Atom "#";ids;sexp_of_pred p]

let rec sexp_of_subst : ('lc,'ty) substitution -> t = fun s ->
  match s.sub_desc with
  | Skip -> Atom "SKIP"
  | Affectation (Tuple xlst,e) ->
    let ids = List (List.map sexp_of_var (Nlist.to_list xlst)) in
    let exprs = List [sexp_of_expr e] in
    List [Atom "AFF1";ids;exprs]
  | Affectation (Function(f,elst),a) ->
    let exprs = List (List.map sexp_of_expr (Nlist.to_list elst)) in
    List [Atom "AFF2";sexp_of_var f;exprs;sexp_of_expr a]
  | Affectation (Record(id,fd),e) ->
    List [Atom "AFF3";sexp_of_var id;sexp_of_record_field fd;sexp_of_expr e]
  | Pre (p,s) -> List [Atom "PRE"; sexp_of_pred p; sexp_of_subst s]
  | Assert (p,s) -> List [Atom "ASSERT"; sexp_of_pred p; sexp_of_subst s]
  | Choice slst ->
    List ( (Atom "CHOICE")::(List.map sexp_of_subst (Nlist.to_list slst)))
  | IfThenElse (ylst,s_opt) ->
    let aux (p,s) = List [sexp_of_pred p;sexp_of_subst s] in
    let yy = List (List.map aux (Nlist.to_list ylst)) in
    begin match s_opt with
      | None -> List [Atom "IF";yy]
      | Some els -> List [Atom "IF";yy;sexp_of_subst els]
    end
  | Select (ylst,s_opt) ->
   let aux (p,s) = List [sexp_of_pred p;sexp_of_subst s] in
    let yy = List (List.map aux (Nlist.to_list ylst)) in
    begin match s_opt with
      | None -> List [Atom "SELECT";yy]
      | Some els -> List [Atom "SELECT";yy;sexp_of_subst els]
    end
  | Case (e,ylst,s_opt) ->
    let aux (lst,s) =
      let lst = List.map sexp_of_expr (Nlist.to_list lst) in
      List [List lst;sexp_of_subst s] in
    let yy = List (List.map aux (Nlist.to_list ylst)) in
    begin match s_opt with
      | None -> List [Atom "SELECT";sexp_of_expr e;yy]
      | Some els -> List [Atom "SELECT";sexp_of_expr e;yy;sexp_of_subst els]
    end
  | Any (xlst,p,s) ->
    let ids = List (List.map sexp_of_var (Nlist.to_list xlst)) in
    List [Atom "ANY";ids;sexp_of_pred p;sexp_of_subst s]
  | Let (xlst,ylst,s) ->
    let ids = List (List.map sexp_of_var (Nlist.to_list xlst)) in
    let aux (id,e) = List [sexp_of_var id;sexp_of_expr e] in
    let exprs = List (List.map aux (Nlist.to_list ylst)) in
    List [Atom "LET";ids;exprs;sexp_of_subst s]
  | BecomesElt (xlst,e) ->
    let ids = List (List.map sexp_of_var (Nlist.to_list xlst)) in
    List [Atom "BECOMESELT";ids;sexp_of_expr e]
  | BecomesSuch (xlst,p) ->
    let ids = List (List.map sexp_of_var (Nlist.to_list xlst)) in
    List [Atom "BECOMESSUCH";ids;sexp_of_pred p]
  | Var (xlst,s) ->
    let ids = List (List.map sexp_of_var (Nlist.to_list xlst)) in
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
  | Imports nlst -> List ( (Atom "IMPORTS")::(List.map sexp_of_minst (Nlist.to_list nlst)) )
  | Includes nlst -> List ( (Atom "INCLUDES")::(List.map sexp_of_minst (Nlist.to_list nlst)) )
  | Extends nlst -> List ( (Atom "EXTENDS")::(List.map sexp_of_minst (Nlist.to_list nlst)) )
  | Properties p -> List [Atom "PROPERTIES";sexp_of_pred p]
  | Invariant p -> List [Atom "INVARIANT";sexp_of_pred p]
  | Assertions nlst -> List ((Atom "ASSERTIONS")::(List.map sexp_of_pred (Nlist.to_list nlst)))
  | Initialization s -> List [Atom "INITIALISATION";sexp_of_subst s]
  | Operations nlst -> List ( (Atom "OPERATIONS")::(List.map sexp_of_op (Nlist.to_list nlst)) )
  | Local_Operations nlst -> List ( (Atom "LOCAL_OPERATIONS")::(List.map sexp_of_op (Nlist.to_list nlst)) )
  | Values nlst -> let aux (id,e) = List [sexp_of_var id;sexp_of_expr e] in
    List ( (Atom "VALUES")::(List.map aux (Nlist.to_list nlst)) )
  | Sees nlst -> List ( (Atom "SEES")::(List.map sexp_of_mch_name (Nlist.to_list nlst)) )
  | Promotes nlst -> List ( (Atom "PROMOTES")::(List.map sexp_of_op_name (Nlist.to_list nlst)) )
  | Uses nlst -> List ( (Atom "USES")::(List.map sexp_of_mch_name (Nlist.to_list nlst)) )
  | Sets nlst -> List ( (Atom "SETS")::(List.map sexp_of_set (Nlist.to_list nlst)) )
  | Constants nlst -> List ( (Atom "CONSTANTS")::(List.map sexp_of_var (Nlist.to_list nlst)) )
  | Abstract_constants nlst -> List ( (Atom "ABSTRACT_CONSTANTS")::(List.map sexp_of_var (Nlist.to_list nlst)) )
  | Concrete_variables nlst -> List ( (Atom "CONCRETE_VARIABLES")::(List.map sexp_of_var (Nlist.to_list nlst)) )
  | Variables nlst -> List ( (Atom "VARIABLES")::(List.map sexp_of_var (Nlist.to_list nlst)) )

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
