open SyntaxCore
module P = PSyntax

type t =
  | Atom of string
  | List of t list

let pp_list (pp:Format.formatter -> 'a -> unit)
    (out:Format.formatter) (nlst:'a Nlist.t) : unit =
  Format.pp_open_box out 0;
  Format.pp_print_string out "(";
  pp out (Nlist.hd nlst);
  List.iter (fun x ->
      Format.pp_print_break out 1 1;
      pp out x
    ) (Nlist.tl nlst);
  Format.pp_print_string out ")";
  Format.pp_close_box out ()

let rec pp_t out = function
  | Atom s -> Format.pp_print_string out s
  | List [] -> Format.pp_print_string out "()"
  | List (hd::tl) -> pp_list pp_t out (Nlist.make hd tl)

let sexp_to_channel (out:out_channel) (x:t) : unit =
  pp_t (Format.formatter_of_out_channel out) x

let sexp_to_string (x:t) : string =
  let buf = Buffer.create 47 in
  let () = pp_t (Format.formatter_of_buffer buf) x in
  Buffer.contents buf

let sexp_of_lident (v:lident) : t = Atom ("ident_" ^ v.lid_str)
let sexp_of_ren_ident (r:ren_ident) : t =
  match r.r_prefix with
  | Some p -> List [Atom "ren_ident";Atom p;Atom r.r_str]
  | None -> Atom ("ident_" ^ r.r_str)

let rec sexp_of_expr : P.expression -> t = fun e ->
  match e.P.exp_desc with
  | Ident (None,id) ->  Atom ("ident_" ^ id)
  | Ident (Some p,id) ->  List [Atom "ren_ident";Atom p;Atom id]
  | Dollar (None,id) -> List [Atom "$0";Atom ("ident_" ^ id)]
  | Dollar (Some p,id) ->List [Atom "$0"; List [Atom "ren_ident";Atom p;Atom id]]
  | Builtin_0 bi -> Atom (builtin0_to_string bi)
  | Builtin_1 (bi,e) -> List [Atom (builtin1_to_string bi);sexp_of_expr e]
  | Builtin_2 (bi,e1,e2) ->
    List [Atom (builtin2_to_string bi);sexp_of_expr e1;sexp_of_expr e2]
  | Pbool p -> List [Atom "bool"; (sexp_of_pred p)]
  | Sequence elst ->
    List ((Atom "Seq")::(List.map sexp_of_expr (Nlist.to_list elst)))
  | Extension elst ->
    List ((Atom "Ext")::(List.map sexp_of_expr (Nlist.to_list elst)))
  | Comprehension (ids,p) ->
    let ids = List (List.map sexp_of_lident (Nlist.to_list ids)) in
    List [Atom "Compr";ids;sexp_of_pred p]
  | Binder (bi,xlst,p,e) ->
    let ids = List (List.map sexp_of_lident (Nlist.to_list xlst)) in
    List [Atom (binder_to_string bi);ids;sexp_of_pred p;sexp_of_expr e]
  | Record_Field_Access (e,id) ->
    List [Atom "Record_Field_Access";sexp_of_expr e;sexp_of_lident id]
  | Record nlst ->
    let aux (id,e) = List [sexp_of_lident id;sexp_of_expr e] in
    List ((Atom "Rec")::(List.map aux (Nlist.to_list nlst)))
  | Record_Type nlst ->
    let aux (id,e) = List [sexp_of_lident id;sexp_of_expr e] in
    List ((Atom "Rec")::(List.map aux (Nlist.to_list nlst)))

and sexp_of_pred : P.predicate -> t = fun p ->
  match p.P.prd_desc with
  | P_Builtin Btrue -> Atom "btrue"
  | P_Builtin Bfalse -> Atom "bfalse"
  | Binary_Prop (bop,p,q) ->
    List [Atom (prop_bop_to_string bop);sexp_of_pred p;sexp_of_pred q]
  | Binary_Pred (bop,e1,e2) ->
    List [Atom (pred_bop_to_string bop);sexp_of_expr e1;sexp_of_expr e2]
  | Negation p -> List [Atom "Neg";sexp_of_pred p]
  | Universal_Q (xlst,p) ->
    let ids = List (List.map sexp_of_lident (Nlist.to_list xlst)) in
    List [Atom "!";ids;sexp_of_pred p]
  | Existential_Q (xlst,p) ->
    let ids = List (List.map sexp_of_lident (Nlist.to_list xlst)) in
    List [Atom "#";ids;sexp_of_pred p]

let rec sexp_of_subst : P.substitution -> t = fun s ->
  match s.P.sub_desc with
  | Skip -> Atom "SKIP"
  | Affectation (Tuple xlst,e) ->
    let ids = List (List.map sexp_of_ren_ident (Nlist.to_list xlst)) in
    let exprs = List [sexp_of_expr e] in
    List [Atom "AFF1";ids;exprs]
  | Affectation (Function(f,elst),a) ->
    let exprs = List (List.map sexp_of_expr (Nlist.to_list elst)) in
    List [Atom "AFF2";sexp_of_ren_ident f;exprs;sexp_of_expr a]
  | Affectation (Record(id,fd),e) ->
    List [Atom "AFF3";sexp_of_ren_ident id;sexp_of_lident fd;sexp_of_expr e]
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
    let ids = List (List.map sexp_of_lident (Nlist.to_list xlst)) in
    List [Atom "ANY";ids;sexp_of_pred p;sexp_of_subst s]
  | Let (xlst,ylst,s) ->
    let ids = List (List.map sexp_of_lident (Nlist.to_list xlst)) in
    let aux (id,e) = List [sexp_of_lident id;sexp_of_expr e] in
    let exprs = List (List.map aux (Nlist.to_list ylst)) in
    List [Atom "LET";ids;exprs;sexp_of_subst s]
  | BecomesElt (xlst,e) ->
    let ids = List (List.map sexp_of_ren_ident (Nlist.to_list xlst)) in
    List [Atom "BECOMESELT";ids;sexp_of_expr e]
  | BecomesSuch (xlst,p) ->
    let ids = List (List.map sexp_of_ren_ident (Nlist.to_list xlst)) in
    List [Atom "BECOMESSUCH";ids;sexp_of_pred p]
  | Var (xlst,s) ->
    let ids = List (List.map sexp_of_lident (Nlist.to_list xlst)) in
    List [Atom "VAR";ids;sexp_of_subst s]
  | CallUp (outs,f,args) ->
    List ( ((Atom "CALL")::(List.map sexp_of_ren_ident outs))@
          ((sexp_of_ren_ident f)::(List.map sexp_of_expr args)) )
  | While (p,s,q,e) ->
    List [Atom "WHILE";sexp_of_pred p;sexp_of_subst s;
          sexp_of_pred q;sexp_of_expr e]
  | Sequencement (s1,s2) ->
    List [Atom "SEQ";sexp_of_subst s1;sexp_of_subst s2]
  | Parallel (s1,s2) ->
    List [Atom "PAR";sexp_of_subst s1;sexp_of_subst s2]


(*
let add lst f = function
  | None -> lst
  | Some x -> (f x)::lst
*)

let sexp_of_set : P.set -> t = function
  | Abstract_Set v -> sexp_of_lident v
  | Concrete_Set (v,lst) ->
    List[sexp_of_lident v;List (List.map sexp_of_lident lst)]

let sexp_of_minst (mi:P.machine_instanciation) : t =
  List [sexp_of_ren_ident mi.mi_mch; List (List.map sexp_of_expr mi.mi_params)]

let sexp_of_op (op:P.operation) : t =
  List [ List (List.map sexp_of_lident op.P.op_in);
         sexp_of_lident op.P.op_name;
         List (List.map sexp_of_lident op.P.op_out);
         sexp_of_subst op.op_body ]

let sexp_of_clause : P.clause -> t = fun c ->
  match c with
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
  | Values nlst -> let aux (id,e) = List [sexp_of_lident id;sexp_of_expr e] in
    List ( (Atom "VALUES")::(List.map aux (Nlist.to_list nlst)) )
  | Sees nlst -> List ( (Atom "SEES")::(List.map sexp_of_ren_ident (Nlist.to_list nlst)) )
  | Promotes nlst -> List ( (Atom "PROMOTES")::(List.map sexp_of_lident (Nlist.to_list nlst)) )
  | Uses nlst -> List ( (Atom "USES")::(List.map sexp_of_ren_ident (Nlist.to_list nlst)) )
  | Sets nlst -> List ( (Atom "SETS")::(List.map sexp_of_set (Nlist.to_list nlst)) )
  | Constants nlst -> List ( (Atom "CONSTANTS")::(List.map sexp_of_lident (Nlist.to_list nlst)) )
  | Abstract_constants nlst -> List ( (Atom "ABSTRACT_CONSTANTS")::(List.map sexp_of_lident (Nlist.to_list nlst)) )
  | Concrete_variables nlst -> List ( (Atom "CONCRETE_VARIABLES")::(List.map sexp_of_lident (Nlist.to_list nlst)) )
  | Variables nlst -> List ( (Atom "VARIABLES")::(List.map sexp_of_lident (Nlist.to_list nlst)) )
  | Refines abs -> List [Atom "REFINES";Atom abs.lid_str]

let sexp_of_mch name parameters clauses : t =
  List ( (Atom "MACHINE")::(Atom ("ident_" ^ name.lid_str))::
         (List (List.map sexp_of_lident parameters))::
         (List.map sexp_of_clause clauses) )

let sexp_of_ref name parameters clauses : t =
  List ( (Atom "REFINEMENT")::(Atom ("ident_" ^ name.lid_str))::
         (List (List.map sexp_of_lident parameters))::
         (List.map sexp_of_clause clauses) )

let sexp_of_imp name parameters clauses : t =
  List ( (Atom "IMPLEMENTATION")::(Atom ("ident_"^name.lid_str))::
         (List (List.map sexp_of_lident parameters))::
         (List.map sexp_of_clause clauses) )

let sexp_of_component co =
  match co.P.co_desc with
  | Machine mch -> sexp_of_mch co.P.co_name mch.P.mch_parameters (P.get_clauses co)
  | Refinement ref -> sexp_of_ref co.P.co_name ref.P.ref_parameters (P.get_clauses co)
  | Implementation imp -> sexp_of_imp co.P.co_name imp.P.imp_parameters (P.get_clauses co)
