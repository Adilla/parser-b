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

let to_string (x:t) : string =
  Easy_format.Pretty.to_string (to_easy_format x)

let to_channel (out:out_channel) (x:t) : unit =
  Easy_format.Pretty.to_channel out (to_easy_format x)

open Expression

let sexp_of_ident (_,s:Utils.ident) : t = Atom ("ident_" ^ s)

(*
let rec get_and_list = function
  | Binary_Prop (_,Conjonction,p1,p2) -> (get_and_list p1)@(get_and_list p2)
  | s -> [s]

let rec get_or_list = function
  | Binary_Prop (_,Disjonction,p1,p2) -> (get_or_list p1)@(get_or_list p2)
  | s -> [s]
*)

let rec sexp_of_expr : expression -> t = function
  | Ident id -> sexp_of_ident id
  | Dollar id -> Atom ("ident_" ^ snd id ^ "$0")
  | Builtin (_,bi) -> Atom (builtin_to_string bi)
  | Pbool (_,p) -> List [Atom "bool"; (sexp_of_pred p)]
  | Parentheses (_,e) -> sexp_of_expr e (*FIXME*)
  | Application (_,e1,e2) -> List [Atom "App";sexp_of_expr e1;sexp_of_expr e2]
  | Couple (_,cm,e1,e2) ->
    let atm = match cm with
      | Maplet -> "Pair_maplet"
      | Comma -> "Pair_comma"
      | Infix -> "Pair_infix"
    in
    List [Atom atm;sexp_of_expr e1;sexp_of_expr e2]
  | Sequence (_,(e,elst)) ->
    List ((Atom "Seq")::(sexp_of_expr e)::(List.map sexp_of_expr elst))
  | Extension (_,(e,elst)) ->
    List ((Atom "Ext")::(sexp_of_expr e)::(List.map sexp_of_expr elst))
  | Comprehension (_,(id,idlst),p) ->
    let ids = List ((sexp_of_ident id)::(List.map sexp_of_ident idlst)) in
    List [Atom "Compr";ids;sexp_of_pred p]
  | Binder (_,bi,(x,xlst),p,e) ->
    let ids = List ((sexp_of_ident x)::(List.map sexp_of_ident xlst)) in
    List [Atom (binder_to_string bi);ids;sexp_of_pred p;sexp_of_expr e]
  | Record_Field_Access (_,e,id) ->
    List [Atom "Record_Field_Access";sexp_of_expr e;sexp_of_ident id]
  | Record (_,(x,xlst)) ->
    let aux = function
      | None, e -> sexp_of_expr e
      | Some id, e -> List [sexp_of_ident id; sexp_of_expr e]
    in
    List ((Atom "Rec")::(aux x)::(List.map aux xlst))
  | Record_Type (_,(x,xlst)) ->
    let aux (id,e) = List [sexp_of_ident id;sexp_of_expr e] in
    List ((Atom "Rec")::(aux x)::(List.map aux xlst))

and sexp_of_pred : predicate -> t = function
  | P_Ident id -> sexp_of_ident id
  | P_Builtin (_,Btrue) -> Atom "btrue"
  | P_Builtin (_,Bfalse) -> Atom "bfalse"
  | Binary_Prop (_,bop,p,q) ->
    List [Atom (prop_bop_to_string bop);sexp_of_pred p;sexp_of_pred q]
  | Binary_Pred (_,bop,e1,e2) ->
    List [Atom (pred_bop_to_string bop);sexp_of_expr e1;sexp_of_expr e2]
  | Negation (_,p) -> List [Atom "Neg";sexp_of_pred p]
  | Pparentheses (_,p) -> sexp_of_pred p (*FIXME*)
  | Universal_Q (_,(x,xlst),p) ->
    let ids = List ((sexp_of_ident x)::(List.map sexp_of_ident xlst)) in
    List [Atom "!";ids;sexp_of_pred p]
  | Existential_Q (_,(x,xlst),p) ->
    let ids = List ((sexp_of_ident x)::(List.map sexp_of_ident xlst)) in
    List [Atom "#";ids;sexp_of_pred p]

open Substitution

(*
let rec get_seq_list = function
  | Sequencement (s1,s2) -> (get_seq_list s1)@(get_seq_list s2)
  | s -> [s]

let rec get_par_list = function
  | Parallel (s1,s2) -> (get_par_list s1)@(get_par_list s2)
  | s -> [s]
*)

let rec sexp_of_subst : substitution -> t = function
  | Skip -> Atom "SKIP"
  | BeginEnd s -> sexp_of_subst s (*FIXME*)
  | Affectation ((x,xlst),(e,elst)) ->
    let ids = List ((sexp_of_ident x)::(List.map sexp_of_ident xlst)) in
    let exprs = List ((sexp_of_expr e)::(List.map sexp_of_expr elst)) in
    List [Atom "AFF1";ids;exprs]
  | Function_Affectation (f,(e,elst),a) ->
    let exprs = List ((sexp_of_expr e)::(List.map sexp_of_expr elst)) in
    List [Atom "AFF2";sexp_of_ident f;exprs;sexp_of_expr a]
  | Record_Affectation (id,fd,e) ->
    List [Atom "AFF3";sexp_of_ident id;sexp_of_ident fd;sexp_of_expr e]
  | Pre (p,s) -> List [Atom "PRE"; sexp_of_pred p; sexp_of_subst s]
  | Assert (p,s) -> List [Atom "ASSERT"; sexp_of_pred p; sexp_of_subst s]
  | Choice (s,slst) ->
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
    let ids = List ((sexp_of_ident x)::(List.map sexp_of_ident xlst)) in
    List [Atom "ANY";ids;sexp_of_pred p;sexp_of_subst s]
  | Let ((x,xlst),(y,ylst),s) ->
    let ids = List ((sexp_of_ident x)::(List.map sexp_of_ident xlst)) in
    let aux (id,e) = List [sexp_of_ident id;sexp_of_expr e] in
    let exprs = List ((aux y)::(List.map aux ylst)) in
    List [Atom "LET";ids;exprs;sexp_of_subst s]
  | BecomesElt ((x,xlst),e) ->
    let ids = List ((sexp_of_ident x)::(List.map sexp_of_ident xlst)) in
    List [Atom "BECOMESELT";ids;sexp_of_expr e]
  | BecomesSuch ((x,xlst),p) ->
    let ids = List ((sexp_of_ident x)::(List.map sexp_of_ident xlst)) in
    List [Atom "BECOMESSUCH";ids;sexp_of_pred p]
  | Var ((x,xlst),s) ->
    let ids = List ((sexp_of_ident x)::(List.map sexp_of_ident xlst)) in
    List [Atom "VAR";ids;sexp_of_subst s]
  | CallUp (outs,f,args) ->
    List ( ((Atom "CALL")::(List.map sexp_of_ident outs))@
          ((sexp_of_ident f)::(List.map sexp_of_expr args)) )
  | While (p,s,q,e) ->
    List [Atom "WHILE";sexp_of_pred p;sexp_of_subst s;
          sexp_of_pred q;sexp_of_expr e]
  | Sequencement (s1,s2) ->
    List [Atom "SEQ";sexp_of_subst s1;sexp_of_subst s2]
  | Parallel (s1,s2) ->
    List [Atom "PAR";sexp_of_subst s1;sexp_of_subst s2]

open Component

let add lst f = function
  | None -> lst
  | Some x -> (f x)::lst

let sexp_of_set : set -> t = function
  | Abstract_Set id -> sexp_of_ident id
  | Concrete_Set (id,lst) ->
    List[sexp_of_ident id;List (List.map sexp_of_ident lst)]

let sexp_of_minst (id,args:machine_instanciation) : t =
  List [Atom (snd id); List (List.map sexp_of_expr args)]

let sexp_of_op (out,name,args,body:operation) : t =
  List [ List (List.map sexp_of_ident out);
         sexp_of_ident name;
         List (List.map sexp_of_ident args);
         sexp_of_subst body ]

let mk_operations (_,lst) =
  List ( (Atom "OPERATIONS")::(List.map sexp_of_op lst) )
let mk_local_operations (_,lst) =
  List ( (Atom "LOCAL_OPERATIONS")::(List.map sexp_of_op lst) )
let mk_initialisation (_,s) =
  List [Atom "INITIALISATION";sexp_of_subst s]
let mk_assertions (_,lst) =
  List ((Atom "ASSERTIONS")::(List.map sexp_of_pred lst))
let mk_invariant (_,p) =
  List [Atom "INVARIANT";sexp_of_pred p]
let mk_variables (_,lst) =
  List ( (Atom "VARIABLES")::(List.map sexp_of_ident lst) )
let mk_concrete_variables (_,lst) =
  List ( (Atom "CONCRETE_VARIABLES")::(List.map sexp_of_ident lst) )
let mk_properties (_,p) =
  List [Atom "PROPERTIES";sexp_of_pred p]
let mk_abstract_constants (_,lst) =
  List ( (Atom "ABSTRACT_CONSTANTS")::(List.map sexp_of_ident lst) )
let mk_constants (_,lst) =
  List ( (Atom "CONSTANTS")::(List.map sexp_of_ident lst) )
let mk_sets (_,lst) =
  List ( (Atom "SETS")::(List.map sexp_of_set lst) )
let mk_uses (_,lst) =
  List ( (Atom "USES")::(List.map sexp_of_ident lst) )
let mk_extends (_,lst) =
  List ( (Atom "EXTENDS")::(List.map sexp_of_minst lst) )
let mk_promotes (_,lst) =
  List ( (Atom "PROMOTES")::(List.map sexp_of_ident lst) )
let mk_includes (_,lst) =
  List ( (Atom "INCLUDES")::(List.map sexp_of_minst lst) )
let mk_sees (_,lst) =
  List ( (Atom "SEES")::(List.map sexp_of_ident lst) )
let mk_constraints (_,p) =
  List [Atom "CONSTRAINTS";sexp_of_pred p]
let mk_imports (_,lst) =
  List ( (Atom "IMPORTS")::(List.map sexp_of_minst lst) )
let mk_values (_,lst) =
  let aux (id,e) = List [sexp_of_ident id;sexp_of_expr e] in
  List ( (Atom "VALUES")::(List.map aux lst) )

let sexp_of_mch (mch:abstract_machine) : t =
  let lst = [] in
  let lst = add lst mk_operations mch.clause_operations in
  let lst = add lst mk_initialisation mch.clause_initialisation in
  let lst = add lst mk_assertions mch.clause_assertions in
  let lst = add lst mk_invariant mch.clause_invariant in
  let lst = add lst mk_variables mch.clause_abstract_variables in
  let lst = add lst mk_concrete_variables mch.clause_concrete_variables in
  let lst = add lst mk_properties mch.clause_properties in
  let lst = add lst mk_abstract_constants mch.clause_abstract_constants in
  let lst = add lst mk_constants mch.clause_concrete_constants in
  let lst = add lst mk_sets mch.clause_sets in
  let lst = add lst mk_uses mch.clause_uses in
  let lst = add lst mk_extends mch.clause_extends in
  let lst = add lst mk_promotes mch.clause_promotes in
  let lst = add lst mk_includes mch.clause_includes in
  let lst = add lst mk_constraints mch.clause_constraints in
  List ( (Atom "MACHINE")::(sexp_of_ident mch.name)::
         (List (List.map sexp_of_ident mch.parameters))::lst)

let sexp_of_ref (ref:refinement) : t =
  let lst = [] in
  let lst = add lst mk_operations ref.clause_operations in
  let lst = add lst mk_local_operations ref.clause_local_operations in
  let lst = add lst mk_initialisation ref.clause_initialisation in
  let lst = add lst mk_assertions ref.clause_assertions in
  let lst = add lst mk_invariant ref.clause_invariant in
  let lst = add lst mk_variables ref.clause_abstract_variables in
  let lst = add lst mk_concrete_variables ref.clause_concrete_variables in
  let lst = add lst mk_properties ref.clause_properties in
  let lst = add lst mk_abstract_constants ref.clause_abstract_constants in
  let lst = add lst mk_constants ref.clause_concrete_constants in
  let lst = add lst mk_sets ref.clause_sets in
  let lst = add lst mk_extends ref.clause_extends in
  let lst = add lst mk_promotes ref.clause_promotes in
  let lst = add lst mk_includes ref.clause_includes in
  let lst = add lst mk_sees ref.clause_sees in
  List ( (Atom "REFINEMENT")::(sexp_of_ident ref.name)::
         (List (List.map sexp_of_ident ref.parameters))::
         (List [Atom "REFINES";sexp_of_ident ref.refines])::lst)

let sexp_of_imp (imp:implementation) : t =
  let lst = [] in
  let lst = add lst mk_operations imp.clause_operations_B0 in
  let lst = add lst mk_local_operations imp.clause_local_operations_B0 in
  let lst = add lst mk_initialisation imp.clause_initialisation_B0 in
  let lst = add lst mk_assertions imp.clause_assertions in
  let lst = add lst mk_invariant imp.clause_invariant in
  let lst = add lst mk_concrete_variables imp.clause_concrete_variables in
  let lst = add lst mk_values imp.clause_values in
  let lst = add lst mk_properties imp.clause_properties in
  let lst = add lst mk_constants imp.clause_concrete_constants in
  let lst = add lst mk_sets imp.clause_sets in
  let lst = add lst mk_extends imp.clause_extends_B0 in
  let lst = add lst mk_promotes imp.clause_promotes in
  let lst = add lst mk_imports imp.clause_imports in
  let lst = add lst mk_sees imp.clause_sees in
  List ( (Atom "IMPLEMENTATION")::(sexp_of_ident imp.name)::
         (List (List.map sexp_of_ident imp.parameters))::
         (List [Atom "REFINES";sexp_of_ident imp.refines])::lst)

let sexp_of_component = function 
  | Abstract_machine mch -> sexp_of_mch mch
  | Refinement ref -> sexp_of_ref ref
  | Implementation imp -> sexp_of_imp imp
