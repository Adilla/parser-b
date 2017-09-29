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

let sexp_of_ident (_,s:'lc Utils.ident) : t = Atom ("ident_" ^ s)
let sexp_of_ident2 (_,(_,s):'ty*'lc Utils.ident) : t = Atom ("ident_" ^ s)

let rec sexp_of_expr : ('lc,'ty) expression -> t = function
  | Ident (_,id) -> sexp_of_ident id
  | Dollar (_,id) -> Atom ("ident_" ^ snd id ^ "$0")
  | Builtin (_,_,bi) -> Atom (builtin_to_string bi)
  | Pbool (_,_,p) -> List [Atom "bool"; (sexp_of_pred p)]
  | Application (_,_,e1,e2) -> List [Atom "App";sexp_of_expr e1;sexp_of_expr e2]
  | Couple (_,_,cm,e1,e2) ->
    let atm = match cm with
      | Maplet -> "Pair_maplet"
      | Comma -> "Pair_comma"
      | Infix -> "Pair_infix"
    in
    List [Atom atm;sexp_of_expr e1;sexp_of_expr e2]
  | Sequence (_,_,(e,elst)) ->
    List ((Atom "Seq")::(sexp_of_expr e)::(List.map sexp_of_expr elst))
  | Extension (_,_,(e,elst)) ->
    List ((Atom "Ext")::(sexp_of_expr e)::(List.map sexp_of_expr elst))
  | Comprehension (_,_,(id,idlst),p) ->
    let ids = List ((sexp_of_ident2 id)::(List.map sexp_of_ident2 idlst)) in
    List [Atom "Compr";ids;sexp_of_pred p]
  | Binder (_,_,bi,(x,xlst),p,e) ->
    let ids = List ((sexp_of_ident2 x)::(List.map sexp_of_ident2 xlst)) in
    List [Atom (binder_to_string bi);ids;sexp_of_pred p;sexp_of_expr e]
  | Record_Field_Access (_,_,e,id) ->
    List [Atom "Record_Field_Access";sexp_of_expr e;sexp_of_ident id]
  | Record (_,_,(x,xlst)) ->
    let aux (id,e) = List [sexp_of_ident id;sexp_of_expr e] in
    List ((Atom "Rec")::(aux x)::(List.map aux xlst))
  | Record_Type (_,_,(x,xlst)) ->
    let aux (id,e) = List [sexp_of_ident id;sexp_of_expr e] in
    List ((Atom "Rec")::(aux x)::(List.map aux xlst))

and sexp_of_pred : ('lc,'ty) predicate -> t = function
(*   | P_Ident id -> sexp_of_ident id *)
  | P_Builtin (_,Btrue) -> Atom "btrue"
  | P_Builtin (_,Bfalse) -> Atom "bfalse"
  | Binary_Prop (_,bop,p,q) ->
    List [Atom (prop_bop_to_string bop);sexp_of_pred p;sexp_of_pred q]
  | Binary_Pred (_,bop,e1,e2) ->
    List [Atom (pred_bop_to_string bop);sexp_of_expr e1;sexp_of_expr e2]
  | Negation (_,p) -> List [Atom "Neg";sexp_of_pred p]
  | Universal_Q (_,(x,xlst),p) ->
    let ids = List ((sexp_of_ident2 x)::(List.map sexp_of_ident2 xlst)) in
    List [Atom "!";ids;sexp_of_pred p]
  | Existential_Q (_,(x,xlst),p) ->
    let ids = List ((sexp_of_ident2 x)::(List.map sexp_of_ident2 xlst)) in
    List [Atom "#";ids;sexp_of_pred p]

open Substitution

let rec sexp_of_subst : ('lc,'ty) substitution -> t = function
  | Skip _ -> Atom "SKIP"
  | Affectation (_,(x,xlst),e) ->
    let ids = List ((sexp_of_ident x)::(List.map sexp_of_ident xlst)) in
    let exprs = List [sexp_of_expr e] in
    List [Atom "AFF1";ids;exprs]
  | Function_Affectation (_,f,(e,elst),a) ->
    let exprs = List ((sexp_of_expr e)::(List.map sexp_of_expr elst)) in
    List [Atom "AFF2";sexp_of_ident f;exprs;sexp_of_expr a]
  | Record_Affectation (_,id,fd,e) ->
    List [Atom "AFF3";sexp_of_ident id;sexp_of_ident fd;sexp_of_expr e]
  | Pre (_,p,s) -> List [Atom "PRE"; sexp_of_pred p; sexp_of_subst s]
  | Assert (_,p,s) -> List [Atom "ASSERT"; sexp_of_pred p; sexp_of_subst s]
  | Choice (_,(s,slst)) ->
    List ( (Atom "CHOICE")::(sexp_of_subst s)::(List.map sexp_of_subst slst))
  | IfThenElse (_,(y,ylst),s_opt) ->
    let aux (p,s) = List [sexp_of_pred p;sexp_of_subst s] in
    let yy = List ((aux y)::(List.map aux ylst)) in
    begin match s_opt with
      | None -> List [Atom "IF";yy]
      | Some els -> List [Atom "IF";yy;sexp_of_subst els]
    end
  | Select (_,(y,ylst),s_opt) ->
   let aux (p,s) = List [sexp_of_pred p;sexp_of_subst s] in
    let yy = List ((aux y)::(List.map aux ylst)) in
    begin match s_opt with
      | None -> List [Atom "SELECT";yy]
      | Some els -> List [Atom "SELECT";yy;sexp_of_subst els]
    end
  | Case (_,e,(y,ylst),s_opt) ->
    let aux (e,s) = List [sexp_of_expr e;sexp_of_subst s] in
    let yy = List ((aux y)::(List.map aux ylst)) in
    begin match s_opt with
      | None -> List [Atom "SELECT";sexp_of_expr e;yy]
      | Some els -> List [Atom "SELECT";sexp_of_expr e;yy;sexp_of_subst els]
    end
  | Any (_,(x,xlst),p,s) ->
    let ids = List ((sexp_of_ident x)::(List.map sexp_of_ident xlst)) in
    List [Atom "ANY";ids;sexp_of_pred p;sexp_of_subst s]
  | Let (_,(x,xlst),(y,ylst),s) ->
    let ids = List ((sexp_of_ident x)::(List.map sexp_of_ident xlst)) in
    let aux (id,e) = List [sexp_of_ident id;sexp_of_expr e] in
    let exprs = List ((aux y)::(List.map aux ylst)) in
    List [Atom "LET";ids;exprs;sexp_of_subst s]
  | BecomesElt (_,(x,xlst),e) ->
    let ids = List ((sexp_of_ident x)::(List.map sexp_of_ident xlst)) in
    List [Atom "BECOMESELT";ids;sexp_of_expr e]
  | BecomesSuch (_,(x,xlst),p) ->
    let ids = List ((sexp_of_ident x)::(List.map sexp_of_ident xlst)) in
    List [Atom "BECOMESSUCH";ids;sexp_of_pred p]
  | Var (_,(x,xlst),s) ->
    let ids = List ((sexp_of_ident x)::(List.map sexp_of_ident xlst)) in
    List [Atom "VAR";ids;sexp_of_subst s]
  | CallUp (_,outs,f,args) ->
    List ( ((Atom "CALL")::(List.map sexp_of_ident outs))@
          ((sexp_of_ident f)::(List.map sexp_of_expr args)) )
  | While (_,p,s,q,e) ->
    List [Atom "WHILE";sexp_of_pred p;sexp_of_subst s;
          sexp_of_pred q;sexp_of_expr e]
  | Sequencement (_,s1,s2) ->
    List [Atom "SEQ";sexp_of_subst s1;sexp_of_subst s2]
  | Parallel (_,s1,s2) ->
    List [Atom "PAR";sexp_of_subst s1;sexp_of_subst s2]

open Component

let add lst f = function
  | None -> lst
  | Some x -> (f x)::lst

let sexp_of_set : 'lc set -> t = function
  | Abstract_Set id -> sexp_of_ident id
  | Concrete_Set (id,lst) ->
    List[sexp_of_ident id;List (List.map sexp_of_ident lst)]

let sexp_of_minst (id,args:('lc,'ty) machine_instanciation) : t =
  List [Atom (snd id); List (List.map sexp_of_expr args)]

let sexp_of_op (out,name,args,body:('lc,'ty) operation) : t =
  List [ List (List.map sexp_of_ident2 out);
         sexp_of_ident name;
         List (List.map sexp_of_ident2 args);
         sexp_of_subst body ]

let sexp_of_clause : ('lc,'ty) clause -> t = function
  | Constraints (l,p) -> List [Atom "CONSTRAINTS";sexp_of_pred p]
  | Imports (l,lst) -> List ( (Atom "IMPORTS")::(List.map sexp_of_minst lst) )
  | Includes (l,lst) -> List ( (Atom "INCLUDES")::(List.map sexp_of_minst lst) )
  | Extends (l,lst) -> List ( (Atom "EXTENDS")::(List.map sexp_of_minst lst) )
  | Properties (l,p) -> List [Atom "PROPERTIES";sexp_of_pred p]
  | Invariant (l,p) -> List [Atom "INVARIANT";sexp_of_pred p]
  | Assertions (l,lst) -> List ((Atom "ASSERTIONS")::(List.map sexp_of_pred lst))
  | Initialization (l,s) -> List [Atom "INITIALISATION";sexp_of_subst s]
  | Operations (l,lst) -> List ( (Atom "OPERATIONS")::(List.map sexp_of_op lst) )
  | Local_Operations (l,lst) -> List ( (Atom "LOCAL_OPERATIONS")::(List.map sexp_of_op lst) )
  | Values (l,lst) ->
    let aux (id,e) = List [sexp_of_ident id;sexp_of_expr e] in
    List ( (Atom "VALUES")::(List.map aux lst) )
  | Sees (l,lst) -> List ( (Atom "SEES")::(List.map sexp_of_ident lst) )
  | Promotes (l,lst) -> List ( (Atom "PROMOTES")::(List.map sexp_of_ident lst) )
  | Uses (l,lst) -> List ( (Atom "USES")::(List.map sexp_of_ident lst) )
  | Sets (l,lst) -> List ( (Atom "SETS")::(List.map sexp_of_set lst) )
  | Constants (l,lst) -> List ( (Atom "CONSTANTS")::(List.map sexp_of_ident lst) )
  | Abstract_constants (l,lst) ->
    List ( (Atom "ABSTRACT_CONSTANTS")::(List.map sexp_of_ident lst) )
  | Concrete_variables (l,lst) ->
    List ( (Atom "CONCRETE_VARIABLES")::(List.map sexp_of_ident lst) )
  | Variables (l,lst) -> List ( (Atom "VARIABLES")::(List.map sexp_of_ident lst) )

let sexp_of_mch (mch:('lc,'ty) abstract_machine) : t =
  List ( (Atom "MACHINE")::(sexp_of_ident mch.name)::
         (List (List.map sexp_of_ident mch.parameters))::
         (List.map sexp_of_clause (clist_of_mch mch)) )

let sexp_of_ref (ref:('lc,'ty) refinement) : t =
  List ( (Atom "REFINEMENT")::(sexp_of_ident ref.name)::
         (List (List.map sexp_of_ident ref.parameters))::
         (List [Atom "REFINES";sexp_of_ident ref.refines])::
         (List.map sexp_of_clause (clist_of_ref ref)) )

let sexp_of_imp (imp:('lc,'ty) implementation) : t =
  List ( (Atom "IMPLEMENTATION")::(sexp_of_ident imp.name)::
         (List (List.map sexp_of_ident imp.parameters))::
         (List [Atom "REFINES";sexp_of_ident imp.refines])::
         (List.map sexp_of_clause (clist_of_imp imp)) )

let sexp_of_component = function 
  | Abstract_machine mch -> sexp_of_mch mch
  | Refinement ref -> sexp_of_ref ref
  | Implementation imp -> sexp_of_imp imp
