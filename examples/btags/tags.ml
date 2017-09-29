open Utils

type scope =
  | Clause of string
  | CSet of string

type tag = { tagname:string; (* also used for tagaddress *)
             tagkind:char; (* s:set, c:constant, v: variable, o:operation,
                              l:local operation, C: abstract constant,
                              V: concrete variable, m: machine, r: refinement
                              i:implementation, p: parameter
                              e: element of concrete set *)
             tagfile:string; tagline:int; tagcolumn:int;
             tagscope:scope; }

let compare t1 t2 = String.compare t1.tagname t2.tagname

type tags = tag list
let empty = []

let clause = function
  | 's' -> "SETS"
  | 'c' -> "CONSTANTS"
  | 'v' -> "VARIABLES"
  | 'o' -> "OPERATIONS"
  | 'l' -> "LOCAL_OPERATIONS"
  | 'C' -> "ABSTRACT_CONSTANTS"
  | 'V' -> "CONCRETE_VARIABLES"
  | 'm' -> "MACHINE"
  | 'r' -> "REFINEMENT"
  | 'i' -> "IMPLEMENTATION"
  | 'p' -> "PARAMETERS"
  | _ -> ""

let mk_tag (c:char) ((lc,id):u_ident) (sc:u_ident option) : tag =
  { tagname = id;
    tagkind = c;
    tagfile = lc.Lexing.pos_fname;
    tagline = lc.Lexing.pos_lnum;
    tagcolumn = lc.Lexing.pos_cnum - lc.Lexing.pos_bol;
    tagscope = match sc with
      | None -> Clause (clause c)
      | Some (_,ss) -> CSet ss
  }

open Component

let add_params (tags:tags) (id:u_ident) : tags =
  (mk_tag 'p' id None)::tags

let add_promotes (tags:tags) (id:u_ident) : tags =
  (mk_tag 'o' id None)::tags

let add_sets (tags:tags) (s:loc set) : tags =
  let lst = match s with
    | Abstract_Set id -> [mk_tag 's' id None]
    | Concrete_Set (id,elts) ->
      (mk_tag 's' id None)::
      ( List.map (fun elt -> mk_tag 'e' elt (Some id)) elts )
  in
  lst@tags

let add_constant (tags:tags) (id:u_ident) : tags =
  (mk_tag 'c' id None)::tags
  
let add_abstract_constant (tags:tags) (id:u_ident) : tags =
  (mk_tag 'C' id None)::tags

let add_variable (tags:tags) (id:u_ident) : tags =
  (mk_tag 'v' id None)::tags

let add_concrete_variable (tags:tags) (id:u_ident) : tags =
  (mk_tag 'V' id None)::tags

let add_operation (tags:tags) (_,name,_,_:u_operation) : tags =
  (mk_tag 'o' name None)::tags

let add_local_operation (tags:tags) (_,name,_,_:u_operation) : tags =
  (mk_tag 'l' name None)::tags

let get_tags_mch (tags:tags) (mch:u_machine) : tags =
  let tags = (mk_tag 'm' mch.name None)::tags in
  let tags = List.fold_left add_params tags mch.parameters in
  let tags = match mch.clause_promotes with
    | None -> tags
    | Some (_,lst) -> List.fold_left add_promotes tags lst
  in
  let tags = match mch.clause_sets with
    | None -> tags
    | Some (_,lst) -> List.fold_left add_sets tags lst
  in
  let tags = match mch.clause_concrete_constants with
    | None -> tags
    | Some (_,lst) -> List.fold_left add_constant tags lst
  in
  let tags = match mch.clause_abstract_constants with
    | None -> tags
    | Some (_,lst) -> List.fold_left add_abstract_constant tags lst
  in
  let tags = match mch.clause_concrete_variables with
    | None -> tags
    | Some (_,lst) -> List.fold_left add_concrete_variable tags lst
  in
  let tags = match mch.clause_abstract_variables with
    | None -> tags
    | Some (_,lst) -> List.fold_left add_variable tags lst
  in
  let tags = match mch.clause_operations with
    | None -> tags
    | Some (_,lst) -> List.fold_left add_operation tags lst
  in
  tags

let get_tags_ref (tags:tags) (ref:u_refinement) : tags =
  let tags = (mk_tag 'r' ref.name None)::tags in
  let tags = List.fold_left add_params tags ref.parameters in
  let tags = match ref.clause_promotes with
    | None -> tags
    | Some (_,lst) -> List.fold_left add_promotes tags lst
  in
  let tags = match ref.clause_sets with
    | None -> tags
    | Some (_,lst) -> List.fold_left add_sets tags lst
  in
  let tags = match ref.clause_concrete_constants with
    | None -> tags
    | Some (_,lst) -> List.fold_left add_constant tags lst
  in
  let tags = match ref.clause_abstract_constants with
    | None -> tags
    | Some (_,lst) -> List.fold_left add_abstract_constant tags lst
  in
  let tags = match ref.clause_concrete_variables with
    | None -> tags
    | Some (_,lst) -> List.fold_left add_concrete_variable tags lst
  in
  let tags = match ref.clause_abstract_variables with
    | None -> tags
    | Some (_,lst) -> List.fold_left add_variable tags lst
  in
  let tags = match ref.clause_operations with
    | None -> tags
    | Some (_,lst) -> List.fold_left add_operation tags lst
  in
  let tags = match ref.clause_local_operations with
    | None -> tags
    | Some (_,lst) -> List.fold_left add_local_operation tags lst
  in
  tags

let get_tags_imp (tags:tags) (imp:u_implementation) : tags =
  let tags = (mk_tag 'i' imp.name None)::tags in
  let tags = List.fold_left add_params tags imp.parameters in
  let tags = match imp.clause_promotes with
    | None -> tags
    | Some (_,lst) -> List.fold_left add_promotes tags lst
  in
  let tags = match imp.clause_sets with
    | None -> tags
    | Some (_,lst) -> List.fold_left add_sets tags lst
  in
  let tags = match imp.clause_concrete_constants with
    | None -> tags
    | Some (_,lst) -> List.fold_left add_constant tags lst
  in
  let tags = match imp.clause_concrete_variables with
    | None -> tags
    | Some (_,lst) -> List.fold_left add_concrete_variable tags lst
  in
  let tags = match imp.clause_operations_B0 with
    | None -> tags
    | Some (_,lst) -> List.fold_left add_operation tags lst
  in
  let tags = match imp.clause_local_operations_B0 with
    | None -> tags
    | Some (_,lst) -> List.fold_left add_local_operation tags lst
  in
  tags

let add_tags (tags:tags) : u_comp -> tags = function
  | Abstract_machine mch -> get_tags_mch tags mch
  | Refinement ref -> get_tags_ref tags ref
  | Implementation imp -> get_tags_imp tags imp

let pp_scope out = function
  | Clause s -> Printf.fprintf out "clause:%s" s
  | CSet id -> Printf.fprintf out "set:%s" id

let print_tags (out:out_channel) (tags:tags) : unit =
  let tags = List.sort compare tags in
  (* {tagname}<Tab>{tagfile}<Tab>{tagaddress};<Double Guillement><Tab>{kind}<Tab>line:{line}<Tab>column:{column} *)
  List.iter (fun tag ->
      Printf.fprintf out "%s\t%s\t%i;\"\t%c\tline:%i\tcolumn:%i\t%a\n"
     tag.tagname tag.tagfile tag.tagline tag.tagkind tag.tagline tag.tagcolumn pp_scope tag.tagscope
    ) tags
