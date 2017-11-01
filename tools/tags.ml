open Utils
open Syntax

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

let mk_tag (c:char) (lc:loc) (id:ident) (sc:ident option) : tag =
  { tagname = id;
    tagkind = c;
    tagfile = lc.Lexing.pos_fname;
    tagline = lc.Lexing.pos_lnum;
    tagcolumn = lc.Lexing.pos_cnum - lc.Lexing.pos_bol;
    tagscope = match sc with
      | None -> Clause (clause c)
      | Some ss -> CSet ss
  }

let add_params (tags:tags) (v:p_var) : tags =
  (mk_tag 'p' v.var_loc v.var_id None)::tags

let add_promotes (tags:tags) (name:loc lident) : tags =
  (mk_tag 'o' name.lid_loc name.lid_str None)::tags

let add_sets (tags:tags) (s:p_set) : tags =
  let lst = match s with
    | Abstract_Set s -> [mk_tag 's' s.var_loc s.var_id None]
    | Concrete_Set (s,elts) ->
      (mk_tag 's' s.var_loc s.var_id None)::
      ( List.map (fun elt -> mk_tag 'e' elt.var_loc elt.var_id (Some s.var_id)) elts )
  in
  lst@tags

let add_constant (tags:tags) (v:p_var) : tags =
  (mk_tag 'c' v.var_loc v.var_id None)::tags
  
let add_abstract_constant (tags:tags) (v:p_var) : tags =
  (mk_tag 'C' v.var_loc v.var_id None)::tags

let add_variable (tags:tags) (v:p_var) : tags =
  (mk_tag 'v' v.var_loc v.var_id None)::tags

let add_concrete_variable (tags:tags) (v:p_var) : tags =
  (mk_tag 'V' v.var_loc v.var_id None)::tags

let add_operation (tags:tags) (op:p_operation) : tags =
  (mk_tag 'o' op.op_name.lid_loc op.op_name.lid_str None)::tags

let add_local_operation (tags:tags) (op:p_operation) : tags =
  (mk_tag 'l' op.op_name.lid_loc op.op_name.lid_str None)::tags

let get_tags_mch (tags:tags) loc name parameters desc : tags =
  let tags = (mk_tag 'm' loc name None)::tags in
  let tags = List.fold_left add_params tags parameters in
  let tags = match desc.mch_promotes with
    | None -> tags
    | Some (_,nlst) -> List.fold_left add_promotes tags (Nlist.to_list nlst)
  in
  let tags = match desc.mch_sets with
    | None -> tags
    | Some (_,nlst) -> List.fold_left add_sets tags (Nlist.to_list nlst)
  in
  let tags = match desc.mch_concrete_constants with
    | None -> tags
    | Some (_,nlst) -> List.fold_left add_constant tags (Nlist.to_list nlst)
  in
  let tags = match desc.mch_abstract_constants with
    | None -> tags
    | Some (_,nlst) -> List.fold_left add_abstract_constant tags (Nlist.to_list nlst)
  in
  let tags = match desc.mch_concrete_variables with
    | None -> tags
    | Some (_,nlst) -> List.fold_left add_concrete_variable tags (Nlist.to_list nlst)
  in
  let tags = match desc.mch_abstract_variables with
    | None -> tags
    | Some (_,nlst) -> List.fold_left add_variable tags (Nlist.to_list nlst)
  in
  let tags = match desc.mch_operations with
    | None -> tags
    | Some (_,nlst) -> List.fold_left add_operation tags (Nlist.to_list nlst)
  in
  tags

let get_tags_ref (tags:tags) loc name parameters desc : tags =
  let tags = (mk_tag 'r' loc name None)::tags in
  let tags = List.fold_left add_params tags parameters in
  let tags = match desc.ref_promotes with
    | None -> tags
    | Some (_,nlst) -> List.fold_left add_promotes tags (Nlist.to_list nlst)
  in
  let tags = match desc.ref_sets with
    | None -> tags
    | Some (_,nlst) -> List.fold_left add_sets tags (Nlist.to_list nlst)
  in
  let tags = match desc.ref_concrete_constants with
    | None -> tags
    | Some (_,nlst) -> List.fold_left add_constant tags (Nlist.to_list nlst)
  in
  let tags = match desc.ref_abstract_constants with
    | None -> tags
    | Some (_,nlst) -> List.fold_left add_abstract_constant tags (Nlist.to_list nlst)
  in
  let tags = match desc.ref_concrete_variables with
    | None -> tags
    | Some (_,nlst) -> List.fold_left add_concrete_variable tags (Nlist.to_list nlst)
  in
  let tags = match desc.ref_abstract_variables with
    | None -> tags
    | Some (_,nlst) -> List.fold_left add_variable tags (Nlist.to_list nlst)
  in
  let tags = match desc.ref_operations with
    | None -> tags
    | Some (_,nlst) -> List.fold_left add_operation tags (Nlist.to_list nlst)
  in
  let tags = match desc.ref_local_operations with
    | None -> tags
    | Some (_,nlst) -> List.fold_left add_local_operation tags (Nlist.to_list nlst)
  in
  tags

let get_tags_imp (tags:tags) loc name parameters desc : tags =
  let tags = (mk_tag 'i' loc name None)::tags in
  let tags = List.fold_left add_params tags parameters in
  let tags = match desc.imp_promotes with
    | None -> tags
    | Some (_,nlst) -> List.fold_left add_promotes tags (Nlist.to_list nlst)
  in
  let tags = match desc.imp_sets with
    | None -> tags
    | Some (_,nlst) -> List.fold_left add_sets tags (Nlist.to_list nlst)
  in
  let tags = match desc.imp_concrete_constants with
    | None -> tags
    | Some (_,nlst) -> List.fold_left add_constant tags (Nlist.to_list nlst)
  in
  let tags = match desc.imp_concrete_variables with
    | None -> tags
    | Some (_,nlst) -> List.fold_left add_concrete_variable tags (Nlist.to_list nlst)
  in
  let tags = match desc.imp_operations with
    | None -> tags
    | Some (_,nlst) -> List.fold_left add_operation tags (Nlist.to_list nlst)
  in
  let tags = match desc.imp_local_operations with
    | None -> tags
    | Some (_,nlst) -> List.fold_left add_local_operation tags (Nlist.to_list nlst)
  in
  tags

let add_tags (tags:tags) co =
  match co.co_desc with
  | Machine mch -> get_tags_mch tags co.co_loc co.co_name co.co_parameters mch
  | Refinement ref -> get_tags_ref tags co.co_loc co.co_name co.co_parameters ref
  | Implementation imp -> get_tags_imp tags co.co_loc co.co_name co.co_parameters imp

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
