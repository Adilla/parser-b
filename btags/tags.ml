open Blib.Utils
open Blib.SyntaxCore
open Blib
module P = Blib.PSyntax

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

let mk_tag (c:char) (lc:loc) (id:string) (sc:string option) : tag =
  { tagname = id;
    tagkind = c;
    tagfile = lc.Lexing.pos_fname;
    tagline = lc.Lexing.pos_lnum;
    tagcolumn = lc.Lexing.pos_cnum - lc.Lexing.pos_bol;
    tagscope = match sc with
      | None -> Clause (clause c)
      | Some ss -> CSet ss
  }

let add_params (tags:tags) (v:lident) : tags =
  (mk_tag 'p' v.lid_loc v.lid_str None)::tags

let add_promotes (tags:tags) (name:lident) : tags =
  (mk_tag 'o' name.lid_loc name.lid_str None)::tags

let add_sets (tags:tags) (s:P.set) : tags =
  let lst = match s with
    | Abstract_Set s -> [mk_tag 's' s.lid_loc s.lid_str None]
    | Concrete_Set (s,elts) ->
      (mk_tag 's' s.lid_loc s.lid_str None)::
      ( List.map (fun elt -> mk_tag 'e' elt.lid_loc elt.lid_str (Some s.lid_str)) elts )
  in
  lst@tags

let add_constant (tags:tags) (v:lident) : tags =
  (mk_tag 'c' v.lid_loc v.lid_str None)::tags
  
let add_abstract_constant (tags:tags) (v:lident) : tags =
  (mk_tag 'C' v.lid_loc v.lid_str None)::tags

let add_variable (tags:tags) (v:lident) : tags =
  (mk_tag 'v' v.lid_loc v.lid_str None)::tags

let add_concrete_variable (tags:tags) (v:lident) : tags =
  (mk_tag 'V' v.lid_loc v.lid_str None)::tags

let add_operation (tags:tags) (op:P.operation) : tags =
  (mk_tag 'o' op.op_name.lid_loc op.op_name.lid_str None)::tags

let add_local_operation (tags:tags) (op:P.operation) : tags =
  (mk_tag 'l' op.op_name.lid_loc op.op_name.lid_str None)::tags

let add_clause (tags:tags) (cl:P.clause) : tags =
  match cl with
  | Constraints _ | Imports _ | Sees _ | Includes _ | Extends _ | Uses _ | Properties _
  | Invariant _ | Assertions _ | Initialization _ | Values _ | Refines _ -> tags
  | Promotes nle -> List.fold_left add_promotes tags (Nlist.to_list nle)
  | Sets nle -> List.fold_left add_sets tags (Nlist.to_list nle)
  | Constants nle -> List.fold_left add_constant tags (Nlist.to_list nle)
  | Abstract_constants nle -> List.fold_left add_abstract_constant tags (Nlist.to_list nle)
  | Concrete_variables nle -> List.fold_left add_concrete_variable tags (Nlist.to_list nle)
  | Variables nle -> List.fold_left add_variable tags (Nlist.to_list nle)
  | Operations nle -> List.fold_left add_operation tags (Nlist.to_list nle)
  | Local_Operations nle -> List.fold_left add_local_operation tags (Nlist.to_list nle)

let add_tags (tags:tags) (co:P.component) =
  match co.P.co_desc with
  | Machine mch ->
    let tags = (mk_tag 'm' co.P.co_name.lid_loc co.P.co_name.lid_str None)::tags in
    let tags = List.fold_left add_params tags mch.P.mch_parameters in
    List.fold_left add_clause tags (P.get_clauses co)
  | Refinement ref ->
    let tags = (mk_tag 'r' co.P.co_name.lid_loc co.P.co_name.lid_str None)::tags in
    let tags = List.fold_left add_params tags ref.P.ref_parameters in
    List.fold_left add_clause tags (P.get_clauses co)
  | Implementation imp ->
    let tags = (mk_tag 'i' co.P.co_name.lid_loc co.P.co_name.lid_str None)::tags in
    let tags = List.fold_left add_params tags imp.P.imp_parameters in
    List.fold_left add_clause tags (P.get_clauses co)

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
