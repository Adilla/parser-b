open Utils
open Syntax

let extended_sees = ref false
let set_extended_sees b = extended_sees := b

type t_kind = 
  | K_Abstract_Variable | K_Concrete_Variable
  | K_Abstract_Constant | K_Concrete_Constant
  | K_Abstract_Set | K_Concrete_Set
  | K_Enumerate

type t_source =
  | S_Current_Mch_Only of loc
  | S_Seen_Mch_Only of p_lident
  | S_Refined_Mch_Only of p_lident
  | S_Included_Mch_Only of p_lident
  | S_Current_And_Refined_Mch of loc*p_lident
  | S_Included_And_Refined_Mch of p_lident*p_lident
  | S_Imported_Mch_Only of p_lident
  | S_Current_And_Imported_Mch of loc * p_lident
  | S_Imported_And_Refined_Mch of p_lident * p_lident
  | S_Current_Imported_And_Refined_Mch of loc * p_lident * p_lident
  | S_Ghost

type t_source2 =
  | Src_Current of loc
  | Src_Current_Local of loc
  | Src_Seen of p_lident
  | Src_Refined of p_lident
  | Src_Imported of p_lident
  | Src_Included of p_lident

type t_op_source =
  | OS_Seen_Mch of p_lident
  | OS_Current_Mch_Only of loc
  | OS_Refined_Mch_Only of p_lident
  | OS_Included_Mch_Only of p_lident
  | OS_Current_And_Refined_Mch of loc * p_lident
  | OS_Included_And_Refined_Mch of p_lident * p_lident
  | OS_Local_Spec of loc
  | OS_Local_Spec_And_Implem of loc*loc
  | OS_Imported_Only of p_lident
  | OS_Imported_And_Promoted of loc * p_lident
  | OS_Imported_And_Refined of p_lident * p_lident
  | OS_Imported_Promoted_And_Refined of loc * p_lident * p_lident

type t_clause =
  | C_Invariant_Or_Assertions
  | C_Properties
  | C_Operations
  | C_Local_Operations
  | C_Values
  | C_Assert_Or_While_Invariant

module MachineInterface :
sig
  type t
  type t_symb = { id:string; typ:Btype.t; kind:t_kind; hidden:bool }
  type t_op = { id:string; args_in: (string*Btype.t) list; args_out: (string*Btype.t) list; readonly:bool }
  val make : t_symb list -> t_op list -> t
  val get_symbols : t -> t_symb list
  val get_operations : t -> t_op list
end = struct
  type t_symb = { id:string; typ:Btype.t; kind:t_kind; hidden:bool }
  type t_op = { id:string; args_in: (string*Btype.t) list; args_out: (string*Btype.t) list; readonly:bool }
  type t = t_symb list * t_op list

  let make l1 l2 = (l1,l2)
  let get_symbols = fst
  let get_operations = snd
end

type t_interface = MachineInterface.t

type t_symbol_infos =
  { sy_typ:Btype.t;
    sy_kind:t_kind;
    sy_src:t_source }

type t_operation_infos  =
  { op_args_in: (string*Btype.t) list;
    op_args_out: (string*Btype.t) list;
    op_readonly:bool;
    op_src: t_op_source; }

type t = { symb:(string,t_symbol_infos) Hashtbl.t;
           ops:(string,t_operation_infos) Hashtbl.t }

let create () : t = { symb=Hashtbl.create 47;
                      ops=Hashtbl.create 47 }

let get_symbol_type (env:t) (id:ident) : Btype.t option =
  try Some (Hashtbl.find env.symb id).sy_typ
  with Not_found -> None

let get_symbol_kind (env:t) (id:ident) : t_kind option =
  try Some (Hashtbl.find env.symb id).sy_kind
  with Not_found -> None

let is_symbol_visible (cl:t_clause) (ki:t_kind) (src:t_source) : bool =
  match cl, ki, src with
  | C_Properties, (K_Abstract_Variable|K_Concrete_Variable), _ -> false
  | C_Properties, _, _ -> true
  | C_Invariant_Or_Assertions, (K_Abstract_Variable|K_Concrete_Variable), S_Seen_Mch_Only _ -> !extended_sees
  | C_Invariant_Or_Assertions, _, _ -> true
  | C_Operations, K_Abstract_Constant, (S_Refined_Mch_Only _|S_Imported_Mch_Only _|S_Imported_And_Refined_Mch _) -> false
  | C_Operations, K_Abstract_Variable, (S_Refined_Mch_Only _|S_Imported_Mch_Only _|S_Imported_And_Refined_Mch _) -> false
  | C_Operations, _, _ -> true
  | C_Local_Operations, (K_Abstract_Constant|K_Abstract_Variable), S_Refined_Mch_Only _ -> false
  | C_Local_Operations, _, _ -> true
  | C_Values, K_Abstract_Constant, _ -> false
  | C_Values, (K_Abstract_Variable|K_Concrete_Variable), _ -> false
  | C_Values, _, _ -> true
  | C_Assert_Or_While_Invariant, _, _ -> true

let get_symbol_type_in_clause (env:t) (loc:loc) (id:ident) (cl:t_clause) : Btype.t Error.t_result =
  try
    begin
      let infos = Hashtbl.find env.symb id in
      if is_symbol_visible cl infos.sy_kind infos.sy_src then
        Ok infos.sy_typ
      else
        Error { Error.err_loc=loc;
                err_txt="The identifier '"^id^"' is not visible in this clause." }
    end
  with
    Not_found -> Error { Error.err_loc=loc;
                         err_txt="Unknown identifier '"^id^"'." }

let is_symbol_writable ki src cl =
  match ki, src, cl with
  | (K_Abstract_Variable|K_Concrete_Variable),
    (S_Current_Mch_Only _|S_Current_And_Refined_Mch _|S_Refined_Mch_Only _), _ -> true
  | (K_Abstract_Variable|K_Concrete_Variable),
    (S_Imported_And_Refined_Mch _|S_Current_And_Imported_Mch _|
     S_Current_Imported_And_Refined_Mch _|S_Imported_Mch_Only _), C_Local_Operations -> true

  | _, _, _ -> false

let get_writable_symbol_type_in_clause (env:t) (loc:loc) (id:ident) (cl:t_clause) : Btype.t Error.t_result =
  try
    begin
      let infos = Hashtbl.find env.symb id in
      if is_symbol_visible cl infos.sy_kind infos.sy_src then
        if is_symbol_writable infos.sy_kind infos.sy_src cl then
          Ok infos.sy_typ
        else
          Error { Error.err_loc=loc;
                  err_txt="The identifier '"^id^"' is not writable." }
      else
        Error { Error.err_loc=loc;
                err_txt="The identifier '"^id^"' is not visible in this clause." }
    end
  with
    Not_found -> Error { Error.err_loc=loc;
                         err_txt="Unknown identifier '"^id^"'." }

let kind_to_string = function
  | K_Abstract_Variable -> "an abstract variable"
  | K_Concrete_Variable -> "a concrete variable"
  | K_Abstract_Constant -> "an abstract constant"
  | K_Concrete_Constant -> "a concrete constant"
  | K_Abstract_Set -> "an abstract set"
  | K_Concrete_Set -> "a concrete set"
  | K_Enumerate -> "an enumerate"

let are_kind_compatible k1 k2 =
  k1 = k2 ||
  match k1, k2 with
  | K_Abstract_Variable, K_Concrete_Variable -> true
  | K_Abstract_Constant, K_Concrete_Constant -> true
  | _, _ -> false

let update_source (id:ident) (src1:t_source) (src2:t_source2) : t_source Error.t_result =
  match src2, src1 with
  | Src_Current lc, S_Refined_Mch_Only mch -> Ok (S_Current_And_Refined_Mch (lc,mch))
  | Src_Current lc, S_Imported_Mch_Only mch -> Ok (S_Current_And_Imported_Mch (lc,mch))
  | Src_Current lc, S_Imported_And_Refined_Mch (imp,ref) -> Ok (S_Current_Imported_And_Refined_Mch (lc,imp,ref))
  | Src_Current lc, S_Current_Mch_Only _
  | Src_Current lc, S_Seen_Mch_Only _
  | Src_Current lc, S_Current_And_Refined_Mch _
  | Src_Current lc, S_Current_And_Imported_Mch _
  | Src_Current lc, S_Current_Imported_And_Refined_Mch _
  | Src_Current lc, S_Ghost
  | Src_Current lc, S_Included_Mch_Only _
  | Src_Current lc, S_Included_And_Refined_Mch _ ->
    Error { Error.err_loc=lc;
            err_txt="The identifier '"^id^"' is already declared." }

  | Src_Seen mch, _ ->
    Error { Error.err_loc=mch.lid_loc;
            err_txt="The identifier '"^id^"' is already declared." }

  | Src_Imported imp, S_Current_Mch_Only lc -> Ok (S_Current_And_Imported_Mch (lc,imp))
  | Src_Imported imp, S_Refined_Mch_Only ref -> Ok (S_Imported_And_Refined_Mch (imp,ref))
  | Src_Imported imp, S_Current_And_Refined_Mch (lc,ref) -> Ok (S_Current_Imported_And_Refined_Mch (lc,imp,ref))
  | Src_Imported imp, S_Seen_Mch_Only _
  | Src_Imported imp, S_Imported_Mch_Only _
  | Src_Imported imp, S_Current_And_Imported_Mch _
  | Src_Imported imp, S_Imported_And_Refined_Mch _
  | Src_Imported imp, S_Current_Imported_And_Refined_Mch _
  | Src_Imported imp, S_Included_And_Refined_Mch _
  | Src_Imported imp, S_Included_Mch_Only _
  | Src_Imported imp, S_Ghost ->
    Error { Error.err_loc=imp.lid_loc;
            err_txt="The identifier '"^id^"' is already declared." }

  | Src_Refined ref, S_Current_Mch_Only lc -> Ok (S_Current_And_Refined_Mch (lc,ref))
  | Src_Refined ref, S_Imported_Mch_Only imp -> Ok (S_Imported_And_Refined_Mch (imp,ref))
  | Src_Refined ref, S_Current_And_Imported_Mch (lc,imp) -> Ok (S_Current_Imported_And_Refined_Mch (lc,imp,ref))
  | Src_Refined ref, S_Included_Mch_Only mch -> Ok (S_Included_And_Refined_Mch (mch,ref))
  | Src_Refined ref, S_Seen_Mch_Only _
  | Src_Refined ref, S_Refined_Mch_Only _
  | Src_Refined ref, S_Current_And_Refined_Mch _
  | Src_Refined ref, S_Imported_And_Refined_Mch _
  | Src_Refined ref, S_Current_Imported_And_Refined_Mch _
  | Src_Refined ref, S_Ghost
  | Src_Refined ref, S_Included_And_Refined_Mch _ ->
    Error { Error.err_loc=ref.lid_loc;
            err_txt="The identifier '"^id^"' is already declared." }

  | Src_Included ref, S_Refined_Mch_Only mch -> Ok (S_Included_And_Refined_Mch (ref,mch))
  | Src_Included ref, S_Current_Mch_Only _
  | Src_Included ref, S_Current_And_Refined_Mch _
  | Src_Included ref, S_Seen_Mch_Only _
  | Src_Included ref, S_Imported_Mch_Only _
  | Src_Included ref, S_Current_And_Imported_Mch _
  | Src_Included ref, S_Imported_And_Refined_Mch _
  | Src_Included ref, S_Current_Imported_And_Refined_Mch _
  | Src_Included ref, S_Included_And_Refined_Mch _
  | Src_Included ref, S_Included_Mch_Only _
  | Src_Included ref, S_Ghost ->
    Error { Error.err_loc=ref.lid_loc;
            err_txt="The identifier '"^id^"' is already declared." }

  | Src_Current_Local _, _ -> assert false

let _add_symbol (env:t) (err_loc:loc) (id:ident) (sy_typ:Btype.t) (sy_kind:t_kind) (sy_src:t_source2) : unit Error.t_result =
  try
    begin
      let infos = Hashtbl.find env.symb id in
      match update_source id infos.sy_src sy_src with
      | Ok sy_src ->
        if are_kind_compatible infos.sy_kind sy_kind then
          if Btype.equal infos.sy_typ sy_typ then
            Ok (Hashtbl.replace env.symb id { sy_typ; sy_kind; sy_src })
          else
            Error { Error.err_loc;
                    err_txt="The identifier '" ^ id ^ "' has type " 
                            ^ Btype.to_string sy_typ
                          ^ " but was previously declared with type "
                          ^ Btype.to_string infos.sy_typ ^ "." }
        else
          Error { Error.err_loc;
                  err_txt="The identifier '" ^ id ^ "' is a " 
                          ^ kind_to_string sy_kind
                          ^ " but was previously declared as a "
                          ^ kind_to_string infos.sy_kind ^ "." }
      | Error _ as err -> err
    end
  with
  | Not_found ->
    let sy_src = match sy_src with
      | Src_Current lc -> S_Current_Mch_Only lc
      | Src_Seen mch -> S_Seen_Mch_Only mch
      | Src_Imported mch -> S_Imported_Mch_Only mch
      | Src_Refined mch -> S_Refined_Mch_Only mch
      | Src_Included mch -> S_Included_Mch_Only mch
      | Src_Current_Local _ -> assert false
    in
    Ok (Hashtbl.add env.symb id { sy_typ; sy_kind; sy_src })

let add_symbol (env:t) (loc:loc) (id:ident) (typ:Btype.t) (kind:t_kind) : unit Error.t_result =
  _add_symbol env loc id typ kind (Src_Current loc)

type t_op_type = { args_in:(ident*Btype.t) list; args_out:(ident*Btype.t) list; }

let update_op_source (id:ident) (src1:t_op_source) (src2:t_source2) : t_op_source Error.t_result =
  match src2, src1 with
  | Src_Current lc, OS_Refined_Mch_Only ref -> Ok (OS_Current_And_Refined_Mch (lc,ref))
  | Src_Current lc, OS_Local_Spec spe -> Ok (OS_Local_Spec_And_Implem (spe,lc))
  | Src_Current lc, OS_Included_Mch_Only _
  | Src_Current lc, OS_Included_And_Refined_Mch _
  | Src_Current lc, OS_Seen_Mch _
  | Src_Current lc, OS_Current_Mch_Only _
  | Src_Current lc, OS_Current_And_Refined_Mch _
  | Src_Current lc, OS_Local_Spec_And_Implem _
  | Src_Current lc, OS_Imported_Only _
  | Src_Current lc, OS_Imported_And_Promoted _
  | Src_Current lc, OS_Imported_And_Refined _
  | Src_Current lc, OS_Imported_Promoted_And_Refined _ ->
    Error { Error.err_loc=lc;
            err_txt="The operation '"^id^"' is already declared." }
  | Src_Current_Local lc, OS_Current_Mch_Only lc2 -> Ok (OS_Local_Spec_And_Implem (lc,lc2))
  | Src_Current_Local lc, OS_Included_Mch_Only _
  | Src_Current_Local lc, OS_Included_And_Refined_Mch _
  | Src_Current_Local lc, OS_Refined_Mch_Only _
  | Src_Current_Local lc, OS_Local_Spec _
  | Src_Current_Local lc, OS_Seen_Mch _
  | Src_Current_Local lc, OS_Current_And_Refined_Mch _
  | Src_Current_Local lc, OS_Local_Spec_And_Implem _
  | Src_Current_Local lc, OS_Imported_Only _
  | Src_Current_Local lc, OS_Imported_And_Promoted _
  | Src_Current_Local lc, OS_Imported_And_Refined _
  | Src_Current_Local lc, OS_Imported_Promoted_And_Refined _ ->
    Error { Error.err_loc=lc;
            err_txt="The operation '"^id^"' is already declared." }
  | Src_Imported imp, OS_Refined_Mch_Only ref -> Ok (OS_Imported_And_Refined (imp,ref))
  | Src_Imported imp, OS_Included_And_Refined_Mch _
  | Src_Imported imp, OS_Included_Mch_Only _
  | Src_Imported imp, OS_Current_Mch_Only _
  | Src_Imported imp, OS_Local_Spec _
  | Src_Imported imp, OS_Seen_Mch _
  | Src_Imported imp, OS_Current_And_Refined_Mch _
  | Src_Imported imp, OS_Local_Spec_And_Implem _
  | Src_Imported imp, OS_Imported_Only _
  | Src_Imported imp, OS_Imported_And_Promoted _
  | Src_Imported imp, OS_Imported_And_Refined _
  | Src_Imported imp, OS_Imported_Promoted_And_Refined _ ->
    Error { Error.err_loc=imp.lid_loc;
            err_txt="The operation '"^id^"' is already declared." }
  | Src_Refined ref, OS_Current_Mch_Only lc -> Ok (OS_Current_And_Refined_Mch (lc,ref))
  | Src_Refined ref, OS_Imported_Only imp -> Ok (OS_Imported_And_Refined (ref,imp))
  | Src_Refined ref, OS_Imported_And_Promoted (lc,imp) -> Ok (OS_Imported_Promoted_And_Refined (lc,imp,ref))
  | Src_Refined ref, OS_Included_Mch_Only mch -> Ok (OS_Included_And_Refined_Mch (mch,ref))
  | Src_Refined ref, OS_Included_And_Refined_Mch _
  | Src_Refined ref, OS_Refined_Mch_Only _
  | Src_Refined ref, OS_Local_Spec _
  | Src_Refined ref, OS_Seen_Mch _
  | Src_Refined ref, OS_Current_And_Refined_Mch _
  | Src_Refined ref, OS_Local_Spec_And_Implem _
  | Src_Refined ref, OS_Imported_And_Refined _
  | Src_Refined ref, OS_Imported_Promoted_And_Refined _ ->
    Error { Error.err_loc=ref.lid_loc;
            err_txt="The operation '"^id^"' is already declared." }
  | Src_Included ref, OS_Refined_Mch_Only mch -> Ok (OS_Included_And_Refined_Mch (ref,mch))
  | Src_Included mch, _ -> 
    Error { Error.err_loc=mch.lid_loc;
            err_txt="The operation '"^id^"' is already declared." }
  | Src_Seen mch, _ -> 
    Error { Error.err_loc=mch.lid_loc;
            err_txt="The operation '"^id^"' is already declared." }

let check_args_type (err_loc:loc) (args:t_op_type) args_in args_out : unit Error.t_result =
  let aux (x1,ty1) (x2,ty2) =
    if ident_eq x1 x2 then
      if Btype.equal ty1 ty2 then ()
      else Error.raise_exn err_loc
          ("The parameter '"^x1^"' has type '"^Btype.to_string ty1^
           "' but parameter of type '"^Btype.to_string ty2^"' was expected.")
    else Error.raise_exn err_loc
        ("Parameter '"^x2^"' expected but '"^x1^"' was found.")
  in
  try
    List.iter2 aux args_in args.args_in;
    List.iter2 aux args_out args.args_out;
    Ok ()
  with
  | Invalid_argument _ ->
    Error { Error.err_loc; err_txt="Unexpected number of parameters." }
  | Error.Error err -> Error err

let _add_operation (env:t) (err_loc:loc) (id:ident) (args:t_op_type) (op_readonly:bool) (op_src:t_source2) : unit Error.t_result =
  let op_args_in = args.args_in in
  let op_args_out = args.args_out in
  try
    begin
      let infos = Hashtbl.find env.ops id in
      match update_op_source id infos.op_src op_src with
      | Error _ as err -> err
      | Ok op_src ->
        begin match check_args_type err_loc args infos.op_args_in infos.op_args_out with
          | Error _ as err -> err
          | Ok () -> Ok (Hashtbl.replace env.ops id { infos with op_src })
        end
    end
  with
    Not_found ->
    let op_src = match op_src with
      | Src_Current lc -> OS_Current_Mch_Only lc
      | Src_Current_Local lc -> OS_Local_Spec lc
      | Src_Seen mch -> OS_Seen_Mch mch
      | Src_Imported mch -> OS_Imported_Only mch
      | Src_Refined mch -> OS_Refined_Mch_Only mch
      | Src_Included mch -> OS_Included_Mch_Only mch
    in
    Ok (Hashtbl.add env.ops id { op_args_in; op_args_out; op_readonly; op_src })

let is_operation_visible is_readonly = function
  | OS_Seen_Mch _ -> is_readonly
  | OS_Current_Mch_Only _ | OS_Refined_Mch_Only _ | OS_Current_And_Refined_Mch _ -> false
  | OS_Local_Spec _ | OS_Local_Spec_And_Implem _ | OS_Imported_Only _
  | OS_Imported_And_Promoted _ | OS_Imported_And_Refined _
  | OS_Imported_Promoted_And_Refined _ | OS_Included_Mch_Only _
  | OS_Included_And_Refined_Mch _ -> true

let get_operation_type (env:t) (err_loc:loc) (id:ident) =
  try
    let infos = Hashtbl.find env.ops id in
    if is_operation_visible infos.op_readonly infos.op_src then
      Ok { args_in=infos.op_args_in; args_out=infos.op_args_out }
    else
      Error { Error.err_loc; err_txt="The operation '"^id^"' is not visible." }
  with
    Not_found -> Error { Error.err_loc; err_txt="Unknown operation '"^id^"'." }

let get_operation_type2 (env:t) (id:ident) : t_op_type option =
  try
    let infos = Hashtbl.find env.ops id in
    Some { args_in=infos.op_args_in; args_out=infos.op_args_out }
  with
    Not_found -> None

let is_operation_readonly (env:t) (id:ident) : bool =
  try (Hashtbl.find env.ops id).op_readonly
  with Not_found -> false

let add_operation (env:t) (loc:loc) (id:ident) (args:t_op_type) (is_readonly:bool) (is_local:bool) : unit Error.t_result =
  let src = if is_local then Src_Current_Local loc else Src_Current loc in
  _add_operation env loc id args is_readonly src

let promote_operation (env:t) (loc:loc) (id:ident) =
  try
    begin
      let infos = Hashtbl.find env.ops id in
      match infos.op_src with
      | OS_Imported_Only mch ->
        Ok (Hashtbl.replace env.ops id { infos with op_src=OS_Imported_And_Promoted (loc,mch) })
      | OS_Imported_And_Refined (imp,ref) ->
        Ok (Hashtbl.replace env.ops id { infos with op_src=OS_Imported_Promoted_And_Refined (loc,imp,ref) })
      | OS_Imported_And_Promoted _ | OS_Imported_Promoted_And_Refined _ ->
        Error { Error.err_loc=loc;
                err_txt="The operation '"^id^"' is already promoted." }
      | OS_Seen_Mch _ | OS_Current_Mch_Only _ | OS_Current_And_Refined_Mch _
      | OS_Local_Spec_And_Implem _ | OS_Refined_Mch_Only _ | OS_Local_Spec _
      | OS_Included_Mch_Only _ | OS_Included_And_Refined_Mch _ ->
        Error { Error.err_loc=loc;
                err_txt="The operation '"^id^"' is not an operation of an imported machine." }
    end
  with
    Not_found -> Error { Error.err_loc=loc; err_txt="Unknown operation '"^id^"'." }

let load_interface_for_seen_machine (env:t) (itf:MachineInterface.t) (mch:p_lident) : unit Error.t_result =
  let open MachineInterface in
  let res = Error.list_iter
      (fun (r:t_symb) -> _add_symbol env mch.lid_loc r.id r.typ r.kind (Src_Seen mch)) (get_symbols itf)
  in
  match res with
  | Error _ as err -> err
  | Ok _ ->
    Error.list_iter (
      fun (r:t_op) -> _add_operation env mch.lid_loc r.id {args_in=r.args_in;args_out=r.args_out} r.readonly (Src_Seen mch)
    ) (get_operations itf)

let load_interface_for_included_machine (env:t) (itf:MachineInterface.t) (mch:p_lident) : unit Error.t_result =
  let open MachineInterface in
  let res =
    Error.list_iter (fun (r:t_symb) ->
        _add_symbol env mch.lid_loc r.id r.typ r.kind (Src_Included mch)) (get_symbols itf)
  in
  match res with
  | Error _ as err -> err
  | Ok _ ->
    Error.list_iter (fun (r:t_op) ->
        _add_operation env mch.lid_loc r.id {args_in=r.args_in; args_out=r.args_out} r.readonly (Src_Included mch)
      ) (get_operations itf)

let load_interface_for_refined_machine (env:t) (itf:MachineInterface.t) (mch:p_lident) : unit Error.t_result =
  let open MachineInterface in
  let res =
    Error.list_iter (fun (r:t_symb) ->
        _add_symbol env mch.lid_loc r.id r.typ r.kind (Src_Refined mch)) (get_symbols itf)
  in
  match res with
  | Error _ as err -> err
  | Ok _ ->
    Error.list_iter (fun (r:t_op) ->
        _add_operation env mch.lid_loc r.id {args_in=r.args_in; args_out=r.args_out} r.readonly (Src_Refined mch)
      ) (get_operations itf)

let load_interface_for_imported_machine (env:t) (itf:MachineInterface.t) (mch:p_lident) : unit Error.t_result =
  let open MachineInterface in
  let res = Error.list_iter (fun (r:t_symb) ->
      _add_symbol env mch.lid_loc r.id r.typ r.kind (Src_Imported mch)) (get_symbols itf)
  in
  match res with
  | Error _ as err -> err
  | Ok _ -> Error.list_iter (
      fun (r:t_op) -> _add_operation env mch.lid_loc r.id {args_in=r.args_in; args_out=r.args_out} r.readonly (Src_Imported mch)
    ) (get_operations itf)

let load_interface_for_extended_machine (env:t) (itf:MachineInterface.t) (mch:p_lident) : unit Error.t_result =
  let open MachineInterface in
  let res = Error.list_iter (fun (r:t_symb) ->
      _add_symbol env mch.lid_loc r.id r.typ r.kind (Src_Imported mch)) (get_symbols itf)
  in
  match res with
  | Error _ as err -> err
  | Ok _ -> Error.list_iter (
      fun (r:t_op) ->
       match _add_operation env mch.lid_loc r.id {args_in=r.args_in; args_out=r.args_out} r.readonly (Src_Imported mch) with
         | Error _ as err -> err
         | Ok () -> promote_operation env mch.lid_loc r.id
    ) (get_operations itf)

let to_interface (env:t) : MachineInterface.t =
  let aux1 x symb lst =
    match symb.sy_src with
    | S_Current_Mch_Only _ | S_Current_And_Refined_Mch _ | S_Current_And_Imported_Mch _
    | S_Current_Imported_And_Refined_Mch _ | S_Refined_Mch_Only _ | S_Included_Mch_Only _
    | S_Included_And_Refined_Mch _ ->
      { MachineInterface.
        id=x; typ=symb.sy_typ; kind=symb.sy_kind; hidden=false }::lst
    | S_Imported_And_Refined_Mch _ | S_Imported_Mch_Only _ | S_Ghost ->
      { MachineInterface.
        id=x; typ=symb.sy_typ; kind=symb.sy_kind; hidden=true }::lst
    | S_Seen_Mch_Only _  -> lst
  in
  let aux2 x op lst =
    match op.op_src with
    | OS_Seen_Mch _ | OS_Local_Spec _ | OS_Local_Spec_And_Implem _
    | OS_Imported_And_Refined _ | OS_Imported_Only _ | OS_Included_Mch_Only _ -> lst
    | OS_Current_Mch_Only _ | OS_Current_And_Refined_Mch _ | OS_Imported_And_Promoted _
    | OS_Imported_Promoted_And_Refined _ | OS_Refined_Mch_Only _ | OS_Included_And_Refined_Mch _ ->
      { MachineInterface.
        id=x; args_in=op.op_args_in; args_out=op.op_args_out; readonly=op.op_readonly }::lst
  in
  let lst1 = Hashtbl.fold aux1 env.symb [] in
  let lst2 = Hashtbl.fold aux2 env.ops [] in
  MachineInterface.make lst1 lst2

let check_operation_coherence (env:t) (err_loc:loc) (is_imp:bool) : unit Error.t_result =
  try
    Hashtbl.iter (
      fun x op ->
        match op.op_src with
        | OS_Seen_Mch _ -> ()
        | OS_Included_Mch_Only _ -> ()
        | OS_Included_And_Refined_Mch _ -> ()
        | OS_Refined_Mch_Only _ ->
          if is_imp then
            Error.raise_exn err_loc ("The operation '"^x^"' is not refined.")
        | OS_Local_Spec lc ->
          Error.raise_exn lc ("The operation '"^x^"' is not implemented.")
        | OS_Local_Spec_And_Implem _ -> ()
        | OS_Imported_And_Refined _ ->
          Error.raise_exn err_loc ("The operation '"^x^"' is not refined (missing promotion?).")
        | OS_Imported_Only _ -> ()
        | OS_Current_Mch_Only _ -> ()
        | OS_Current_And_Refined_Mch _ -> ()
        | OS_Imported_And_Promoted _ -> ()
        | OS_Imported_Promoted_And_Refined _ -> ()
    ) env.ops; Ok ()
  with Error.Error err -> Error err
