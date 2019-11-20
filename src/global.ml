type lident = SyntaxCore.lident
type ren_ident = SyntaxCore.ren_ident
type loc = Utils.loc

type t_param_kind = Set | Scalar

type 'a t_symbol_infos = {
  sy_typ:Btype.t;
  sy_kind:'a
}

type 'a t_operation_infos =
  { op_args_in: (string*Btype.t) list;
    op_args_out: (string*Btype.t) list;
    op_readonly:bool;
    op_src: 'a; }

type t_global_kind =
  | K_Parameter  of t_param_kind
  | K_Abstract_Variable
  | K_Abstract_Constant
  | K_Concrete_Variable
  | K_Concrete_Constant
  | K_Abstract_Set
  | K_Concrete_Set of string list
  | K_Enumerate

module MachineInterface = struct
  type t_symb = { id:string; typ:Btype.t; kind:t_global_kind; }
  type t_op = { id:string; args_in: (string*Btype.t) list; args_out: (string*Btype.t) list; readonly:bool }
  type t_param = { id:string; typ:Btype.t; kind:t_param_kind }
  type t = {
    params: t_param list;
    symbs: t_symb list;
    ops: t_op list;
    hidden: string Utils.SMap.t }
end

type t_interface = MachineInterface.t

let is_in_deps (deps:string list) (mch:string) : bool =
  List.exists (String.equal mch) deps

module type S = sig
  type t_source
  type t_kind
  type t_op_source
  val mk_kind : t_global_kind -> t_source -> t_kind
  val update_kind : t_kind -> t_global_kind -> t_source -> t_kind option
  val mk_op_source : t_source -> t_op_source option
  val update_op_source : t_op_source -> t_source -> t_op_source option
  val check_hidden : string Utils.SMap.t -> string -> t_source -> bool
end

module Mch = struct

  type t_source =
    | Machine of loc
    | Seen of ren_ident
    | Used of ren_ident
    | Included of ren_ident

  type t_kind =
    | Parameter of t_param_kind*loc
    | Abstract_Variable of t_source
    | Abstract_Constant of t_source
    | Concrete_Variable of t_source
    | Concrete_Constant of t_source
    | Abstract_Set of t_source
    | Concrete_Set of string list * t_source
    | Enumerate of t_source

  type t_op_source =
    | O_Machine of loc
    | O_Seen of ren_ident
    | O_Used of ren_ident
    | O_Included of ren_ident
    | O_Included_And_Promoted of ren_ident

  let mk_kind kind src =
    match kind with
    | K_Parameter k ->
     begin match src with
       | Machine l -> Parameter (k,l)
       | _ -> failwith "Error in Global.Mch.mk_kind"
     end
    | K_Abstract_Variable -> Abstract_Variable src
    | K_Abstract_Constant -> Abstract_Constant src
    | K_Concrete_Variable -> Concrete_Variable src
    | K_Concrete_Constant -> Concrete_Constant src
    | K_Abstract_Set -> Abstract_Set src
    | K_Concrete_Set elts -> Concrete_Set (elts,src)
    | K_Enumerate -> Enumerate src

  let update_kind _ _ _ = None

  let mk_op_source = function
    | Machine l -> Some (O_Machine l)
    | Seen mch -> Some (O_Seen mch)
    | Used mch -> Some (O_Used mch)
    | Included mch -> Some (O_Included mch)

  let update_op_source _ _ = None

  let get_symbols (x:string) (symb:t_kind t_symbol_infos) (lst:MachineInterface.t_symb list) =
    match symb.sy_kind with
    | Parameter (_, _) -> lst
    | Abstract_Variable (Machine _|Included _)->
      { id=x; typ=symb.sy_typ; kind=K_Abstract_Variable }::lst
    | Abstract_Constant (Machine _|Included _) ->
      { id=x; typ=symb.sy_typ; kind=K_Abstract_Constant }::lst
    | Concrete_Variable (Machine _|Included _) ->
      { id=x; typ=symb.sy_typ; kind=K_Concrete_Variable }::lst
    | Concrete_Constant (Machine _|Included _) ->
      { id=x; typ=symb.sy_typ; kind=K_Concrete_Constant }::lst
    | Abstract_Set (Machine _|Included _) ->
      { id=x; typ=symb.sy_typ; kind=K_Abstract_Set }::lst
    | Concrete_Set (elts, (Machine _|Included _)) ->
      { id=x; typ=symb.sy_typ; kind=K_Concrete_Set elts }::lst
    | Enumerate (Machine _|Included _) ->
      { id=x; typ=symb.sy_typ; kind=K_Enumerate }::lst
    | _ -> lst

  let get_hidden (x:string) (symb:t_kind t_symbol_infos) (map:string Utils.SMap.t) : string Utils.SMap.t =
    match symb.sy_kind with
    | Abstract_Set (Seen mch|Used mch) | Concrete_Set (_, (Seen mch|Used mch)) ->
      Utils.SMap.add x mch.r_str map
    | _ -> map

  let get_operations (id:string) (op:t_op_source t_operation_infos) lst =
    match op.op_src with
    | O_Machine _ | O_Included_And_Promoted _ ->
      { MachineInterface.id; args_in=op.op_args_in;
        args_out=op.op_args_out; readonly=op.op_readonly }::lst
    | _ -> lst

  let param_kind = function
    | Parameter (k,_) -> Some k
    | _ -> None

  let check_hidden (map:string Utils.SMap.t) (id:string) (src:t_source) : bool =
    match Utils.SMap.find_opt id map with
    | None -> true
    | Some mch ->
      begin match src with
        | Machine _ -> false
        | Used mch2 | Included mch2 | Seen mch2 -> String.equal mch mch2.r_str
      end
end

module Ref = struct

  type t_source =
    | Machine of loc
    | Seen of ren_ident
    | Refined
    | Included of ren_ident

  type t_source_2 =
    | A_Machine of loc
    | A_Seen of ren_ident
    | A_Refined
    | A_Included of ren_ident
    | A_Redeclared_In_Machine of loc
    | A_Redeclared_In_Included of ren_ident

  type t_kind =
    | Parameter of t_param_kind*loc
    | Abstract_Variable of t_source_2
    | Abstract_Constant of t_source_2
    | Concrete_Variable of t_source_2
    | Concrete_Constant of t_source_2
    | Abstract_Set of t_source
    | Concrete_Set of string list * t_source
    | Enumerate of t_source

  type t_op_source =
    | O_Refined
    | O_Refined_And_Machine of loc
    | O_Seen of ren_ident
    | O_Included of ren_ident
    | O_Refined_And_Included of ren_ident
    | O_Refined_Included_And_Promoted of ren_ident

  let update_kind (decl:t_kind) (ki:t_global_kind) (src:t_source) : t_kind option =
    match decl, ki with
    | Abstract_Variable A_Refined, K_Abstract_Variable ->
      begin match src with
        | Machine l ->
          Some (Abstract_Variable (A_Redeclared_In_Machine l))
        | Included mch ->
          Some (Abstract_Variable (A_Redeclared_In_Included mch))
        | _ -> None
      end
    | Abstract_Variable A_Refined, K_Concrete_Variable ->
      begin match src with
        | Machine l ->
          Some (Concrete_Variable (A_Redeclared_In_Machine l))
        | Included mch->
          Some (Concrete_Variable (A_Redeclared_In_Included mch))
        | _ -> None
      end
    | Abstract_Constant A_Refined, K_Abstract_Constant ->
      begin match src with
        | Machine l ->
          Some (Abstract_Constant (A_Redeclared_In_Machine l))
        | Included mch ->
          Some (Abstract_Constant (A_Redeclared_In_Included mch))
        | _ -> None
      end
    | Abstract_Constant src2, K_Concrete_Constant ->
      begin match src2, src with
        | A_Refined, Machine l ->
          Some (Concrete_Constant (A_Redeclared_In_Machine l))
        | A_Refined, Included mch->
          Some (Concrete_Constant (A_Redeclared_In_Included mch))
        | _, _ -> None
      end
    | Abstract_Set Refined, K_Abstract_Set ->
      begin match src with
        (*XXX should be allowed only when the same machine is included in the
         * refined machine and the refinement *)
        | Included mch -> Some (Abstract_Set (Included mch)) 
        | _ -> None
      end
    | Concrete_Variable A_Refined, K_Concrete_Variable ->
      begin match src with
        (*XXX idem*)
        | Included mch -> Some (Concrete_Variable (A_Redeclared_In_Included mch)) 
        | _ -> None
      end
    | Concrete_Constant A_Refined, K_Concrete_Constant ->
      begin match src with
        (*XXX idem*)
        | Included mch -> Some (Concrete_Constant (A_Redeclared_In_Included mch)) 
        | _ -> None
      end
    | Enumerate Refined, K_Enumerate ->
      begin match src with
        (*XXX idem*)
        | Included mch -> Some (Enumerate (Included mch)) 
        | _ -> None
      end
    | Concrete_Set (elts1,Refined), K_Concrete_Set elts2 ->
      begin match src with
        (*XXX idem*)
        | Included mch ->
          begin try
              if List.for_all2 String.equal elts1 elts2 then
                Some (Concrete_Set (elts1,Included mch))
              else None
            with Invalid_argument _ -> None
          end
        | _ -> None
      end

    | _ -> None

  let to_source_2 = function
    | Machine l -> A_Machine l
    | Seen mch -> A_Seen mch
    | Refined -> A_Refined
    | Included mch -> A_Included mch

  let mk_kind (kind:t_global_kind) (src:t_source) : t_kind =
    match kind with
    | K_Abstract_Variable -> Abstract_Variable (to_source_2 src)
    | K_Abstract_Constant -> Abstract_Constant (to_source_2 src)
    | K_Concrete_Variable -> Concrete_Variable (to_source_2 src)
    | K_Concrete_Constant -> Concrete_Constant (to_source_2 src)
    | K_Abstract_Set -> Abstract_Set src
    | K_Concrete_Set elts -> Concrete_Set (elts,src)
    | K_Enumerate -> Enumerate src
    | K_Parameter k ->
      begin match src with
        | Machine l -> Parameter(k,l)
        | _ -> failwith "Internal error: Global.Ref.mk_kind"
      end

  let mk_op_source = function
    | Machine _ -> None
    | Seen mch -> Some (O_Seen mch)
    | Refined -> Some O_Refined
    | Included mch -> Some (O_Included mch)

  let update_op_source old_src new_src =
    match old_src, new_src with
    | O_Refined, Machine l -> Some (O_Refined_And_Machine l)
    | O_Refined, Included l -> Some (O_Refined_And_Included l)
    | _, _ -> None

  let get_symbols (x:string) (symb:t_kind t_symbol_infos) (lst:MachineInterface.t_symb list) =
    match symb.sy_kind with
    | Parameter (_, _) -> lst
    | Abstract_Variable (A_Machine _|A_Redeclared_In_Machine _|
                         A_Included _|A_Redeclared_In_Included _)->
      { id=x; typ=symb.sy_typ; kind=K_Abstract_Variable }::lst
    | Abstract_Variable (A_Seen _|A_Refined) -> lst
    | Abstract_Constant (A_Machine _|A_Redeclared_In_Machine _|
                         A_Included _|A_Redeclared_In_Included _) ->
      { id=x; typ=symb.sy_typ; kind=K_Abstract_Constant }::lst
    | Abstract_Constant (A_Seen _|A_Refined) -> lst
    | Concrete_Variable (A_Machine _|A_Refined|A_Redeclared_In_Machine _|
                         A_Included _|A_Redeclared_In_Included _) ->
      { id=x; typ=symb.sy_typ; kind=K_Concrete_Variable }::lst
    | Concrete_Variable (A_Seen _) -> lst
    | Concrete_Constant (A_Machine _|A_Refined|A_Redeclared_In_Machine _|
                         A_Included _|A_Redeclared_In_Included _) ->
      { id=x; typ=symb.sy_typ; kind=K_Concrete_Constant }::lst
    | Concrete_Constant (A_Seen _) -> lst
    | Abstract_Set (Machine _ |Refined|Included _) ->
      { id=x; typ=symb.sy_typ; kind=K_Abstract_Set }::lst
    | Abstract_Set (Seen _) -> lst
    | Concrete_Set (elts, (Machine _|Refined|Included _)) ->
      { id=x; typ=symb.sy_typ; kind=K_Concrete_Set elts }::lst
    | Concrete_Set (_,Seen _) -> lst
    | Enumerate (Machine _|Refined|Included _) ->
      { id=x; typ=symb.sy_typ; kind=K_Enumerate }::lst
    | Enumerate (Seen _) -> lst

  let get_hidden (x:string) (symb:t_kind t_symbol_infos) (map:string Utils.SMap.t) : string Utils.SMap.t =
    match symb.sy_kind with
    | Abstract_Set (Seen mch) | Concrete_Set (_, (Seen mch)) ->
      Utils.SMap.add x mch.r_str map
    | _ -> map

  let get_operations (id:string) (op:t_op_source t_operation_infos) lst =
    match op.op_src with
    | O_Refined_And_Machine _ | O_Refined | O_Refined_Included_And_Promoted _ ->
      { MachineInterface.id; args_in=op.op_args_in;
        args_out=op.op_args_out; readonly=op.op_readonly }::lst
    | O_Seen _ | O_Included _ | O_Refined_And_Included _ -> lst

    let param_kind = function
      | Parameter (kind,_) -> Some kind
      | _ -> None

  let check_hidden (map:string Utils.SMap.t) (id:string) (src:t_source) : bool =
    match Utils.SMap.find_opt id map with
    | None -> true
    | Some mch ->
      begin match src with
        | Machine _ | Refined -> false
        | Included mch2 | Seen mch2 -> String.equal mch mch2.r_str
      end

end

module Imp = struct

  type t_source =
    | Machine of loc
    | Seen of ren_ident
    | Refined
    | Imported of ren_ident

  type t_abstract_decl =
    | A_Seen of ren_ident
    | A_Refined
    | A_Imported of ren_ident
    | A_Redeclared_In_Imported of ren_ident

  type t_concrete_var_decl =
    | V_Machine of loc
    | V_Seen of ren_ident
    | V_Refined
    | V_Imported of ren_ident
    | V_Redeclared_In_Imported of ren_ident
    | V_Redeclared_In_Machine of loc

  type t_concrete_const_decl =
    | C_Machine of loc
    | C_Seen of ren_ident
    | C_Refined
    | C_Imported of ren_ident
    | C_Redeclared_In_Seen of ren_ident
    | C_Redeclared_In_Imported of ren_ident
    | C_Redeclared_In_Machine of loc

  type t_concrete_data_decl =
    | D_Machine of loc
    | D_Seen of ren_ident
    | D_Refined
    | D_Imported of ren_ident

  type t_abstract_set_decl =
    | S_Machine of loc
    | S_Seen of ren_ident
    | S_Refined
    | S_Imported of ren_ident
    | S_Redeclared_In_Seen of ren_ident
    | S_Redeclared_In_Imported of ren_ident

  type t_kind =
    | Parameter of t_param_kind*loc
    | Abstract_Variable of t_abstract_decl
    | Abstract_Constant of t_abstract_decl
    | Concrete_Variable of t_concrete_var_decl
    | Concrete_Constant of t_concrete_const_decl
    | Abstract_Set of t_abstract_set_decl
    | Concrete_Set of string list * t_concrete_data_decl
    | Enumerate of t_concrete_data_decl

  type t_op_source =
    | O_Seen of ren_ident
    | O_Imported of ren_ident
    | O_Refined
    | O_Current_And_Refined of loc
    | O_Imported_And_Refined of ren_ident
    | O_Imported_Promoted_And_Refined of ren_ident*loc
    | O_Local_Spec of loc
    | O_Local_Spec_And_Implem of loc*loc

  let to_abstract_decl (src:t_source) : t_abstract_decl =
    match src with
    | Machine _ ->
      failwith "Internal error: Global.Imp.to_abstract_decl" (*no abstract data in implementations*)
    | Seen mch -> A_Seen mch
    | Refined -> A_Refined
    | Imported mch -> A_Imported mch

  let to_concrete_var_decl (src:t_source) : t_concrete_var_decl =
    match src with
    | Machine l -> V_Machine l 
    | Seen mch -> V_Seen mch
    | Refined -> V_Refined
    | Imported mch -> V_Imported mch 

  let to_concrete_const_decl (src:t_source) : t_concrete_const_decl =
    match src with
    | Machine l -> C_Machine l
    | Seen mch -> C_Seen mch
    | Refined -> C_Refined
    | Imported mch -> C_Imported mch

  let to_concrete_data_decl (src:t_source) : t_concrete_data_decl =
    match src with
    | Machine l -> D_Machine l
    | Seen mch -> D_Seen mch
    | Refined -> D_Refined
    | Imported mch -> D_Imported mch

  let to_abstract_set_decl (src:t_source) : t_abstract_set_decl =
    match src with
    | Machine l -> S_Machine l
    | Seen mch -> S_Seen mch
    | Refined -> S_Refined
    | Imported mch -> S_Imported mch

  let mk_kind (kind:t_global_kind) (src:t_source) : t_kind =
    match kind with
    | K_Abstract_Variable -> Abstract_Variable (to_abstract_decl src)
    | K_Abstract_Constant -> Abstract_Constant (to_abstract_decl src)
    | K_Concrete_Variable -> Concrete_Variable (to_concrete_var_decl src)
    | K_Concrete_Constant -> Concrete_Constant (to_concrete_const_decl src)
    | K_Abstract_Set -> Abstract_Set (to_abstract_set_decl src)
    | K_Concrete_Set elts -> Concrete_Set (elts,to_concrete_data_decl src)
    | K_Enumerate -> Enumerate (to_concrete_data_decl src)
    | K_Parameter k ->
      begin match src with
        | Machine l -> Parameter (k,l)
        | _ -> failwith "Internal error: Global.Imp.mk_kind"
      end

  let update_kind (decl:t_kind) (ki:t_global_kind) (src:t_source) : t_kind option =
    match decl, ki with
    | Abstract_Variable src2, K_Abstract_Variable ->
      begin match src2, src with
        | A_Refined, Machine _ ->
          failwith "Internal error: Global.Imp.update_kind" (*no abstract data in implementations*)
        | A_Refined, Imported mch ->
          Some (Abstract_Variable (A_Redeclared_In_Imported mch))
        | _, _ -> None
      end
    | Abstract_Variable src2, K_Concrete_Variable ->
      begin match src2, src with
        | A_Refined, Machine l ->
          Some (Concrete_Variable (V_Redeclared_In_Machine l))
        | A_Refined, Imported mch->
          Some (Concrete_Variable (V_Redeclared_In_Imported mch))
        | _, _ -> None
      end
    | Abstract_Constant src2, K_Abstract_Constant ->
      begin match src2, src with
        | A_Refined, Machine _ ->
          failwith "Internal error: Global.Imp.update_kind" (*no abstract data in implementations*)
        | A_Refined, Imported mch ->
          Some (Abstract_Constant (A_Redeclared_In_Imported mch))
        | _, _ -> None
      end
    | Abstract_Constant src2, K_Concrete_Constant ->
      begin match src2, src with
        | A_Refined, Machine l ->
          Some (Concrete_Constant (C_Redeclared_In_Machine l))
        | A_Refined, Imported mch->
          Some (Concrete_Constant (C_Redeclared_In_Imported mch))
        | A_Refined, Seen mch->
          Some (Concrete_Constant (C_Redeclared_In_Seen mch))
        | _, _ -> None
      end
    | Abstract_Set S_Refined, K_Abstract_Set ->
        begin match src with
        | Imported mch -> Some (Abstract_Set (S_Redeclared_In_Imported mch))
        | Seen mch -> Some (Abstract_Set (S_Redeclared_In_Seen mch))
        | _ -> None
      end
    | Concrete_Constant C_Refined, K_Concrete_Constant ->
      begin match src with
        | Imported mch -> Some (Concrete_Constant (C_Redeclared_In_Imported mch))
        | Seen mch -> Some (Concrete_Constant (C_Redeclared_In_Seen mch))
        | _ -> None
      end
    | Concrete_Variable V_Refined, K_Concrete_Variable ->
        begin match src with
        | Imported mch -> Some (Concrete_Variable (V_Redeclared_In_Imported mch))
        | _ -> None
      end
    | Enumerate D_Refined, K_Enumerate ->
      begin match src with
        (*XXX should be allowed only when the same machine is included in the
         * refined machine and imported in the implementation *)
        | Imported mch -> Some (Enumerate (D_Imported mch)) 
        | _ -> None
      end
    | Concrete_Set (elts1,D_Refined), K_Concrete_Set elts2 ->
      begin match src with
        | Imported mch ->
          begin try
              if List.for_all2 String.equal elts1 elts2 then
                Some (Concrete_Set (elts1,D_Imported mch)) (*XXX idem*)
              else None
            with Invalid_argument _ -> None
          end
        | _ -> None
      end
    | _ -> None

  let mk_op_source = function
    | Machine _ -> None
    | Refined -> Some O_Refined
    | Seen mch -> Some (O_Seen mch)
    | Imported mch -> Some (O_Imported mch)

  let update_op_source (old_src:t_op_source) (new_src:t_source) : t_op_source option =
    match old_src, new_src with
    | O_Refined, Machine l -> Some (O_Current_And_Refined l)
    | O_Refined, Imported mch -> Some (O_Imported_And_Refined mch)
    | O_Local_Spec l1 , Machine l2 -> Some (O_Local_Spec_And_Implem (l1,l2))
    | _, _ -> None

  let check_hidden (map:string Utils.SMap.t) (id:string) (src:t_source) : bool =
    match Utils.SMap.find_opt id map with
    | None -> true
    | Some mch ->
      begin match src with
        | Machine _ | Refined -> false
        | Imported mch2 | Seen mch2 -> String.equal mch mch2.r_str
      end
end

type (_,_) c_kind =
  | Mch : (Mch.t_kind,Mch.t_op_source) c_kind
  | Ref : (Ref.t_kind,Ref.t_op_source) c_kind
  | Imp : (Imp.t_kind,Imp.t_op_source) c_kind

type ('sy_ki,'op_ki) t = {
  witness: ('sy_ki,'op_ki) c_kind;
  mutable deps: string list;
  params: lident list;
  mutable alias:Btype.t_alias;
  symb:(string,'sy_ki t_symbol_infos) Hashtbl.t;
  ops:(string,'op_ki t_operation_infos) Hashtbl.t;
  mutable hidden_types: string Utils.SMap.t
}

let create witness params : _ t =
  { witness;
    deps=[];
    params;
    hidden_types=Utils.SMap.empty;
    alias=Btype.no_alias;
    symb=Hashtbl.create 47;
    ops=Hashtbl.create 47 }

type mEnv = (Mch.t_kind,Mch.t_op_source) t
type rEnv = (Ref.t_kind,Ref.t_op_source) t
type iEnv = (Imp.t_kind,Imp.t_op_source) t

let get_alias env = env.alias

let get_symbol (env:_ t) (id:string) : _ t_symbol_infos option =
  Hashtbl.find_opt env.symb id

let get_operation (env:_ t) (id:string) : _ t_operation_infos option =
  Hashtbl.find_opt env.ops id

let fold_symbols (f:string -> 'mr t_symbol_infos -> 'a -> 'a) (env:('mr,_) t) : 'a -> 'a =
  Hashtbl.fold f env.symb

let fold_operations (f:string -> 'mr t_operation_infos -> 'a -> 'a) (env:(_,'mr) t) : 'a -> 'a =
  Hashtbl.fold f env.ops

let add_alias (s:_ t) (alias:string) (ty:Btype.t) : bool =
  match Btype.add_alias s.alias alias ty with
  | None -> false
  | Some alias -> (s.alias <- alias; true)

type ('sy_ki,'op_ki,'src) func = (
  module S with
    type t_kind = 'sy_ki and
  type t_op_source = 'op_ki and
  type t_source = 'src
)

let merge_hidden (_:_ t) (_:string Utils.SMap.t) : unit = assert false (*FIXME*)

let mFunc : (Mch.t_kind,Mch.t_op_source,Mch.t_source) func =
  (module Mch : S with type t_kind=Mch.t_kind and type t_source=Mch.t_source and type t_op_source=Mch.t_op_source )
let rFunc : (Ref.t_kind,Ref.t_op_source,Ref.t_source) func =
  (module Ref : S with type t_kind=Ref.t_kind and type t_source=Ref.t_source and type t_op_source=Ref.t_op_source )
let iFunc : (Imp.t_kind,Imp.t_op_source,Imp.t_source) func =
  (module Imp : S with type t_kind=Imp.t_kind and type t_source=Imp.t_source and type t_op_source=Imp.t_op_source )

let _add_symbol (type a b src) (f:(a,b,src) func) (env:(a,b)t) (err_loc:loc)
    (id:string) (sy_typ:Btype.t) (ki:t_global_kind) (src:src) : unit
  =
  let module F = (val f) in
  match Hashtbl.find_opt env.symb id with
  | Some infos ->
    let sy_kind = match F.update_kind infos.sy_kind ki src with
      | Some x -> x
      | None -> Error.error err_loc ("The identifier '" ^ id ^
                                     "' clashes with previous declaration.")
    in
    if not (Btype.is_equal env.alias infos.sy_typ sy_typ) then
      Error.error  err_loc
        ("The identifier '" ^ id ^ "' has type " 
         ^ Btype.to_string sy_typ
         ^ " but was previously declared with type "
         ^ Btype.to_string infos.sy_typ ^ ".")
    else
      Hashtbl.replace env.symb id { sy_typ; sy_kind }
  | None ->
      if F.check_hidden env.hidden_types id src then
        Hashtbl.add env.symb id { sy_typ; sy_kind=F.mk_kind ki src }
      else
        assert false (*FIXME error hidden clash*)

let add_symbol (type a b) (env:(a,b) t) l id ty ki : unit =
  match env.witness with
  | Mch -> _add_symbol mFunc env l id ty ki (Mch.Machine l)
  | Ref -> _add_symbol rFunc env l id ty ki (Ref.Machine l)
  | Imp -> _add_symbol iFunc env l id ty ki (Imp.Machine l)

let rec find_duplicate : (string*'a) list -> string option = function
  | [] -> None
  | (x,_)::tl ->
    let aux (y,_) = String.equal x y in
    if List.exists aux tl then Some x
    else find_duplicate tl

let check_args_type alias (err_loc:loc) (args_old:(string*Btype.t)list) (args_new:(string*Btype.t)list) : unit =
  let aux (x1,ty1) (x2,ty2) =
    if String.equal x1 x2 then
      if Btype.is_equal alias ty1 ty2 then ()
      else Error.error err_loc
          ("The parameter '"^x1^"' has type '"^Btype.to_string ty1^
           "' but parameter of type '"^Btype.to_string ty2^"' was expected.")
    else Error.error err_loc ("Parameter '"^x2^"' expected but '"^x1^"' was found.")
  in
  try List.iter2 aux args_old args_new;
  with Invalid_argument _ -> Error.error err_loc "Unexpected number of parameters."

let _add_operation (type a b src) (f:(a,b,src)func) (env:(a,b)t) (err_loc:loc) (id:string)
    (op_args_in:(string*Btype.t)list) (op_args_out:(string*Btype.t)list)
    (src:src) op_readonly : unit
  =
  let module F = (val f) in
  match Hashtbl.find_opt env.ops id with
  | Some infos ->
    begin match F.update_op_source infos.op_src src with
      | None ->
        Error.error err_loc ("The operation '" ^ id ^ "' clashes with previous declaration.") 
      | Some op_src ->
        ( check_args_type env.alias err_loc infos.op_args_in op_args_in;
          check_args_type env.alias err_loc infos.op_args_out op_args_out;
          Hashtbl.replace env.ops id { infos with op_src } )
    end
  | None ->
    begin match F.mk_op_source src with
      | Some op_src ->
        Hashtbl.add env.ops id { op_args_in; op_args_out; op_readonly; op_src }
      | None -> assert false (*FIXME error*)
    end

let add_operation (type a b) (env:(a,b) t) (loc:loc) (id:string)
    (args_in:(string*Btype.t) list) (args_out:(string*Btype.t) list)
    ~is_readonly : unit
  =
  match find_duplicate (args_in@args_out) with
  | None ->
    begin match env.witness with
      | Mch -> _add_operation mFunc env loc id args_in args_out (Mch.Machine loc) is_readonly
      | Ref -> _add_operation rFunc env loc id args_in args_out (Ref.Machine loc) is_readonly
      | Imp -> _add_operation iFunc env loc id args_in args_out (Imp.Machine loc) is_readonly
    end
  | Some arg ->
    Error.error loc ("The argument '"^arg^"' appears twice in this operation declaration.")

let add_local_operation  (env:iEnv) (lc:loc) (id:string)
    (op_args_in:(string*Btype.t)list) (op_args_out:(string*Btype.t)list) : unit
  =
  match Hashtbl.find_opt env.ops id with
  | Some _ ->
    Error.error lc ("The operation '" ^ id ^ "' clashes with previous declaration.") 
  | None ->
    Hashtbl.add env.ops id { op_args_in; op_args_out; op_readonly=false;
                             op_src=Imp.O_Local_Spec lc }

let mch_promote _ = function
  | Mch.O_Included mch -> Some (Mch.O_Included_And_Promoted mch)
  | _ -> None

let ref_promote _ = function
  | Ref.O_Refined_And_Included mch -> Some (Ref.O_Refined_Included_And_Promoted mch)
  | Ref.O_Included _ -> assert false (*FIXME error*)
  | Ref.O_Refined_Included_And_Promoted _ -> assert false (*FIXME err*)
  | _ -> None

let imp_promote lc = function
  | Imp.O_Imported_And_Refined mch -> Some (Imp.O_Imported_Promoted_And_Refined (mch,lc))
  | Imp.O_Imported _ -> assert false (*FIXME error*)
  | Imp.O_Imported_Promoted_And_Refined _ -> assert false (*FIXME error*)
  | _ -> None

let _promote_operation (type a b) promote (env:(a,b) t) (lc:loc) (id:string) : unit =
  match get_operation env id with
  | None -> Error.error lc ("Unknown operation '"^id^"'.")
  | Some op ->
    begin match promote lc op.op_src with
      | Some op_src -> Hashtbl.replace env.ops id { op with op_src }
      | None ->
        Error.error lc ("The operation '"^id^"' is not an operation of an included/imported/extended machine.")
    end

let promote_operation (type a b) (env:(a,b) t) l id : unit =
  match env.witness with
  | Mch -> _promote_operation mch_promote env l id
  | Ref -> _promote_operation ref_promote env l id
  | Imp -> _promote_operation imp_promote env l id

let add_parameter env loc id ty ki = add_symbol env loc id ty (K_Parameter ki)
let add_abstract_variable env loc id ty = add_symbol env loc id ty K_Abstract_Variable
let add_concrete_variable env loc id ty = add_symbol env loc id ty K_Concrete_Variable
let add_abstract_constant env loc id ty = add_symbol env loc id ty K_Abstract_Constant
let add_concrete_constant env loc id ty = add_symbol env loc id ty K_Concrete_Constant

let add_abstract_set env loc id =
  let ty = Btype.mk_Power (Btype.mk_Abstract_Set id) in
  add_symbol env loc id ty K_Abstract_Set

let add_concrete_set env loc id elts =
  let ty = Btype.mk_Abstract_Set id in
  List.iter (fun x -> add_symbol env x.SyntaxCore.lid_loc x.SyntaxCore.lid_str ty K_Enumerate) elts;
  add_symbol env loc id (Btype.mk_Power ty)
    (K_Concrete_Set (List.map (fun x -> x.SyntaxCore.lid_str) elts))

let load_external_symbol (type a b src) f (env:(a,b)t) alias (mch:ren_ident)
    (src:src) ({id;typ;kind;_}:MachineInterface.t_symb) : unit =
  let mk_id id = match mch.r_prefix with
    | None -> id
    | Some p -> p ^ "." ^ id 
  in
  match kind with
  | K_Abstract_Variable
  | K_Concrete_Variable ->
    let ty = Btype.subst alias typ in
    _add_symbol f env mch.SyntaxCore.r_loc (mk_id id) ty kind src
  | _ ->
    if (is_in_deps env.deps mch.SyntaxCore.r_str) then ()
    else
      _add_symbol f env mch.SyntaxCore.r_loc id typ kind src 

let load_external_op (type a b src) (f:(a,b,src)func) (env:(a,b)t)
    (mch:ren_ident) (src:src) (op:MachineInterface.t_op) : unit
  =
  let op_name = match mch.SyntaxCore.r_prefix with
    | None -> op.id
    | Some p -> p ^ "." ^ op.id
  in
  _add_operation f env mch.SyntaxCore.r_loc op_name (op.args_in) (op.args_out) src op.readonly

let load_interface_for_used_machine (env:mEnv) (itf:t_interface) (mch:ren_ident) : unit =
  (*FIXME check not already in deps*)
  (*FIXME parameters*)
  merge_hidden env itf.hidden;
  let open MachineInterface in
  List.iter (load_external_symbol mFunc env Utils.SMap.empty mch (Mch.Used mch)) itf.symbs;
  env.deps <- mch.r_str::env.deps

let load_interface_for_seen_machine (type a b) (env:(a,b) t) (itf:t_interface) (mch:ren_ident) : unit =
  let open MachineInterface in
  (*FIXME check not already in deps*)
  (*FIXME parameters*)
  merge_hidden env itf.hidden;
  (match env.witness with
  | Mch ->
    List.iter (load_external_symbol mFunc env Utils.SMap.empty mch (Mch.Seen mch)) itf.symbs;
    List.iter (load_external_op mFunc env mch (Mch.Seen mch)) itf.ops
  | Ref ->
    List.iter (load_external_symbol rFunc env Utils.SMap.empty mch (Ref.Seen mch)) itf.symbs;
    List.iter (load_external_op rFunc env mch (Ref.Seen mch)) itf.ops
  | Imp ->
    List.iter (load_external_symbol iFunc env Utils.SMap.empty mch (Imp.Seen mch)) itf.symbs;
    List.iter (load_external_op iFunc env mch (Imp.Seen mch)) itf.ops );
  env.deps <- mch.r_str::env.deps

let rec check_set_params err_loc lst1 lst2 alias =
  let open MachineInterface in
  match lst1, lst2 with
  | [], [] -> alias
  | hd1::tl1, (lc,hd2)::tl2 ->
    begin match hd1.kind with
      | Scalar -> check_set_params err_loc tl1 tl2 alias
      | Set ->
        begin match Btype.view hd2 with
          | Btype.T_Power ty -> check_set_params err_loc tl1 tl2 (Utils.SMap.add hd1.id ty alias)
          | _ ->
            let err_txt = Printf.sprintf
                "This expression has type '%s' but an expression of type '%s' was expected."
                (Btype.to_string hd2)
                (Btype.Open.to_string (Btype.Open.mk_Power (Btype.Open.new_meta ())))
            in
            Error.error lc err_txt
        end
    end
  | _, _ -> Error.error err_loc "Wrong number of parameters."

let rec check_scalar_params alias lst1 lst2 =
  let open MachineInterface in
  match lst1, lst2 with
  | [], [] -> ()
  | hd1::tl1, (lc,hd2)::tl2 ->
    begin match hd1.kind with
      | Scalar ->
        if Btype.equal (Btype.subst alias hd1.typ) hd2 then
          check_scalar_params alias tl1 tl2
        else
          let err_txt = Printf.sprintf
              "This expression has type '%s' but an expression of type '%s' was expected."
              (Btype.to_string hd2)
              (Btype.Open.to_string (Btype.Open.mk_Power (Btype.Open.new_meta ())))
          in
          Error.error lc err_txt
      | Set -> check_scalar_params alias tl1 tl2
    end
  | _, _ -> assert false (*FIXME error *)

let load_interface_for_included_or_imported_machine (type a b) (env:(a,b)t) (itf:t_interface) (mch:ren_ident) (params:(loc*Btype.t) list) : unit =
  let open MachineInterface in
  (*FIXME check not already in deps*)
  merge_hidden env itf.hidden;
  let alias = check_set_params mch.r_loc itf.params params Utils.SMap.empty in
  check_scalar_params alias itf.params params;
  (match env.witness with
  | Mch ->
    List.iter (load_external_symbol mFunc env alias mch (Mch.Included mch)) itf.symbs;
    List.iter (load_external_op mFunc env mch (Mch.Included mch)) itf.ops
  | Ref ->
    List.iter (load_external_symbol rFunc env alias mch (Ref.Included mch)) itf.symbs;
    List.iter (load_external_op rFunc env mch (Ref.Included mch)) itf.ops
  | Imp ->
    List.iter (load_external_symbol iFunc env alias mch (Imp.Imported mch)) itf.symbs;
    List.iter (load_external_op iFunc env mch (Imp.Imported mch)) itf.ops );
  env.deps <- mch.r_str::env.deps

let load_interface_for_extended_machine (type a b) (env:(a,b)t) (itf:t_interface) (mch:ren_ident) (params:(loc*Btype.t) list) : unit =
  let open MachineInterface in
  load_interface_for_included_or_imported_machine env itf mch params;
  List.iter (fun (r:t_op) ->
      promote_operation env mch.SyntaxCore.r_loc r.id
    ) itf.ops

let rec check_param err_loc (add_param:loc -> MachineInterface.t_param -> unit)
    (lst1:lident list) (lst2:MachineInterface.t_param list) : unit =
    match lst1, lst2 with
    | [], [] -> ()
    | hd1::tl1, hd2::tl2 ->
      if hd1.lid_str = hd2.id then
        ( add_param hd1.lid_loc hd2; check_param err_loc add_param tl1 tl2 )
      else Error.error hd1.lid_loc "Parameter mismatch."
    | hd1::_, [] -> Error.error hd1.lid_loc "Parameter mismatch."
    | [], hd2::_ -> Error.error err_loc ("Missing parameter '"^hd2.id^"'.")

let load_interface_for_refined_machine (type a b) (env:(a,b)t) (itf:t_interface) (mch:lident) (params:lident list) : unit =
  (*FIXME check not already in deps*)
  merge_hidden env itf.hidden;
  let open MachineInterface in
  let ren_mch = { SyntaxCore.r_prefix=None; r_str=mch.lid_str; r_loc=mch.lid_loc } in
  ( match env.witness with
  | Mch -> failwith "Internal error: Global.load_interface_for_refined_machine"
  | Ref ->
    let add_param l p =
      _add_symbol rFunc env l p.id p.typ (K_Parameter p.kind)(Ref.Machine l)
    in
    check_param mch.lid_loc add_param params itf.params;
    List.iter (load_external_symbol rFunc env Utils.SMap.empty ren_mch Ref.Refined) itf.symbs;
    List.iter (load_external_op rFunc env ren_mch Ref.Refined) itf.ops
  | Imp ->
    let add_param l p =
      _add_symbol iFunc env l p.id p.typ (K_Parameter p.kind)(Imp.Machine l)
    in
    check_param mch.lid_loc add_param params itf.params;
    List.iter (load_external_symbol iFunc env Utils.SMap.empty ren_mch Imp.Refined) itf.symbs;
    List.iter (load_external_op iFunc env ren_mch Imp.Refined) itf.ops );
  env.deps <- mch.lid_str::env.deps

let _to_interface (type a b) get_symbols get_operations param_kind get_hidden
    (env:(a,b)t) : t_interface =
  let get_params (lid:lident) : MachineInterface.t_param =
    let id = lid.SyntaxCore.lid_str in
    match Hashtbl.find_opt env.symb id with
    | None -> assert false
    | Some infos ->
      begin match param_kind infos.sy_kind with
        | Some kind -> { MachineInterface.id; typ=infos.sy_typ; kind }
        | None -> assert false
      end
  in
  let params:MachineInterface.t_param list = List.map get_params env.params in
  let symbs = Hashtbl.fold get_symbols env.symb [] in
  let ops = Hashtbl.fold get_operations env.ops [] in
  let hidden = Hashtbl.fold get_hidden env.symb Utils.SMap.empty in
  MachineInterface.({symbs;ops;params;hidden})

let to_interface (type a b) (env:(a,b)t) : t_interface =
  match env.witness with
  | Mch -> _to_interface Mch.get_symbols Mch.get_operations Mch.param_kind Mch.get_hidden env
  | Ref -> _to_interface Ref.get_symbols Ref.get_operations Ref.param_kind Ref.get_hidden env
  | Imp -> failwith "Internal error: Global.to_interface"

let check_operation_coherence (env:iEnv) (lc:loc) : unit =
  Hashtbl.iter (
    fun x op ->
      match op.op_src with
      | Imp.O_Seen _ -> ()
      | Imp.O_Imported _ -> ()
      | Imp.O_Local_Spec_And_Implem _ -> ()
      | Imp.O_Current_And_Refined _ -> ()
      | Imp.O_Imported_Promoted_And_Refined _ -> ()
      | Imp.O_Refined -> Error.error lc ("The operation '"^x^"' is not refined.")
      | Imp.O_Local_Spec lc -> Error.error lc ("The local operation '"^x^"' is not implemented.")
      | Imp.O_Imported_And_Refined _ ->
        Error.error lc ("The operation '"^x^"' is not refined (missing promotion?).")
  ) env.ops
(*



let load_interface_for_included_or_imported_machine (env:'a t) (itf:MachineInterface.t)
    (mch:ren_ident) (params:(loc*Btype.t) list) : unit =
  let open MachineInterface in
  
  List.iter (fun (S { id;typ;kind}:t_symb) ->
      match kind with
      | G_Parameter _ -> ()
      | G_Abstract_Variable ->
        let ren_id = match mch.r_prefix with
          | None -> id
          | Some p -> p ^ "." ^ id 
        in
        _add_symbol env mch.SyntaxCore.r_loc ren_id (Btype.subst alias typ) kind (S_Included_Or_Imported mch)
      | G_Concrete_Variable ->
        let ren_id = match mch.r_prefix with
          | None -> id
          | Some p -> p ^ "." ^ id 
        in
        _add_symbol env mch.SyntaxCore.r_loc ren_id (Btype.subst alias typ) kind (S_Included_Or_Imported mch)
      | _ ->
        begin match mch.r_prefix with
          | None ->
            _add_symbol env mch.SyntaxCore.r_loc id typ kind (S_Included_Or_Imported mch)
          | Some _ ->
            if is_in_deps env mch.SyntaxCore.r_str then ()
            else
              _add_symbol env mch.SyntaxCore.r_loc id typ kind (S_Included_Or_Imported mch)
        end
    ) (get_symbols itf);
  List.iter (fun (r:t_op) ->
      let op_name = match mch.SyntaxCore.r_prefix with
        | None -> r.id
        | Some p -> p ^ "." ^ r.id
      in
      _add_operation env mch.SyntaxCore.r_loc op_name r.args_in r.args_out r.readonly (SO_Included_Or_Imported mch)
    ) (get_operations itf);
  env.deps <- mch.r_str::env.deps

let load_interface_for_refined_machine (env:t_ref t) (itf:MachineInterface.t) (mch:lident) (params:lident list): unit =
  let open MachineInterface in
  let rec check_param (lst1:lident list) (lst2:t_param list) =
    match lst1, lst2 with
    | [], [] -> ()
    | hd1::tl1, hd2::tl2 ->
      if hd1.lid_str = hd2.id then
        ( _add_symbol env hd1.lid_loc hd2.id hd2.typ (G_Parameter hd2.kind) (S_Current hd1.lid_loc);
          check_param tl1 tl2 )
      else Error.error hd1.lid_loc "Parameter mismatch."
    | hd1::_, [] -> Error.error hd1.lid_loc "Parameter mismatch."
    | [], hd2::_ -> Error.error mch.lid_loc ("Missing parameter '"^hd2.id^"'.")
  in
  check_param params (get_params itf);
  List.iter (fun (S { id;typ;kind}:t_symb) ->
      _add_symbol env mch.SyntaxCore.lid_loc id typ kind (S_Refined mch)
    ) (get_symbols itf);
  List.iter (fun (r:t_op) ->
      _add_operation env mch.SyntaxCore.lid_loc r.id r.args_in r.args_out r.readonly (SO_Refined mch)
    ) (get_operations itf);
  env.deps <- mch.lid_str::env.deps



let add_abstract_sets (src:Btype.t_atomic_src) (accu:(Btype.t_atomic_src*string) list)
    (itf:t_interface) : (Btype.t_atomic_src*string) list =
  let open MachineInterface in
  let aux accu (x:t_symb) = match x.kind with
    | G_Abstract_Set -> (src,x.id)::accu
    | _ -> accu
  in
  List.fold_left aux accu (get_symbols itf)
   *)

