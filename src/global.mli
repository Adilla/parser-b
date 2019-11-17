(** Global typing environment *)

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

type t_interface

type ('sy_ki,'op_ki) t

val get_symbol : ('sy_ki,_) t -> string -> 'sy_ki t_symbol_infos option
val get_operation : (_,'op_ki) t -> string -> 'op_ki t_operation_infos option
val get_alias : _ t -> Btype.t_alias 

val add_alias : _ t -> string -> Btype.t -> bool
val add_symbol : _ t -> loc -> string -> Btype.t -> t_global_kind -> unit
val add_operation : _ t -> loc -> string -> (string*Btype.t) list -> (string*Btype.t) list -> is_readonly:bool -> unit
val promote_operation : _ t -> loc -> string -> unit

val add_parameter : _ t -> loc -> string -> Btype.t -> t_param_kind -> unit
val add_abstract_variable : _ t -> loc -> string -> Btype.t -> unit
val add_concrete_variable : _ t -> loc -> string -> Btype.t -> unit
val add_abstract_constant : _ t -> loc -> string -> Btype.t -> unit
val add_concrete_constant : _ t -> loc -> string -> Btype.t -> unit
val add_abstract_set : _ t -> loc -> string -> unit
val add_concrete_set : _ t -> loc -> string -> lident list -> unit

val to_interface : _ t -> t_interface

val fold_symbols: (string -> 'x t_symbol_infos -> 'a -> 'a) -> ('x,_) t -> 'a -> 'a
val fold_operations: (string -> 'x t_operation_infos -> 'a -> 'a) -> (_,'x) t -> 'a -> 'a

module Mch : sig

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

end

module Ref : sig
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
    | O_Refined_And_Included of ren_ident (*FIXME*)
    | O_Refined_Included_And_Promoted of ren_ident

end

module Imp : sig
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
    | C_Redeclared_In_Machine of loc (*FIXME only of constant*)

  type t_kind =
    | Parameter of t_param_kind*loc
    | Abstract_Variable of t_abstract_decl
    | Abstract_Constant of t_abstract_decl
    | Concrete_Variable of t_concrete_var_decl
    | Concrete_Constant of t_concrete_const_decl
    | Abstract_Set of t_concrete_const_decl
    | Concrete_Set of string list * t_concrete_const_decl
    | Enumerate of t_concrete_const_decl

  type t_op_source =
    | O_Seen of ren_ident
    | O_Imported of ren_ident
    | O_Refined
    | O_Current_And_Refined of loc
    | O_Imported_And_Refined of ren_ident
    | O_Imported_Promoted_And_Refined of ren_ident*loc
    | O_Local_Spec of loc
    | O_Local_Spec_And_Implem of loc*loc

end

type (_,_) c_kind =
  | Mch : (Mch.t_kind,Mch.t_op_source) c_kind
  | Ref : (Ref.t_kind,Ref.t_op_source) c_kind
  | Imp : (Imp.t_kind,Imp.t_op_source) c_kind

val create : ('a,'b) c_kind -> lident list -> ('a,'b) t

type mEnv = (Mch.t_kind,Mch.t_op_source) t
type rEnv = (Ref.t_kind,Ref.t_op_source) t
type iEnv = (Imp.t_kind,Imp.t_op_source) t

val load_interface_for_used_machine : mEnv -> t_interface -> ren_ident -> unit

val load_interface_for_seen_machine : _ t -> t_interface -> ren_ident -> unit
val load_interface_for_included_or_imported_machine : _ t -> t_interface -> ren_ident -> (loc*Btype.t) list -> unit
val load_interface_for_extended_machine : _ t -> t_interface -> ren_ident -> (loc*Btype.t) list -> unit
val load_interface_for_refined_machine : _ t -> t_interface -> lident -> lident list -> unit

val check_operation_coherence : iEnv -> loc -> unit
val add_local_operation : iEnv -> loc -> string -> (string*Btype.t) list -> (string*Btype.t) list -> unit
