(** Global typing environment *)
type lident = SyntaxCore.lident
type loc = Utils.loc

type t_abstract = private T_Abs
type t_concrete = private T_Conc
type t_mch = private T_Mch
type t_ref = private T_Ref

type 'ac t_redeclared =
  | Implicitely : t_concrete t_redeclared
  | By_Machine : loc -> 'ac t_redeclared
  | By_Included_Or_Imported : lident -> 'ac t_redeclared

type ('mr,'ac) t_decl =
  | D_Machine : loc -> ('mr,'ac) t_decl
  | D_Seen : lident -> ('mr,'ac) t_decl
  | D_Included_Or_Imported : lident -> ('mr,'ac) t_decl
  | D_Disappearing : (t_ref,t_abstract) t_decl
  | D_Redeclared : 'ac t_redeclared -> (t_ref,'ac) t_decl

type t_variable = private T_Var
type t_constant = private T_Const

type _ t_global_kind = 
  | K_Abstract_Variable : t_abstract t_global_kind
  | K_Abstract_Constant : t_abstract t_global_kind
  | K_Concrete_Variable : t_concrete t_global_kind
  | K_Concrete_Constant : t_concrete t_global_kind
  | K_Abstract_Set : t_concrete t_global_kind
  | K_Concrete_Set : string list  -> t_concrete t_global_kind
  | K_Enumerate : t_concrete t_global_kind

type 'mr t_kind = Pack : 'ac t_global_kind*('mr,'ac) t_decl -> 'mr t_kind

type 'a t_symbol_infos = {
  sy_typ:Btype.t;
  sy_kind:'a t_kind
}

type 'a t_op_decl =
  | OD_Current : loc -> t_mch t_op_decl
  | OD_Seen : lident -> 'a t_op_decl
  | OD_Included_Or_Imported : lident -> 'a t_op_decl
  | OD_Included_Or_Imported_And_Promoted : lident*loc -> t_mch t_op_decl
  | OD_Refined : lident -> t_ref t_op_decl
  | OD_Current_And_Refined : loc*lident -> t_ref t_op_decl
  | OD_Included_Or_Imported_And_Refined : lident*lident -> t_ref t_op_decl
  | OD_Included_Or_Imported_Promoted_And_Refined : lident*loc*lident -> t_ref t_op_decl
  | OD_Local_Spec : loc -> t_ref t_op_decl
  | OD_Local_Spec_And_Implem : loc*loc -> t_ref t_op_decl

type 'a t_operation_infos =
  { op_args_in: (string*Btype.t) list;
    op_args_out: (string*Btype.t) list;
    op_readonly:bool;
    op_src: 'a t_op_decl; }

type 'a t

val create_mch : unit -> t_mch t
val create_ref : unit -> t_ref t

val get_symbol : 'a t -> string -> 'a t_symbol_infos option
val add_symbol : 'mr t -> loc -> string -> Btype.t -> 'ac t_global_kind -> unit Error.t_result

val get_operation : 'a t -> string -> 'a t_operation_infos option
val add_mch_operation : t_mch t -> loc -> string -> (string*Btype.t) list -> (string*Btype.t) list -> is_readonly:bool -> unit Error.t_result
val add_ref_operation : t_ref t -> loc -> string -> (string*Btype.t) list -> (string*Btype.t) list -> is_local:bool -> unit Error.t_result
val promote_operation : 'a t -> loc -> string -> unit Error.t_result

val get_alias : 'a t -> Btype.t_alias 
val add_alias : 'a t -> string -> Btype.t -> bool

type t_interface
val to_interface : 'a t -> t_interface 

val load_interface_for_seen_machine : 'a t -> t_interface -> lident -> unit Error.t_result
val load_interface_for_refined_machine : t_ref t -> t_interface -> lident -> unit Error.t_result
val load_interface_for_included_or_imported_machine : 'a t -> t_interface -> lident -> unit Error.t_result
val load_interface_for_extended_machine : 'a t -> t_interface -> lident -> unit Error.t_result

val check_operation_coherence_ref : t_ref t  -> loc -> unit Error.t_result
val check_operation_coherence_imp : t_ref t  -> loc -> unit Error.t_result

val fold_symbols: (string -> 'x t_symbol_infos -> 'a -> 'a) -> 'x t -> 'a -> 'a
val fold_operations: (string -> 'x t_operation_infos -> 'a -> 'a) -> 'x t -> 'a -> 'a
