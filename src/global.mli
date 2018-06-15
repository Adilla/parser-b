(** Global typing environment *)
open Utils
open Syntax

type t

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

type t_kind = 
  | K_Abstract_Variable | K_Concrete_Variable
  | K_Abstract_Constant | K_Concrete_Constant
  | K_Abstract_Set | K_Concrete_Set of ident list
  | K_Enumerate

type t_clause =
  | C_Invariant_Or_Assertions
  | C_Properties
  | C_Operations
  | C_Local_Operations
  | C_Values
  | C_Assert_Or_While_Invariant

val create : unit -> t

val get_symbol_type : t -> ident -> Btype.t option
val get_symbol_kind : t -> ident -> t_kind option
val get_symbol_type_in_clause : t -> loc -> ident -> t_clause -> Btype.t Error.t_result
val get_writable_symbol_type_in_clause : t -> loc -> ident -> t_clause -> Btype.t Error.t_result

val get_alias : t -> Btype.t_alias 
val add_alias : t -> string -> Btype.t -> bool

type t_op_type = { args_in:(ident*Btype.t) list; args_out:(ident*Btype.t) list; }

val get_operation_type2 : t -> ident -> t_op_type option
val get_operation_type : t -> loc -> ident -> t_op_type Error.t_result
val is_operation_readonly : t -> ident -> bool
val is_operation_local : t -> ident -> bool

val add_symbol : t -> loc -> ident -> Btype.t -> t_kind -> unit Error.t_result

val add_operation : t -> loc -> ident -> t_op_type -> bool (*is_read_only*) -> bool (*is_local*) -> unit Error.t_result
val promote_operation : t -> loc -> ident -> unit Error.t_result

type t_interface
val to_interface : t -> t_interface 

val load_interface_for_seen_machine : t -> t_interface -> p_lident -> unit Error.t_result
val load_interface_for_included_machine : t -> t_interface -> p_lident -> unit Error.t_result
val load_interface_for_refined_machine : t -> t_interface -> p_lident -> unit Error.t_result
val load_interface_for_imported_machine : t -> t_interface -> p_lident -> unit Error.t_result
val load_interface_for_extended_machine : t -> t_interface -> p_lident -> unit Error.t_result

val check_operation_coherence: t -> loc -> bool -> unit Error.t_result

val set_extended_sees: bool -> unit
val fold_symbols : ('a -> ident -> t_kind -> t_source -> Btype.t -> 'a) -> t -> 'a -> 'a
val fold_operations : ('a -> ident -> t_op_source -> t_op_type -> 'a) -> t -> 'a -> 'a
val get_op_source : t -> ident -> ident option
val get_symbol_source : t -> ident -> ident option
