(** Global typing environment *)
open Utils
open Syntax

type t

type t_source =
  | S_Current_Mch_Only of loc
  | S_Seen_Mch_Only of lident
  | S_Refined_Mch_Only of lident
  | S_Included_Mch_Only of lident
  | S_Current_And_Refined_Mch of loc*lident
  | S_Included_And_Refined_Mch of lident*lident
  | S_Imported_Mch_Only of lident
  | S_Current_And_Imported_Mch of loc*lident
  | S_Imported_And_Refined_Mch of lident*lident
  | S_Current_Imported_And_Refined_Mch of loc*lident*lident

type t_op_source =
  | OS_Seen_Mch of lident
  | OS_Current_Mch_Only of loc
  | OS_Refined_Mch_Only of lident
  | OS_Included_Mch_Only of lident
  | OS_Current_And_Refined_Mch of loc*lident
  | OS_Included_And_Refined_Mch of lident*lident
  | OS_Local_Spec of loc
  | OS_Local_Spec_And_Implem of loc*loc
  | OS_Imported_Only of lident
  | OS_Imported_And_Promoted of loc*lident
  | OS_Imported_And_Refined of lident*lident
  | OS_Imported_Promoted_And_Refined of loc*lident*lident

type t_kind = 
  | K_Abstract_Variable | K_Concrete_Variable
  | K_Abstract_Constant | K_Concrete_Constant
  | K_Abstract_Set | K_Concrete_Set of P.ident list
  | K_Enumerate

type t_clause =
  | C_Invariant_Or_Assertions
  | C_Properties
  | C_Operations
  | C_Local_Operations
  | C_Values
  | C_Assert_Or_While_Invariant

val create : unit -> t

val get_symbol_type : t -> P.ident -> Btype.t option
val get_symbol_kind : t -> P.ident -> t_kind option
val get_symbol_type_in_clause : t -> loc -> P.ident -> t_clause -> Btype.t Error.t_result
val get_writable_symbol_type_in_clause : t -> loc -> P.ident -> t_clause -> Btype.t Error.t_result

val get_alias : t -> Btype.t_alias 
val add_alias : t -> string -> Btype.t -> bool

type t_op_type = {
  args_in:(P.ident*Btype.t) list;
  args_out:(P.ident*Btype.t) list; }

val get_operation_type2 : t -> P.ident -> t_op_type option
val get_operation_type : t -> loc -> P.ident -> t_op_type Error.t_result
val is_operation_readonly : t -> P.ident -> bool
val is_operation_local : t -> P.ident -> bool

val add_symbol : t -> loc -> P.ident -> Btype.t -> t_kind -> unit Error.t_result

val add_operation : t -> loc -> P.ident -> t_op_type -> bool (*is_read_only*) -> bool (*is_local*) -> unit Error.t_result
val promote_operation : t -> loc -> P.ident -> unit Error.t_result

type t_interface
val to_interface : t -> t_interface 

val load_interface_for_seen_machine : t -> t_interface -> lident -> unit Error.t_result
val load_interface_for_included_machine : t -> t_interface -> lident -> unit Error.t_result
val load_interface_for_refined_machine : t -> t_interface -> lident -> unit Error.t_result
val load_interface_for_imported_machine : t -> t_interface -> lident -> unit Error.t_result
val load_interface_for_extended_machine : t -> t_interface -> lident -> unit Error.t_result

val check_operation_coherence: t -> loc -> bool -> unit Error.t_result

val set_extended_sees: bool -> unit
val fold_symbols : ('a -> P.ident -> t_kind -> t_source -> Btype.t -> 'a) -> t -> 'a -> 'a
val fold_operations : ('a -> P.ident -> t_op_source -> t_op_type -> 'a) -> t -> 'a -> 'a
val get_op_source : t -> P.ident -> P.ident option
val get_symbol_source : t -> P.ident -> P.ident option
