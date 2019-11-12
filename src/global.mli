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

type t_interface

type ('sy_ki,'op_ki) env
val get_symbol : ('sy_ki,_) env -> string -> 'sy_ki t_symbol_infos option
val get_operation : (_,'op_ki) env -> string -> 'op_ki t_operation_infos option
val get_alias : _ env -> Btype.t_alias 

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

  type t_op_decl =
    | O_Machine of loc
    | O_Seen of ren_ident
    | O_Used of ren_ident
    | O_Included of ren_ident
    | O_Included_And_Promoted of ren_ident

  type t = (t_kind,t_op_decl) env

  val create : lident list -> t
  val add_symbol : t -> loc -> string -> Btype.t -> t_kind -> unit


  val add_operation : t -> loc -> string -> (string*Btype.t) list -> (string*Btype.t) list -> is_readonly:bool -> unit
  val promote_operation : t -> loc -> string -> unit

  val load_interface_for_seen_machine : t -> t_interface -> ren_ident -> unit
  val load_interface_for_used_machine : t -> t_interface -> ren_ident -> unit
  val load_interface_for_included_machine : t -> t_interface -> ren_ident -> (loc*Btype.t) list -> unit
  val load_interface_for_extended_machine : t -> t_interface -> ren_ident -> (loc*Btype.t) list -> unit

  val to_interface : t -> t_interface
end

module Ref : sig
  type t_source =
    | Machine of loc
    | Seen of ren_ident
    | Refined
    | Included of ren_ident

  type t_abstract_source =
    | A_Machine of loc
    | A_Seen of ren_ident
    | A_Refined
    | A_Included of ren_ident
    | A_Redeclared_In_Machine of loc
    | A_Redeclared_In_Included of ren_ident

  type t_kind =
    | Parameter of t_param_kind*loc
    | Abstract_Variable of t_abstract_source
    | Abstract_Constant of t_abstract_source
    | Concrete_Variable of t_source
    | Concrete_Constant of t_source
    | Abstract_Set of t_source
    | Concrete_Set of string list * t_source
    | Enumerate of t_source

  type t_op_decl =
    | OD_Current of loc
    | OD_Seen of ren_ident
    | OD_Included of ren_ident
    | OD_Included_And_Promoted of ren_ident*loc
    | OD_Refined of lident
    | OD_Current_And_Refined of loc*lident
    | OD_Included_And_Refined of ren_ident*lident
    | OD_Included_Promoted_And_Refined of ren_ident*loc*lident

  type t = (t_kind,t_op_decl) env

  val create : lident list -> t
  val add_symbol : t -> loc -> string -> Btype.t -> t_kind -> unit

  val add_operation : t -> loc -> string -> (string*Btype.t) list -> (string*Btype.t) list -> is_readonly:bool -> unit
  val promote_operation : t -> loc -> string -> unit

  val load_interface_for_seen_machine : t -> t_interface -> ren_ident -> unit
  val load_interface_for_refined_machine : t -> t_interface -> lident -> unit
  val load_interface_for_included_machine : t -> t_interface -> ren_ident -> (loc*Btype.t) list -> unit
  val load_interface_for_extended_machine : t -> t_interface -> ren_ident -> (loc*Btype.t) list -> unit

  val to_interface : t -> t_interface
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
    | V_Included of ren_ident
    | V_Redeclared_In_Imported of ren_ident

  type t_concrete_const_decl =
    | C_Machine of loc
    | C_Seen of ren_ident
    | C_Refined
    | C_Imported of ren_ident
    | C_Redeclared_In_Seen of ren_ident
    | C_Redeclared_In_Imported of ren_ident

  type t_kind =
    | Parameter of t_param_kind*loc
    | Abstract_Variable of t_abstract_decl
    | Abstract_Constant of t_abstract_decl
    | Concrete_Variable of t_concrete_var_decl
    | Concrete_Constant of t_concrete_const_decl
    | Abstract_Set of t_concrete_const_decl
    | Concrete_Set of string list * t_concrete_const_decl
    | Enumerate of t_concrete_const_decl

  type t_op_decl =
    | OD_Current of loc
    | OD_Seen of ren_ident
    | OD_Imported of ren_ident
    | OD_Imported_And_Promoted of ren_ident*loc
    | OD_Refined of lident
    | OD_Current_And_Refined of loc*lident
    | OD_Imported_And_Refined of ren_ident*lident
    | OD_Imported_Promoted_And_Refined of ren_ident*loc*lident
    | OD_Local_Spec of loc
    | OD_Local_Spec_And_Implem of loc*loc

  type t = (t_kind,t_op_decl) env

  val create : lident list -> t
  val add_symbol : t -> loc -> string -> Btype.t -> t_kind -> unit

  val add_operation : t -> loc -> string -> (string*Btype.t) list -> (string*Btype.t) list -> is_readonly:bool -> unit
  val promote_operation : t -> loc -> string -> unit

  val get_alias : t -> Btype.t_alias 
  val add_alias : t -> string -> Btype.t -> bool

  val load_interface_for_seen_machine : t -> t_interface -> ren_ident -> unit
  val load_interface_for_refined_machine : t -> t_interface -> lident -> unit
  val load_interface_for_imported_machine : t -> t_interface -> ren_ident -> (loc*Btype.t) list -> unit
  val load_interface_for_extended_machine : t -> t_interface -> ren_ident -> (loc*Btype.t) list -> unit

  val to_interface : t -> t_interface

  val check_operation_coherence : t  -> loc -> unit
end

(*

type 'a t_op_decl =
  | OD_Current : loc -> t_mch t_op_decl
  | OD_Seen : ren_ident -> 'a t_op_decl
  | OD_Included_Or_Imported : ren_ident -> 'a t_op_decl
  | OD_Included_Or_Imported_And_Promoted : ren_ident*loc -> t_mch t_op_decl
  | OD_Refined : lident -> t_ref t_op_decl
  | OD_Current_And_Refined : loc*lident -> t_ref t_op_decl
  | OD_Included_Or_Imported_And_Refined : ren_ident*lident -> t_ref t_op_decl
  | OD_Included_Or_Imported_Promoted_And_Refined : ren_ident*loc*lident -> t_ref t_op_decl


*)

(* val add_abstract_sets : Btype.t_atomic_src -> (Btype.t_atomic_src*string) list -> t_interface -> (Btype.t_atomic_src*string) list *)


(*
val fold_symbols: (string -> 'x t_symbol_infos -> 'a -> 'a) -> 'x t -> 'a -> 'a
val fold_operations: (string -> 'x t_operation_infos -> 'a -> 'a) -> 'x t -> 'a -> 'a
*)
