type t_pkg_id
type t_id
type ident

val make_id: SyntaxCore.lident -> t_id (*FIXME*)

val ident_to_string : ident -> string
val pkg_to_lident : t_pkg_id -> SyntaxCore.lident
val id_to_lident : t_id -> SyntaxCore.lident

type t_id_2 =
  | Intern of t_id
  | Extern of t_pkg_id*ident

type qident = { q_nspace:t_pkg_id option; q_id:t_id }
type t_type_id = { t_nspace:ident option; t_id:ident }

type t_b0_constant =
  | B0_Integer of Int64.t
  | B0_String of string
  | B0_MaxInt
  | B0_MinInt
  | B0_True
  | B0_False

type t_b0_unary_op =
  | B0_Negation
  | B0_Minus

type t_b0_binary_op =
  | B0_Conjonction
  | B0_Disjonction
  | B0_Equality
  | B0_Disequality
  | B0_Inequality of SyntaxCore.inequality
  | B0_Product
  | B0_Difference
  | B0_Addition
  | B0_Division
  | B0_Modulo
  | B0_Power


type t_array_index =
  | I_Int
  | I_Bool
  | I_Enum of t_type_id

type t_b0_type =
  | T_Int
  | T_Bool
  | T_String
  | T_Abstract of t_type_id
  | T_Enum of t_type_id
  | T_Array of t_array_index*t_b0_type
  | T_Record of (ident*t_b0_type) list

type t_ident_kind =
  | IK_Constant of t_pkg_id option
  | IK_Variable of t_pkg_id option
  | IK_Enum of t_pkg_id option

type t_exp0_desc =
  | B0_Local_Ident of t_id*Local.t_local_kind
  | B0_Global_Ident of t_id*t_ident_kind
  | B0_Builtin_0 of t_b0_constant
  | B0_Builtin_1 of t_b0_unary_op * t_b0_expr
  | B0_Builtin_2 of t_b0_binary_op * t_b0_expr * t_b0_expr
  | B0_Array_Access of t_b0_expr * t_b0_expr Nlist.t
  | B0_Array of t_b0_expr list
  | B0_Array_Init of t_b0_range Nlist.t*t_b0_expr
  | B0_Record of (t_id*t_b0_expr) list
  | B0_Record_Access of t_b0_expr*t_id

and t_b0_range =
  | R_Interval of t_b0_expr*t_b0_expr
  | R_Concrete_Set of qident

and t_b0_expr =
  { exp0_loc: Utils.loc;
    exp0_type: t_b0_type;
    exp0_desc: t_exp0_desc }

type t_constant_init =
  | Init of t_b0_expr
  | Promoted of t_pkg_id

type t_constant =
  { c_name: t_id_2;
    c_type: t_b0_type;
    c_init: t_constant_init }

type t_arg =
  { arg_name: t_id_2;
    arg_type: t_b0_type }

type t_fun =
  { f_name: t_id;
    f_args: t_arg Nlist.t;
    f_ret: t_b0_expr }

type t_variable =
  { v_name: t_id_2;
    v_type: t_b0_type;
    v_promoted_from: t_pkg_id option;
  }

type t_mut_ident_kind =
  | MIK_Variable
  | MIK_Param
  | MIK_Local

type t_case =
  | CS_Int of Int64.t
  | CS_Bool of bool
  | CS_Enum of qident
  | CS_Constant of qident

type t_sub0_desc =
  | B0_Affectation of t_b0_lhs*t_b0_expr
  | B0_IfThenElse of (t_b0_expr*t_b0_subst) Nlist.t * t_b0_subst option
  | B0_Case of t_b0_expr * (t_case Nlist.t*t_b0_subst) Nlist.t * t_b0_subst option
  | B0_Var of (t_id*t_b0_type) Nlist.t * t_b0_subst
  | B0_While of t_b0_expr * t_b0_subst
  | B0_CallUp of (t_mut_ident_kind*t_id) list * qident * t_b0_expr list

and t_b0_lhs =
  | LHS_Variable of t_id*t_mut_ident_kind
  | LHS_Array of t_id*t_mut_ident_kind*t_b0_expr Nlist.t
  | LHS_Record of t_id*t_mut_ident_kind*t_id

and t_b0_subst_atomic =
  { sub0_loc: Utils.loc;
    sub0_desc: t_sub0_desc }

and t_b0_subst = t_b0_subst_atomic list

type t_procedure_body =
  | Renames of qident
  | Body of t_b0_subst

type t_procedure =
  { p_name: t_id;
    p_is_local: bool;
    p_args_in: t_arg list;
    p_args_out: t_arg list;
    p_body: t_procedure_body }

type t_type_def =
  | D_Int
  | D_Alias of t_pkg_id*ident
  | D_Enum of ident list

type t_type =
  { ty_name: t_id_2;
    ty_def: t_type_def }

type t_dep_kind = DK_Sees | DK_Imports | DK_Extends

type t_package =
  { pkg_name: t_pkg_id;
    pkg_dependencies: (t_pkg_id*t_dep_kind) list;
    pkg_types: t_type list;
    pkg_constants: t_constant list;
    pkg_variables: t_variable list;
    pkg_procedures: t_procedure list;
    pkg_init: t_b0_subst option }

val to_package : SyntaxCore.lident -> TSyntax.component -> t_package Error.t_result
