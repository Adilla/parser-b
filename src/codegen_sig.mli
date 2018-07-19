module type S =
sig
  type t_id
  type t_pkg_id

  type t_b0_constant =
    | B0_Integer of Int32.t
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
    | B0_Inequality of Syntax.inequality
    | B0_Product
    | B0_Difference
    | B0_Addition
    | B0_Division
    | B0_Modulo
    | B0_Power

  type qident = { q_nspace:t_pkg_id option; q_id:t_id }

  type t_array_element_type =
    | A_Int
    | A_Bool
    | A_String

  type t_b0_type =
    | T_Int
    | T_Bool
    | T_String
    | T_Abstract of qident
    | T_Array of int*t_array_element_type

  type t_exp0_desc =
    | B0_Ident of qident
    | B0_Builtin_0 of t_b0_constant
    | B0_Builtin_1 of t_b0_unary_op * t_b0_expr
    | B0_Builtin_2 of t_b0_binary_op * t_b0_expr * t_b0_expr
    | B0_Application of qident * t_b0_expr Nlist.t
    | B0_Array_Access of qident * t_b0_expr
    | B0_Array of t_b0_expr list

  and t_b0_expr =
    { exp0_loc: Utils.loc;
      exp0_type: t_b0_type;
      exp0_desc: t_exp0_desc }

  type t_constant_type =
    | Cst of t_b0_type
    | Fun of t_b0_type Nlist.t*t_b0_type

  type t_constant =
    { c_name: t_id;
      c_type: t_constant_type;
      c_init: t_b0_expr }

  type t_variable =
    { v_name: t_id;
      v_type: t_b0_type }

  type t_arg =
    { arg_loc: Utils.loc;
      arg_name: t_id;
      arg_type: t_b0_type }

  type t_sub0_desc =
    | B0_Null
    | B0_Affectation of (t_id*t_b0_expr) Nlist.t
    | B0_Array_Affectation of t_id*t_b0_expr Nlist.t*t_b0_expr
    | B0_IfThenElse of (t_b0_expr*t_b0_subst) Nlist.t * t_b0_subst option
    | B0_Case of t_b0_expr * (Int32.t Nlist.t*t_b0_subst) Nlist.t * t_b0_subst option
    | B0_Var of (t_id*t_b0_type) Nlist.t * t_b0_subst
    | B0_While of t_b0_expr * t_b0_subst
    | B0_CallUp of t_id list * qident * t_b0_expr list
    | B0_Sequencement of t_b0_subst * t_b0_subst

  and t_b0_subst =
    { sub0_loc: Utils.loc;
      sub0_desc: t_sub0_desc }

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
    | D_Alias of qident
    | D_Enumerate of t_id list

  type t_type =
    { ty_name: t_id;
      ty_def: t_type_def }

  type t_package =
    { pkg_name: t_pkg_id;
      pkg_dependencies: t_pkg_id list;
      pkg_types: t_type list;
      pkg_constants: t_constant list;
      pkg_variables: t_variable list;
      pkg_procedures: t_procedure list;
      pkg_init: t_b0_subst option }

  val to_package : Global.t -> t_pkg_id -> Typechecker.t_component -> t_package Error.t_result
end
