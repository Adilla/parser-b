open SyntaxCore
module V = Visibility
module G = Global

type 'mr t_ident =
  | K_Local of string * Local.t_local_kind
  | K_Global of string option * string * 'mr G.t_kind

type t_op_source =
  | SO_Included_Or_Imported of ren_ident
  | SO_Seen_Read_Only of ren_ident
  | SO_Local of Utils.loc

type called_op = {
  op_prefix:string option;
  op_id:string;
  op_loc:Utils.loc;
  op_src:t_op_source;
}

type arg = {
  arg_loc:Utils.loc;
  arg_id:string;
  arg_typ:Btype.t;
}

type rfield = lident

type bvar = {
  bv_loc:Utils.loc;
  bv_id:string;
  bv_typ:Btype.t;
}

type ('mr,'typ) expression_desc =
  | Ident of 'mr t_ident
  | Dollar of 'mr t_ident
  | Builtin_0 of e_builtin_0
  | Builtin_1 of e_builtin_1*('mr,'typ) expression
  | Builtin_2 of e_builtin_2*('mr,'typ) expression*('mr,'typ) expression
  | Pbool of ('mr,'typ) predicate
  | Sequence of ('mr,'typ) expression Nlist.t
  | Extension of ('mr,'typ) expression Nlist.t
  | Comprehension of bvar Nlist.t * ('mr,'typ) predicate
  | Binder of expr_binder * bvar Nlist.t * ('mr,'typ) predicate * ('mr,'typ) expression
  | Record_Field_Access of ('mr,'typ) expression * rfield
  | Record of (rfield * ('mr,'typ) expression) Nlist.t
  | Record_Type of (rfield * ('mr,'typ) expression) Nlist.t

and ('mr,'typ) expression = {
  exp_loc: Utils.loc;
  exp_typ: 'typ;
  exp_desc: ('mr,'typ) expression_desc
}

and ('mr,'typ) predicate_desc =
  | P_Builtin of p_builtin
  | Binary_Prop of prop_bop * ('mr,'typ) predicate * ('mr,'typ) predicate
  | Binary_Pred of pred_bop * ('mr,'typ) expression * ('mr,'typ) expression
  | Negation of ('mr,'typ) predicate
  | Universal_Q of bvar Nlist.t * ('mr,'typ) predicate
  | Existential_Q of bvar Nlist.t * ('mr,'typ) predicate

and ('mr,'typ) predicate = {
  prd_loc: Utils.loc;
  prd_desc: ('mr,'typ) predicate_desc
}

type 'mr t_mutable_ident =
  | MI_Subst_Binder
  | MI_Out_Param
  | MI_Global of 'mr G.t_kind

type ('mr,'typ) mut_var = {
  mv_loc:Utils.loc;
  mv_prefix:string option;
  mv_id:string;
  mv_typ:'typ;
  mv_kind: ('mr) t_mutable_ident
}

type ('mr,'typ) lhs =
  | Tuple of ('mr,'typ) mut_var Nlist.t
  | Function of ('mr,'typ) mut_var * ('mr,'typ) expression Nlist.t
  | Record of ('mr,'typ) mut_var * rfield

type ('mr,'typ) substitution_desc =
  | Skip
  | Affectation of ('mr,'typ) lhs * ('mr,'typ) expression
  | Pre of ('mr,'typ) predicate * ('mr,'typ) substitution
  | Assert of ('mr,'typ) predicate * ('mr,'typ) substitution
  | Choice of ('mr,'typ) substitution Nlist.t
  | IfThenElse of (('mr,'typ) predicate * ('mr,'typ) substitution) Nlist.t * ('mr,'typ) substitution option
  | Select of (('mr,'typ) predicate * ('mr,'typ) substitution) Nlist.t * ('mr,'typ) substitution option
  | Case of ('mr,'typ) expression * (('mr,'typ) expression Nlist.t * ('mr,'typ) substitution) Nlist.t * ('mr,'typ) substitution option
  | Any of bvar Nlist.t * ('mr,'typ) predicate * ('mr,'typ) substitution
  | Let of bvar Nlist.t * (bvar * ('mr,'typ) expression) Nlist.t * ('mr,'typ) substitution
  | BecomesElt of ('mr,'typ) mut_var Nlist.t * ('mr,'typ) expression
  | BecomesSuch of ('mr,'typ) mut_var Nlist.t * ('mr,'typ) predicate
  | Var of bvar Nlist.t * ('mr,'typ) substitution
  | CallUp of ('mr,'typ) mut_var list * called_op * ('mr,'typ) expression list
  | While of ('mr,'typ) predicate * ('mr,'typ) substitution * ('mr,'typ) predicate * ('mr,'typ) expression
  | Sequencement of ('mr,'typ) substitution * ('mr,'typ) substitution
  | Parallel of ('mr,'typ) substitution * ('mr,'typ) substitution

and ('mr,'typ) substitution = {
  sub_loc: Utils.loc;
  sub_desc: ('mr,'typ) substitution_desc
}

type mch_name = lident

type 'mr operation =
  | O_Specified : { op_out: arg list;
                     op_name: lident;
                     op_in: arg list;
                     op_body: ('mr,Btype.t) substitution
                   } -> 'mr operation
  | O_Promoted : { op_out: (string*Btype.t) list;
                   op_name: lident;
                   op_in: (string*Btype.t) list;
                   op_source:ren_ident
                 } -> 'mr operation
  | O_Local : { op_out: arg list;
                op_name: lident;
                op_in: arg list;
                op_spec: (G.t_ref,Btype.t) substitution;
                op_body: (G.t_ref,Btype.t) substitution
              } -> G.t_ref operation

type ('mr,'ac) symb = {
  sy_id:string;
  sy_typ:Btype.t;
  sy_src:('mr,'ac) G.t_decl;
}

type 'ac t_mch_symb_src = (G.t_mch,'ac) G.t_decl

type 'mr machine_instanciation = {
  mi_mch: ren_ident;
  mi_params: ('mr,Btype.t) expression list
}

type t_param = {
  p_id:string;
  p_loc:Utils.loc;
  p_typ:Btype.t;
}

type machine = {
  mch_set_parameters: lident list;
  mch_scalar_parameters: t_param list;

  mch_sees: ren_ident list;
  mch_uses: ren_ident list;
  mch_includes: G.t_mch machine_instanciation list;
  mch_extends: G.t_mch machine_instanciation list;

  mch_abstract_sets: (G.t_mch,G.t_concrete) symb list;
  mch_concrete_sets: ((G.t_mch,G.t_concrete) symb*string list) list;
  mch_concrete_constants: (G.t_mch,G.t_concrete) symb list;
  mch_abstract_constants: (G.t_mch,G.t_abstract) symb list;
  mch_concrete_variables: (G.t_mch,G.t_concrete) symb list;
  mch_abstract_variables: (G.t_mch,G.t_abstract) symb list;

  mch_constraints: (G.t_mch,Btype.t) predicate option;
  mch_properties: (G.t_mch,Btype.t) predicate option;
  mch_invariant: (G.t_mch,Btype.t) predicate option;
  mch_assertions: (G.t_mch,Btype.t) predicate list;
  mch_initialisation: (G.t_mch,Btype.t) substitution option;
  mch_operations: G.t_mch operation list;
}

type refinement = {
  ref_refines: mch_name;

  ref_set_parameters: lident list;
  ref_scalar_parameters: t_param list;

  ref_sees: ren_ident list;
  ref_includes: G.t_ref machine_instanciation list;
  ref_extends: G.t_ref machine_instanciation list;

  ref_abstract_sets: (G.t_ref,G.t_concrete) symb list;
  ref_concrete_sets: ((G.t_ref,G.t_concrete) symb*string list) list;
  ref_concrete_constants: (G.t_ref,G.t_concrete) symb list;
  ref_abstract_constants: (G.t_ref,G.t_abstract) symb list;
  ref_concrete_variables: (G.t_ref,G.t_concrete) symb list;
  ref_abstract_variables: (G.t_ref,G.t_abstract) symb list;
  
  ref_properties: (G.t_ref,Btype.t) predicate option;
  ref_invariant: (G.t_ref,Btype.t) predicate option;
  ref_assertions: (G.t_ref,Btype.t) predicate list;
  ref_initialisation: (G.t_ref,Btype.t) substitution option;
  ref_operations: G.t_ref operation list;
}

type val_kind_source =
  | VKS_Machine
  | VKS_Implicit
  | VKS_Redeclared

type val_kind =
  | VK_Abstract_Set of val_kind_source
  | VK_Concrete_Constant of val_kind_source

type value =
  { val_loc:Utils.loc;
    val_id:string;
    val_kind:val_kind }

type t_asy_src =
  | I_Imported of ren_ident
  | I_Seen of ren_ident
  | I_Disappearing
  | I_Redeclared_By_Importation of ren_ident
  | I_Redeclared_By_Seen of ren_ident

type t_abs_imp_symb = {
  asy_id:string;
  asy_typ:Btype.t;
  asy_src:t_asy_src
}
type implementation = {
  imp_set_parameters: lident list;
  imp_scalar_parameters: t_param list;

  imp_refines: mch_name;

  imp_sees: ren_ident list;
  imp_imports: G.t_ref machine_instanciation list;
  imp_extends: G.t_ref machine_instanciation list;

  imp_abstract_sets: (G.t_ref,G.t_concrete) symb list;
  imp_concrete_sets: ((G.t_ref,G.t_concrete) symb*string list) list;
  imp_abstract_constants: t_abs_imp_symb list;
  imp_concrete_constants: (G.t_ref,G.t_concrete) symb list;
  imp_abstract_variables: t_abs_imp_symb list;
  imp_concrete_variables: (G.t_ref,G.t_concrete) symb list;

  imp_values: (value*(G.t_ref,Btype.t) expression) list;

  imp_properties: (G.t_ref,Btype.t) predicate option;
  imp_invariant: (G.t_ref,Btype.t) predicate option;
  imp_assertions: (G.t_ref,Btype.t) predicate list;
  imp_initialisation: (G.t_ref,Btype.t) substitution option;
  imp_operations: G.t_ref operation list;
}

type component_desc =
  | Machine of machine
  | Refinement of refinement
  | Implementation of implementation

type component = {
  co_name: mch_name;
  co_desc: component_desc
}
