open SyntaxCore
module V = Visibility
module G = Global

type ('mr,'cl) t_ident =
  | K_Local of string * Local.t_local_kind
  | K_Global of string option*string * ('mr,'cl) V.t_global_ident

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

type ('mr,'cl,'typ) expression_desc =
  | Ident of ('mr,'cl) t_ident
  | Dollar of ('mr,'cl) t_ident
  | Builtin_0 of e_builtin_0
  | Builtin_1 of e_builtin_1*('mr,'cl,'typ) expression
  | Builtin_2 of e_builtin_2*('mr,'cl,'typ) expression*('mr,'cl,'typ) expression
  | Pbool of ('mr,'cl,'typ) predicate
  | Sequence of ('mr,'cl,'typ) expression Nlist.t
  | Extension of ('mr,'cl,'typ) expression Nlist.t
  | Comprehension of bvar Nlist.t * ('mr,'cl,'typ) predicate
  | Binder of expr_binder * bvar Nlist.t * ('mr,'cl,'typ) predicate * ('mr,'cl,'typ) expression
  | Record_Field_Access of ('mr,'cl,'typ) expression * rfield
  | Record of (rfield * ('mr,'cl,'typ) expression) Nlist.t
  | Record_Type of (rfield * ('mr,'cl,'typ) expression) Nlist.t

and ('mr,'cl,'typ) expression = {
  exp_loc: Utils.loc;
  exp_typ: 'typ;
  exp_desc: ('mr,'cl,'typ) expression_desc
}

and ('mr,'cl,'typ) predicate_desc =
  | P_Builtin of p_builtin
  | Binary_Prop of prop_bop * ('mr,'cl,'typ) predicate * ('mr,'cl,'typ) predicate
  | Binary_Pred of pred_bop * ('mr,'cl,'typ) expression * ('mr,'cl,'typ) expression
  | Negation of ('mr,'cl,'typ) predicate
  | Universal_Q of bvar Nlist.t * ('mr,'cl,'typ) predicate
  | Existential_Q of bvar Nlist.t * ('mr,'cl,'typ) predicate

and ('mr,'cl,'typ) predicate = {
  prd_loc: Utils.loc;
  prd_desc: ('mr,'cl,'typ) predicate_desc
}

type ('mr,'cl) t_mutable_ident =
  | MI_Subst_Binder
  | MI_Out_Param
  | MI_Global of ('mr,'cl) V.t_mutable_ident

type ('mr,'cl,'typ) mut_var = {
  mv_loc:Utils.loc;
  mv_prefix:string option;
  mv_id:string;
  mv_typ:'typ;
  mv_kind: ('mr,'cl) t_mutable_ident
}

type ('mr,'cl,'typ) lhs =
  | Tuple of ('mr,'cl,'typ) mut_var Nlist.t
  | Function of ('mr,'cl,'typ) mut_var * ('mr,'cl,'typ) expression Nlist.t
  | Record of ('mr,'cl,'typ) mut_var * rfield

type ('mr,'cl,'typ) substitution_desc =
  | Skip
  | Affectation of ('mr,'cl,'typ) lhs * ('mr,'cl,'typ) expression
  | Pre of ('mr,'cl,'typ) predicate * ('mr,'cl,'typ) substitution
  | Assert of ('mr,'cl V.t_assert,'typ) predicate * ('mr,'cl,'typ) substitution
  | Choice of ('mr,'cl,'typ) substitution Nlist.t
  | IfThenElse of (('mr,'cl,'typ) predicate * ('mr,'cl,'typ) substitution) Nlist.t * ('mr,'cl,'typ) substitution option
  | Select of (('mr,'cl,'typ) predicate * ('mr,'cl,'typ) substitution) Nlist.t * ('mr,'cl,'typ) substitution option
  | Case of ('mr,'cl,'typ) expression * (('mr,'cl,'typ) expression Nlist.t * ('mr,'cl,'typ) substitution) Nlist.t * ('mr,'cl,'typ) substitution option
  | Any of bvar Nlist.t * ('mr,'cl,'typ) predicate * ('mr,'cl,'typ) substitution
  | Let of bvar Nlist.t * (bvar * ('mr,'cl,'typ) expression) Nlist.t * ('mr,'cl,'typ) substitution
  | BecomesElt of ('mr,'cl,'typ) mut_var Nlist.t * ('mr,'cl,'typ) expression
  | BecomesSuch of ('mr,'cl,'typ) mut_var Nlist.t * ('mr,'cl,'typ) predicate
  | Var of bvar Nlist.t * ('mr,'cl,'typ) substitution
  | CallUp of ('mr,'cl,'typ) mut_var list * called_op * ('mr,'cl,'typ) expression list
  | While of ('mr,'cl,'typ) predicate * ('mr,'cl,'typ) substitution * ('mr,'cl V.t_assert,'typ) predicate * ('mr,'cl V.t_assert,'typ) expression
  | Sequencement of ('mr,'cl,'typ) substitution * ('mr,'cl,'typ) substitution
  | Parallel of ('mr,'cl,'typ) substitution * ('mr,'cl,'typ) substitution

and ('mr,'cl,'typ) substitution = {
  sub_loc: Utils.loc;
  sub_desc: ('mr,'cl,'typ) substitution_desc
}

type mch_name = lident

type ('mr,'cl) operation =
  | O_Specified : { op_out: arg list;
                     op_name: lident;
                     op_in: arg list;
                     op_body: ('mr,'cl,Btype.t) substitution
                   } -> ('mr,'cl) operation
  | O_Promoted : { op_out: (string*Btype.t) list;
                   op_name: lident;
                   op_in: (string*Btype.t) list;
                   op_source:ren_ident
                 } -> ('mr,'cl) operation
  | O_Local : { op_out: arg list;
                op_name: lident;
                op_in: arg list;
                op_spec: (G.t_ref,V.t_imp_lop,Btype.t) substitution;
                op_body: (G.t_ref,V.t_imp_op,Btype.t) substitution
              } -> (G.t_ref,V.t_imp_op) operation

type ('mr,'ac) symb = {
  sy_id:string;
  sy_typ:Btype.t;
  sy_src:('mr,'ac) G.t_decl;
}

type 'ac t_mch_symb_src = (G.t_mch,'ac) G.t_decl

type ('mr,'cl) machine_instanciation = {
  mi_mch: ren_ident;
  mi_params: ('mr,'cl,Btype.t) expression list
}

type machine = {
  mch_set_parameters: lident list;
  mch_scalar_parameters: (G.t_mch,G.t_concrete) symb list;

  mch_sees: ren_ident list;
  mch_uses: ren_ident list;
  mch_includes: (G.t_mch,V.t_mch_param) machine_instanciation list;
  mch_extends: (G.t_mch,V.t_mch_param) machine_instanciation list;

  mch_abstract_sets: (G.t_mch,G.t_concrete) symb list;
  mch_concrete_sets: ((G.t_mch,G.t_concrete) symb*string list) list;
  mch_concrete_constants: (G.t_mch,G.t_concrete) symb list;
  mch_abstract_constants: (G.t_mch,G.t_abstract) symb list;
  mch_concrete_variables: (G.t_mch,G.t_concrete) symb list;
  mch_abstract_variables: (G.t_mch,G.t_abstract) symb list;

  mch_constraints: (G.t_mch,V.t_mch_constr,Btype.t) predicate option;
  mch_properties: (G.t_mch,V.t_mch_prop,Btype.t) predicate option;
  mch_invariant: (G.t_mch,V.t_mch_inv,Btype.t) predicate option;
  mch_assertions: (G.t_mch,V.t_mch_inv,Btype.t) predicate list;
  mch_initialisation: (G.t_mch,V.t_mch_op,Btype.t) substitution option;
  mch_operations: (G.t_mch,V.t_mch_op) operation list;
}

type refinement = {
  ref_refines: mch_name;

  ref_set_parameters: lident list;
  ref_scalar_parameters: (G.t_ref,G.t_concrete) symb list;

  ref_sees: ren_ident list;
  ref_includes: (G.t_ref,V.t_ref_param) machine_instanciation list;
  ref_extends: (G.t_ref,V.t_ref_param) machine_instanciation list;

  ref_abstract_sets: (G.t_ref,G.t_concrete) symb list;
  ref_concrete_sets: ((G.t_ref,G.t_concrete) symb*string list) list;
  ref_concrete_constants: (G.t_ref,G.t_concrete) symb list;
  ref_abstract_constants: (G.t_ref,G.t_abstract) symb list;
  ref_concrete_variables: (G.t_ref,G.t_concrete) symb list;
  ref_abstract_variables: (G.t_ref,G.t_abstract) symb list;
  
  ref_properties: (G.t_ref,V.t_ref_prop,Btype.t) predicate option;
  ref_invariant: (G.t_ref,V.t_ref_inv,Btype.t) predicate option;
  ref_assertions: (G.t_ref,V.t_ref_inv,Btype.t) predicate list;
  ref_initialisation: (G.t_ref,V.t_ref_op,Btype.t) substitution option;
  ref_operations: (G.t_ref,V.t_ref_op) operation list;
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

type t_abs_imp_symb = {
  asy_id:string;
  asy_typ:Btype.t;
  asy_src:t_asy_src
}
type implementation = {
  imp_set_parameters: lident list;
  imp_scalar_parameters: (G.t_ref,G.t_concrete) symb list;

  imp_refines: mch_name;

  imp_sees: ren_ident list;
  imp_imports: (G.t_ref,V.t_imp_param) machine_instanciation list;
  imp_extends: (G.t_ref,V.t_imp_param) machine_instanciation list;

  imp_abstract_sets: (G.t_ref,G.t_concrete) symb list;
  imp_concrete_sets: ((G.t_ref,G.t_concrete) symb*string list) list;
  imp_abstract_constants: t_abs_imp_symb list;
  imp_concrete_constants: (G.t_ref,G.t_concrete) symb list;
  imp_abstract_variables: t_abs_imp_symb list;
  imp_concrete_variables: (G.t_ref,G.t_concrete) symb list;

  imp_values: (value*(G.t_ref,V.t_imp_val,Btype.t) expression) list;

  imp_properties: (G.t_ref,V.t_imp_prop,Btype.t) predicate option;
  imp_invariant: (G.t_ref,V.t_imp_inv,Btype.t) predicate option;
  imp_assertions: (G.t_ref,V.t_imp_inv,Btype.t) predicate list;
  imp_initialisation: (G.t_ref,V.t_imp_op,Btype.t) substitution option;
  imp_operations: (G.t_ref,V.t_imp_op) operation list;
}

type component_desc =
  | Machine of machine
  | Refinement of refinement
  | Implementation of implementation

type component = {
  co_name: mch_name;
  co_desc: component_desc
}
