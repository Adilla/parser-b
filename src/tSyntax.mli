open SyntaxCore
module V = Visibility
module G = Global

type ('ki,'typ) t_ident = 
  { id_loc:Utils.loc;
    id_name:string;
    id_type:'typ;
    id_kind:'ki }
  
type 'op_src called_op = {
  op_prefix:string option;
  op_id:string;
  op_loc:Utils.loc;
  op_src:'op_src;
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

type ('ki,'typ) expression_desc =
  | Ident of ('ki,'typ) t_ident
  | Dollar of ('ki,'typ) t_ident
  | Builtin_0 of e_builtin_0
  | Builtin_1 of e_builtin_1*('ki,'typ) expression
  | Builtin_2 of e_builtin_2*('ki,'typ) expression*('ki,'typ) expression
  | Pbool of ('ki,'typ) predicate
  | Sequence of ('ki,'typ) expression Nlist.t
  | Extension of ('ki,'typ) expression Nlist.t
  | Comprehension of bvar Nlist.t * ('ki,'typ) predicate
  | Binder of expr_binder * bvar Nlist.t * ('ki,'typ) predicate * ('ki,'typ) expression
  | Record_Field_Access of ('ki,'typ) expression * rfield
  | Record of (rfield * ('ki,'typ) expression) Nlist.t
  | Record_Type of (rfield * ('ki,'typ) expression) Nlist.t

and ('ki,'typ) expression = {
  exp_loc: Utils.loc;
  exp_typ: 'typ;
  exp_desc: ('ki,'typ) expression_desc
}

and ('ki,'typ) predicate_desc =
  | P_Builtin of p_builtin
  | Binary_Prop of prop_bop * ('ki,'typ) predicate * ('ki,'typ) predicate
  | Binary_Pred of pred_bop * ('ki,'typ) expression * ('ki,'typ) expression
  | Negation of ('ki,'typ) predicate
  | Universal_Q of bvar Nlist.t * ('ki,'typ) predicate
  | Existential_Q of bvar Nlist.t * ('ki,'typ) predicate

and ('ki,'typ) predicate = {
  prd_loc: Utils.loc;
  prd_desc: ('ki,'typ) predicate_desc
}

type ('id_ki,'mut_ki,'typ) lhs =
  | Tuple of ('mut_ki,'typ) t_ident Nlist.t
  | Function of ('mut_ki,'typ) t_ident * ('id_ki,'typ) expression Nlist.t
  | Record of ('mut_ki,'typ) t_ident * rfield

type ('id_ki,'mut_ki,'assert_ki,'op_src,'typ) substitution_desc =
  | Skip
  | Affectation of ('id_ki,'mut_ki,'typ) lhs * ('id_ki,'typ) expression
  | Pre of ('id_ki,'typ) predicate * ('id_ki,'mut_ki,'assert_ki,'op_src,'typ) substitution
  | Assert of ('assert_ki,'typ) predicate * ('id_ki,'mut_ki,'assert_ki,'op_src,'typ) substitution
  | Choice of ('id_ki,'mut_ki,'assert_ki,'op_src,'typ) substitution Nlist.t
  | IfThenElse of (('id_ki,'typ) predicate * ('id_ki,'mut_ki,'assert_ki,'op_src,'typ) substitution) Nlist.t * ('id_ki,'mut_ki,'assert_ki,'op_src,'typ) substitution option
  | Select of (('id_ki,'typ) predicate * ('id_ki,'mut_ki,'assert_ki,'op_src,'typ) substitution) Nlist.t * ('id_ki,'mut_ki,'assert_ki,'op_src,'typ) substitution option
  | Case of ('id_ki,'typ) expression * (('id_ki,'typ) expression Nlist.t * ('id_ki,'mut_ki,'assert_ki,'op_src,'typ) substitution) Nlist.t * ('id_ki,'mut_ki,'assert_ki,'op_src,'typ) substitution option
  | Any of bvar Nlist.t * ('id_ki,'typ) predicate * ('id_ki,'mut_ki,'assert_ki,'op_src,'typ) substitution
  | Let of bvar Nlist.t * (bvar * ('id_ki,'typ) expression) Nlist.t * ('id_ki,'mut_ki,'assert_ki,'op_src,'typ) substitution
  | BecomesElt of ('mut_ki,'typ) t_ident Nlist.t * ('id_ki,'typ) expression
  | BecomesSuch of ('mut_ki,'typ) t_ident Nlist.t * ('id_ki,'typ) predicate
  | Var of bvar Nlist.t * ('id_ki,'mut_ki,'assert_ki,'op_src,'typ) substitution
  | CallUp of ('mut_ki,'typ) t_ident list * 'op_src called_op * ('id_ki,'typ) expression list
  | While of ('id_ki,'typ) predicate * ('id_ki,'mut_ki,'assert_ki,'op_src,'typ) substitution * ('assert_ki,'typ) predicate * ('assert_ki,'typ) expression
  | Sequencement of ('id_ki,'mut_ki,'assert_ki,'op_src,'typ) substitution * ('id_ki,'mut_ki,'assert_ki,'op_src,'typ) substitution
  | Parallel of ('id_ki,'mut_ki,'assert_ki,'op_src,'typ) substitution * ('id_ki,'mut_ki,'assert_ki,'op_src,'typ) substitution

and ('id_ki,'mut_ki,'assert_ki,'op_src,'typ) substitution = {
  sub_loc: Utils.loc;
  sub_desc: ('id_ki,'mut_ki,'assert_ki,'op_src,'typ) substitution_desc
}

type mch_name = lident

type 'src symb = {
  sy_id:string;
  sy_typ:Btype.t;
  sy_src:'src
}

type ('id_ki,'mut_ki,'assert_ki,'op_src) operation =
  { op_out: arg list;
    op_name: lident;
    op_in: arg list;
    op_body: ('id_ki,'mut_ki,'assert_ki,'op_src,Btype.t) substitution
  }

type promoted =
  { pop_out: (string*Btype.t) list;
    pop_name: lident;
    pop_in: (string*Btype.t) list;
    pop_source:ren_ident
  }
  
type 'id_ki machine_instanciation = {
  mi_mch: ren_ident;
  mi_params: ('id_ki,Btype.t) expression list
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
  mch_includes: V.Mch.Includes.t machine_instanciation list;
  mch_extends: V.Mch.Includes.t machine_instanciation list;

  mch_abstract_sets: G.Mch.t_source symb list;
  mch_concrete_sets: (G.Mch.t_source symb*string list) list;
  mch_concrete_constants: G.Mch.t_source symb list;
  mch_abstract_constants: G.Mch.t_source symb list;
  mch_concrete_variables: G.Mch.t_source symb list;
  mch_abstract_variables: G.Mch.t_source symb list;

  mch_constraints: (V.Mch.Constraints.t,Btype.t) predicate option;
  mch_properties: (V.Mch.Properties.t,Btype.t) predicate option;
  mch_invariant: (V.Mch.Invariant.t,Btype.t) predicate option;
  mch_assertions: (V.Mch.Invariant.t,Btype.t) predicate list;
  mch_initialisation: (V.Mch.Operations.t,V.Mch.Operations.t_mut,
                       V.Mch.Assert.t,V.Mch.Operations.t_op,Btype.t) substitution option;
  mch_promoted: promoted list;
  mch_operations: (V.Mch.Operations.t,V.Mch.Operations.t_mut,
                   V.Mch.Assert.t,V.Mch.Operations.t_op) operation list;
}

type refinement = {
  ref_refines: mch_name;

  ref_set_parameters: lident list;
  ref_scalar_parameters: t_param list;

  ref_sees: ren_ident list;
  ref_includes: V.Ref.Includes.t machine_instanciation list;
  ref_extends: V.Ref.Includes.t machine_instanciation list;

  ref_abstract_sets: G.Ref.t_source symb list;
  ref_concrete_sets: (G.Ref.t_source symb*string list) list;
  ref_concrete_constants: G.Ref.t_source_2 symb list;
  ref_abstract_constants: G.Ref.t_source_2 symb list;
  ref_concrete_variables: G.Ref.t_source_2 symb list;
  ref_abstract_variables: G.Ref.t_source_2 symb list;
  
  ref_properties: (V.Ref.Properties.t,Btype.t) predicate option;
  ref_invariant: (V.Ref.Invariant.t,Btype.t) predicate option;
  ref_assertions: (V.Ref.Invariant.t,Btype.t) predicate list;
  ref_initialisation: (V.Ref.Operations.t,V.Ref.Operations.t_mut,V.Ref.Assert.t,
                       V.Ref.Operations.t_op,Btype.t) substitution option;
  ref_promoted: promoted list;
  ref_operations: (V.Ref.Operations.t,V.Ref.Operations.t_mut,V.Ref.Assert.t,
                   V.Ref.Operations.t_op) operation list;
}

(*
type val_kind_source =
  | VKS_Machine
  | VKS_Implicit
  | VKS_Redeclared
*)

type val_kind =
  | VK_Abstract_Set (*of val_kind_source*)
  | VK_Concrete_Constant (*of val_kind_source*)

type value =
  { val_loc:Utils.loc;
    val_id:string;
    val_kind:val_kind }

(*
type t_asy_src =
  | I_Imported of ren_ident
  | I_Seen of ren_ident
  | I_Disappearing
  | I_Redeclared_By_Importation of ren_ident
  | I_Redeclared_By_Seen of ren_ident
*)

type t_local_operation =
  { lop_out: arg list;
    lop_name: lident;
    lop_in: arg list;
    lop_spec: (V.Imp.Local_Operations.t,V.Imp.Local_Operations.t_mut,
              V.Imp.Assert.t,V.Imp.Local_Operations.t_op,Btype.t) substitution;
    lop_body: (V.Imp.Operations.t,V.Imp.Operations.t_mut,V.Imp.Assert.t,
              V.Imp.Operations.t_op,Btype.t) substitution
  }

type implementation = {
  imp_set_parameters: lident list;
  imp_scalar_parameters: t_param list;

  imp_refines: mch_name;

  imp_sees: ren_ident list;
  imp_imports: V.Imp.Imports.t machine_instanciation list;
  imp_extends: V.Imp.Imports.t machine_instanciation list;

  imp_abstract_sets: G.Imp.t_abstract_set_decl symb list;
  imp_concrete_sets: (G.Imp.t_concrete_data_decl symb*string list) list;
  imp_abstract_constants: G.Imp.t_abstract_decl symb list;
  imp_concrete_constants: G.Imp.t_concrete_const_decl symb list;
  imp_abstract_variables: G.Imp.t_abstract_decl symb list;
  imp_concrete_variables: G.Imp.t_concrete_var_decl symb list;

  imp_values: (value*(V.Imp.Values.t,Btype.t) expression) list;

  imp_properties: (V.Imp.Properties.t,Btype.t) predicate option;
  imp_invariant: (V.Imp.Invariant.t,Btype.t) predicate option;
  imp_assertions: (V.Imp.Invariant.t,Btype.t) predicate list;
  imp_initialisation: (V.Imp.Operations.t,V.Imp.Operations.t_mut,
                       V.Imp.Assert.t,V.Imp.Operations.t_op,Btype.t) substitution option;
  imp_promoted: promoted list;
  imp_local_operations: t_local_operation list;
  imp_operations: (V.Imp.Operations.t,V.Imp.Operations.t_mut,V.Imp.Assert.t,
                   V.Imp.Operations.t_op) operation list;
}

type component_desc =
  | Machine of machine
  | Refinement of refinement
  | Implementation of implementation

type component = {
  co_name: mch_name;
  co_desc: component_desc
}
