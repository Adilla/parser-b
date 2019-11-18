module G = Global
val extended_sees: bool ref

module Mch : sig
  module Constraints : sig
    type t =
      | Expr_Binder
      | Parameter of Global.t_param_kind*Utils.loc 
  end

  module Includes : sig
    type t_source = Machine of Utils.loc | Seen of SyntaxCore.ren_ident
    type t =
      | Expr_Binder
      | Parameter of G.t_param_kind*Utils.loc 
      | Concrete_Constant of  t_source
      | Concrete_Set of string list * t_source
      | Abstract_Set of t_source
      | Enumerate of t_source
  end

  module Assert : sig
    type t =
      | Global of G.Mch.t_kind
      | Local of Local.t_local_kind
  end

  module Properties : sig
    type t =
      | Expr_Binder
      | Concrete_Constant of G.Mch.t_source
      | Abstract_Set of G.Mch.t_source
      | Enumerate of G.Mch.t_source
      | Concrete_Set of string list*G.Mch.t_source
      | Abstract_Constant of G.Mch.t_source
  end

  module Invariant : sig
    type t =
      | Global of G.Mch.t_kind
      | Expr_Binder
  end

  module Operations : sig
    type t =
      | Global of G.Mch.t_kind
      | Local of Local.t_local_kind

    type t_mut =
      | Param_Out
      | Subst_Binder
      | Abstract_Variable of Utils.loc
      | Concrete_Variable of Utils.loc

    type t_op =
      | O_Seen of SyntaxCore.ren_ident
      | O_Used of SyntaxCore.ren_ident
      | O_Included of SyntaxCore.ren_ident
      | O_Included_And_Promoted of SyntaxCore.ren_ident
  end
end

module Ref : sig

  module Includes : sig
    type t_source = Machine of Utils.loc | Refined | Seen of SyntaxCore.ren_ident
    type t =
      | Expr_Binder
      | Parameter of G.t_param_kind*Utils.loc 
      | Concrete_Constant of  t_source
      | Concrete_Set of string list * t_source
      | Abstract_Set of t_source
      | Enumerate of t_source
  end

  module Assert : sig
    type t =
      | Global of G.Ref.t_kind
      | Local of Local.t_local_kind
  end

  module Properties : sig
    type t =
      | Expr_Binder
      | Concrete_Constant of G.Ref.t_source_2
      | Abstract_Set of G.Ref.t_source
      | Enumerate of G.Ref.t_source
      | Concrete_Set of string list*G.Ref.t_source
      | Abstract_Constant of G.Ref.t_source_2
    end

  module Invariant : sig
    type t =
      | Global of G.Ref.t_kind
      | Expr_Binder
  end

  module Operations : sig
    type t =
      | Global of G.Ref.t_kind (*FIXME*)
      | Local of Local.t_local_kind
  
    type t_mut =
      | Param_Out
      | Subst_Binder
      | Abstract_Variable of Utils.loc
      | Concrete_Variable of Utils.loc
    
    type t_op =
      | O_Seen of SyntaxCore.ren_ident
      | O_Included of SyntaxCore.ren_ident
      | O_Refined_And_Included of SyntaxCore.ren_ident
      | O_Included_And_Promoted of SyntaxCore.ren_ident
      | O_Refined_Included_And_Promoted of SyntaxCore.ren_ident
  end
end

module Imp : sig

  module Imports : sig
    
    type t_source = Machine of Utils.loc | Refined | Seen of SyntaxCore.ren_ident
    type t =
      | Expr_Binder
      | Parameter of G.t_param_kind*Utils.loc 
      | Concrete_Constant of  t_source
      | Concrete_Set of string list * t_source
      | Abstract_Set of t_source
      | Enumerate of t_source

  end

  module Assert : sig
    type t =
      | Global of G.Imp.t_kind
      | Local of Local.t_local_kind
  end

  module Properties : sig
    type t =
      | Expr_Binder
      | Concrete_Constant of G.Imp.t_concrete_const_decl
      | Abstract_Set of G.Imp.t_concrete_const_decl
      | Enumerate of G.Imp.t_concrete_const_decl
      | Concrete_Set of string list*G.Imp.t_concrete_const_decl
      | Abstract_Constant of G.Imp.t_abstract_decl
  end

  module Invariant : sig
    type t =
      | Global of G.Imp.t_kind
      | Expr_Binder
  end

  module Operations : sig
    type t =
      | Global of G.Imp.t_kind (*FIXME*)
      | Local of Local.t_local_kind

    type t_source =
      | Machine of Utils.loc
      | Refined
      | Redeclared_In_Machine of Utils.loc

    type t_mut =
      | Param_Out
      | Subst_Binder
      | Concrete_Variable of t_source
    
    type t_op =
      | O_Seen of SyntaxCore.ren_ident
      | O_Imported of SyntaxCore.ren_ident
      | O_Refined_And_Imported of SyntaxCore.ren_ident
      | O_Included_And_Promoted of SyntaxCore.ren_ident
      | O_Refined_Imported_And_Promoted of SyntaxCore.ren_ident
      | O_Local of Utils.loc
   
  end

  module Local_Operations : sig
    type t =
      | Global of G.Imp.t_kind (*FIXME*)
      | Local of Local.t_local_kind

    type t_source_a =
      | A_Imported of SyntaxCore.ren_ident
      | A_Redeclared_In_Imported of SyntaxCore.ren_ident

    type t_source_c =
      | C_Imported of SyntaxCore.ren_ident
      | C_Redeclared_In_Imported of SyntaxCore.ren_ident
      | C_Machine of Utils.loc
      | C_Refined
      | C_Redeclared_In_Machine of Utils.loc

    type t_mut =
      | Param_Out
      | Subst_Binder
      | Abstract_Variable of t_source_a
      | Concrete_Variable of t_source_c

    type t_op =
      | O_Seen of SyntaxCore.ren_ident
      | O_Imported of SyntaxCore.ren_ident
      | O_Refined_And_Imported of SyntaxCore.ren_ident
      | O_Included_And_Promoted of SyntaxCore.ren_ident
      | O_Refined_Imported_And_Promoted of SyntaxCore.ren_ident
      | O_Local of Utils.loc

  end

  module Values : sig
    type t =
      | Expr_Binder
      | Concrete_Constant of G.Imp.t_concrete_const_decl
      | Abstract_Set of G.Imp.t_concrete_const_decl
      | Enumerate of G.Imp.t_concrete_const_decl
      | Concrete_Set of string list*G.Imp.t_concrete_const_decl
  end
end

type (_,_) clause =
  | M_Constraints : (Global.Mch.t_kind,Mch.Constraints.t) clause
  | M_Includes : (Global.Mch.t_kind,Mch.Includes.t) clause
  | M_Assert : (Global.Mch.t_kind,Mch.Assert.t) clause
  | M_Properties : (Global.Mch.t_kind,Mch.Properties.t) clause
  | M_Invariant : (Global.Mch.t_kind,Mch.Invariant.t) clause
  | M_Operations : (Global.Mch.t_kind,Mch.Operations.t) clause
  | R_Includes : (Global.Ref.t_kind,Ref.Includes.t) clause
  | R_Assert : (Global.Ref.t_kind,Ref.Assert.t) clause
  | R_Properties : (Global.Ref.t_kind,Ref.Properties.t) clause
  | R_Invariant : (Global.Ref.t_kind,Ref.Invariant.t) clause
  | R_Operations : (Global.Ref.t_kind,Ref.Operations.t) clause
  | I_Imports : (Global.Imp.t_kind,Imp.Imports.t) clause
  | I_Assert : (Global.Imp.t_kind,Imp.Assert.t) clause
  | I_Properties : (Global.Imp.t_kind,Imp.Properties.t) clause
  | I_Invariant : (Global.Imp.t_kind,Imp.Invariant.t) clause
  | I_Operations : (Global.Imp.t_kind,Imp.Operations.t) clause
  | I_Local_Operations : (Global.Imp.t_kind,Imp.Local_Operations.t) clause
  | I_Values : (Global.Imp.t_kind,Imp.Values.t) clause

val mk_global : ('env_ki,'id_ki) clause -> 'env_ki -> 'id_ki option
val mk_local : (_,'id_ki) clause -> Local.t_local_kind -> 'id_ki option

type ('env_ki,'id_ki,'mut_ki,'assert_ki,'env_op_ki,'op_ki) sclause =
  | MS_Operations : (Global.Mch.t_kind,
                   Mch.Operations.t,
                   Mch.Operations.t_mut,
                   Mch.Assert.t,
                   Global.Mch.t_op_source,
                   Mch.Operations.t_op) sclause
  | RS_Operations : (Global.Ref.t_kind,
                   Ref.Operations.t,
                   Ref.Operations.t_mut,
                   Ref.Assert.t,
                   Global.Ref.t_op_source,
                   Ref.Operations.t_op) sclause
  | IS_Operations : (Global.Imp.t_kind,
                   Imp.Operations.t,
                   Imp.Operations.t_mut,
                   Imp.Assert.t,
                   Global.Imp.t_op_source,
                   Imp.Operations.t_op) sclause
  | IS_Local_Operations : (Global.Imp.t_kind,
                   Imp.Local_Operations.t,
                   Imp.Local_Operations.t_mut,
                   Imp.Assert.t,
                   Global.Imp.t_op_source,
                   Imp.Local_Operations.t_op) sclause

val mk_global_mut : ('env_ki,_,'mut_ki,_,_,_) sclause -> 'env_ki -> 'mut_ki option
val mk_local_mut : (_,_,'mut_ki,_,_,_) sclause -> Local.t_local_kind -> 'mut_ki option
val mk_op : (_,_,_,_,'env_op_ki,'op_ki) sclause -> 'env_op_ki -> 'op_ki option

val to_clause : ('env_ki,'id_ki,_,_,_,_) sclause -> ('env_ki,'id_ki) clause
val to_assert : ('env_ki,_,_,'assert_ki,_,_) sclause -> ('env_ki,'assert_ki) clause
