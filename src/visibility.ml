module G = Global

let extended_sees = ref false

module Mch = struct
  module Constraints = struct
    type t =
      | Expr_Binder
      | Parameter of Global.t_param_kind*Utils.loc 

    let mk_global = function
      | G.Mch.Parameter (k,l) -> Some (Parameter (k,l))
      | _ -> None

    let mk_local = function
      | Local.L_Expr_Binder -> Some Expr_Binder
      | _ -> None
  end

  module Includes = struct
    type t_source = Machine of Utils.loc | Seen of SyntaxCore.ren_ident
    type t =
      | Expr_Binder
      | Parameter of G.t_param_kind*Utils.loc 
      | Concrete_Constant of  t_source
      | Concrete_Set of string list * t_source
      | Abstract_Set of t_source
      | Enumerate of t_source

    let mk_global = function
      | G.Mch.Parameter (k,l) -> Some (Parameter (k,l))
      | G.Mch.Concrete_Constant (Global.Mch.Machine l) -> Some (Concrete_Constant (Machine l))
      | G.Mch.Concrete_Constant (Global.Mch.Seen mch) -> Some (Concrete_Constant (Seen mch))
      | G.Mch.Abstract_Set (Global.Mch.Machine l) -> Some (Abstract_Set (Machine l))
      | G.Mch.Abstract_Set (Global.Mch.Seen mch) -> Some (Abstract_Set (Seen mch))
      | G.Mch.Concrete_Set (elts,Global.Mch.Machine l) -> Some (Concrete_Set (elts,Machine l))
      | G.Mch.Concrete_Set (elts,Global.Mch.Seen mch) -> Some (Concrete_Set (elts,Seen mch))
      | G.Mch.Enumerate (Global.Mch.Machine l) -> Some (Enumerate (Machine l))
      | G.Mch.Enumerate (Global.Mch.Seen mch) -> Some (Enumerate (Seen mch))
      | _ -> None

    let mk_local = function
      | Local.L_Expr_Binder -> Some Expr_Binder
      | _ -> None

  end

  module Assert = struct
    type t =
      | Global of G.Mch.t_kind
      | Local of Local.t_local_kind

    let mk_global x = Some (Global x)

    let mk_local x = Some (Local x)
  end

  module Properties = struct
    type t =
      | Expr_Binder
      | Concrete_Constant of G.Mch.t_source
      | Abstract_Set of G.Mch.t_source
      | Enumerate of G.Mch.t_source
      | Concrete_Set of string list*G.Mch.t_source
      | Abstract_Constant of G.Mch.t_source

    let mk_global = function
      | G.Mch.Abstract_Variable _ -> None
      | G.Mch.Concrete_Variable _ -> None
      | G.Mch.Parameter _ -> None
      | G.Mch.Concrete_Constant src -> Some (Concrete_Constant src)
      | G.Mch.Abstract_Set src -> Some (Abstract_Set src)
      | G.Mch.Enumerate src -> Some (Enumerate src)
      | G.Mch.Concrete_Set (elts,src) -> Some (Concrete_Set (elts,src))
      | G.Mch.Abstract_Constant src -> Some (Abstract_Constant src)

    let mk_local = function
      | Local.L_Expr_Binder -> Some Expr_Binder
      | _ -> None
  end

  module Invariant = struct
    type t =
      | Global of G.Mch.t_kind
      | Expr_Binder

    let mk_global x =
      if !extended_sees then Some (Global x)
      else match x with
      | G.Mch.Abstract_Variable (G.Mch.Seen _) -> None
      | G.Mch.Concrete_Variable (G.Mch.Seen _) -> None
      | _ -> Some (Global x)

    let mk_local = function
      | Local.L_Expr_Binder -> Some Expr_Binder
      | _ -> None
  end

  module Operations = struct
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

    let mk_global x = Some (Global x)

    let mk_global_mut = function
      | G.Mch.Abstract_Variable (G.Mch.Machine l) -> Some (Abstract_Variable (l))
      | G.Mch.Concrete_Variable (G.Mch.Machine l) -> Some (Concrete_Variable (l))
      | _ -> None

    let mk_op = function
      | G.Mch.O_Machine _ -> None
      | G.Mch.O_Seen mch -> Some (O_Seen mch) (*FIXME*)
      | G.Mch.O_Used mch -> Some (O_Used mch)
      | G.Mch.O_Included mch -> Some (O_Included mch)
      | G.Mch.O_Included_And_Promoted mch -> Some (O_Included_And_Promoted mch)

    let mk_local x = Some (Local x)

    let mk_local_mut = function
      | Local.L_Expr_Binder -> None
      | Local.L_Subst_Binder -> Some Subst_Binder
      | Local.L_Param_In -> None
      | Local.L_Param_Out -> Some Param_Out

  end

end
 
module Ref = struct

  module Includes = struct
    type t_source = Machine of Utils.loc | Refined | Seen of SyntaxCore.ren_ident
    type t =
      | Expr_Binder
      | Parameter of G.t_param_kind*Utils.loc 
      | Concrete_Constant of  t_source
      | Concrete_Set of string list * t_source
      | Abstract_Set of t_source
      | Enumerate of t_source

    let mk_global = function 
      | G.Ref.Parameter (k,l) -> Some (Parameter (k,l))
      | G.Ref.Concrete_Constant (Global.Ref.A_Machine l) -> Some (Concrete_Constant (Machine l))
      | G.Ref.Concrete_Constant (Global.Ref.A_Seen mch) -> Some (Concrete_Constant (Seen mch))
      | G.Ref.Concrete_Constant (Global.Ref.A_Refined) -> Some (Concrete_Constant (Refined))
      | G.Ref.Concrete_Constant (Global.Ref.A_Redeclared_In_Machine l) -> Some (Concrete_Constant (Machine l))
      | G.Ref.Abstract_Set (Global.Ref.Machine l) -> Some (Abstract_Set (Machine l))
      | G.Ref.Abstract_Set (Global.Ref.Seen mch) -> Some (Abstract_Set (Seen mch))
      | G.Ref.Abstract_Set (Global.Ref.Refined) -> Some (Abstract_Set (Refined))
      | G.Ref.Concrete_Set (elts,Global.Ref.Machine l) -> Some (Concrete_Set (elts,Machine l))
      | G.Ref.Concrete_Set (elts,Global.Ref.Seen mch) -> Some (Concrete_Set (elts,Seen mch))
      | G.Ref.Concrete_Set (elts,Global.Ref.Refined) -> Some (Concrete_Set (elts,Refined))
      | G.Ref.Enumerate (Global.Ref.Machine l) -> Some (Enumerate (Machine l))
      | G.Ref.Enumerate (Global.Ref.Seen mch) -> Some (Enumerate (Seen mch))
      | G.Ref.Enumerate (Global.Ref.Refined) -> Some (Enumerate (Refined))
      | _ -> None

    let mk_local = function 
      | Local.L_Expr_Binder -> Some Expr_Binder
      | _ -> None
  end

  module Assert = struct
    type t =
      | Global of G.Ref.t_kind
      | Local of Local.t_local_kind

    let mk_global x = Some (Global x)

    let mk_local x = Some (Local x)
  end

  module Properties = struct
    type t =
      | Expr_Binder
      | Concrete_Constant of G.Ref.t_source_2
      | Abstract_Set of G.Ref.t_source
      | Enumerate of G.Ref.t_source
      | Concrete_Set of string list*G.Ref.t_source
      | Abstract_Constant of G.Ref.t_source_2

    let mk_global = function
      | G.Ref.Abstract_Variable _ -> None
      | G.Ref.Concrete_Variable _ -> None
      | G.Ref.Parameter _ -> None
      | G.Ref.Concrete_Constant src -> Some (Concrete_Constant src)
      | G.Ref.Abstract_Set src -> Some (Abstract_Set src)
      | G.Ref.Enumerate src -> Some (Enumerate src)
      | G.Ref.Concrete_Set (elts,src) -> Some (Concrete_Set (elts,src))
      | G.Ref.Abstract_Constant src -> Some (Abstract_Constant src)

    let mk_local = function 
      | Local.L_Expr_Binder -> Some Expr_Binder
      | _ -> None
  end

  module Invariant = struct
    type t =
      | Global of G.Ref.t_kind
      | Expr_Binder

    let mk_global x =
      if !extended_sees then Some (Global x)
      else match x with
      | G.Ref.Abstract_Variable (G.Ref.A_Seen _) -> None
      | G.Ref.Concrete_Variable (G.Ref.A_Seen _) -> None
      | _ -> Some (Global x)

    let mk_local = function
      | Local.L_Expr_Binder -> Some Expr_Binder
      | _ -> None
  end

  module Operations = struct
    type t =
      | Global of G.Ref.t_kind (*FIXME*)
      | Local of Local.t_local_kind

    let mk_global = function 
      | G.Ref.Abstract_Variable G.Ref.A_Refined
      | G.Ref.Abstract_Constant G.Ref.A_Refined -> None
      | x -> Some (Global x)

    type t_source_1 =
      | A_Machine of Utils.loc
      | A_Redeclared of Utils.loc

    type t_source_2 =
      | C_Machine of Utils.loc
      | C_Refined
      | C_Redeclared of Utils.loc

    type t_mut =
      | Param_Out
      | Subst_Binder
      | Abstract_Variable of t_source_1
      | Concrete_Variable of t_source_2

    let mk_local x = Some (Local x)

    let mk_local_mut = function
      | Local.L_Expr_Binder -> None
      | Local.L_Subst_Binder -> Some Subst_Binder
      | Local.L_Param_In -> None
      | Local.L_Param_Out -> Some Param_Out

    let mk_global_mut = function
      | G.Ref.Abstract_Variable (G.Ref.A_Machine l) -> Some (Abstract_Variable (A_Machine l))
      | G.Ref.Abstract_Variable (G.Ref.A_Redeclared_In_Machine l) -> Some (Abstract_Variable (A_Redeclared l))
      | G.Ref.Concrete_Variable (G.Ref.A_Machine l) -> Some (Concrete_Variable (C_Machine l))
      | G.Ref.Concrete_Variable (G.Ref.A_Redeclared_In_Machine l) -> Some (Concrete_Variable (C_Redeclared l))
      | G.Ref.Concrete_Variable (G.Ref.A_Refined) -> Some (Concrete_Variable C_Refined)
      | _ -> None

    type t_op =
      | O_Seen of SyntaxCore.ren_ident
      | O_Included of SyntaxCore.ren_ident
      | O_Refined_And_Included of SyntaxCore.ren_ident
      | O_Included_And_Promoted of SyntaxCore.ren_ident
      | O_Refined_Included_And_Promoted of SyntaxCore.ren_ident

    let mk_op = function
      | G.Ref.O_Refined -> None
      | G.Ref.O_Refined_And_Machine _ -> None
      | G.Ref.O_Seen mch -> Some (O_Seen mch) (*FIXME*)
      | G.Ref.O_Included mch -> Some (O_Included mch)
      | G.Ref.O_Refined_And_Included mch -> Some (O_Refined_And_Included mch)
      | G.Ref.O_Refined_Included_And_Promoted mch -> Some (O_Refined_Included_And_Promoted mch)

  end
end

module Imp = struct

  module Imports = struct
    
    type t_source = Machine of Utils.loc | Refined | Seen of SyntaxCore.ren_ident
    type t =
      | Expr_Binder
      | Parameter of G.t_param_kind*Utils.loc 
      | Concrete_Constant of  t_source
      | Concrete_Set of string list * t_source
      | Abstract_Set of t_source
      | Enumerate of t_source

    let mk_global = function 
      | G.Imp.Parameter (k,l) -> Some (Parameter (k,l))
      | G.Imp.Concrete_Constant (G.Imp.C_Machine l) -> Some (Concrete_Constant (Machine l))
      | G.Imp.Concrete_Constant (G.Imp.C_Seen mch) -> Some (Concrete_Constant (Seen mch))
      | G.Imp.Concrete_Constant (G.Imp.C_Refined) -> Some (Concrete_Constant (Refined))
      | G.Imp.Concrete_Constant (G.Imp.C_Redeclared_In_Machine l) -> Some (Concrete_Constant (Machine l)) (*FIXME*)
      | G.Imp.Concrete_Constant (G.Imp.C_Redeclared_In_Seen mch) -> Some (Concrete_Constant (Seen mch)) (*FIXME*)
      | G.Imp.Concrete_Constant(G.Imp.C_Imported _|G.Imp.C_Redeclared_In_Imported _) -> None
      | G.Imp.Abstract_Set (G.Imp.S_Machine l) -> Some (Abstract_Set (Machine l))
      | G.Imp.Abstract_Set (G.Imp.S_Seen mch) -> Some (Abstract_Set (Seen mch))
      | G.Imp.Abstract_Set (G.Imp.S_Refined) -> Some (Abstract_Set (Refined))
      | G.Imp.Abstract_Set (G.Imp.S_Redeclared_In_Seen mch) -> Some (Abstract_Set (Seen mch)) (*FIXME*)
      | G.Imp.Abstract_Set (G.Imp.S_Imported _|G.Imp.S_Redeclared_In_Imported _) -> None
      | G.Imp.Concrete_Set (elts,G.Imp.D_Machine l) -> Some (Concrete_Set (elts,Machine l))
      | G.Imp.Concrete_Set (elts,G.Imp.D_Seen mch) -> Some (Concrete_Set (elts,Seen mch))
      | G.Imp.Concrete_Set (elts,G.Imp.D_Refined) -> Some (Concrete_Set (elts,Refined))
      | G.Imp.Concrete_Set (_,G.Imp.D_Imported _) -> None
      | G.Imp.Enumerate (G.Imp.D_Machine l) -> Some (Enumerate (Machine l))
      | G.Imp.Enumerate (G.Imp.D_Seen mch) -> Some (Enumerate (Seen mch))
      | G.Imp.Enumerate (G.Imp.D_Refined) -> Some (Enumerate (Refined))
      | G.Imp.Enumerate (G.Imp.D_Imported _) -> None
      | G.Imp.Abstract_Variable _
      | G.Imp.Concrete_Variable _
      | G.Imp.Abstract_Constant _ -> None

    let mk_local = function 
      | Local.L_Expr_Binder -> Some Expr_Binder
      | _ -> None

  end

  module Assert = struct
    type t =
      | Global of G.Imp.t_kind
      | Local of Local.t_local_kind

    let mk_global x = Some (Global x)

    let mk_local x = Some (Local x)
  end

  module Properties = struct
    type t =
      | Expr_Binder
      | Concrete_Constant of G.Imp.t_concrete_const_decl
      | Abstract_Set of G.Imp.t_abstract_set_decl
      | Enumerate of G.Imp.t_concrete_data_decl
      | Concrete_Set of string list*G.Imp.t_concrete_data_decl
      | Abstract_Constant of G.Imp.t_abstract_decl

    let mk_global = function
      | G.Imp.Abstract_Variable _ -> None
      | G.Imp.Concrete_Variable _ -> None
      | G.Imp.Parameter _ -> None
      | G.Imp.Concrete_Constant src -> Some (Concrete_Constant src)
      | G.Imp.Abstract_Set src -> Some (Abstract_Set src)
      | G.Imp.Enumerate src -> Some (Enumerate src)
      | G.Imp.Concrete_Set (elts,src) -> Some (Concrete_Set (elts,src))
      | G.Imp.Abstract_Constant src -> Some (Abstract_Constant src)

    let mk_local = function 
      | Local.L_Expr_Binder -> Some Expr_Binder
      | _ -> None
  end

  module Invariant = struct
    type t =
      | Global of G.Imp.t_kind (*FIXME*)
      | Expr_Binder

    let mk_global x =
      if !extended_sees then Some (Global x)
      else match x with
      | G.Imp.Abstract_Variable (G.Imp.A_Seen _) -> None
      | G.Imp.Concrete_Variable (G.Imp.V_Seen _) -> None
      | _ -> Some (Global x)

    let mk_local = function
      | Local.L_Expr_Binder -> Some Expr_Binder
      | _ -> None
  end

  module Operations = struct
    type t =
      | Global of G.Imp.t_kind (*FIXME*)
      | Local of Local.t_local_kind

    let mk_global = function 
      | G.Imp.Abstract_Variable _ | G.Imp.Abstract_Constant _ -> None
      | x -> Some (Global x)

    type t_source =
      | Machine of Utils.loc
      | Refined
      | Redeclared_In_Machine of Utils.loc

    type t_mut =
      | Param_Out
      | Subst_Binder
      | Concrete_Variable of t_source

    let mk_local x = Some (Local x)

    let mk_local_mut = function
      | Local.L_Expr_Binder -> None
      | Local.L_Subst_Binder -> Some Subst_Binder
      | Local.L_Param_In -> None
      | Local.L_Param_Out -> Some Param_Out

    let mk_global_mut = function
      | G.Imp.Concrete_Variable (G.Imp.V_Machine l) -> Some (Concrete_Variable (Machine l))
      | G.Imp.Concrete_Variable (G.Imp.V_Redeclared_In_Machine l) -> Some (Concrete_Variable (Redeclared_In_Machine l))
      | G.Imp.Concrete_Variable (G.Imp.V_Refined) -> Some (Concrete_Variable Refined)
      | _ -> None

    type t_op =
      | O_Seen of SyntaxCore.ren_ident
      | O_Imported of SyntaxCore.ren_ident
      | O_Refined_And_Imported of SyntaxCore.ren_ident
      | O_Included_And_Promoted of SyntaxCore.ren_ident
      | O_Refined_Imported_And_Promoted of SyntaxCore.ren_ident
      | O_Local of Utils.loc

    let mk_op = function
      | G.Imp.O_Refined -> None
      | G.Imp.O_Current_And_Refined _ -> None
      | G.Imp.O_Seen mch -> Some (O_Seen mch) (*FIXME*)
      | G.Imp.O_Imported mch -> Some (O_Imported mch)
      | G.Imp.O_Imported_And_Refined mch -> Some (O_Refined_And_Imported mch)
      | G.Imp.O_Imported_Promoted_And_Refined (mch,_) -> Some (O_Refined_Imported_And_Promoted mch)
      | O_Local_Spec l -> Some (O_Local l)
      | O_Local_Spec_And_Implem (l,_) -> Some (O_Local l)

  end

  module Local_Operations = struct
    type t =
      | Global of G.Imp.t_kind (*FIXME*)
      | Local of Local.t_local_kind

    let mk_global = function 
      | G.Imp.Abstract_Variable G.Imp.A_Refined
      | G.Imp.Abstract_Constant G.Imp.A_Refined -> None
      | x -> Some (Global x)

    let mk_local x = Some (Local x)

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

    let mk_local_mut = function
      | Local.L_Expr_Binder -> None
      | Local.L_Subst_Binder -> Some Subst_Binder
      | Local.L_Param_In -> None
      | Local.L_Param_Out -> Some Param_Out

    let mk_global_mut = function
      | G.Imp.Abstract_Variable (G.Imp.A_Imported mch) -> Some (Abstract_Variable (A_Imported mch))
      | G.Imp.Abstract_Variable (G.Imp.A_Redeclared_In_Imported mch) -> Some (Abstract_Variable (A_Redeclared_In_Imported mch))
      | G.Imp.Concrete_Variable (G.Imp.V_Machine l) -> Some (Concrete_Variable (C_Machine l))
      | G.Imp.Concrete_Variable (G.Imp.V_Redeclared_In_Machine l) -> Some (Concrete_Variable (C_Redeclared_In_Machine l))
      | G.Imp.Concrete_Variable (G.Imp.V_Redeclared_In_Imported mch) -> Some (Concrete_Variable (C_Redeclared_In_Imported mch))
      | G.Imp.Concrete_Variable (G.Imp.V_Imported mch) -> Some (Concrete_Variable (C_Imported mch))
      | G.Imp.Concrete_Variable (G.Imp.V_Refined) -> Some (Concrete_Variable (C_Refined))
      | _ -> None

    type t_op =
      | O_Seen of SyntaxCore.ren_ident
      | O_Imported of SyntaxCore.ren_ident
      | O_Refined_And_Imported of SyntaxCore.ren_ident
      | O_Included_And_Promoted of SyntaxCore.ren_ident
      | O_Refined_Imported_And_Promoted of SyntaxCore.ren_ident
      | O_Local of Utils.loc

    let mk_op = function
      | G.Imp.O_Refined -> None
      | G.Imp.O_Current_And_Refined _ -> None
      | G.Imp.O_Seen mch -> Some (O_Seen mch)
      | G.Imp.O_Imported mch -> Some (O_Imported mch)
      | G.Imp.O_Imported_And_Refined mch -> Some (O_Refined_And_Imported mch)
      | G.Imp.O_Imported_Promoted_And_Refined (mch,_) -> Some (O_Refined_Imported_And_Promoted mch)
      | O_Local_Spec l -> Some (O_Local l)
      | O_Local_Spec_And_Implem (l,_) -> Some (O_Local l)
  end

  module Values = struct
    type t =
      | Expr_Binder
      | Concrete_Constant of G.Imp.t_concrete_const_decl
      | Abstract_Set of G.Imp.t_abstract_set_decl
      | Enumerate of G.Imp.t_concrete_data_decl
      | Concrete_Set of string list*G.Imp.t_concrete_data_decl

    let mk_global = function
      | G.Imp.Abstract_Variable _ -> None
      | G.Imp.Concrete_Variable _ -> None
      | G.Imp.Parameter _ -> None
      | G.Imp.Abstract_Constant _ -> None
      | G.Imp.Concrete_Constant src -> Some (Concrete_Constant src)
      | G.Imp.Abstract_Set src -> Some (Abstract_Set src)
      | G.Imp.Enumerate src -> Some (Enumerate src)
      | G.Imp.Concrete_Set (elts,src) -> Some (Concrete_Set (elts,src))

    let mk_local = function 
      | Local.L_Expr_Binder -> Some Expr_Binder
      | _ -> None

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

let mk_global (type a b) (cl:(a,b) clause) (ki:a) : b option =
  match cl with
  | M_Constraints -> Mch.Constraints.mk_global ki
  | M_Includes -> Mch.Includes.mk_global ki
  | M_Assert -> Mch.Assert.mk_global ki
  | M_Properties -> Mch.Properties.mk_global ki
  | M_Invariant -> Mch.Invariant.mk_global ki
  | M_Operations -> Mch.Operations.mk_global ki
  | R_Includes -> Ref.Includes.mk_global ki
  | R_Assert -> Ref.Assert.mk_global ki
  | R_Properties -> Ref.Properties.mk_global ki
  | R_Invariant -> Ref.Invariant.mk_global ki
  | R_Operations -> Ref.Operations.mk_global ki
  | I_Imports -> Imp.Imports.mk_global ki
  | I_Assert -> Imp.Assert.mk_global ki
  | I_Properties -> Imp.Properties.mk_global ki
  | I_Invariant -> Imp.Invariant.mk_global ki
  | I_Operations -> Imp.Operations.mk_global ki
  | I_Local_Operations -> Imp.Local_Operations.mk_global ki
  | I_Values -> Imp.Values.mk_global ki

let mk_local (type a b) (cl:(a,b) clause) (ki:Local.t_local_kind) : b option =
  match cl with
  | M_Constraints -> Mch.Constraints.mk_local ki
  | M_Includes -> Mch.Includes.mk_local ki
  | M_Assert -> Mch.Assert.mk_local ki
  | M_Properties -> Mch.Properties.mk_local ki
  | M_Invariant -> Mch.Invariant.mk_local ki
  | M_Operations -> Mch.Operations.mk_local ki
  | R_Includes -> Ref.Includes.mk_local ki
  | R_Assert -> Ref.Assert.mk_local ki
  | R_Properties -> Ref.Properties.mk_local ki
  | R_Invariant -> Ref.Invariant.mk_local ki
  | R_Operations -> Ref.Operations.mk_local ki
  | I_Imports -> Imp.Imports.mk_local ki
  | I_Assert -> Imp.Assert.mk_local ki
  | I_Properties -> Imp.Properties.mk_local ki
  | I_Invariant -> Imp.Invariant.mk_local ki
  | I_Operations -> Imp.Operations.mk_local ki
  | I_Local_Operations -> Imp.Local_Operations.mk_local ki
  | I_Values -> Imp.Values.mk_local ki

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

let mk_global_mut (type a b c d e f) (cl:(a,b,c,d,e,f) sclause) (ki:a) : c option =
  match cl with
  | MS_Operations -> Mch.Operations.mk_global_mut ki
  | RS_Operations ->  Ref.Operations.mk_global_mut ki
  | IS_Operations ->  Imp.Operations.mk_global_mut ki
  | IS_Local_Operations ->  Imp.Local_Operations.mk_global_mut ki

let mk_local_mut (type a b c d e f) (cl:(a,b,c,d,e,f) sclause) (ki:Local.t_local_kind) : c option =
  match cl with
  | MS_Operations -> Mch.Operations.mk_local_mut ki
  | RS_Operations ->  Ref.Operations.mk_local_mut ki
  | IS_Operations ->  Imp.Operations.mk_local_mut ki
  | IS_Local_Operations ->  Imp.Local_Operations.mk_local_mut ki

let mk_op (type a b c d e f) (cl: (a,b,c,d,e,f) sclause) (ki:e) : f option =
  match cl with
  | MS_Operations -> Mch.Operations.mk_op ki
  | RS_Operations ->  Ref.Operations.mk_op ki
  | IS_Operations ->  Imp.Operations.mk_op ki
  | IS_Local_Operations -> Imp.Local_Operations.mk_op ki

let to_clause : type a b c d e f. (a,b,c,d,e,f) sclause -> (a,b) clause = function
  | MS_Operations -> M_Operations
  | RS_Operations -> R_Operations
  | IS_Operations -> I_Operations
  | IS_Local_Operations -> I_Local_Operations

let to_assert : type a b c d e f.(a,b,c,d,e,f) sclause -> (a,d) clause = function
  | MS_Operations -> M_Assert
  | RS_Operations -> R_Assert
  | IS_Operations -> I_Assert
  | IS_Local_Operations -> I_Assert

let mch_kind (ki:G.Mch.t_kind) : string = (*FIXME*)
  match ki with
  | Parameter _ -> "parameter"
  | Abstract_Variable (Global.Mch.Machine _) -> "abstract variable"
  | Abstract_Variable (Global.Mch.Seen _) -> "seen abstract variable"
  | Abstract_Variable (Global.Mch.Used _) -> "used abstract variable"
  | Abstract_Variable (Global.Mch.Included _) -> "included abstract variable"
  | Abstract_Constant _ -> "abstract constant"
  | Concrete_Variable _ -> "concrete variable"
  | Concrete_Constant _ -> "concrete constant"
  | Abstract_Set _ -> "abstract set"
  | Concrete_Set _ -> "concrete set"
  | Enumerate _ -> "enumerate"

let ref_kind (ki:G.Ref.t_kind) : string = (*FIXME*)
  match ki with
  | Parameter _ -> "parameter"
  | Abstract_Variable (Global.Ref.A_Machine _) -> "abstract variable"
  | Abstract_Variable (Global.Ref.A_Seen _) -> "seen abstract variable"
  | Abstract_Variable (Global.Ref.A_Refined) -> "disappearing abstract variable"
  | Abstract_Variable (Global.Ref.A_Included _) -> "included abstract variable"
  | Abstract_Variable (Global.Ref.A_Redeclared_In_Machine _) -> "abstract variable"
  | Abstract_Variable (Global.Ref.A_Redeclared_In_Included _) -> "included abstract variable"
  | Abstract_Constant _ -> "abstract constant"
  | Concrete_Variable _ -> "concrete variable"
  | Concrete_Constant _ -> "concrete constant"
  | Abstract_Set _ -> "abstract set"
  | Concrete_Set _ -> "concrete set"
  | Enumerate _ -> "enumerate"

let imp_kind (ki:G.Imp.t_kind) : string = (*FIXME*)
  match ki with
  | Parameter _ -> "parameter"
  | Abstract_Variable (G.Imp.A_Seen _) -> "seen abstract variable"
  | Abstract_Variable (G.Imp.A_Refined) -> "disappearing abstract variable"
  | Abstract_Variable (G.Imp.A_Imported _) -> "imported abstract variable"
  | Abstract_Variable (G.Imp.A_Redeclared_In_Imported _) -> "imported abstract variable"
  | Abstract_Constant _ -> "abstract constant"
  | Concrete_Variable _ -> "concrete variable"
  | Concrete_Constant _ -> "concrete constant"
  | Abstract_Set _ -> "abstract set"
  | Concrete_Set _ -> "concrete set"
  | Enumerate _ -> "enumerate"

let error (type a b c) (loc:Utils.loc) (id:string) (cl:(a,b) clause) (ki:a) : c =
  let (ki_str,cl_str) =  match cl with
    | M_Constraints -> mch_kind ki, "the clause CONSTRAINTS"
    | M_Includes -> mch_kind ki, "the clauses INCLUDES and EXTENDS"
    | M_Assert -> mch_kind ki, "the ASSERT substitution"
    | M_Properties -> mch_kind ki, "the clause PROPERTIES"
    | M_Invariant -> mch_kind ki, "the clause INVARIANT"
    | M_Operations -> mch_kind ki, "the clause OPERATIONS"
    | R_Includes -> ref_kind ki, "the clauses INCLUDES and EXTENDS"
    | R_Assert -> ref_kind ki, "the ASSERT substitution"
    | R_Properties -> ref_kind ki, "the clause PROPERTIES"
    | R_Invariant -> ref_kind ki, "the clause INVARIANT"
    | R_Operations -> ref_kind ki, "the clause OPERATIONS"
    | I_Imports -> imp_kind ki, "the clauses IMPORTS and EXTENDS"
    | I_Assert -> imp_kind ki, "the ASSERT substitution nor in the VARIANT and INVARIANT part of a WHILE"
    | I_Properties -> imp_kind ki, "the clause PROPERTIES"
    | I_Invariant -> imp_kind ki, "the clause INVARIANT"
    | I_Operations -> imp_kind ki, "the clause OPERATIONS"
    | I_Local_Operations -> imp_kind ki, "the clause LOCAL_OPERATIONS"
    | I_Values -> imp_kind ki, "the clause VALUES"
  in
  Error.error loc ("The "^ki_str^" '"^id^"' is not visible in "^cl_str^".")
