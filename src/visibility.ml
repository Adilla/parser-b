module G = Global

let extended_sees = ref false

type 'a t_kind =
  | Local of Local.t_local_kind
  | Global of 'a

module Mch = struct
  module Constraints = struct
    type t = G.Mch.t_kind t_kind

    let mk_global = function
      | G.Mch.Parameter (_,G.Mch.P_Machine _) as x -> Some (Global x)
      | _ -> None

    let mk_local = function
      | Local.L_Expr_Binder as x -> Local x
      | _ -> assert false
  end

  module Includes = struct
    type t = G.Mch.t_kind t_kind

    let mk_global = function
      | G.Mch.Parameter (_,G.Mch.P_Machine _)
      | G.Mch.Concrete_Constant (Global.Mch.Machine _)
      | G.Mch.Concrete_Constant (Global.Mch.Seen _)
      | G.Mch.Abstract_Set (Global.Mch.Machine _)
      | G.Mch.Abstract_Set (Global.Mch.Seen _)
      | G.Mch.Concrete_Set (_,Global.Mch.Machine _)
      | G.Mch.Concrete_Set (_,Global.Mch.Seen _)
      | G.Mch.Enumerate (Global.Mch.Machine _)
      | G.Mch.Enumerate (Global.Mch.Seen _) as x -> Some (Global x)
      | _ -> None

    let mk_local = function
      | Local.L_Expr_Binder as x -> Local x
      | _ -> assert false

  end

  module Assert = struct
    type t = G.Mch.t_kind t_kind
    let mk_global = function
      | G.Mch.Parameter (_,G.Mch.P_Machine _)
      | G.Mch.Concrete_Variable _
      | G.Mch.Abstract_Variable _
      | G.Mch.Concrete_Constant _
      | G.Mch.Abstract_Constant _
      | G.Mch.Abstract_Set _
      | G.Mch.Concrete_Set _
      | G.Mch.Enumerate _ as x -> Some (Global x)
      | G.Mch.Parameter (_,_) -> None

    let mk_local x = Local x
  end

  module Properties = struct
    type t = G.Mch.t_kind t_kind

    let mk_global = function
      | G.Mch.Abstract_Variable _ -> None
      | G.Mch.Concrete_Variable _ -> None
      | G.Mch.Parameter _ -> None
      | G.Mch.Concrete_Constant _
      | G.Mch.Abstract_Set _
      | G.Mch.Enumerate _
      | G.Mch.Concrete_Set (_,_)
      | G.Mch.Abstract_Constant _ as x -> Some (Global x)

    let mk_local = function
      | Local.L_Expr_Binder as x -> Local x
      | _ -> assert false
  end

  module Invariant = struct
    type t = G.Mch.t_kind t_kind

    let mk_global = function
      | G.Mch.Parameter (_,G.Mch.P_Machine _)
      | G.Mch.Abstract_Variable (G.Mch.Machine _|G.Mch.Included _|G.Mch.Used _)
      | G.Mch.Concrete_Variable (G.Mch.Machine _|G.Mch.Included _|G.Mch.Used _)
      | G.Mch.Abstract_Constant _
      | G.Mch.Abstract_Set _
      | G.Mch.Concrete_Set _
      | G.Mch.Concrete_Constant _
      | G.Mch.Enumerate _ as x -> Some (Global x)
      | G.Mch.Abstract_Variable (G.Mch.Seen _)
      | G.Mch.Concrete_Variable (G.Mch.Seen _) as x ->
        if !extended_sees then Some (Global x)
        else None
      | G.Mch.Parameter (_,_) -> None

    let mk_local = function
      | Local.L_Expr_Binder as x -> Local x
      | _ -> assert false
  end

  module Operations = struct
    type t = G.Mch.t_kind t_kind
    type t_mut = G.Mch.t_kind t_kind

    type t_op = G.Mch.t_op_source

    let mk_global = function
      | G.Mch.Parameter (_,G.Mch.P_Machine _)
      | G.Mch.Concrete_Variable _
      | G.Mch.Abstract_Variable _
      | G.Mch.Concrete_Constant _
      | G.Mch.Abstract_Constant _
      | G.Mch.Abstract_Set _
      | G.Mch.Concrete_Set _
      | G.Mch.Enumerate _ as x -> Some (Global x)
      | G.Mch.Parameter (_,_) -> None


    let mk_global_mut = function
      | G.Mch.Abstract_Variable (G.Mch.Machine _)
      | G.Mch.Concrete_Variable (G.Mch.Machine _) as x -> Some (Global x)
      | _ -> None

    let mk_op readonly = function
      | G.Mch.O_Machine _ -> None
      | G.Mch.O_Seen _ as x -> if readonly then Some x else None
      | G.Mch.O_Used _
      | G.Mch.O_Included _
      | G.Mch.O_Included_And_Promoted (_,_) as x -> Some x

    let mk_local x = Local x

    let mk_local_mut = function
      | Local.L_Expr_Binder | Local.L_Param_In -> None
      | Local.L_Subst_Binder | Local.L_Param_Out as x -> Some (Local x)

  end

end
 
module Ref = struct

  module Includes = struct
    type t = G.Ref.t_kind t_kind

    let mk_global = function
      | G.Ref.Parameter (_,G.Ref.P_Machine _)
      | G.Ref.Concrete_Constant (G.Ref.A_Machine _|G.Ref.A_Seen _|G.Ref.A_Refined
                                |G.Ref.A_Redeclared_In_Machine _)
      | G.Ref.Abstract_Set (G.Ref.Machine _|G.Ref.Seen _|G.Ref.Refined)
      | G.Ref.Concrete_Set (_,(G.Ref.Machine _|G.Ref.Seen _|G.Ref.Refined))
      | G.Ref.Enumerate (G.Ref.Machine _|G.Ref.Seen _|G.Ref.Refined) as x ->
        Some (Global x)
      | _ -> None

    let mk_local = function 
      | Local.L_Expr_Binder as x -> Local x
      | _ -> assert false
  end

  module Assert = struct
    type t = G.Ref.t_kind t_kind

    let mk_global = function
      | G.Ref.Parameter (_,G.Ref.P_Machine _)
      | G.Ref.Concrete_Variable _
      | G.Ref.Abstract_Variable _
      | G.Ref.Concrete_Constant _
      | G.Ref.Abstract_Constant _
      | G.Ref.Abstract_Set _
      | G.Ref.Concrete_Set _
      | G.Ref.Enumerate _ as x -> Some (Global x)
      | G.Ref.Parameter (_,_) -> None

    let mk_local x = Local x
  end

  module Properties = struct
    type t = G.Ref.t_kind t_kind

    let mk_global = function
      | G.Ref.Abstract_Variable _ | G.Ref.Concrete_Variable _ | G.Ref.Parameter _ -> None
      | G.Ref.Concrete_Constant _ | G.Ref.Abstract_Set _ | G.Ref.Enumerate _
      | G.Ref.Concrete_Set (_,_) | G.Ref.Abstract_Constant _ as x -> Some (Global x)

    let mk_local = function 
      | Local.L_Expr_Binder as x -> Local x
      | _ -> assert false
  end

  module Invariant = struct
    type t = G.Ref.t_kind t_kind

    let mk_global = function
      | G.Ref.Parameter (_,G.Ref.P_Machine _)
      | G.Ref.Concrete_Variable
          (G.Ref.A_Machine _|G.Ref.A_Refined|G.Ref.A_Included _|
           G.Ref.A_Redeclared_In_Machine _|G.Ref.A_Redeclared_In_Included _)
      | G.Ref.Abstract_Variable
          (G.Ref.A_Machine _|G.Ref.A_Refined|G.Ref.A_Included _|
           G.Ref.A_Redeclared_In_Machine _|G.Ref.A_Redeclared_In_Included _)
      | G.Ref.Concrete_Constant _
      | G.Ref.Abstract_Constant _
      | G.Ref.Abstract_Set _
      | G.Ref.Concrete_Set _
      | G.Ref.Enumerate _ as x -> Some (Global x)
      | G.Ref.Abstract_Variable (G.Ref.A_Seen _)
      | G.Ref.Concrete_Variable (G.Ref.A_Seen _) as x ->
        if !extended_sees then Some (Global x)
        else None
      | G.Ref.Parameter (_,_) -> None

    let mk_local = function
      | Local.L_Expr_Binder as x -> Local x
      | _ -> assert false
  end

  module Operations = struct
    type t = G.Ref.t_kind t_kind

    let mk_global = function 
      | G.Ref.Parameter (_,G.Ref.P_Machine _)
      | G.Ref.Abstract_Variable
          (A_Machine _|A_Included _|A_Seen _ |A_Redeclared_In_Machine _
          |A_Redeclared_In_Included _)
      | G.Ref.Abstract_Constant
          (A_Machine _|A_Included _|A_Seen _ |A_Redeclared_In_Machine _
          |A_Redeclared_In_Included _)
      | G.Ref.Concrete_Variable _
      | G.Ref.Concrete_Constant _
      | G.Ref.Abstract_Set _
      | G.Ref.Concrete_Set _
      | G.Ref.Enumerate _ as x -> Some (Global x)
      | G.Ref.Abstract_Variable G.Ref.A_Refined
      | G.Ref.Abstract_Constant G.Ref.A_Refined
      | G.Ref.Parameter (_,_) -> None

    let mk_local x = Local x

    type t_mut = G.Ref.t_kind t_kind

    let mk_local_mut = function
      | Local.L_Expr_Binder | Local.L_Param_In -> None
      | Local.L_Subst_Binder | Local.L_Param_Out as x -> Some (Local x)

    let mk_global_mut = function
      | G.Ref.Abstract_Variable (G.Ref.A_Machine _|G.Ref.A_Redeclared_In_Machine _)
      | G.Ref.Concrete_Variable (G.Ref.A_Machine _|G.Ref.A_Redeclared_In_Machine _
                                |G.Ref.A_Refined) as x -> Some (Global x)
      | _ -> None

    type t_op = G.Ref.t_op_source

    let mk_op readonly = function
      | G.Ref.O_Refined | G.Ref.O_Refined_And_Machine _ -> None
      | G.Ref.O_Seen _ as x -> if readonly then Some x else None
      | G.Ref.O_Included _
      | G.Ref.O_Refined_And_Included _
      | G.Ref.O_Refined_Included_And_Promoted _ as x -> Some x

  end
end

module Imp = struct

  module Imports = struct
    type t = G.Imp.t_kind t_kind

    let mk_global = function 
      | G.Imp.Parameter (_,G.Imp.P_Machine _)
      | G.Imp.Concrete_Constant
          (G.Imp.C_Machine _|G.Imp.C_Seen _|G.Imp.C_Refined
          |G.Imp.C_Redeclared_In_Machine _|G.Imp.C_Redeclared_In_Seen _)
      | G.Imp.Abstract_Set
          (G.Imp.S_Machine _|G.Imp.S_Seen _|G.Imp.S_Refined
          |G.Imp.S_Redeclared_In_Seen _)
      | G.Imp.Concrete_Set (_,(G.Imp.D_Machine _|G.Imp.D_Seen _|G.Imp.D_Refined))
      | G.Imp.Enumerate (G.Imp.D_Machine _|G.Imp.D_Seen _|G.Imp.D_Refined) as x ->
        Some (Global x)

      | G.Imp.Enumerate (G.Imp.D_Imported _)
      | G.Imp.Concrete_Constant(G.Imp.C_Imported _|G.Imp.C_Redeclared_In_Imported _)
      | G.Imp.Abstract_Set (G.Imp.S_Imported _|G.Imp.S_Redeclared_In_Imported _)
      | G.Imp.Concrete_Set (_,G.Imp.D_Imported _)
      | G.Imp.Parameter (_,G.Imp.P_Seen _)
      | G.Imp.Abstract_Variable _
      | G.Imp.Concrete_Variable _
      | G.Imp.Abstract_Constant _ -> None

    let mk_local = function 
      | Local.L_Expr_Binder as x -> Local x
      | _ -> assert false

  end

  module Assert = struct
    type t = G.Imp.t_kind t_kind

    let mk_global = function
      | G.Imp.Parameter (_,P_Machine _)
      | G.Imp.Concrete_Variable _
      | G.Imp.Abstract_Variable _
      | G.Imp.Concrete_Constant _
      | G.Imp.Abstract_Constant _
      | G.Imp.Abstract_Set _
      | G.Imp.Concrete_Set _
      | G.Imp.Enumerate _ as x -> Some (Global x)
      | G.Imp.Parameter (_,_) -> None

    let mk_local x = Local x
  end

  module Properties = struct
    type t = G.Imp.t_kind t_kind

    let mk_global = function
      | G.Imp.Concrete_Constant _
      | G.Imp.Abstract_Set _
      | G.Imp.Enumerate _
      | G.Imp.Concrete_Set _
      | G.Imp.Abstract_Constant _ as x -> Some (Global x)
      | G.Imp.Abstract_Variable _
      | G.Imp.Concrete_Variable _
      | G.Imp.Parameter _ -> None

    let mk_local = function 
      | Local.L_Expr_Binder as x -> Local x
      | _ -> assert false
  end

  module Invariant = struct
    type t = G.Imp.t_kind t_kind

    let mk_global = function
      | G.Imp.Parameter (_,P_Machine _)
      | G.Imp.Concrete_Variable
          (V_Machine _|V_Refined|V_Imported _|
           V_Redeclared_In_Machine _|V_Redeclared_In_Imported _)
      | G.Imp.Abstract_Variable
          (A_Refined|A_Imported _|A_Redeclared_In_Imported _)
      | G.Imp.Concrete_Constant _
      | G.Imp.Abstract_Constant _
      | G.Imp.Abstract_Set _
      | G.Imp.Concrete_Set _
      | G.Imp.Enumerate _ as x -> Some (Global x)
      | G.Imp.Abstract_Variable (A_Seen _)
      | G.Imp.Concrete_Variable (V_Seen _) as x ->
        if !extended_sees then Some (Global x)
        else None
      | G.Imp.Parameter (_,_) -> None

    let mk_local = function
      | Local.L_Expr_Binder as x -> Local x
      | _ -> assert false
  end

  module Operations = struct
    type t = G.Imp.t_kind t_kind

    let mk_global = function
      | G.Imp.Parameter (_,P_Machine _)
      | G.Imp.Concrete_Variable _
      | G.Imp.Concrete_Constant _
      | G.Imp.Abstract_Set _
      | G.Imp.Concrete_Set _
      | G.Imp.Enumerate _ as x -> Some (Global x)
      | G.Imp.Abstract_Variable _
      | G.Imp.Abstract_Constant _
      | G.Imp.Parameter (_,_) -> None

    let mk_local x = Local x

    type t_mut = G.Imp.t_kind t_kind

    let mk_local_mut = function
      | Local.L_Param_In | Local.L_Expr_Binder -> None
      | Local.L_Subst_Binder | Local.L_Param_Out as x -> Some (Local x)

    let mk_global_mut = function
      | G.Imp.Concrete_Variable (G.Imp.V_Machine _)
      | G.Imp.Concrete_Variable (G.Imp.V_Redeclared_In_Machine _)
      | G.Imp.Concrete_Variable (G.Imp.V_Refined) as x -> Some (Global x)
      | _ -> None

    type t_op = G.Imp.t_op_source

    let mk_op readonly = function
      | G.Imp.O_Refined
      | G.Imp.O_Current_And_Refined _ -> None
      | G.Imp.O_Seen _ as x -> if readonly then Some x else None
      | G.Imp.O_Imported _
      | G.Imp.O_Imported_And_Refined _
      | G.Imp.O_Imported_Promoted_And_Refined (_,_)
      | O_Local_Spec _
      | O_Local_Spec_And_Implem (_,_) as x -> Some x

  end

  module Local_Operations = struct
    type t = G.Imp.t_kind t_kind

    let mk_global = function
      | G.Imp.Parameter (_,P_Machine _)
      | G.Imp.Concrete_Variable _
      | G.Imp.Concrete_Constant _
      | G.Imp.Abstract_Set _
      | G.Imp.Concrete_Set _
      | G.Imp.Abstract_Variable
          (G.Imp.A_Seen _|G.Imp.A_Imported _|G.Imp.A_Redeclared_In_Imported _)
      | G.Imp.Abstract_Constant
          (G.Imp.A_Seen _|G.Imp.A_Imported _|G.Imp.A_Redeclared_In_Imported _)
      | G.Imp.Enumerate _ as x -> Some (Global x)
      | G.Imp.Abstract_Variable G.Imp.A_Refined
      | G.Imp.Abstract_Constant G.Imp.A_Refined
      | G.Imp.Parameter (_,_) -> None

    let mk_local x = Local x

    type t_mut = G.Imp.t_kind t_kind

    let mk_local_mut = function
      | Local.L_Expr_Binder | Local.L_Param_In -> None
      | Local.L_Subst_Binder | Local.L_Param_Out as x -> Some (Local x)

    let mk_global_mut = function
      | G.Imp.Abstract_Variable (G.Imp.A_Imported _)
      | G.Imp.Abstract_Variable (G.Imp.A_Redeclared_In_Imported _)
      | G.Imp.Concrete_Variable (G.Imp.V_Machine _)
      | G.Imp.Concrete_Variable (G.Imp.V_Redeclared_In_Machine _)
      | G.Imp.Concrete_Variable (G.Imp.V_Redeclared_In_Imported _)
      | G.Imp.Concrete_Variable (G.Imp.V_Imported _)
      | G.Imp.Concrete_Variable (G.Imp.V_Refined) as x -> Some (Global x)
      | _ -> None

    type t_op = G.Imp.t_op_source

    let mk_op readonly = function
      | G.Imp.O_Refined
      | G.Imp.O_Current_And_Refined _ -> None
      | G.Imp.O_Seen _ as x -> if readonly then Some x else None
      | G.Imp.O_Imported _
      | G.Imp.O_Imported_And_Refined _
      | G.Imp.O_Imported_Promoted_And_Refined (_,_)
      | G.Imp.O_Local_Spec _
      | G.Imp.O_Local_Spec_And_Implem (_,_) as x -> Some x
  end

  module Values = struct
    type t = G.Imp.t_kind t_kind

    let mk_global = function
      | G.Imp.Abstract_Variable _
      | G.Imp.Concrete_Variable _
      | G.Imp.Parameter _
      | G.Imp.Abstract_Constant _ -> None
      | G.Imp.Concrete_Constant _
      | G.Imp.Abstract_Set _
      | G.Imp.Enumerate _
      | G.Imp.Concrete_Set (_,_) as x -> Some (Global x)

    let mk_local = function 
      | Local.L_Expr_Binder as x -> Local x
      | _ -> assert false

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

let mk_local (type a b) (cl:(a,b) clause) (ki:Local.t_local_kind) : b =
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

let mk_op (type a b c d e f) (cl: (a,b,c,d,e,f) sclause) readonly (ki:e) : f option =
  match cl with
  | MS_Operations -> Mch.Operations.mk_op readonly ki
  | RS_Operations ->  Ref.Operations.mk_op readonly ki
  | IS_Operations ->  Imp.Operations.mk_op readonly ki
  | IS_Local_Operations -> Imp.Local_Operations.mk_op readonly ki


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

let mch_src = function
  | G.Mch.Machine _ -> ""
  | G.Mch.Seen mch -> " from seen machine '"^mch.r_str^"'"
  | G.Mch.Used mch -> " from used machine '"^mch.r_str^"'"
  | G.Mch.Included mch -> " from included machine '"^mch.r_str^"'"

let mch_kind (ki:G.Mch.t_kind) : string =
  match ki with
  | Parameter (_,P_Machine _) -> "parameter"
  | Parameter (_,P_Seen mch) -> "parameter from seen machine '"^mch.r_str^"'"
  | Parameter (_,P_Used mch) -> "parameter from used machine '"^mch.r_str^"'"
  | Abstract_Variable src -> "abstract variable" ^ mch_src src
  | Abstract_Constant src -> "abstract constant" ^ mch_src src
  | Concrete_Variable src -> "concrete variable" ^ mch_src src
  | Concrete_Constant src -> "concrete constant" ^ mch_src src
  | Abstract_Set src -> "abstract set" ^ mch_src src
  | Concrete_Set (_,src) -> "concrete set" ^ mch_src src
  | Enumerate src -> "enumerate" ^ mch_src src

let ref_src = function
  | G.Ref.Machine _ -> ""
  | G.Ref.Seen mch -> " from the seen machine '"^mch.r_str^"'"
  | G.Ref.Included mch -> " from the included machine '"^mch.r_str^"'"
  | G.Ref.Refined -> " from the refined machine"

let ref_src2 = function
  | G.Ref.A_Redeclared_In_Machine _
  | G.Ref.A_Machine _ -> ""
  | G.Ref.A_Seen mch -> " from the seen machine '"^mch.r_str^"'"
  | G.Ref.A_Redeclared_In_Included mch
  | G.Ref.A_Included mch -> " from the included machine '"^mch.r_str^"'"
  | G.Ref.A_Refined -> " from the refined machine"

let ref_kind (ki:G.Ref.t_kind) : string = (*FIXME*)
  match ki with
  | Parameter (_,P_Machine _) -> "parameter"
  | Parameter (_,P_Seen mch) -> "parameter from seen machine '"^mch.r_str^"'"
  | Abstract_Variable src -> "abstract variable" ^ ref_src2 src
  | Abstract_Constant src -> "abstract constant" ^ ref_src2 src
  | Concrete_Variable src -> "concrete variable" ^ ref_src2 src
  | Concrete_Constant src -> "concrete constant" ^ ref_src2 src
  | Abstract_Set src -> "abstract set" ^ ref_src src
  | Concrete_Set (_,src) -> "concrete set" ^ ref_src src
  | Enumerate src -> "enumerate" ^ ref_src src

let imp_asrc = function
  | G.Imp.A_Seen mch -> " from the seen machine '"^mch.r_str^"'"
  | G.Imp.A_Redeclared_In_Imported mch
  | G.Imp.A_Imported mch -> " from the imported machine '"^mch.r_str^"'"
  | G.Imp.A_Refined -> " from the refined machine"

let imp_vsrc = function
  | G.Imp.V_Machine _
  | G.Imp.V_Redeclared_In_Machine _ -> ""
  | G.Imp.V_Seen mch -> " from the seen machine '"^mch.r_str^"'"
  | G.Imp.V_Redeclared_In_Imported mch
  | G.Imp.V_Imported mch -> " from the imported machine '"^mch.r_str^"'"
  | G.Imp.V_Refined -> " from the refined machine"

let imp_csrc = function
  | G.Imp.C_Machine _
  | G.Imp.C_Redeclared_In_Machine _ -> ""
  | G.Imp.C_Redeclared_In_Seen mch
  | G.Imp.C_Seen mch -> " from the seen machine '"^mch.r_str^"'"
  | G.Imp.C_Redeclared_In_Imported mch
  | G.Imp.C_Imported mch -> " from the imported machine '"^mch.r_str^"'"
  | G.Imp.C_Refined -> " from the refined machine"

let imp_ssrc = function
  | G.Imp.S_Machine _ -> ""
  | G.Imp.S_Redeclared_In_Seen mch
  | G.Imp.S_Seen mch -> " from the seen machine '"^mch.r_str^"'"
  | G.Imp.S_Redeclared_In_Imported mch
  | G.Imp.S_Imported mch -> " from the imported machine '"^mch.r_str^"'"
  | G.Imp.S_Refined -> " from the refined machine"

let imp_dsrc = function
  | G.Imp.D_Machine _ -> ""
  | G.Imp.D_Seen mch -> " from the seen machine '"^mch.r_str^"'"
  | G.Imp.D_Imported mch -> " from the imported machine '"^mch.r_str^"'"
  | G.Imp.D_Refined -> " from the refined machine"

let imp_kind (ki:G.Imp.t_kind) : string =
  match ki with
  | Parameter (_,P_Machine _) -> "parameter"
  | Parameter (_,P_Seen mch) -> "parameter from seen machine '"^mch.r_str^"'"
  | Abstract_Variable src -> "abstract variable" ^ imp_asrc src
  | Abstract_Constant src -> "abstract constant" ^ imp_asrc src
  | Concrete_Variable src -> "concrete variable" ^ imp_vsrc src
  | Concrete_Constant src -> "concrete constant" ^ imp_csrc src
  | Abstract_Set src -> "abstract set" ^ imp_ssrc src
  | Concrete_Set (_,src) -> "concrete set" ^ imp_dsrc src
  | Enumerate src -> "enumerate" ^ imp_dsrc src

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

let mch_op_src readonly = function
  | G.Mch.O_Machine _ -> ""
  | G.Mch.O_Seen mch -> " (" ^ (if readonly then "ro" else "rw") ^ ") from seen machine '"^mch.r_str^"'"
  | G.Mch.O_Used mch  -> " from used machine '"^mch.r_str^"'"
  | G.Mch.O_Included mch | G.Mch.O_Included_And_Promoted (_,mch) ->
    " from included machine '"^mch.r_str^"'"

let ref_op_src readonly = function
  | G.Ref.O_Refined | G.Ref.O_Refined_And_Machine _ -> ""
  | G.Ref.O_Seen mch -> " (" ^ (if readonly then "ro" else "rw") ^ ") from seen machine '"^mch.r_str^"'"
  | G.Ref.O_Included mch | G.Ref.O_Refined_And_Included mch
  | G.Ref.O_Refined_Included_And_Promoted (_,mch) ->
    " from included machine '"^mch.r_str^"'"

let imp_op_src readonly = function
  | G.Imp.O_Refined | G.Imp.O_Current_And_Refined _ -> ""
  | G.Imp.O_Seen mch -> " (" ^ (if readonly then "ro" else "rw") ^ ") from seen machine '"^mch.r_str^"'"
  | G.Imp.O_Imported mch | G.Imp.O_Imported_And_Refined mch | G.Imp.O_Imported_Promoted_And_Refined (mch,_) ->
    " from included machine '"^mch.r_str^"'"
  | G.Imp.O_Local_Spec _ | G.Imp.O_Local_Spec_And_Implem _ -> " (local)"

let error_op (type a b c d e f g) loc id (cl: (a,b,c,d,e,f) sclause) readonly (ki:e) : g =
  match cl with
  | MS_Operations -> Error.error loc ("The operation '"^id^"'"^(mch_op_src readonly ki)^" cannot be used here.")
  | RS_Operations ->  Error.error loc ("The operation '"^id^"'"^(ref_op_src readonly ki)^" cannot be used here.")
  | IS_Operations ->  Error.error loc ("The operation '"^id^"'"^(imp_op_src readonly ki)^" cannot be used here.")
  | IS_Local_Operations -> Error.error loc ("The operation '"^id^"'"^(imp_op_src readonly ki)^" cannot be used here.")
