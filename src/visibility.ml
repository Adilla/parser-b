module G = Global

let extended_sees = ref false
    (*
## Parameter

|| CONSTRAINTS | INCLUDES/EXTENDS | PROPERTIES | INVARIANT | OPERATIONS
---------|-----|-----|-----|-----|---
Machine (D) | yes | yes | no  | yes | yes
Used (D)    | no  | no  | no  | yes | yes

## Abstract Set/Concrete Set/Enumerate/Concrete Constant/Abstract Constant

|| CONSTRAINTS | INCLUDES/EXTENDS | PROPERTIES | INVARIANT | OPERATIONS
---------|-----|-----|-----|-----|---
Machine (D) | no  | yes | yes | yes | yes
Seen    (E) | no  | yes | yes | yes | yes
Used    (E) | no  | no  | yes | yes | yes
Included (E) | no  | no  | yes | yes | yes

## Concrete Variable/Abstract Variable

|| CONSTRAINTS | INCLUDES/EXTENDS | PROPERTIES | INVARIANT | OPERATIONS
---------|-----|-----|-----|-----|---
Machine  (D) | no  | no  | no  | yes   | yes (rw)
Seen     (E) | no  | no  | no  | no (1)| yes (ro)
Used     (E) | no  | no  | no  | yes   | yes (ro)
Included (E) | no  | no  | no  | yes   | yes (ro)
       *)


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

    let mk_global = function
      | G.Mch.Parameter (k,l) -> Some (Parameter (k,l))
      | G.Mch.Concrete_Constant (Global.Mch.Machine l) -> Some (Concrete_Constant (Machine l))
      | G.Mch.Concrete_Constant (Global.Mch.Seen mch) -> Some (Concrete_Constant (Seen mch))
      | G.Mch.Abstract_Set (Global.Mch.Machine l) -> Some (Abstract_Set (Machine l))
      | G.Mch.Abstract_Set (Global.Mch.Seen mch) -> Some (Abstract_Set (Seen mch))
      | G.Mch.Concrete_Set (elts,Global.Mch.Machine l) -> Some (Concrete_Set (elts,Machine l))
      | G.Mch.Concrete_Set (elts,Global.Mch.Seen mch) -> Some (Concrete_Set (elts,Seen mch))
      | _ -> None

    let mk_local = function
      | Local.L_Expr_Binder -> Some Expr_Binder
      | _ -> None

  end

  module Assert = struct
    type t = unit
  end
  module Properties = struct
    type t = unit
  end
  module Invariant = struct
    type t = unit
  end
  module Operations = struct
    type t = unit
    type t_mut = unit
    type t_op = unit
  end

end

module Ref = struct
  module Includes = struct
    type t = unit
  end
  module Assert = struct
    type t = unit
  end
  module Properties = struct
    type t = unit
  end
  module Invariant = struct
    type t = unit
  end
  module Operations = struct
    type t = unit
    type t_mut = unit
    type t_op = unit
  end
end

module Imp = struct
  module Imports = struct
    type t = unit
  end
  module Assert = struct
    type t = unit
  end
  module Properties = struct
    type t = unit
  end
  module Invariant = struct
    type t = unit
  end
  module Operations = struct
    type t = unit
    type t_mut = unit
    type t_op = unit
  end
  module Local_Operations = struct
    type t = unit
    type t_mut = unit
    type t_op = unit
  end
  module Values = struct
    type t = unit
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

let mk_global (type a b) (_:(a,b) clause) (_:a) : b option = assert false (*FIXME*)

let mk_local (type b) (_:(_,b) clause) (_:Local.t_local_kind) : b option = assert false (*FIXME*)

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

let mk_global_mut (type a b) (_:(a,_,b,_,_,_) sclause) (_:a) : b = assert false (*FIXME*)
let mk_local_mut (type a) (_:(_,_,a,_,_,_) sclause) (_:Local.t_local_kind) : a = assert false (*FIXME*)

let mk_op (type a b) (_: (_,_,_,_,a,b) sclause) (_:a) : b = assert false (*FIXME*)

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

