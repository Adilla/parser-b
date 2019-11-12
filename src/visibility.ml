module G = Global

let extended_sees = ref false

module Mch = struct
  module Constraints = struct
    type t = private
      | Expr_Binder
      | Parameter of Utils.loc 

(*
    val mk_local : Local.t_local_kind -> t
    val mk_global : G.Mch.t_kind -> t
*)
  end
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

let mk_global (type a b) (cl:(a,b) clause) (ki:a) : b = assert false (*FIXME*)

let mk_local (type b) (cl:(_,b) clause) (ki:Local.t_local_kind) : b = assert false (*FIXME*)

type ('env_ki,'id_ki,'mut_ki,'assert_ki,'env_op_ki,'op_ki) sclause =
  | MS_Operations : (Global.Mch.t_kind,
                   Mch.Operations.t,
                   Mch.Operations.t_mut,
                   Mch.Assert.t,
                   Global.Mch.t_op_decl,
                   Mch.Operations.t_op) sclause
  | RS_Operations : (Global.Ref.t_kind,
                   Ref.Operations.t,
                   Ref.Operations.t_mut,
                   Ref.Assert.t,
                   Global.Ref.t_op_decl,
                   Ref.Operations.t_op) sclause
  | IS_Operations : (Global.Imp.t_kind,
                   Imp.Operations.t,
                   Imp.Operations.t_mut,
                   Imp.Assert.t,
                   Global.Imp.t_op_decl,
                   Imp.Operations.t_op) sclause
  | IS_Local_Operations : (Global.Imp.t_kind,
                   Imp.Local_Operations.t,
                   Imp.Local_Operations.t_mut,
                   Imp.Assert.t,
                   Global.Imp.t_op_decl,
                   Imp.Local_Operations.t_op) sclause

let mk_global_mut (type a b) (cl:(a,_,b,_,_,_) sclause) (ki:a) : b = assert false (*FIXME*)
let mk_local_mut (type a) (cl:(_,_,a,_,_,_) sclause) (ki:Local.t_local_kind) : a = assert false (*FIXME*)

let mk_op (type a b) (cl: (_,_,_,_,a,b) sclause) (ki:a) : b = assert false (*FIXME*)

let to_clause : type a b c d e f. (a,b,c,d,e,f) sclause -> (a,b) clause = function
  | MS_Operations -> M_Operations
  | RS_Operations -> R_Operations
  | IS_Operations -> I_Operations
  | IS_Local_Operations -> I_Local_Operations

let to_assert : ('env_ki,_,_,'assert_ki,_,_) sclause -> ('env_ki,'assert_ki) clause
