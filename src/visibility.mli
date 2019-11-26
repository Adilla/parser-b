module G = Global
val extended_sees: bool ref

type 'a t_kind =
  | Local of Local.t_local_kind
  | Global of 'a

module Mch : sig

  module Constraints : sig
    type t = private G.Mch.t_kind t_kind
  end

  module Includes : sig
    type t = private G.Mch.t_kind t_kind
  end

  module Assert : sig
    type t = private G.Mch.t_kind t_kind
  end

  module Properties : sig
    type t = private G.Mch.t_kind t_kind
  end

  module Invariant : sig
    type t = private G.Mch.t_kind t_kind
  end

  module Operations : sig
    type t = private G.Mch.t_kind t_kind
    type t_mut = private G.Mch.t_kind t_kind
    type t_op = private G.Mch.t_op_source
  end
end

module Ref : sig

  module Includes : sig
    type t = private G.Ref.t_kind t_kind
  end

  module Assert : sig
    type t = private G.Ref.t_kind t_kind
  end

  module Properties : sig
    type t = private G.Ref.t_kind t_kind
  end

  module Invariant : sig
    type t = private G.Ref.t_kind t_kind
  end

  module Operations : sig
    type t = private G.Ref.t_kind t_kind
    type t_mut = private G.Ref.t_kind t_kind
    type t_op = private G.Ref.t_op_source
  end
end

module Imp : sig

  module Imports : sig
    type t = private G.Imp.t_kind t_kind
  end

  module Assert : sig
    type t = private G.Imp.t_kind t_kind
  end

  module Properties : sig
    type t = private G.Imp.t_kind t_kind
  end

  module Invariant : sig
    type t = private G.Imp.t_kind t_kind
  end

  module Operations : sig
    type t = private G.Imp.t_kind t_kind
    type t_mut = private G.Imp.t_kind t_kind
    type t_op = private G.Imp.t_op_source
  end

  module Local_Operations : sig
    type t = private G.Imp.t_kind t_kind
    type t_mut = private G.Imp.t_kind t_kind
    type t_op = private G.Imp.t_op_source
  end

  module Values : sig
    type t = private G.Imp.t_kind t_kind
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
val mk_local : (_,'id_ki) clause -> Local.t_local_kind -> 'id_ki

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
val mk_op : (_,_,_,_,'env_op_ki,'op_ki) sclause -> bool -> 'env_op_ki -> 'op_ki option

val to_clause : ('env_ki,'id_ki,_,_,_,_) sclause -> ('env_ki,'id_ki) clause
val to_assert : ('env_ki,_,_,'assert_ki,_,_) sclause -> ('env_ki,'assert_ki) clause

val error : Utils.loc -> string -> ('a,'b) clause -> 'a -> 'c
val error_op : Utils.loc -> string -> (_,_,_,_,'a,_) sclause -> bool -> 'a -> 'b
