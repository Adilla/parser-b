module G = Global

let extended_sees = ref false

type t_mch_prop = private A
type t_mch_inv = private B
type t_mch_op = private C
type t_ref_prop = private D
type t_ref_inv = private E
type t_ref_op = private F
type t_imp_prop = private G
type t_imp_inv = private H
type t_imp_op = private I
type t_imp_lop = private J
type t_imp_val = private K
type 'cl t_assert = private L

type ('mr,'cl) clause =
  | C_Mch_Prop : (G.t_mch,t_mch_prop) clause
  | C_Mch_Inv : (G.t_mch,t_mch_inv) clause
  | C_Mch_Op : (G.t_mch,t_mch_op) clause
  | C_Mch_Assert : (G.t_mch,t_mch_op t_assert) clause
  | C_Ref_Prop : (G.t_ref,t_ref_prop) clause
  | C_Ref_Inv : (G.t_ref,t_ref_inv) clause
  | C_Ref_Op : (G.t_ref,t_ref_op) clause
  | C_Ref_Assert : (G.t_ref,t_ref_op t_assert) clause
  | C_Imp_Prop : (G.t_ref,t_imp_prop) clause
  | C_Imp_Inv : (G.t_ref,t_imp_inv) clause
  | C_Imp_Op : (G.t_ref,t_imp_op) clause
  | C_Imp_Lop : (G.t_ref,t_imp_lop) clause
  | C_Imp_Val : (G.t_ref,t_imp_val) clause
  | C_Imp_Assert : (G.t_ref,t_imp_op t_assert) clause

let mk_assert_clause (type mr cl) (cl:(mr,cl) clause) : (mr,cl t_assert) clause =
  match cl with
  | C_Mch_Op -> C_Mch_Assert
  | C_Ref_Op -> C_Ref_Assert
  | C_Imp_Op -> C_Imp_Assert
  | _ -> assert false

type ('mr,_) t_global_ident = 'mr Global.t_kind
type ('mr,_) t_mutable_ident = 'mr Global.t_kind

let make_mch_prop : G.t_mch G.t_kind -> (G.t_mch,t_mch_prop) t_global_ident option = function
  | G.Pack(G.K_Abstract_Variable,_) -> None
  | G.Pack(G.K_Concrete_Variable,_) -> None
  | x -> Some x

let make_mch_inv (x:G.t_mch G.t_kind) : (G.t_mch,t_mch_inv) t_global_ident option =
  if !extended_sees then Some x
  else match x with
  | G.Pack(G.K_Abstract_Variable,G.D_Seen _) -> None
  | G.Pack(G.K_Concrete_Variable,G.D_Seen _) -> None
  | _ -> Some x

let make_mch_op (x:G.t_mch G.t_kind) : (G.t_mch,t_mch_op) t_global_ident option = Some x

let make_mch_mut: G.t_mch G.t_kind -> (G.t_mch,t_mch_op) t_mutable_ident option = function
  | G.Pack(G.K_Abstract_Variable,G.D_Machine _) as x -> Some x
  | G.Pack(G.K_Concrete_Variable,G.D_Machine _) as x -> Some x
  | _ -> None

let make_mch_assert (x:G.t_mch G.t_kind) : (G.t_mch,t_mch_op t_assert) t_global_ident option = Some x

let make_ref_prop: G.t_ref G.t_kind -> (G.t_ref,t_ref_prop) t_global_ident option = function
  | G.Pack(G.K_Abstract_Variable,_) -> None
  | G.Pack(G.K_Concrete_Variable,_) -> None
  | x -> Some x

let make_ref_inv (x:G.t_ref G.t_kind) : (G.t_ref,t_ref_inv) t_global_ident option =
 if !extended_sees then Some x
  else match x with
  | G.Pack(G.K_Abstract_Variable,G.D_Seen _) -> None
  | G.Pack(G.K_Concrete_Variable,G.D_Seen _) -> None
  | _ -> Some x

let make_ref_op: G.t_ref G.t_kind -> (G.t_ref,t_ref_op) t_global_ident option = function
  | G.Pack(G.K_Abstract_Variable,G.D_Disappearing) -> None
  | G.Pack(G.K_Abstract_Constant,G.D_Disappearing) -> None
  | x -> Some x

let make_ref_mut: G.t_ref G.t_kind -> (G.t_ref,t_ref_op) t_mutable_ident option = function
  | G.Pack(G.K_Abstract_Variable, G.D_Machine _) as x -> Some x
  | G.Pack(G.K_Abstract_Variable, G.D_Redeclared G.By_Machine _) as x -> Some x
  | G.Pack(G.K_Concrete_Variable, G.D_Machine _ ) as x -> Some x
  | G.Pack(G.K_Concrete_Variable, G.D_Redeclared G.By_Machine _) as x -> Some x
  | G.Pack(G.K_Concrete_Variable, G.D_Redeclared G.Implicitely) as x -> Some x
  | _ -> None

let make_ref_assert (x:G.t_ref G.t_kind) : (G.t_ref,t_ref_op t_assert) t_global_ident option = Some x

let make_imp_prop: G.t_ref G.t_kind -> (G.t_ref,t_imp_prop) t_global_ident option = function
  | G.Pack(G.K_Abstract_Variable,_) -> None
  | G.Pack(G.K_Concrete_Variable,_) -> None
  | x -> Some x 

let make_imp_inv (x:G.t_ref G.t_kind) : (G.t_ref,t_imp_inv) t_global_ident option =
  if !extended_sees then Some x
  else match x with
    | G.Pack(G.K_Abstract_Variable,G.D_Seen _) -> None
    | G.Pack(G.K_Concrete_Variable,G.D_Seen _) -> None
    | _ -> Some x

let make_imp_op: G.t_ref G.t_kind -> (G.t_ref,t_imp_op) t_global_ident option = function
  | G.Pack(G.K_Abstract_Variable,_) -> None
  | G.Pack(G.K_Abstract_Constant,_) -> None
  | x -> Some x

let make_imp_mut: G.t_ref G.t_kind -> (G.t_ref,t_imp_op) t_mutable_ident option = function
  | G.Pack(G.K_Concrete_Variable,G.D_Machine _) as x -> Some x
  | G.Pack(G.K_Concrete_Variable,G.D_Redeclared G.Implicitely) as x -> Some x
  | G.Pack(G.K_Concrete_Variable,G.D_Redeclared G.By_Machine _) as x -> Some x
  | _ -> None

let make_imp_assert (x:G.t_ref G.t_kind) : (G.t_ref,t_imp_op t_assert) t_global_ident option = Some x

let make_imp_val: G.t_ref G.t_kind -> (G.t_ref,t_imp_val) t_global_ident option = function
  | G.Pack(G.K_Concrete_Constant,_) as x -> Some x
  | G.Pack(G.K_Abstract_Set,_) as x -> Some x
  | G.Pack(G.K_Concrete_Set _,_) as x -> Some x
  | G.Pack(G.K_Enumerate,_) as x -> Some x
  | _ -> None

type t_imp_op_view =
  | IOV_Concrete_Variable of (G.t_ref,G.t_concrete) G.t_decl
  | IOV_Concrete_Constant of (G.t_ref,G.t_concrete) G.t_decl
  | IOV_Abstract_Set of (G.t_ref,G.t_concrete) G.t_decl
  | IOV_Concrete_Set of string list * (G.t_ref,G.t_concrete) G.t_decl
  | IOV_Enumerate of (G.t_ref,G.t_concrete) G.t_decl

type t_imp_val_view =
  | IVV_Concrete_Constant of (G.t_ref,G.t_concrete) G.t_decl
  | IVV_Abstract_Set of (G.t_ref,G.t_concrete) G.t_decl
  | IVV_Concrete_Set of string list * (G.t_ref,G.t_concrete) G.t_decl
  | IVV_Enumerate of (G.t_ref,G.t_concrete) G.t_decl

type t_imp_mut_view =
  | IMV_Concrete_Variable_From_Machine of Utils.loc
  | IMV_Concrete_Variable_Implicitely_Redeclared
  | IMV_Concrete_Variable_Redeclared_By_Machine of Utils.loc

let view_imp_op: (G.t_ref,t_imp_val) t_global_ident -> t_imp_op_view = function
  | G.Pack(G.K_Concrete_Variable,d) -> IOV_Concrete_Variable d
  | G.Pack(G.K_Concrete_Constant,d) -> IOV_Concrete_Constant d
  | G.Pack(G.K_Abstract_Set,d) -> IOV_Abstract_Set d
  | G.Pack(G.K_Concrete_Set elts,d) -> IOV_Concrete_Set (elts,d)
  | G.Pack(G.K_Enumerate,d) -> IOV_Enumerate d
  | _ -> assert false

let view_imp_val: (G.t_ref,t_imp_val) t_global_ident -> t_imp_val_view = function
  | G.Pack(G.K_Concrete_Constant,d) -> IVV_Concrete_Constant d
  | G.Pack(G.K_Abstract_Set,d) -> IVV_Abstract_Set d
  | G.Pack(G.K_Concrete_Set elts,d) -> IVV_Concrete_Set (elts,d)
  | G.Pack(G.K_Enumerate,d) -> IVV_Enumerate d
  | _ -> assert false

let view_imp_mut: (G.t_ref,t_imp_val) t_mutable_ident -> t_imp_mut_view = function
  | G.Pack(G.K_Concrete_Variable,G.D_Machine l) -> IMV_Concrete_Variable_From_Machine l
  | G.Pack(G.K_Concrete_Variable,G.D_Redeclared G.Implicitely) -> IMV_Concrete_Variable_Implicitely_Redeclared
  | G.Pack(G.K_Concrete_Variable,G.D_Redeclared G.By_Machine l) -> IMV_Concrete_Variable_Redeclared_By_Machine l
  | _ -> assert false

let make_imp_lop: G.t_ref G.t_kind -> (G.t_ref,t_imp_lop) t_global_ident option = function
  | G.Pack(G.K_Abstract_Variable,G.D_Disappearing) -> None
  | G.Pack(G.K_Abstract_Constant,G.D_Disappearing) -> None
  | x -> Some x

let make_imp_lmut: G.t_ref G.t_kind -> (G.t_ref,t_imp_lop) t_mutable_ident option = function
  | G.Pack(G.K_Concrete_Variable,G.D_Machine _) as x -> Some x
  | G.Pack(G.K_Concrete_Variable,G.D_Redeclared G.By_Machine _) as x-> Some x
  | G.Pack(G.K_Concrete_Variable,G.D_Redeclared G.Implicitely) as x-> Some x
  | G.Pack(G.K_Concrete_Variable,G.D_Included_Or_Imported _) as x-> Some x
  | G.Pack(G.K_Concrete_Variable,G.D_Redeclared G.By_Included_Or_Imported _) as x-> Some x
  | G.Pack(G.K_Abstract_Variable,G.D_Included_Or_Imported _) as x -> Some x
  | G.Pack(G.K_Abstract_Variable,G.D_Redeclared G.By_Included_Or_Imported _) as x -> Some x
  | _ -> None

let make_imp_lassert (x:G.t_ref G.t_kind) : (G.t_ref,t_imp_lop t_assert) t_global_ident option = Some x

let get_ident_in_clause (type mr cl) (cl:(mr,cl) clause) (ki:mr G.t_kind) : (mr,cl) t_global_ident option =
  match cl with
  | C_Mch_Prop -> make_mch_prop ki
  | C_Mch_Inv -> make_mch_inv ki
  | C_Mch_Op -> make_mch_op ki
  | C_Mch_Assert -> make_mch_assert ki
  | C_Ref_Prop -> make_ref_prop ki
  | C_Ref_Inv -> make_ref_inv ki
  | C_Ref_Op -> make_ref_op ki
  | C_Ref_Assert -> make_ref_assert ki
  | C_Imp_Prop -> make_imp_prop ki
  | C_Imp_Inv -> make_imp_inv ki
  | C_Imp_Op -> make_imp_op ki
  | C_Imp_Lop -> make_imp_lop ki
  | C_Imp_Val -> make_imp_val ki
  | C_Imp_Assert -> make_imp_assert ki

let get_mutable_in_clause (type mr cl) (cl:(mr,cl) clause) (ki:mr G.t_kind) : (mr,cl) t_mutable_ident option =
  match cl with
  | C_Mch_Op -> make_mch_mut ki
  | C_Ref_Op -> make_ref_mut ki
  | C_Imp_Op -> make_imp_mut ki
  | C_Imp_Lop -> make_imp_lmut ki
  | _ -> assert false
