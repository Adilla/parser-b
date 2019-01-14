open Codegen

module F : sig
  type t
  val atm : string -> t

  val box : ?sep:string -> t -> (int*int*t) list -> t
  val vbox : ?sep:string -> t -> (int*int*t) list -> t
(*   val hbox : ?sep:string -> t -> (int*int*t) list -> t *)
(*   val hvbox : ?sep:string -> t -> (int*int*t) list -> t *)

  val list : ?sp:int -> ?ind:int -> ?sep:string -> t list -> t
  val vlist : ?ind:int -> ?sep:string -> t list -> t
  val hlist : ?sp:int -> ?sep:string -> t list -> t
(*   val hvlist : ?sp:int -> ?ind:int -> ?sep:string -> t list -> t *)

  val parens : t -> t
  val surround : ?sp:bool -> string -> string -> t -> t

  val print : Format.formatter -> t -> unit
end = struct
  type box_type = HBox (*| HVBox*) | Box | VBox
  type t =
    | Atom of string
    | Box of box_type*string*t*(int*int*t) list
    | List of box_type*int*int*string*t list

  let atm s = Atom s

  let box ?sep:(s="") hd tl = Box(Box,s,hd,tl)
  let vbox ?sep:(s="") hd tl = Box(VBox,s,hd,tl)
(*   let hbox ?sep:(s="") hd tl = Box(HBox,s,hd,tl) *)
(*   let hvbox ?sep:(s="") hd tl = Box(HVBox,s,hd,tl) *)

  let list ?sp:(sp=1) ?ind:(i=0) ?sep:(s="") lst = List(Box,sp,i,s,lst)
  let vlist ?ind:(i=0) ?sep:(s="") lst = List(VBox,0,i,s,lst)
  let hlist ?sp:(sp=1) ?sep:(s="") lst = List(HBox,sp,0,s,lst)
(*   let hvlist ?sp:(sp=1) ?ind:(i=0) ?sep:(s="") lst = List(HVBox,sp,i,s,lst) *)

  let parens x = List(HBox,0,0,"",[Atom "(";x;Atom ")"])
  let surround ?sp:(sp=false) opn cl x =
    if sp then List(HBox,1,0,"",[Atom opn;x;Atom cl])
    else List(HBox,0,0,"",[Atom opn;x;Atom cl])

  let open_box (out:Format.formatter) = function
    | HBox -> Format.pp_open_hbox out ()
(*     | HVBox -> Format.pp_open_hvbox out 0 *)
    | Box -> Format.pp_open_box out 0
    | VBox -> Format.pp_open_vbox out 0

  let rec print out = function
    | Atom s -> Format.fprintf out "%s" s
    | Box (typ,sep,hd,tl) ->
      begin
        open_box out typ;
        print out hd;
        List.iter (fun (sp,ind,x) ->
            Format.pp_print_string out sep;
            Format.pp_print_break out sp ind;
            print out x
          ) tl;
        Format.pp_close_box out ()
      end
    | List (_,_,_,_,[]) -> ()
    | List (typ,sp,ind,sep,hd::tl) ->
      begin
        open_box out typ;
        print out hd;
        List.iter (fun x ->
            Format.pp_print_string out sep;
            Format.pp_print_break out sp ind;
            print out x
          ) tl;
        Format.pp_close_box out ()
      end
end

let b0_constant_to_string : t_b0_constant -> string = function
  | B0_Integer i -> Int64.to_string i
  | B0_String s -> "\"" ^ s ^ "\""
  | B0_MaxInt -> "std::i32::MAX"
  | B0_MinInt -> "std::i32::MIN"
  | B0_True -> "true"
  | B0_False -> "false"

let b0_binary_op_to_string : t_b0_binary_op -> string = function
  | B0_Conjonction -> "&&"
  | B0_Disjonction -> "||"
  | B0_Equality -> "=="
  | B0_Disequality -> "!="
  | B0_Inequality Smaller_or_Equal -> "<="
  | B0_Inequality Strictly_Smaller -> "<"
  | B0_Inequality Greater_or_Equal -> ">="
  | B0_Inequality Strictly_Greater -> ">"
  | B0_Product -> "*"
  | B0_Difference -> "-"
  | B0_Addition -> "+"
  | B0_Division -> "/"
  | B0_Modulo -> "%"
  | B0_Power -> assert false

let b0_unary_op_to_string : t_b0_unary_op -> string = function
  | B0_Negation -> "!"
  | B0_Minus -> "-"

let reserved_list = [
    "as"; "break"; "const"; "continue"; "crate"; "else"; "enum"; "extern"; "false";
    "fn"; "for"; "if"; "impl"; "in"; "let"; "loop"; "match"; "mod"; "move"; "mut";
    "pub"; "ref"; "return"; "self"; "Self"; "static"; "struct"; "super"; "trait";
    "true"; "type"; "unsafe"; "use"; "where"; "while"; "abstract"; "become"; "box";
    "do"; "final"; "macro"; "override"; "priv"; "typeof"; "unsized"; "virtual";
    "yield"; "union"; "dyn";
    "state" (*state is not a rust keyword but used in the translation*)
  ]

module SSet = Set.Make(String)
let reserved_set = List.fold_left (fun x y -> SSet.add y x) SSet.empty reserved_list

let is_valid (s:string) : bool =
  let reg = Str.regexp {|\([a-zA-Z][a-zA-Z0-9_]*\)\|\(_[a-zA-Z0-9_]+\)$|} in
  Str.string_match reg s 0

let get_rust_ident (lc:Utils.loc) (s:string) : string =
  if SSet.mem s reserved_set then "_" ^ s 
  else if is_valid s then s
  else Error.raise_exn lc ("The string '"^s^"' is not a valid rust identifier.")

let pkg_to_rust_ident (x:t_pkg_id) : string =
  let lid = pkg_to_lident x in
  get_rust_ident lid.lid_loc lid.lid_str

let ident_to_rust_ident (x:ident) : string option =
  try Some (get_rust_ident Utils.dloc (ident_to_string x))
  with Error.Error _ -> None

let id_to_rust_ident (x:t_id) : string =
  let lid = id_to_lident x in
  get_rust_ident lid.lid_loc lid.lid_str

let id2_to_rust_ident (x:t_id_2) : string =
  match x with
  | Intern id -> id_to_rust_ident id
  | Extern (mch,id) ->
    begin match ident_to_rust_ident id with
      | None ->
        let lid = pkg_to_lident mch in
        Error.raise_exn lid.lid_loc
                  ("The string '"^ident_to_string id^"' from machine '"^
                   lid.lid_str^"' is not a valid rust identifier.")
      | Some id -> id
    end

let type_id_to_rust_ident (t:t_type_id) : (string,string) result =
  match t.t_nspace with
  | None ->
    begin match ident_to_rust_ident t.t_id with
      | None -> Error (ident_to_string t.t_id)
      | Some s -> Ok s
    end
  | Some ns ->
    begin match ident_to_rust_ident ns, ident_to_rust_ident t.t_id with
      | Some ns, Some id -> Ok (ns ^ "::" ^ id)
      | _, _ -> Error (ident_to_string ns ^ "::" ^ ident_to_string t.t_id)
    end

let qident_to_string (ts:qident) : string =
  match ts.q_nspace with
  | None -> id_to_rust_ident ts.q_id
  | Some ns -> (pkg_to_rust_ident ns) ^ "::" ^ (id_to_rust_ident ts.q_id)

let rec is_copy_type = function
  | T_Int | T_Bool | T_Abstract _ | T_Enum _ -> true
  | T_String | T_Array _ -> false
  | T_Record lst -> List.for_all (fun (_,ty) -> is_copy_type ty) lst

let rec is_lvalue e =
  match e.exp0_desc with
  | B0_Local_Ident _ | B0_Global_Ident _ -> true
  | B0_Array_Access (e,_) | B0_Record_Access (e,_) -> is_lvalue e
  | B0_Builtin_0 _
  | B0_Builtin_1 _
  | B0_Builtin_2 _
  | B0_Array _
  | B0_Array_Init _
  | B0_Record _ -> false

let rec mk_expr (paren:bool) (e0:t_b0_expr) : F.t =
  let add_paren_if_true (b:bool) (x:F.t) : F.t = if b then F.parens x else x in
  match e0.exp0_desc with
  | B0_Global_Ident (id,IK_Variable (Some pkg)) ->
    F.atm (Format.sprintf "%s::_get_%s(state)" (pkg_to_rust_ident pkg) (id_to_rust_ident id))
  | B0_Global_Ident (id,IK_Variable None) ->
    F.atm (Format.sprintf "_get_%s(state)" (id_to_rust_ident id))
  | B0_Global_Ident(id,IK_Enum (Some pkg)) ->
    F.atm (Format.sprintf "%s::%s" (pkg_to_rust_ident pkg) (id_to_rust_ident id))
  | B0_Global_Ident(id,(IK_Constant (Some pkg))) ->
    if is_copy_type e0.exp0_type then
      F.atm (Format.sprintf "%s::%s" (pkg_to_rust_ident pkg) (id_to_rust_ident id))
    else add_paren_if_true paren
        (F.atm (Format.sprintf "*(%s::%s)" (pkg_to_rust_ident pkg) (id_to_rust_ident id)))
  | B0_Global_Ident(id,IK_Enum None) -> F.atm (id_to_rust_ident id)
  | B0_Local_Ident(id,Local.L_Param_In)
  | B0_Global_Ident(id,IK_Constant None) ->
    if is_copy_type e0.exp0_type then F.atm (id_to_rust_ident id)
    else add_paren_if_true paren (F.atm (Format.sprintf "*%s" (id_to_rust_ident id)))
  | B0_Local_Ident(id,(Local.L_Expr_Binder|Local.L_Subst_Binder|Local.L_Param_Out)) ->
    F.atm (id_to_rust_ident id)
  | B0_Builtin_0 cst -> F.atm (b0_constant_to_string cst)
  | B0_Builtin_1 (op,e) -> (*rmk: only boolean/integer operations*)
    add_paren_if_true paren
      (F.box (F.atm (b0_unary_op_to_string op)) [(0,4,mk_expr true e)])
  | B0_Builtin_2 (B0_Power,e1,e2) ->
    F.box (F.atm "i32::pow")
      [(0,4,F.parens (F.list ~sep:"," [mk_expr false e1; mk_expr false e2]))]
  | B0_Builtin_2 (op,e1,e2) -> (*rmk: only boolean/integer operations*)
    add_paren_if_true paren
      (F.list [mk_expr true e1;F.atm (b0_binary_op_to_string op); mk_expr true e2])
  | B0_Array lst ->
    F.surround "vec![" "]"
      (F.list ~sep:"," (List.map (mk_movable false) lst))
  | B0_Array_Init (rg,def) ->
    let rec aux = function
      | [] -> mk_movable false def
      | hd::tl ->
        F.surround "vec![" "]"
          (F.list ~sep:";" [ aux tl; F.surround "(" ") as usize" (mk_range hd) ])
    in
    aux (Nlist.to_list rg)
  | B0_Array_Access (f,args) ->
    let aux arg = F.surround "[(" ") as usize]" (mk_expr false arg) in
    F.box (mk_expr true f)
      [(0,4,F.list (List.map aux (Nlist.to_list args)))]
  | B0_Record lst ->
    let cmp (x,_) (y,_) = String.compare (id_to_rust_ident x) (id_to_rust_ident y) in
    let lst = List.fast_sort cmp lst in
    F.parens (F.list ~sep:"," (List.map (fun (_,x) -> mk_movable false x) lst))
  | B0_Record_Access (e,fd) ->
    let pos = match e.exp0_type with
      | T_Record lst ->
        let aux (x,_) (y,_) =
          String.compare (Codegen.ident_to_string x) (Codegen.ident_to_string y)
        in
        let lst = List.fast_sort aux lst in
        let fd = id_to_rust_ident fd in
        let rec find_pos i = function
          | [] -> assert false
          | (x,_)::tl ->
            if String.equal (Codegen.ident_to_string x) fd then i
            else find_pos (i+1) tl
        in
        find_pos 0 lst
      | _ -> assert false
    in
    F.box (mk_expr true e) [(0,4,F.atm (Format.sprintf ".%i" pos))]

and mk_movable (paren:bool) (e:t_b0_expr) : F.t =
  if (not (is_copy_type e.exp0_type)) && is_lvalue e then
    F.box (mk_expr true e) [(0,4,F.atm ".clone()")]
  else
    mk_expr paren e

and mk_range : t_b0_range -> F.t = function
  | R_Interval (_,y) -> mk_expr false y (*rmk:lower bound not taken into account*)
  | R_Concrete_Set qid -> F.atm (Format.sprintf "%s_size" (qident_to_string qid))
  
let mk_arg (e:t_b0_expr) : F.t =
  if is_copy_type e.exp0_type then mk_expr false e
  else
    match e.exp0_desc with
    | B0_Local_Ident (id,Local.L_Param_In)
    | B0_Global_Ident (id,IK_Constant None) -> F.atm (id_to_rust_ident id)
    | B0_Global_Ident (id,IK_Constant (Some pkg)) ->
      (F.atm (Format.sprintf "&%s::%s" (pkg_to_rust_ident pkg) (id_to_rust_ident id)))
    | _ -> F.surround "&(" ")" (mk_expr false e)

type tts = I of Utils.loc | E of t_pkg_id

let rec b0_type_to_string_exn (lc:tts) (ty:t_b0_type) : string =
  match ty with
  | T_Int -> "i32"
  | T_Bool -> "bool"
  | T_String -> "&str"
  | T_Abstract ts
  | T_Enum ts ->
   begin match type_id_to_rust_ident ts with
     | Error s ->
       begin match lc with
         | I lc ->
           Error.raise_exn lc
             ("The type of this expression contains the invalid rust identifier '"^ s ^"'.")
         | E pkg ->
           let pkg = pkg_to_lident pkg in
           Error.raise_exn pkg.lid_loc
             ("Some expression in the machine '"^pkg.lid_str
              ^"' contains the invalid rust identifier '"^ s ^"'.")
       end
     | Ok s -> s
   end
  | T_Array (_,ty) ->
    "Vec<" ^ b0_type_to_string_exn lc ty ^ ">"
  | T_Record lst ->
    let aux2 (x,_) (y,_) =
      String.compare (Codegen.ident_to_string x) (Codegen.ident_to_string y)
    in
    let lst = List.fast_sort aux2 lst in
    let lst = List.map (fun (_,ty) -> b0_type_to_string_exn lc ty) lst in
    "(" ^ String.concat "," lst ^ ")"

let rec mk_default_value (ty:t_b0_type) : F.t =
  match ty with
  | T_Int -> F.atm "0"
  | T_String -> F.atm "\"\""
  | T_Bool -> F.atm "true"
  | T_Enum _ | T_Abstract _ -> F.atm "Default::default()"
  | T_Array (_,_) -> F.atm "vec![]"
  | T_Record lst ->
    let aux (x,_) (y,_) =
      String.compare (Codegen.ident_to_string x) (Codegen.ident_to_string y)
    in
    let lst = List.fast_sort aux lst in
    F.surround "(" ")" (F.list ~sep:"," (List.map (fun (_,ty) -> mk_default_value ty) lst))

let id_eq (x:t_id) (y:t_id) : bool =
  String.equal (id_to_lident x).lid_str (id_to_lident y).lid_str

let rec is_used_e (x:t_id) (e:t_b0_expr) : bool =
  match e.exp0_desc with
  | B0_Local_Ident (y,_) -> id_eq x y
  | B0_Global_Ident _ -> false
  | B0_Builtin_0 _ -> false
  | B0_Builtin_1 (_,e) -> is_used_e x e
  | B0_Builtin_2 (_,e1,e2) -> (is_used_e x e1) || (is_used_e x e2)
  | B0_Array_Access (f,args) -> (is_used_e x f) || (Nlist.exists (is_used_e x) args)
  | B0_Array lst -> List.exists (is_used_e x) lst
  | B0_Array_Init (nle,e) -> (Nlist.exists (is_used_rg x) nle) || (is_used_e x e)
  | B0_Record lst -> List.exists (fun (_,e) -> is_used_e x e) lst
  | B0_Record_Access (e,_) -> is_used_e x e

and is_used_rg (x:t_id) (rg:t_b0_range) : bool =
  match rg with
  | R_Interval (e1,e2) -> (is_used_e x e1) || (is_used_e x e2)
  | R_Concrete_Set _ -> false

let is_used_lhs (x:t_id) (lhs:t_b0_lhs) : bool =
  match lhs with
  | LHS_Variable (y,_) -> id_eq x y
  | LHS_Array (y,_,args) -> (id_eq x y) || (Nlist.exists (is_used_e x) args)
  | LHS_Record (y,_,_) -> id_eq x y

type t_assign = Assigned | Used | Not_Used

let occurs = function
  | Not_Used -> false
  | Used | Assigned -> true

let rec is_used_before_assignement (id:t_id) (s:t_b0_subst) : t_assign =
  let assign = function
    | B0_Affectation (LHS_Variable(y,_),_) -> id_eq id y
    | B0_CallUp (out,_,_) -> List.exists (fun (_,y) -> id_eq id y) out
    | _ -> false
  in
  match s with
  | [] -> Not_Used
  | hd::tl ->
    if assign hd.sub0_desc then Assigned
    else
        match is_used_before_assignement_atm id hd with
          | Used -> Used
          | Assigned -> Assigned
          | Not_Used -> is_used_before_assignement id tl

and is_used_before_assignement_atm (id:t_id) (s:t_b0_subst_atomic) : t_assign =
  match s.sub0_desc with
  | B0_Affectation (lhs,e) ->
    if (is_used_lhs id lhs) || (is_used_e id e) then Used
    else Not_Used
  | B0_IfThenElse (cases,def) ->
    if (Nlist.exists (fun (c,s) -> is_used_e id c || occurs (is_used_before_assignement id s)) cases)
    || (match def with None -> false | Some s -> occurs (is_used_before_assignement id s))
    then Used
    else Not_Used
  | B0_Case (e,cases,def) ->
    if (is_used_e id e)
    || (Nlist.exists (fun (_,s) -> occurs (is_used_before_assignement id s)) cases)
    || (match def with None -> false | Some s -> occurs (is_used_before_assignement id s))
    then Used
    else Not_Used
  | B0_Var (_,s) -> is_used_before_assignement id s
  | B0_While (cond,s) ->
    if (is_used_e id cond) || (occurs (is_used_before_assignement id s)) then Used
    else Not_Used
  | B0_CallUp (_,_,args) ->
    if List.exists (is_used_e id) args then Used
    else Not_Used

let rec mk_subst (mch_name:string) (s0:t_b0_subst) : F.t =
  match s0 with
  | [] -> F.atm "{};"
  | _::_ -> F.vlist (List.map (mk_atomic_subst mch_name) s0)

and mk_lhs (mch_name:string) (lhs:t_b0_lhs) : F.t =
  match lhs with
  | LHS_Variable (x,MIK_Variable) ->
    F.atm (Format.sprintf "state.%s.%s" mch_name (id_to_rust_ident x))
  | LHS_Variable (x,(MIK_Param|MIK_Local)) ->
    F.atm (Format.sprintf "%s" (id_to_rust_ident x))
  | LHS_Array(f,MIK_Variable,args) ->
    let aux e = F.surround "[(" ") as usize]" (mk_expr false e) in
    F.box
      (F.atm (Format.sprintf "state.%s.%s" mch_name (id_to_rust_ident f)))
      [(0,4,F.list (List.map aux (Nlist.to_list args)))]
  | LHS_Array(f,(MIK_Param|MIK_Local),args) ->
    let aux e = F.surround "[(" ") as usize]" (mk_expr false e) in
    F.box (F.atm (id_to_rust_ident f))
      [(0,4,F.list (List.map aux (Nlist.to_list args)))]
  | LHS_Record(rd,MIK_Variable,fd) ->
    F.atm (Format.sprintf "state.%s.%s.%s" mch_name
             (id_to_rust_ident rd) (id_to_rust_ident fd))
  | LHS_Record(rd,(MIK_Param|MIK_Local),fd) ->
    F.atm (Format.sprintf "%s.%s" (id_to_rust_ident rd) (id_to_rust_ident fd))

and mk_atomic_subst (mch_name:string) (s0:t_b0_subst_atomic) : F.t =
  match s0.sub0_desc with
  | B0_Affectation (lhs,e) ->
    F.box (mk_lhs mch_name lhs) [(1,4,F.atm "=");(1,4,mk_movable false e);(0,0,F.atm ";")]
  | B0_IfThenElse (cases,def) ->
    let rec aux = function
      | [] -> []
      | (p,s)::tl ->
        (1,0,F.surround ~sp:true "} else if" "{" (mk_expr false p))::(1,4,mk_subst mch_name s)::(aux tl)
    in
    let def = match def with
      | None -> [(1,0,F.atm "}")]
      | Some s -> [(1,0,F.atm "} else {");(1,4,mk_subst mch_name s);(1,0,F.atm "}")]
    in
    let (p_if,s_if) = Nlist.hd cases in
    F.vbox
      (F.surround ~sp:true "if" "{" (mk_expr false p_if))
      ((1,4,mk_subst mch_name s_if)::(aux (Nlist.tl cases))@def)
  | B0_Case (e,cases,def) ->
    let def = match def with
      | None -> F.atm "_ => {}"
      | Some def ->
        F.box (F.atm "_ => {") [(1,4,mk_subst mch_name def);(1,0,F.atm "}")] 
    in
    let mk_cs = function
      | CS_Int e -> F.atm (Int64.to_string e)
      | CS_Bool true -> F.atm "true"
      | CS_Bool false -> F.atm "false"
      | CS_Constant qid | CS_Enum qid -> F.atm (qident_to_string qid)
    in
    let cases = List.map (fun (lst,s) ->
        F.box (F.surround "" "=> {" (F.list ~sep:"|" (List.map mk_cs (Nlist.to_list lst))))
                 [(1,4,mk_subst mch_name s); (1,0,F.atm "}")]
      ) (Nlist.to_list cases)
    in
    F.vbox
      (F.list [F.atm "match";mk_expr false e;F.atm "{"])
      [(1,4,F.vlist (cases@[def])); (1,0,F.atm "}")]
  | B0_Var (vars,s) ->
    let aux (id,ty) : F.t =
      match is_used_before_assignement id s with
      | Used | Not_Used ->
        F.box (F.atm (Format.sprintf "let mut %s : %s =" (id_to_rust_ident id)
                        (b0_type_to_string_exn (I(id_to_lident id).lid_loc) ty)))
          [(1,4,F.surround "" ";" (mk_default_value ty))]
      | Assigned ->
        F.atm (Format.sprintf "let mut %s : %s;" (id_to_rust_ident id)
          (b0_type_to_string_exn (I(id_to_lident id).lid_loc) ty))
    in
    F.vlist ((List.map aux (Nlist.to_list vars))@[mk_subst mch_name s])
  | B0_While (p,s) ->
    F.vbox
      (F.surround ~sp:true "while" "{" (mk_expr false p))
      [(1,4,mk_subst mch_name s);(1,0,F.atm "}")]
  | B0_CallUp ([],f,args) ->
    F.box (F.atm (qident_to_string f))
      [(0,4,F.surround "(" ");" (F.list ~sep:"," ((List.map mk_arg args)@[F.atm "state"])))]
  | B0_CallUp ([(is_var,r)],f,args) ->
    let lhs =
      match is_var with
      | MIK_Variable -> F.atm (Format.sprintf "state.%s.%s" mch_name (id_to_rust_ident r))
      | MIK_Local | MIK_Param -> F.atm (id_to_rust_ident r)
    in
    let rhs =
      F.box (F.atm (qident_to_string f))
        [(0,4,F.surround "(" ");" (F.list ~sep:"," ((List.map mk_arg args)@[F.atm "state"])))]
    in
    F.box (F.hlist [lhs;F.atm "="]) [(1,4,rhs)]
  | B0_CallUp (ret,f,args) ->
    let rhs =
      F.box (F.atm (qident_to_string f))
        [(0,4,F.surround "(" ");" (F.list ~sep:"," ((List.map mk_arg args)@[F.atm "state"])))]
    in
    F.vlist (
      (F.box (F.atm "let _x =") [(1,4,rhs)])::
      (List.mapi (fun i (is_var,x) ->
           match is_var with
           | MIK_Variable ->
             F.atm (Format.sprintf "state.%s.%s = _x.%s;"
                      mch_name (id_to_rust_ident x) (string_of_int i))
           | MIK_Local | MIK_Param ->
             F.atm (Format.sprintf "%s = _x.%s;" (id_to_rust_ident x) (string_of_int i))
         ) ret ))

let loc_from_id2 (x:t_id_2) : Utils.loc =
  match x with
  | Intern id -> (id_to_lident id).lid_loc
  | Extern (mch,_) -> (pkg_to_lident mch).lid_loc

let mk_type (ty:t_type) : F.t =
  match ty.ty_def with
  | D_Alias (pkg,id) ->
    begin match ident_to_rust_ident id with
      | None ->
        let pkg = pkg_to_lident pkg in
        Error.raise_exn pkg.lid_loc
          ("The type '"^ident_to_string id^"' from machine '"^pkg.lid_str^
           "' is not a valid rust identifier.")
      | Some id ->
        F.atm (Format.sprintf "pub type %s = %s::%s;"
                 (id2_to_rust_ident ty.ty_name) (pkg_to_rust_ident pkg) id)
    end
  | D_Int -> F.atm (Format.sprintf "pub type %s = i32;" (id2_to_rust_ident ty.ty_name))
  | D_Enum elts ->
    F.vlist 
      ((F.atm (Format.sprintf "pub type %s = i32;" (id2_to_rust_ident ty.ty_name)))::
       (F.atm (Format.sprintf "pub const %s_size : i32 = %s;"
                 (id2_to_rust_ident ty.ty_name) (string_of_int (List.length elts))))::
       (List.mapi (fun i e ->
            match ident_to_rust_ident e with
            | None ->
              Error.raise_exn (loc_from_id2 ty.ty_name)
                  ("The string '"^ident_to_string e^"' is not a valid rust identifier.")
            | Some e ->
              F.atm (Format.sprintf "pub const %s : %s = %s;"
                       e (id2_to_rust_ident ty.ty_name) (string_of_int i))
          )  elts))

let mk_const (c:t_constant) : F.t =
  match c with
  | { c_init=Promoted mch; _} ->
    F.atm (Format.sprintf "pub use %s::%s;" (pkg_to_rust_ident mch) (id2_to_rust_ident c.c_name))
  | { c_init=Init c_init; _ } ->
    let lc = match c.c_name with
      | Intern id -> I (id_to_lident id).lid_loc
      | Extern (pkg,_) -> E pkg
    in
    if not (is_copy_type c_init.exp0_type) then
      F.surround "lazy_static! {" "}"
        (F.box
           (F.atm (Format.sprintf "pub static ref %s : %s ="
                     (id2_to_rust_ident c.c_name) (b0_type_to_string_exn lc c.c_type)))
           [(1,4,F.surround "" ";" (mk_movable false c_init))])
    else
      F.box 
        (F.list [F.atm "pub const";
                F.atm (id2_to_rust_ident c.c_name);
                F.atm ":";
                F.atm (b0_type_to_string_exn lc c.c_type);
                F.atm "="])
        [(1,4,F.list [mk_expr false c_init; F.atm ";"])]

let mk_arg_in (a:t_arg) : F.t =
  let ref = if is_copy_type a.arg_type then "" else "&" in
  let lc = match a.arg_name with
    | Intern id -> I (id_to_lident id).lid_loc
    | Extern (pkg,_) -> E pkg
  in
  F.box (F.atm (Format.sprintf "%s:" (id2_to_rust_ident a.arg_name)))
    [(1,4,F.atm (Format.sprintf "%s%s" ref (b0_type_to_string_exn lc a.arg_type)))]

let mk_arg_out (a:t_arg) : F.t =
  let lc = match a.arg_name with
    | Intern id -> I (id_to_lident id).lid_loc
    | Extern (pkg,_) -> E pkg
  in
  F.atm (b0_type_to_string_exn lc a.arg_type)

let mk_ret_id (a:t_arg) : F.t = F.atm (id2_to_rust_ident a.arg_name)

let mk_ret_decl (a:t_arg) : F.t =
  let lc = match a.arg_name with
    | Intern id -> I (id_to_lident id).lid_loc
    | Extern (pkg,_) -> E pkg
  in
  F.box 
    (F.atm ("let mut "^id2_to_rust_ident a.arg_name ^ ":"))
    [ (1,4,F.atm (b0_type_to_string_exn lc a.arg_type ^ " ="));
      (1,4,F.surround "" ";" (mk_default_value a.arg_type)) ]

let mk_proc_body (mch_name:string) (p:t_procedure) : F.t =
  match p.p_body with
  | Body s ->
    let args_in = List.map mk_arg_in p.p_args_in in
    let args_out = List.map mk_arg_out p.p_args_out in	  
    let vars = List.map mk_ret_decl p.p_args_out in
    let return : F.t =
      match p.p_args_out with
      | [] -> F.atm "return ()"
      | [xx] -> F.box (F.atm "return") [(1,4,mk_ret_id xx)]
      | _::_ -> F.box (F.atm "return") [(1,4,F.surround "(" ")" (F.list ~sep:"," (List.map mk_ret_id p.p_args_out)))]
    in
    let params =
      F.list [
        F.parens (F.list ~sep:"," (args_in@[F.atm "state:&mut state::State"]));
        F.atm "->";
        F.parens (F.list ~sep:"," args_out) ]
    in
    F.vbox
      (F.atm ("pub fn " ^ id_to_rust_ident p.p_name))
      [ (0,4,params);
        (0,0,F.atm "{");
        (match vars, s with
         | [], [] -> (0,4,return)
         | _::_, [] -> (0,4,F.vlist (vars@[return]));
         | _, _::_ -> (0,4,F.vlist (vars@[mk_subst mch_name s;return])));
        (0,0,F.atm "}")]
  | Renames ts -> F.box (F.atm "pub use") [(1,4,F.atm (qident_to_string ts ^ ";"))]

let mk_init (mch_name:string) (init:t_b0_subst option) : F.t = (*FIXME generer aussi un init global*)
  let s = match init with
    | None -> []
    | Some s -> s
  in
  mk_proc_body mch_name
    { p_name = Codegen.make_id {lid_loc=Utils.dloc;lid_str="init"};
      p_is_local = false;
      p_args_in = [];
      p_args_out = [];
      p_body = Body s
    }

let mk_var (mch_name:string) (v:t_variable) : F.t =
  let (name,tts) = match v.v_name with
    | Intern n -> (id_to_rust_ident n,I (id_to_lident n).lid_loc)
    | Extern (pkg,n) -> (ident_to_string n,E pkg)
  in
  let params =
    F.list ~sep:"->" [
        F.parens (F.atm "state:&mut state::State");
        F.parens (F.atm (b0_type_to_string_exn tts v.v_type)) ]
  in
  F.vbox
    (F.atm ("pub fn _get_" ^ name))
    [ (0,4,params);
      (0,0,F.atm "{");
      (0,4,match v.v_promoted_from with
       | None -> F.atm (Format.sprintf "state.%s.%s" mch_name name)
       | Some pkg -> F.atm (Format.sprintf "%s::_get_%s(state)" (pkg_to_lident pkg).lid_str name));
      (0,0,F.atm "}")]

let mk_package (mch_name:string) (pkg:t_package) : F.t =
  F.vlist [
    F.atm "#![allow(bad_style, unused_mut, unused_variables)]";
    F.atm "use super::*;";
    F.atm "/* Types */";
    F.vlist (List.map mk_type pkg.pkg_types);
    F.atm "/* Constants */";
    F.vlist (List.map mk_const pkg.pkg_constants);
    F.atm "/* Variables */";
    F.vlist (List.map (mk_var mch_name) pkg.pkg_variables);
    F.atm "/* Initialisation */";
    mk_init mch_name pkg.pkg_init;
    F.atm "/* Operations */";
    F.vlist (List.map (mk_proc_body mch_name) pkg.pkg_procedures)
  ]

let print_package (out:out_channel) (mch_name:string) (pkg:t_package) : unit Error.t_result =
  let out = Format.formatter_of_out_channel out in
  try Ok ( F.print out (mk_package mch_name pkg); Format.pp_print_flush out () )
  with Error.Error err -> Error err

let not_promoted (v:t_variable) : t_variable option =
  match v.v_promoted_from with
  | Some _ -> None
  | None -> Some v

let mk_var (v:t_variable) : F.t =
  let lc = match v.v_name with
    | Intern id -> I (id_to_lident id).lid_loc
    | Extern (pkg,_) -> E pkg
  in
  F.box (F.atm (Format.sprintf "pub %s:" (id2_to_rust_ident v.v_name)))
           [(1,4,F.atm (b0_type_to_string_exn lc v.v_type))]

let mk_mch_state (name,lst:string*t_variable list) : F.t option =
  match Utils.filter_map not_promoted lst with
  | [] -> None
  | (_::_) as lst ->
    Some (
      F.vlist [
        F.atm "#[derive(Default)]";
        F.surround (Format.sprintf "pub struct State_%s {" name) "}"
          (F.vlist ~sep:"," (List.map mk_var lst))
      ])

let mk_state_field (name,lst:string*t_variable list) : F.t =
  let is_promoted v = match v.v_promoted_from with Some _ -> true | None -> false in
  if List.for_all is_promoted lst then
    F.atm (Format.sprintf "/* No variable in machine %s */" name)
  else
    F.atm (Format.sprintf "pub %s : State_%s ," name name)

let mk_state (vars:(string*t_variable list) list) : F.t =
  F.vlist [
    F.atm "#![allow(bad_style)]";
    F.atm "use super::*;";
    F.vlist (Utils.filter_map mk_mch_state vars);
    F.atm "#[derive(Default)]";
    F.surround "pub struct State {" "}"
      (F.vlist (List.map mk_state_field vars))
  ]

let mk_global_init (lst:(string*_) list) =
  let params = F.list [ F.atm "()"; F.atm "->"; F.atm "State" ] in
  let var = F.box (F.atm "let mut state : State =")
      [ (1,4,F.atm "Default::default();") ]
  in
  F.vbox
    (F.atm "pub fn makeState")
    [ (0,4,params);
      (0,0,F.atm "{");
      (0,4,var);
      (0,4,F.vlist (List.map (fun (mch,_) -> F.atm (mch^"::init(&mut state);")) lst));
      (0,4,F.atm "return state");
      (0,0,F.atm "}")]

let print_state (out:out_channel) (pkgs:(string*t_package) list) : unit =
  let out = Format.formatter_of_out_channel out in
  let vars = List.map (fun (mch_name,pkg) -> (mch_name,pkg.pkg_variables)) pkgs in
  F.print out (F.vlist [mk_state vars;mk_global_init pkgs]);
  Format.pp_print_flush out ()
