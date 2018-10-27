open Codegen.Rust
open SyntaxCore

let mk_atom (s:string) : Easy_format.t =
  let open Easy_format in Atom (s,atom)

let mk_label (indent_after_label:int) (space_after_label:bool)
    (label_break:Easy_format.label_break) (a:Easy_format.t) (b:Easy_format.t) : Easy_format.t =
  let open Easy_format in
  Label ((a,{ label with space_after_label; indent_after_label; label_break}),b)

let mk_list (op:string) (sep:string) (cl:string) (st:Easy_format.list_param)
    (lst:Easy_format.t list) : Easy_format.t =
  Easy_format.List ((op,sep,cl,st),lst)

let t_symb_to_string (ts:qident) : string =
  match ts.q_nspace with
  | None -> Codegen.Rust_ident.to_string ts.q_id
  | Some ns -> (Codegen.Rust_ident.pkg_to_string ns) ^ "::" ^ (Codegen.Rust_ident.to_string ts.q_id)

let b0_constant_to_string : t_b0_constant -> string = function
  | B0_Integer i -> Int32.to_string i
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

let mk_ident (ts:qident) : Easy_format.t = mk_atom (t_symb_to_string ts)

let mk_const (cst:t_b0_constant) : Easy_format.t = mk_atom (b0_constant_to_string cst)

let mk_prefix_op (op:t_b0_unary_op) (e:Easy_format.t) : Easy_format.t =
  mk_label 2 true `Auto (mk_atom (b0_unary_op_to_string op)) e

let mk_infix_op (op:t_b0_binary_op) (e1:Easy_format.t) (e2:Easy_format.t) : Easy_format.t =
  let st = { Easy_format.list with
             Easy_format.space_after_opening = false;
             stick_to_label = false;
             space_before_separator = true;
             space_after_separator = true;
             separators_stick_left = false;
             space_before_closing = false;
             align_closing = false;
             wrap_body = `Wrap_atoms;
             indent_body = 2 } in
  mk_list "" (b0_binary_op_to_string op) "" st [e1;e2]

let mk_fun_app (f:Easy_format.t) (args:Easy_format.t Nlist.t) : Easy_format.t  =
  let st = { Easy_format.list with
             Easy_format.space_after_opening = false;
             stick_to_label = false;
             space_before_separator = false;
             space_after_separator = true;
             separators_stick_left = false;
             space_before_closing = false;
             align_closing = false;
             wrap_body = `Wrap_atoms;
             indent_body = 2 } in
  mk_label 2 false `Auto f (mk_list "(" "," ")" st (Nlist.to_list args))

let mk_array_access (f:Easy_format.t) (args:Easy_format.t Nlist.t) : Easy_format.t  =
  let st = { Easy_format.list with
             Easy_format.space_after_opening = false;
             stick_to_label = false;
             space_before_separator = false;
             space_after_separator = true;
             separators_stick_left = false;
             space_before_closing = false;
             align_closing = false;
             wrap_body = `Wrap_atoms;
             indent_body = 2 } in
  mk_label 2 false `Auto f (mk_list "[" "][" "]" st (Nlist.to_list args))

let add_par (e:Easy_format.t) : Easy_format.t =
  let st = { Easy_format.list with
             Easy_format.space_after_opening = false;
             stick_to_label = true;
             space_before_separator = false;
             space_after_separator = false;
             separators_stick_left = false;
             space_before_closing = false;
             align_closing = false;
             wrap_body = `Wrap_atoms;
             indent_body = 2 } in
  mk_list "(" "" ")" st [e]

let mk_array (lst:Easy_format.t list) : Easy_format.t =
  let st = { Easy_format.list with
             Easy_format.space_after_opening = false;
             stick_to_label = false;
             space_before_separator = false;
             space_after_separator = true;
             separators_stick_left = false;
             space_before_closing = false;
             align_closing = false;
             wrap_body = `Wrap_atoms;
             indent_body = 2 } in
  mk_list "vec![" "," "]" st lst

let rec mk_array_init (nle:Easy_format.t Nlist.t) (def:Easy_format.t) : Easy_format.t =
  let st = { Easy_format.list with
             Easy_format.space_after_opening = false;
             stick_to_label = false;
             space_before_separator = false;
             space_after_separator = true;
             separators_stick_left = false;
             space_before_closing = false;
             align_closing = false;
             wrap_body = `Wrap_atoms;
             indent_body = 2 }
  in
  match Nlist.tl nle with
  | [] ->
    let sz = Nlist.hd nle in
    let sz = mk_list "(" "" ") as usize" Easy_format.list [sz] in
    mk_list "vec![" ";" "]" st [def;sz]
  | hd::tl ->
    let sz = Nlist.hd nle in
    let sz = mk_list "(" "" ") as usize" Easy_format.list [sz] in
    let def = mk_array_init (Nlist.make hd tl) def in
    mk_list "vec![" ";" "]" st [def;sz]

let rec is_by_ref_type = function
  | T_Int | T_Bool | T_Abstract _ | T_Enum _ -> false
  | T_String | T_Array _ -> true
  | T_Record lst -> List.exists (fun (_,ty) -> is_by_ref_type ty) lst

let rec mk_expr clone (e0:t_b0_expr) : Easy_format.t =
  match e0.exp0_desc with
  | B0_Global_Ident(id,IK_Variable (Some pkg)) ->
    mk_atom (Codegen.Rust_ident.pkg_to_string pkg^"::_get_"^Codegen.Rust_ident.to_string id^"()")
  | B0_Global_Ident (id,IK_Variable None) ->
    mk_atom ("_get_"^Codegen.Rust_ident.to_string id^"()") 
  | B0_Global_Ident(id,(IK_Constant (Some pkg)|IK_Enum (Some pkg))) ->
    if is_by_ref_type e0.exp0_type then
      if clone then
        mk_atom ("(" ^ Codegen.Rust_ident.pkg_to_string pkg ^ "::" ^ Codegen.Rust_ident.to_string id ^ ").clone()")
      else
        mk_atom ("(" ^ Codegen.Rust_ident.pkg_to_string pkg ^ "::" ^ Codegen.Rust_ident.to_string id ^ ")")
    else
      mk_atom (Codegen.Rust_ident.pkg_to_string pkg ^ "::" ^ Codegen.Rust_ident.to_string id)
  | B0_Local_Ident(id,Local.L_Param_In)
  | B0_Global_Ident(id,(IK_Constant None|IK_Enum None)) ->
    if is_by_ref_type e0.exp0_type then
      if clone then
        mk_atom ("(*" ^ Codegen.Rust_ident.to_string id ^ ").clone()")
      else
        mk_atom ("*" ^ Codegen.Rust_ident.to_string id)
    else mk_atom (Codegen.Rust_ident.to_string id)
  | B0_Local_Ident(id,(Local.L_Expr_Binder|Local.L_Subst_Binder|Local.L_Param_Out)) ->
    if clone then
      mk_atom (Codegen.Rust_ident.to_string id ^ ".clone()")
    else
      mk_atom (Codegen.Rust_ident.to_string id)
  | B0_Builtin_0 cst -> mk_const cst
  | B0_Builtin_1 (op,e) -> mk_prefix_op op (mk_expr_wp false e)
  | B0_Builtin_2 (B0_Power,e1,e2) ->
    mk_list "i32::pow(" "," ")" Easy_format.list [mk_expr false e1;mk_expr false e2]
  | B0_Builtin_2 (op,e1,e2) -> mk_infix_op op (mk_expr_wp false e1) (mk_expr_wp false e2)
  | B0_Array lst -> mk_array (List.map (mk_expr false) lst)
  | B0_Array_Init (rg,def) ->
    let rg = Nlist.map (
        function
        | R_Interval (_,y) -> mk_expr false y (*FIXME*)
        | R_Concrete_Set (i,_) -> mk_const (B0_Integer (Int32.of_int i))
      ) rg
    in
    mk_array_init rg (mk_expr false def)
  | B0_Array_Access (f,args) -> (*FIXME*)
    let aux arg = mk_list "(" "" ") as usize" Easy_format.list [mk_expr false arg] in
    let aa =
      let args = Nlist.map aux args in
      mk_array_access (mk_expr_wp false f) args
    in
    if clone && is_by_ref_type e0.exp0_type then
      mk_list "(" "" ").clone()" Easy_format.list [aa]
    else
      aa
  | B0_Record lst ->
    let aux (x,_) (y,_) =
      String.compare (Codegen.Rust_ident.to_string x) (Codegen.Rust_ident.to_string y)
    in
    let lst = List.fast_sort aux lst in
    mk_list "(" "," ")" Easy_format.list (List.map (fun (_,e) -> mk_expr clone e) lst)
  | B0_Record_Access (e,fd) ->
    let pos = match e.exp0_type with
      | T_Record lst ->
        let aux (x,_) (y,_) =
          String.compare (Codegen.Rust_ident.to_string x) (Codegen.Rust_ident.to_string y)
        in
        let lst = List.fast_sort aux lst in
        let fd = Codegen.Rust_ident.to_string fd in
        let rec find_pos i = function
          | [] -> assert false
          | (x,_)::tl ->
            if String.equal (Codegen.Rust_ident.to_string x) fd then i
            else find_pos (i+1) tl
        in
        find_pos 0 lst
      | _ -> assert false
    in
    let ra =
      mk_label 2 false `Auto (mk_expr_wp false e) (mk_atom ("."^string_of_int pos))
    in
    if clone && is_by_ref_type e0.exp0_type then
      mk_list "(" "" ").clone()" Easy_format.list [ra]
    else
      ra
  | B0_Fun_App (f,args) -> mk_fun_app (mk_ident f) (Nlist.map (mk_arg) args)

and mk_expr_wp clone (e0:t_b0_expr) : Easy_format.t =
  match e0.exp0_desc with
  | B0_Global_Ident(_,_) when not (is_by_ref_type e0.exp0_type)  -> mk_expr clone e0 (*FIXME*)
  | B0_Local_Ident(_,Local.L_Param_In) when not (is_by_ref_type e0.exp0_type) -> mk_expr clone e0
  | B0_Global_Ident(_,IK_Constant None) when not (is_by_ref_type e0.exp0_type) -> mk_expr clone e0
  | B0_Builtin_0 _ | B0_Local_Ident(_,(Local.L_Expr_Binder|Local.L_Subst_Binder|Local.L_Param_Out)) -> mk_expr clone e0
  | _ -> add_par (mk_expr clone e0)

and mk_arg e =
  if is_by_ref_type e.exp0_type then
    match e.exp0_desc with
(*     | B0_Extern (pkg,id) -> mk_atom (Codegen.Rust_ident.pkg_to_string pkg ^ "::" ^ Codegen.Rust_ident.to_string id) *)
    | B0_Local_Ident (id,Local.L_Param_In) (*| B0_Ident (id,IK_Constant)*) -> mk_atom (Codegen.Rust_ident.to_string id)
    | _ -> mk_list "&(" "" ")" Easy_format.list [mk_expr false e]
  else
    mk_expr false e

let mk_sequence_nl (lst:Easy_format.t list) : Easy_format.t =
  let st = { Easy_format.list with
             Easy_format.space_after_opening = false;
             stick_to_label = false;
             space_before_separator = false;
             space_after_separator = false;
             separators_stick_left = false;
             space_before_closing = false;
             align_closing = false;
             wrap_body = `Force_breaks;
             indent_body = 0 } in
  mk_list "" "" "" st lst

let rec get_seq_list (s:t_b0_subst) =
  match s.sub0_desc with
  | B0_Sequencement (s1,s2) ->
    begin match s2.sub0_desc with
      | B0_Null -> get_seq_list s1
      | _ -> (get_seq_list s1)@[s2]
    end
  | B0_Null -> []
  | _ -> [s]

let rec b0_type_to_string : t_b0_type -> string = function
  | T_Int -> "i32"
  | T_Bool -> "bool"
  | T_String -> "String"
  | T_Abstract ts -> t_symb_to_string ts
  | T_Enum ts -> t_symb_to_string ts
  | T_Array (dim,ty) ->
    let rec aux i str =
      if i <= 0 then str
      else "Vec<" ^ aux (i-1) str ^ ">"
    in
    aux dim (b0_type_to_string ty)
  | T_Record lst ->
    let aux (x,_) (y,_) =
      String.compare (Codegen.Rust_ident.to_string x) (Codegen.Rust_ident.to_string y)
    in
    let lst = List.fast_sort aux lst in
    let lst = List.map (fun (_,ty) -> b0_type_to_string ty) lst in
    "(" ^ String.concat "," lst ^ ")"

let rec get_default_value (ty:t_b0_type) : Easy_format.t =
    match ty with
    | T_Int -> mk_atom "0"
    | T_String -> mk_atom "\"\""
    | T_Bool -> mk_atom "true"
    | T_Enum _ | T_Abstract _ -> mk_atom "Default::default()"
    | T_Array (_,_) -> mk_atom "vec![]"
    | T_Record lst ->
      let aux (x,_) (y,_) =
        String.compare (Codegen.Rust_ident.to_string x) (Codegen.Rust_ident.to_string y)
      in
      let lst = List.fast_sort aux lst in
      let aux (_,ty) = get_default_value ty in
      mk_list "(" "," ")" Easy_format.list (List.map aux lst)

let rec mk_subst (s0:t_b0_subst) : Easy_format.t =
  match s0.sub0_desc with
  | B0_Null -> mk_atom "{};"
  | B0_Affectation (LHS_Variable (x,MIK_Variable),e) ->
    mk_label 2 false `Auto (mk_atom ("_set_"^Codegen.Rust_ident.to_string x))
      (mk_list "(" "" ");" Easy_format.list [mk_expr true e])
  | B0_Affectation (LHS_Variable (x,(MIK_Param|MIK_Local)),e) ->
    let st = Easy_format.list in
    mk_list "" "=" ";" st [mk_atom (Codegen.Rust_ident.to_string x);mk_expr true e]
  | B0_Affectation (LHS_Array(x,MIK_Variable,args),e) ->
    let st = Easy_format.list in
    let args = mk_list "[(" ") as usize][(" ") as usize]" st (List.map (mk_expr false) (Nlist.to_list args)) in
    let arr = mk_label 2 false `Auto (mk_atom "_aux") args in
    let def = mk_expr true e in
    let st = Easy_format.list in
    let aff1 = mk_atom ("let mut _aux = _get_"^Codegen.Rust_ident.to_string x^"();") in
    let aff2 = mk_list "" "=" ";" st [arr;def] in
    let aff3 = 
      mk_label 2 false `Auto (mk_atom ("_set_"^Codegen.Rust_ident.to_string x))
      (mk_atom "(_aux);")
    in
    mk_sequence_nl [aff1;aff2;aff3]
  | B0_Affectation (LHS_Array(f,(MIK_Param|MIK_Local),args),e) ->
    let st = Easy_format.list in
    let args = mk_list "[(" ") as usize][(" ") as usize]" st (List.map (mk_expr false) (Nlist.to_list args)) in
    let arr = mk_label 2 false `Auto (mk_atom (Codegen.Rust_ident.to_string f)) args in
    let def = mk_expr true e in
    let st = Easy_format.list in
    mk_list "" "=" ";" st [arr;def]
  | B0_Affectation (LHS_Record(_,MIK_Variable,_),_) ->
    Error.raise_exn s0.sub0_loc "Not implemented: assignment of global record field." (*FIXME*)
  | B0_Affectation (LHS_Record(rd,(MIK_Param|MIK_Local),fd),e) ->
    let st = Easy_format.list in
    let arr = mk_atom (Codegen.Rust_ident.to_string rd ^ "." ^ Codegen.Rust_ident.to_string fd) in
    let def = mk_expr true e in
    mk_list "" "=" ";" st [arr;def]
  | B0_IfThenElse (cases,def) ->
    let (p,s) = Nlist.hd cases in
    let st = Easy_format.list in
    let ifthen = mk_list "if" "" "{" st [mk_expr false p] in
    let ifthen_s = mk_label 2 false `Always_rec ifthen (mk_subst s) in
    let aux (p,s) =
      let elsifthen = mk_list "} else if" "" "{" st [mk_expr false p] in
      mk_label 2 false `Always_rec elsifthen (mk_subst s)
    in
    let clst = List.map aux (Nlist.tl cases) in
    let endif = mk_atom "};" in
    let lst =
      begin match def with
        | None -> ifthen_s::(clst@[endif])
        | Some s ->
          let els = mk_label 2 true `Always_rec (mk_atom "} else {") (mk_subst s) in
          ifthen_s::(clst@[els;endif])
      end
    in
    mk_sequence_nl lst
  | B0_Case (e,cases,def) ->
    let mtch = mk_label 2 true `Auto (mk_atom "match") (mk_expr false e) in
    let aux (lst,s) =
      let lst = List.map ( function
          | CS_Int e -> mk_atom (Int32.to_string e)
          | CS_Bool true -> mk_atom "true"
          | CS_Bool false -> mk_atom "false"
          | CS_Enum qid -> mk_ident qid
        ) (Nlist.to_list lst) in
      let st = Easy_format.list in
      let whn = mk_list "" "," "=>" st lst in
      mk_label 4 true `Auto whn (mk_list "{" "" "}" Easy_format.list [mk_subst s])
    in
    let clst = List.map aux (Nlist.to_list cases) in
    let lst = match def with
      | None -> clst@[mk_atom "_ => {}"]
      | Some s ->
        let others = mk_list "_ => {" "" "}" Easy_format.list [mk_subst s] in
        clst@[others]
    in
    let cases = mk_list "{" "," "};" Easy_format.list lst in
    mk_label 2 true `Auto mtch cases
  | B0_Var (vars,s) ->
    let aux (id,ty) = 
      mk_list
        ("let mut "^Codegen.Rust_ident.to_string id ^ ": " ^ b0_type_to_string ty ^ " = ")
        "" ";" Easy_format.list [get_default_value ty]
    in
    let st = Easy_format.list in
    let vars = mk_list "" "" "" st (List.map aux (Nlist.to_list vars)) in
    mk_sequence_nl [vars;mk_subst s]
  | B0_While (cond,s) ->
    let st = Easy_format.list in
    mk_sequence_nl [ mk_label 2 true `Always_rec (mk_atom "while") (mk_expr false cond);
                     mk_list "{" "" "};" st [mk_subst s] ]
  | B0_CallUp ([],f,args) ->
    let args = List.map (mk_arg) args in
    let st = { Easy_format.list with
             Easy_format.space_after_opening = false;
             stick_to_label = false;
             space_before_separator = false;
             space_after_separator = true;
             separators_stick_left = false;
             space_before_closing = false;
             align_closing = false;
             wrap_body = `Wrap_atoms;
             indent_body = 2 } in
    mk_label 2 false `Auto (mk_atom (t_symb_to_string f)) (mk_list "(" "," ");" st args)
  | B0_CallUp ([(is_var,r)],f,args) ->
    let args = List.map (mk_arg) args in
    let st = { Easy_format.list with
             Easy_format.space_after_opening = false;
             stick_to_label = false;
             space_before_separator = false;
             space_after_separator = true;
             separators_stick_left = false;
             space_before_closing = false;
             align_closing = false;
             wrap_body = `Wrap_atoms;
             indent_body = 2 }
    in
    let call = mk_label 2 false `Auto (mk_atom (t_symb_to_string f)) (mk_list "(" "," ")" st args) in
    begin match is_var with
      | MIK_Variable ->
        mk_label 2 false `Auto (mk_atom ("_set_" ^ (Codegen.Rust_ident.to_string r)))
          (mk_list "(" "" ");" Easy_format.list [call])
      | MIK_Local | MIK_Param ->
        let lhs = mk_atom (Codegen.Rust_ident.to_string r) in
        mk_list "" "=" ";" Easy_format.list [lhs;call]
    end
  | B0_CallUp (ret,f,args) ->
    let args = (List.map (mk_arg) args) in
    let st = { Easy_format.list with
             Easy_format.space_after_opening = false;
             stick_to_label = false;
             space_before_separator = false;
             space_after_separator = true;
             separators_stick_left = false;
             space_before_closing = false;
             align_closing = false;
             wrap_body = `Wrap_atoms;
             indent_body = 2 } in
    let call = mk_label 2 false `Auto (mk_atom ("let _x = "^t_symb_to_string f)) (mk_list "(" "," ");" st args) in
    let affs = List.mapi (fun i (is_var,x) ->
        match is_var with
        | MIK_Variable ->
          mk_atom ("_set_" ^ (Codegen.Rust_ident.to_string x) ^ "(_x."^string_of_int i^");")
        | MIK_Local | MIK_Param ->
          mk_atom (Codegen.Rust_ident.to_string x ^ " = _x." ^ string_of_int i ^ ";")
      ) ret in 
    mk_sequence_nl (call::affs)

  | B0_Sequencement (_,_) ->
    begin match get_seq_list s0 with
      | [] -> mk_atom "{};"
      | seqs ->
        let seqs = List.map (fun s -> mk_subst s) seqs in
        let st = Easy_format.list in
        mk_list "" "" "" st seqs
    end

let mk_dep (dep,_:Codegen.Rust_ident.t_pkg_id*_) =
  mk_atom ("use " ^ Codegen.Rust_ident.pkg_to_string dep ^ ";")

let mk_type (ty:t_type) : Easy_format.t =
  match ty.ty_def with
  | D_Alias ts -> mk_atom ("pub type " ^ Codegen.Rust_ident.to_string ty.ty_name ^ " = " ^ t_symb_to_string ts ^ ";")
  | D_Int -> mk_atom ("pub type " ^ Codegen.Rust_ident.to_string ty.ty_name ^ " = i32;")
  | D_Enum elts ->
   begin
     let hd = mk_atom ("pub type " ^ Codegen.Rust_ident.to_string ty.ty_name ^ " = i32;") in
     let tl = List.mapi (fun i e ->
         mk_atom ("pub const " ^ Codegen.Rust_ident.to_string e ^ ": i32 = " ^ string_of_int i ^ ";")
       )  elts in
     mk_list "" "" "" Easy_format.list (hd::tl)
   end

let mk_arg_in (a:t_arg) : Easy_format.t =
  let ref = if is_by_ref_type a.arg_type then "&" else "" in
  mk_label 2 true `Auto (mk_atom (Codegen.Rust_ident.to_string a.arg_name^":"))
    (mk_atom (ref^b0_type_to_string a.arg_type))

let mk_fun (f:t_fun) : Easy_format.t =
  let pr = mk_atom ("pub fn " ^ Codegen.Rust_ident.to_string f.f_name) in
  let args_in = List.map (mk_arg_in) (Nlist.to_list f.f_args) in
  let proc =
      let st = { Easy_format.list with
                 Easy_format.space_after_opening = false;
                 stick_to_label = false;
                 space_before_separator = false;
                 space_after_separator = true;
                 separators_stick_left = true;
                 space_before_closing = false;
                 align_closing = false;
                 wrap_body = `Never_wrap;
                 indent_body = 3 }
      in
      let args = mk_list "(" "," ") -> " st (args_in) in
      let ret_ty = mk_atom (b0_type_to_string f.f_ret.exp0_type) in
      let args = mk_label 2 false `Auto args (mk_list "" "" "" st [ret_ty]) in
      mk_label 2 false `Always_rec pr args
    in
    let st = { Easy_format.list with
               Easy_format.space_after_opening = true;
               stick_to_label = false;
               space_before_separator = false;
               space_after_separator = false;
               separators_stick_left = false;
               space_before_closing = true;
               align_closing = true;
               wrap_body = `Force_breaks;
               indent_body = 3 }
    in
    let return =
      mk_label 2 true `Auto (mk_atom "return") (mk_expr false f.f_ret)
    in
    let body = mk_list "{" "" "}" st ([return]) in
    mk_sequence_nl [proc;body]

let mk_const (c:t_constant_or_fun) : Easy_format.t =
   match c with
  | Fun f -> mk_fun f
  | Cst ({ c_init=Promoted mch} as c) ->
    if is_by_ref_type c.c_type then assert false (*FIXME*)
    else
      mk_atom ("pub use " ^ Codegen.Rust_ident.pkg_to_string mch ^ "::" ^ Codegen.Rust_ident.to_string c.c_name ^ ";")
  | Cst ({ c_init=Init c_init } as c) ->
    let st = { Easy_format.list with
               Easy_format.space_after_opening = false;
               stick_to_label = false;
               space_before_separator = false;
               space_after_separator = true;
               separators_stick_left = false;
               space_before_closing = false;
               align_closing = false;
               wrap_body = `Wrap_atoms;
               indent_body = 3 }
    in
    if is_by_ref_type c_init.exp0_type then
      let ty =
        mk_atom ("lazy_static! {pub static ref " ^ Codegen.Rust_ident.to_string c.c_name ^ ": "
                 ^ b0_type_to_string c.c_type ^ " =")
      in
      mk_label 2 true `Auto ty (mk_list "" "" ";}" st [mk_expr true c_init])
    else
      let ty =
        mk_atom ("pub const " ^ Codegen.Rust_ident.to_string c.c_name ^ ": "
                 ^ b0_type_to_string c.c_type ^ " =")
      in
      mk_label 2 true `Auto ty (mk_list "" "" ";" st [mk_expr false c_init])
      
let mk_var (v:t_variable) : Easy_format.t option =
  match v.v_promoted_from with
  | None ->
    Some (mk_label 2 true `Auto
            (mk_atom ("static " ^ Codegen.Rust_ident.to_string v.v_name ^ ": RefCell<" ^ b0_type_to_string v.v_type ^ "> = "))
            (mk_list "RefCell::new(" "" ");" Easy_format.list [get_default_value v.v_type])
         )
  | Some _ -> None

let mk_var_setter (v:t_variable) : Easy_format.t option =
  match v.v_promoted_from with
  | None ->
    Some (mk_label 2 true `Auto
            (mk_atom ("fn _set_" ^ Codegen.Rust_ident.to_string v.v_name ^ "(value:" ^ b0_type_to_string v.v_type ^ ") {"))
            (mk_atom (Codegen.Rust_ident.to_string v.v_name ^ ".with(|global_var| *global_var.borrow_mut() = value) }")))
  | Some _ -> None

let mk_var_getter (v:t_variable) : Easy_format.t =
  match v.v_promoted_from with
  | None ->
    mk_label 2 true `Auto
      (mk_atom ("pub fn _get_" ^ Codegen.Rust_ident.to_string v.v_name ^ "() -> " ^ b0_type_to_string v.v_type ^ " {"))
      (mk_atom (Codegen.Rust_ident.to_string v.v_name ^ ".with(|global_var| global_var.borrow().clone()) }"))
  | Some pkg ->
    mk_atom ("pub use " ^ Codegen.Rust_ident.pkg_to_string pkg ^ "::_get_"^Codegen.Rust_ident.to_string v.v_name ^";")

let mk_arg_out (a:t_arg) : Easy_format.t =
    mk_atom (b0_type_to_string a.arg_type)

let mk_ret_id (a:t_arg) : Easy_format.t = mk_atom (Codegen.Rust_ident.to_string a.arg_name)

let mk_ret_decl (a:t_arg) : Easy_format.t =
  mk_list 
    ("let mut "^Codegen.Rust_ident.to_string a.arg_name ^ ": " ^ b0_type_to_string a.arg_type ^ " = ")
    "" ";" Easy_format.list [get_default_value a.arg_type]

let mk_proc_body (p:t_procedure) : Easy_format.t =
  let pr = mk_atom ("pub fn " ^ Codegen.Rust_ident.to_string p.p_name) in
  let args_in = List.map mk_arg_in p.p_args_in in
  let args_out = List.map mk_arg_out p.p_args_out in
  match p.p_body with
  | Body s ->
    let proc =
      let st = { Easy_format.list with
                 Easy_format.space_after_opening = false;
                 stick_to_label = false;
                 space_before_separator = false;
                 space_after_separator = true;
                 separators_stick_left = true;
                 space_before_closing = false;
                 align_closing = false;
                 wrap_body = `Never_wrap;
                 indent_body = 3 }
      in
      let args = mk_list "(" "," ") -> " st (args_in) in
      let args = mk_label 2 false `Auto args (mk_list "(" "," ")" st args_out) in
      mk_label 2 false `Always_rec pr args
    in
    let st = { Easy_format.list with
               Easy_format.space_after_opening = true;
               stick_to_label = false;
               space_before_separator = false;
               space_after_separator = false;
               separators_stick_left = false;
               space_before_closing = true;
               align_closing = true;
               wrap_body = `Force_breaks;
               indent_body = 3 }
    in
    let vars = List.map mk_ret_decl p.p_args_out in
    let return =
      match p.p_args_out with
      | [] -> mk_atom "return ()"
      | [xx] -> mk_label 2 true `Auto (mk_atom "return") (mk_ret_id xx)
      | _::_ -> mk_list "return (" "," ")" Easy_format.list (List.map mk_ret_id p.p_args_out)
    in
    let body = mk_list "{" "" "}" st (vars@[mk_subst s;return]) in
    mk_sequence_nl [proc;body]
  | Renames ts -> mk_atom ("pub use " ^t_symb_to_string ts^ ";")

let rec check_call_to_seen_mch seen (s:t_b0_subst) : unit =
  match s.sub0_desc with
  | B0_Null | B0_Affectation _ -> ()
  | B0_IfThenElse (nle,opt) ->
    let aux (_,s) = check_call_to_seen_mch seen s in
    let () = List.iter aux (Nlist.to_list nle) in
    begin match opt with
      | None -> ()
      | Some s -> check_call_to_seen_mch seen s
    end
  | B0_Case (_,nle,opt) ->
    let aux (_,s) = check_call_to_seen_mch seen s in
    let () = List.iter aux (Nlist.to_list nle) in
    begin match opt with
      | None -> ()
      | Some s -> check_call_to_seen_mch seen s
    end
  | B0_Var (_,s) | B0_While (_,s) -> check_call_to_seen_mch seen s
  | B0_Sequencement (s1,s2) ->
    let () = check_call_to_seen_mch seen s1 in
    check_call_to_seen_mch seen s2
  | B0_CallUp (_,{q_nspace=Some ext},_) ->
    if List.mem ext seen then () (*FIXME*)
(*       Error.raise_exn s.sub0_loc "Call to seen machine in initialisation not supported." (*FIXME equality of strings*) *)
  | B0_CallUp _ -> ()

let mk_init (pkg:t_package) : t_procedure =
  let aux (id,ki) = match ki with
    | DK_Sees -> None
    | _ -> Some id
  in
  let get_seen (id,ki) = match ki with
    | DK_Sees -> Some id
    | _ -> None
  in
  let decorate sub0_desc = {sub0_loc=Utils.dloc;sub0_desc} in
  let init_call dep =
    let q_id = match Codegen.Rust_ident.make "init" with
      | Some id -> id
      | None -> assert false
    in
    decorate (B0_CallUp ([],{q_nspace=Some dep;q_id},[])) in
  let mk_seq s1 s2 = decorate (B0_Sequencement (s1,s2)) in
  let rec mk_seqs s = function
    | [] -> s
    | hd::tl -> mk_seqs (mk_seq (init_call hd) s) tl
  in
  let s = match pkg.pkg_init, Utils.filter_map aux pkg.pkg_dependencies with
    | None, [] -> decorate B0_Null
    | None, hd::tl ->
      
      mk_seqs (init_call hd) tl
    | Some s, imps ->
      let seen = Utils.filter_map get_seen pkg.pkg_dependencies in
      let () = check_call_to_seen_mch seen s in
      mk_seqs s imps
  in
  { p_name = (match Codegen.Rust_ident.make "init" with | None -> assert false | Some id -> id);
    p_is_local = false;
    p_args_in = [];
    p_args_out = [];
    p_body = Body s }

let package_to_format (pkg:t_package) : Easy_format.t =
  let deps = List.rev_map mk_dep pkg.pkg_dependencies in
  let types = List.map mk_type pkg.pkg_types in
  let consts = List.map mk_const pkg.pkg_constants in
  let vars =
    if pkg.pkg_variables = [] then [] (*FIXME*)
    else [mk_list "thread_local!{" "" "}" Easy_format.list (Utils.filter_map mk_var pkg.pkg_variables)]
  in
  let vars_getter = List.map mk_var_getter pkg.pkg_variables in
  let vars_setter = Utils.filter_map mk_var_setter pkg.pkg_variables in
  let init = mk_init pkg in
  let procs = List.map mk_proc_body (init::pkg.pkg_procedures) in
  let st_body = { Easy_format.list with
                  Easy_format.space_after_opening = true;
                  stick_to_label = false;
                  space_before_separator = false;
                  space_after_separator = false;
                  separators_stick_left = false;
                  space_before_closing = true;
                  align_closing = true;
                  wrap_body = `Force_breaks;
                  indent_body = 0 }
  in 
  let body = mk_list "" "" "" st_body (types@consts@vars@vars_getter@vars_setter@procs) in
  let st = { Easy_format.list with
               Easy_format.space_after_opening = false;
               stick_to_label = false;
               space_before_separator = false;
               space_after_separator = false;
               separators_stick_left = false;
               space_before_closing = false;
               align_closing = false;
               wrap_body = `Force_breaks;
               indent_body = 0 }
  in
  let deps = match pkg.pkg_variables with
    | [] -> deps
    | _::_ -> (mk_atom "use std::cell::RefCell;")::deps
  in
  mk_list "" "" "" st ((mk_atom "#![allow(bad_style,dead_code,unused_mut,unused_imports)]\n")::(List.rev (body::(mk_atom "")::deps)))

let print_package (out:out_channel) (pkg:t_package) : unit Error.t_result =
  try Ok (Easy_format.Pretty.to_channel out (package_to_format pkg))
  with Error.Error err -> Error err
