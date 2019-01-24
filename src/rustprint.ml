open SyntaxCore
open Codegen

let mk_atom (s:string) : Easy_format.t =
  let open Easy_format in Atom (s,atom)

let mk_label (indent_after_label:int) (space_after_label:bool)
    (label_break:Easy_format.label_break) (a:Easy_format.t) (b:Easy_format.t) : Easy_format.t =
  let open Easy_format in
  Label ((a,{ label with space_after_label; indent_after_label; label_break}),b)

let mk_list (op:string) (sep:string) (cl:string) (st:Easy_format.list_param)
    (lst:Easy_format.t list) : Easy_format.t =
  Easy_format.List ((op,sep,cl,st),lst)

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

let reserved_list = [
    "as"; "break"; "const"; "continue"; "crate"; "else"; "enum"; "extern"; "false";
    "fn"; "for"; "if"; "impl"; "in"; "let"; "loop"; "match"; "mod"; "move"; "mut";
    "pub"; "ref"; "return"; "self"; "Self"; "static"; "struct"; "super"; "trait";
    "true"; "type"; "unsafe"; "use"; "where"; "while"; "abstract"; "become"; "box";
    "do"; "final"; "macro"; "override"; "priv"; "typeof"; "unsized"; "virtual";
    "yield"; "union"; "dyn"]

module SSet = Set.Make(String)
let reserved_set = List.fold_left (fun x y -> SSet.add y x) SSet.empty reserved_list

let is_valid_ident (id:string) : bool =
  let reg = Str.regexp {|\([a-zA-Z][a-zA-Z0-9_]*\)\|\(_[a-zA-Z0-9_]+\)$|} in
  not (SSet.mem id reserved_set) &&
  (Str.string_match reg id 0)

let get_rust_ident (lc:Utils.loc) (s:string) : string =
  if is_valid_ident s then s
  else Error.raise_exn lc ("The string '"^s^"' is not a valid rust identifier.")

let pkg_to_rust_ident (x:t_pkg_id) : string =
  let lid = pkg_to_lident x in
  get_rust_ident lid.lid_loc lid.lid_str

let ident_to_rust_ident (x:ident) : string option =
  let s = Codegen.ident_to_string x in 
  if is_valid_ident s then Some s
  else None

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

let rec is_copy_type = function
  | T_Int | T_Bool | T_Abstract _ | T_Enum _ -> true
  | T_String | T_Array _ -> false
  | T_Record lst -> List.for_all (fun (_,ty) -> is_copy_type ty) lst

let rec is_lvalue e =
  match e.exp0_desc with
  | B0_Local_Ident _ | B0_Global_Ident _ -> true
  | B0_Array_Access (e,_) | B0_Record_Access (e,_) -> is_lvalue e (*FIXME check*)
  | B0_Builtin_0 _
  | B0_Builtin_1 _
  | B0_Builtin_2 _
  | B0_Array _
  | B0_Array_Init _
  | B0_Record _ -> false

let rec mk_expr (init:bool) (purity:bool ref) (paren:bool) (e0:t_b0_expr) : Easy_format.t =
  let add_paren_if_true b e = if b then add_par e else e in
  match e0.exp0_desc with
  | B0_Global_Ident(_,IK_Variable (Some _)) -> assert false
  (*FIXME on peut vraiment acceder directement a des var externes?*)
  (* mk_atom (pkg_to_string pkg^"::_get_"^id_to_string id^"()") *)
  | B0_Global_Ident (id,IK_Variable None) ->
    if init then mk_atom (id_to_rust_ident id)
    else
      ( purity := false;
        mk_atom ("st.borrow()." ^ id_to_rust_ident id) )
  | B0_Global_Ident(id,IK_Enum (Some pkg)) ->
    mk_atom (pkg_to_rust_ident pkg ^ "::" ^ id_to_rust_ident id)
  | B0_Global_Ident(id,(IK_Constant (Some pkg))) ->
    if is_copy_type e0.exp0_type then
      mk_atom (pkg_to_rust_ident pkg ^ "::" ^ id_to_rust_ident id)
    else
     add_paren_if_true paren (mk_atom ("*(" ^ pkg_to_rust_ident pkg ^ "::" ^ id_to_rust_ident id ^ ")"))
  | B0_Global_Ident(id,IK_Enum None) -> mk_atom (id_to_rust_ident id)
  | B0_Local_Ident(id,Local.L_Param_In)
  | B0_Global_Ident(id,IK_Constant None) ->
    if is_copy_type e0.exp0_type then mk_atom (id_to_rust_ident id)
    else add_paren_if_true paren (mk_atom ("*" ^ id_to_rust_ident id))
  | B0_Local_Ident(id,(Local.L_Expr_Binder|Local.L_Subst_Binder|Local.L_Param_Out)) ->
    mk_atom (id_to_rust_ident id)
  | B0_Builtin_0 cst -> mk_const cst
  | B0_Builtin_1 (op,e) -> (*rmk: only boolean/integer operations*)
    let e = mk_expr init purity true e in
    add_paren_if_true paren (mk_prefix_op op e)
  | B0_Builtin_2 (B0_Power,e1,e2) ->
    let e1 = mk_expr init purity false e1 in
    let e2 = mk_expr init purity false e2 in
    mk_list "i32::pow(" "," ")" Easy_format.list [e1;e2]
  | B0_Builtin_2 (op,e1,e2) -> (*rmk: only boolean/integer operations*)
    let e1 = mk_expr init purity true e1 in
    let e2 = mk_expr init purity true e2 in
    add_paren_if_true paren (mk_infix_op op e1 e2)
  | B0_Array lst ->
    mk_array (List.map (mk_movable init purity false) lst)
  | B0_Array_Init (rg,def) ->
    let def = mk_movable init purity false def in
    let aux = function
      | R_Interval (_,y) -> mk_expr init purity false y (*FIXME*)
      | R_Concrete_Set qid -> mk_atom (qident_to_string qid ^ "_size")
    in
    let rg = Nlist.map aux rg in
    mk_array_init rg def
  | B0_Array_Access (f,args) ->
    let aux arg =
      let arg = mk_expr init purity false arg in
      mk_list "(" "" ") as usize" Easy_format.list [arg]
    in
    let f = mk_expr init purity true f in
    let args = Nlist.map aux args in
    mk_array_access f args
  | B0_Record lst ->
    let cmp (x,_) (y,_) = String.compare (id_to_rust_ident x) (id_to_rust_ident y) in
    let lst = List.fast_sort cmp lst in
    let aux (_,arg0) = mk_movable init purity false arg0 in
    let lst = List.map aux lst in
    mk_list "(" "," ")" Easy_format.list lst
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
    let e = mk_expr init purity true e in
    mk_label 2 false `Auto e (mk_atom ("."^string_of_int pos))

and mk_movable init purity paren e =
  if (not (is_copy_type e.exp0_type)) && is_lvalue e then
    mk_list "" "" ".clone()" Easy_format.list [mk_expr init purity true e]
  else
    mk_expr init purity paren e

let mk_arg init purity e =
  if is_copy_type e.exp0_type then mk_expr init purity false e
  else
    match e.exp0_desc with
    | B0_Local_Ident (id,Local.L_Param_In)
    | B0_Global_Ident (id,IK_Constant None) -> mk_atom (id_to_rust_ident id)
    | B0_Global_Ident (id,IK_Constant (Some pkg)) ->
      mk_atom ("&" ^ pkg_to_rust_ident pkg ^ "::" ^ id_to_rust_ident id)
    | _ -> mk_list "&(" "" ")" Easy_format.list [mk_expr init purity false e]

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

let rec b0_type_to_string_exn lc (ty:t_b0_type) : string =
  match ty with
  | T_Int -> "i32"
  | T_Bool -> "bool"
  | T_String -> "String"
  | T_Abstract ts ->
   begin match type_id_to_rust_ident ts with
     | Error s -> 
       Error.raise_exn lc ("The type of this expression contains the set '"^
                           s^"' that is not a valid rust identifier.")
     | Ok s -> s
   end
  | T_Enum ts ->
   begin match type_id_to_rust_ident ts with
     | Error s ->
       Error.raise_exn lc ("The type of this expression contains the set '"^
                           s^"' that is not a valid rust identifier.")
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

let b0_type_to_string (ty:t_b0_type) : string option =
  try Some(b0_type_to_string_exn Utils.dloc ty)
  with Error.Error _ -> None

let rec get_default_value (ty:t_b0_type) : Easy_format.t =
  match ty with
  | T_Int -> mk_atom "0"
  | T_String -> mk_atom "\"\""
  | T_Bool -> mk_atom "true"
  | T_Enum _ | T_Abstract _ -> mk_atom "Default::default()"
  | T_Array (_,_) -> mk_atom "vec![]"
  | T_Record lst ->
    let aux (x,_) (y,_) =
      String.compare (Codegen.ident_to_string x) (Codegen.ident_to_string y)
    in
    let lst = List.fast_sort aux lst in
    let aux (_,ty) = get_default_value ty in
    mk_list "(" "," ")" Easy_format.list (List.map aux lst)

let rec mk_subst (init:bool) (purity:bool ref) (s0:t_b0_subst) : Easy_format.t =
  match s0.sub0_desc with
  | B0_Null -> mk_atom "{};"
  | B0_Affectation (LHS_Variable (x,MIK_Variable),e) ->
    if init then
      let st = Easy_format.list in
      mk_list "" "=" ";" st [mk_atom (id_to_rust_ident x);mk_movable init purity false e]
    else
      begin
        purity := false;
        let v = "st.borrow_mut()."^id_to_rust_ident x in
        let st = Easy_format.list in
        mk_list "" "=" ";" st [mk_atom v;mk_movable init purity false e]
      end
  | B0_Affectation (LHS_Variable (x,(MIK_Param|MIK_Local)),e) ->
    let st = Easy_format.list in
    mk_list "" "=" ";" st [mk_atom (id_to_rust_ident x);mk_movable init purity false e]
  | B0_Affectation (LHS_Array(f,MIK_Variable,args),e) ->
    if init then
      assert false (*FIXME*)
    else
      begin
        purity := false;
        let st = Easy_format.list in
        let args = mk_list "[(" ") as usize][(" ") as usize]" st
            (List.map (mk_expr init purity false) (Nlist.to_list args))
        in
        let a = mk_atom ("st.borrow_mut()." ^ id_to_rust_ident f) in
        let arr = mk_label 2 false `Auto a args in
        let e = mk_movable init purity false e in
        let st = Easy_format.list in
        mk_list "" "=" ";" st [arr;e]
      end
  | B0_Affectation (LHS_Array(f,(MIK_Param|MIK_Local),args),e) ->
    let st = Easy_format.list in
    let args = mk_list "[(" ") as usize][(" ") as usize]" st
        (List.map (mk_expr init purity false) (Nlist.to_list args))
    in
    let arr = mk_label 2 false `Auto (mk_atom (id_to_rust_ident f)) args in
    let e = mk_movable init purity false e in
    let st = Easy_format.list in
    mk_list "" "=" ";" st [arr;e]
  | B0_Affectation (LHS_Record(rd,MIK_Variable,fd),e) ->
    if init then
      assert false (*FIXME*)
    else
      begin
        purity := false;
        let st = Easy_format.list in
        let rc = mk_atom ("st.borrow_mut()." ^ id_to_rust_ident rd ^ "." ^ id_to_rust_ident fd) in
        let def = mk_movable init purity false e in
        mk_list "" "=" ";" st [rc;def]
      end
  | B0_Affectation (LHS_Record(rd,(MIK_Param|MIK_Local),fd),e) ->
    let st = Easy_format.list in
    let arr = mk_atom (id_to_rust_ident rd ^ "." ^ id_to_rust_ident fd) in
    let def = mk_movable init purity false e in
    mk_list "" "=" ";" st [arr;def]
  | B0_IfThenElse (cases,def) ->
    let (p,s) = Nlist.hd cases in
    let st = Easy_format.list in
    let ifthen = mk_list "if" "" "{" st [mk_expr init purity false p] in
    let ifthen_s = mk_label 2 false `Always_rec ifthen (mk_subst init purity s) in
    let aux (p,s) =
      let elsifthen = mk_list "} else if" "" "{" st [mk_expr init purity false p] in
      mk_label 2 false `Always_rec elsifthen (mk_subst init purity s)
    in
    let clst = List.map aux (Nlist.tl cases) in
    let endif = mk_atom "};" in
    let lst =
      begin match def with
        | None -> ifthen_s::(clst@[endif])
        | Some s ->
          let els =
            mk_label 2 true `Always_rec (mk_atom "} else {") (mk_subst init purity s)
          in
          ifthen_s::(clst@[els;endif])
      end
    in
    mk_sequence_nl lst
  | B0_Case (e,cases,def) ->
    let mtch = mk_label 2 true `Auto (mk_atom "match") (mk_expr init purity false e) in
    let aux (lst,s) =
      let lst = List.map ( function
          | CS_Int e -> mk_atom (Int32.to_string e)
          | CS_Bool true -> mk_atom "true"
          | CS_Bool false -> mk_atom "false"
          | CS_Enum qid -> mk_atom (qident_to_string qid)
        ) (Nlist.to_list lst) in
      let st = Easy_format.list in
      let whn = mk_list "" "," "=>" st lst in
      mk_label 4 true `Auto whn (mk_list "{" "" "}" Easy_format.list [mk_subst init purity s])
    in
    let clst = List.map aux (Nlist.to_list cases) in
    let lst = match def with
      | None -> clst@[mk_atom "_ => {}"]
      | Some s ->
        let others =
          mk_list "_ => {" "" "}" Easy_format.list [mk_subst init purity s]
        in
        clst@[others]
    in
    let cases = mk_list "{" "," "};" Easy_format.list lst in
    mk_label 2 true `Auto mtch cases
  | B0_Var (vars,s) ->
    let aux (id,ty) = 
      mk_list
        ("let mut "^id_to_rust_ident id ^ ": " ^
         b0_type_to_string_exn (id_to_lident id).lid_loc ty ^ " = ")
        "" ";" Easy_format.list [get_default_value ty]
    in
    let st = Easy_format.list in
    let vars = mk_list "" "" "" st (List.map aux (Nlist.to_list vars)) in
    mk_sequence_nl [vars;mk_subst init purity s]
  | B0_While (cond,s) ->
    let st = Easy_format.list in
    mk_sequence_nl
      [ mk_label 2 true `Always_rec (mk_atom "while") (mk_expr init purity false cond);
        mk_list "{" "" "};" st [mk_subst init purity s] ]
  | B0_CallUp ([],f,args) ->
    let args = List.map (mk_arg init purity) args in
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
    mk_label 2 false `Auto (mk_atom (qident_to_string f)) (mk_list "(" "," ");" st args)
  | B0_CallUp ([(is_var,r)],f,args) ->
    let args = List.map (mk_arg init purity) args in
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
    let call =
      mk_label 2 false `Auto (mk_atom (qident_to_string f)) (mk_list "(" "," ")" st args)
    in
    let lhs = match is_var with
      | MIK_Variable ->
        if init then
          assert false (*FIXME*)
        else
          begin
            purity := false;
            mk_atom ("st.borrow_mut()." ^ (id_to_rust_ident r))
          end
      | MIK_Local | MIK_Param -> mk_atom (id_to_rust_ident r)
    in
    mk_list "" "=" ";" Easy_format.list [lhs;call]
  | B0_CallUp (ret,f,args) ->
    let args = (List.map (mk_arg init purity) args) in
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
    let call = mk_label 2 false `Auto
        (mk_atom ("let _x = "^qident_to_string f))
        (mk_list "(" "," ");" st args)
    in
    let affs = List.mapi (fun i (is_var,x) ->
        match is_var with
        | MIK_Variable ->
          if init then
            assert false (*FIXME*)
          else
            begin
              purity := false;
              mk_atom ("st.borrow_mut()." ^ id_to_rust_ident x ^ " = _x." ^ string_of_int i ^ ";")
            end
        | MIK_Local | MIK_Param ->
          mk_atom (id_to_rust_ident x ^ " = _x." ^ string_of_int i ^ ";")
      ) ret in 
    mk_sequence_nl (call::affs)
  | B0_Sequencement (_,_) ->
    begin match get_seq_list s0 with
      | [] -> mk_atom "{};"
      | seqs ->
        let seqs = List.map (fun s -> mk_subst init purity s) seqs in
        let st = Easy_format.list in
        mk_list "" "" "" st seqs
    end

let mk_dep (dep,_:t_pkg_id*_) =
  mk_atom ("use " ^ pkg_to_rust_ident dep ^ ";")

let mk_type (ty:t_type) : Easy_format.t =
  match ty.ty_def with
  | D_Alias (pkg,id) ->
    begin match ident_to_rust_ident id with
      | None -> assert false (*FIXME*)
      | Some id -> mk_atom ("pub type " ^ id2_to_rust_ident ty.ty_name ^ " = "
                            ^ pkg_to_rust_ident pkg ^ "::" ^ id ^ ";")
    end
  | D_Int -> mk_atom ("pub type " ^ id2_to_rust_ident ty.ty_name ^ " = i32;")
  | D_Enum elts ->
    begin
      let hd1 = mk_atom ("pub type " ^ id2_to_rust_ident ty.ty_name ^ " = i32;") in
      let hd2 = mk_atom ("pub const " ^ id2_to_rust_ident ty.ty_name ^ "_size : i32 = "
                        ^ string_of_int (List.length elts) ^";") in
      let tl = List.mapi (fun i e ->
          match ident_to_rust_ident e with
          | None -> assert false (*FIXME*)
          | Some e -> mk_atom ("pub const " ^ e ^ ": " ^ id2_to_rust_ident ty.ty_name ^ " = " ^ string_of_int i ^ ";")
        )  elts in
      mk_list "" "" "" Easy_format.list (hd1::hd2::tl)
    end

let mk_const (c:t_constant) : Easy_format.t =
  match c with
  | { c_init=Promoted mch} ->
    mk_atom ("pub use " ^ pkg_to_rust_ident mch ^ "::" ^ id2_to_rust_ident c.c_name ^ ";")
  | { c_init=Init c_init } ->
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
    match b0_type_to_string c.c_type with
      | None -> assert false (*FIXME*)
      | Some typ ->
        if not (is_copy_type c_init.exp0_type) then
          let ty =
            mk_atom ("lazy_static! {pub static ref " ^ id2_to_rust_ident c.c_name ^ ": "
                     ^ typ ^ " =")
          in
          let purity = ref true in
          mk_label 2 true `Auto ty (mk_list "" "" ";}" st [mk_movable false purity false c_init])
        else
          let ty =
            mk_atom ("pub const " ^ id2_to_rust_ident c.c_name ^ ": "
                     ^ typ ^ " =")
          in
          let purity = ref true in
          mk_label 2 true `Auto ty (mk_list "" "" ";" st [mk_expr false purity false c_init])

let mk_ret_id (a:t_arg) : Easy_format.t = mk_atom (id2_to_rust_ident a.arg_name)

let mk_ret_decl (a:t_arg) : Easy_format.t =
match b0_type_to_string a.arg_type with
  | None -> assert false (*FIXME*)
  | Some typ ->
    mk_list 
      ("let mut "^id2_to_rust_ident a.arg_name ^ ": " ^ typ ^ " = ")
      "" ";" Easy_format.list [get_default_value a.arg_type]

let mk_arg_in (a:t_arg) : Easy_format.t =
  let ref = if is_copy_type a.arg_type then "" else "&" in
  match b0_type_to_string a.arg_type with
  | None -> assert false (*FIXME*)
  | Some typ ->
    mk_label 2 true `Auto (mk_atom (id2_to_rust_ident a.arg_name^":"))
      (mk_atom (ref^typ))

let mk_arg_out (a:t_arg) : Easy_format.t =
  match b0_type_to_string a.arg_type with
  | None -> assert false (*FIXME*)
  | Some s -> mk_atom s

let mk_proc_body (p:t_procedure) : Easy_format.t =
  let pr = mk_atom ("pub fn " ^ id_to_rust_ident p.p_name) in
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
    let purity = ref true in
    let s = mk_subst false purity s in
    let s =
      if !purity then s
      else mk_list "state.with(|st| {" "" "} );" Easy_format.list [s]
    in
    let body = mk_list "{" "" "}" st (vars@[s;return]) in
    mk_sequence_nl [proc;body]
  | Renames ts -> mk_atom ("pub use " ^qident_to_string ts^ ";")

let mk_var_decl (id,ty:t_id*t_b0_type) : Easy_format.t =
  mk_list 
    ("let mut "^id_to_rust_ident id ^ ": " ^ b0_type_to_string_exn (id_to_lident id).lid_loc ty ^ " = ")
    "" ";" Easy_format.list [get_default_value ty]

let mk_init (vars:(t_id*t_b0_type) list) (init:t_b0_subst option) : Easy_format.t =
    let decls = List.map mk_var_decl vars in
    let s = match init with
      | None -> {sub0_loc=Utils.dloc;sub0_desc=B0_Null}
      | Some s -> s
    in
    let s = mk_subst true (ref false) s in
    let ret =
      mk_list "State{" "," "}" Easy_format.list
        (List.map (fun (id,_) -> mk_atom (id_to_rust_ident id)) vars)
    in
    mk_sequence_nl (List.rev (ret::s::decls))

let mk_var (id,ty:t_id*t_b0_type) : Easy_format.t =
  mk_label 2 true `Auto (mk_atom (id_to_rust_ident id ^ ":"))
    (mk_atom (b0_type_to_string_exn (id_to_lident id).lid_loc ty))

let mk_state (vars:t_variable list) (init:t_b0_subst option) : Easy_format.t list =
  let vars = Utils.filter_map
     (fun v -> match v.v_name with Intern id -> Some (id,v.v_type) | Extern _ -> None) vars
  in
  if vars = [] then []
  else
    [
      mk_list "struct State {" "," "}" Easy_format.list (List.map mk_var vars);
      mk_list "thread_local!{" "" "}" Easy_format.list
        [mk_list "static state: RefCell<State> = RefCell::new({" "" "})"
           Easy_format.list [(mk_init vars init)] ]
    ]

let package_to_format (pkg:t_package) : Easy_format.t =
  let deps = List.rev_map mk_dep pkg.pkg_dependencies in
  let types = List.map mk_type pkg.pkg_types in
  let consts = List.map mk_const pkg.pkg_constants in
  let vars = mk_state pkg.pkg_variables pkg.pkg_init in
  let procs = List.map mk_proc_body pkg.pkg_procedures in
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
  let body = mk_list "" "" "" st_body (types@consts@vars@procs) in
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
  mk_list "" "" "" st ((mk_atom "#![allow(bad_style,dead_code,unused_mut,unused_imports,unused_variables,unused_assignments)]\n")::(List.rev (body::(mk_atom "")::deps)))

let print_package (out:out_channel) (pkg:t_package) : unit Error.t_result =
  try Ok (Easy_format.Pretty.to_channel out (package_to_format pkg))
  with Error.Error err -> Error err
