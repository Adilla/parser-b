open Codegen.Ada

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
  | None -> Codegen.Ada_ident.to_string ts.q_id
  | Some ns -> (Codegen.Ada_ident.pkg_to_string ns) ^ "." ^ (Codegen.Ada_ident.to_string ts.q_id)

let b0_constant_to_string : t_b0_constant -> string = function
  | B0_Integer i -> string_of_int i
  | B0_String s -> "\"" ^ s ^ "\""
  | B0_MaxInt -> "Integer'Last"
  | B0_MinInt -> "Integer'First"
  | B0_True -> "True"
  | B0_False -> "False"

let b0_binary_op_to_string : t_b0_binary_op -> string = function
  | B0_Conjonction -> "and then"
  | B0_Disjonction -> "or else"
  | B0_Equality -> "="
  | B0_Disequality -> "/="
  | B0_Inequality Syntax.Smaller_or_Equal -> "<="
  | B0_Inequality Syntax.Strictly_Smaller -> "<"
  | B0_Inequality Syntax.Greater_or_Equal -> ">="
  | B0_Inequality Syntax.Strictly_Greater -> ">"
  | B0_Product -> "*"
  | B0_Difference -> "-"
  | B0_Addition -> "+"
  | B0_Division -> "/"
  | B0_Modulo -> "mod"
  | B0_Power -> "**"

let b0_unary_op_to_string : t_b0_unary_op -> string = function
  | B0_Negation -> "not"
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

let mk_fun_app (f:string) (args:Easy_format.t Nlist.t Nlist.t) : Easy_format.t  =
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
  let mk_args nle = mk_list "(" "," ")" st (Nlist.to_list nle) in
  mk_label 2 false `Auto (mk_atom f) (mk_list "" "" "" st (List.map mk_args (Nlist.to_list args)))

let mk_proc_call (f:string) (args:Easy_format.t list) : Easy_format.t  =
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
  mk_label 2 false `Auto (mk_atom f) (mk_list "(" "," ");" st args)

let mk_array_init (e:Easy_format.t) : Easy_format.t =
  let st = { Easy_format.list with
             Easy_format.space_after_opening = true;
             stick_to_label = false;
             space_before_separator = false;
             space_after_separator = false;
             separators_stick_left = false;
             space_before_closing = false;
             align_closing = false;
             wrap_body = `Wrap_atoms;
             indent_body = 2 } in
  mk_list "(others =>" "" ")" st [e]

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
  let aux i e = mk_label 2 true `Auto (mk_atom (string_of_int i ^ " =>")) e in
  mk_list "(" "," ")" st (List.mapi aux lst)

let rec mk_expr (e0:t_b0_expr) : Easy_format.t =
  match e0.exp0_desc with
  | B0_Ident ts -> mk_ident ts 
  | B0_Builtin_0 cst -> mk_const cst
  | B0_Builtin_1 (op,e) -> mk_prefix_op op (mk_expr_wp e)
  | B0_Builtin_2 ((B0_Equality|B0_Disequality) as op,e1,e2) ->
    mk_infix_op op (mk_expr_wp e1) (mk_expr_wp e2)
  | B0_Builtin_2 (op,e1,e2) -> mk_infix_op op (mk_expr_wp e1) (mk_expr_wp e2)
  | B0_Application (f,lst) -> 
    mk_fun_app (t_symb_to_string f) (Nlist.make1 (Nlist.map mk_expr lst))
  | B0_Array lst -> mk_array (List.map mk_expr lst)
  | B0_Array_Access (f,arg) ->
    mk_fun_app (t_symb_to_string f) (Nlist.make1 (Nlist.make1 (mk_expr arg)))

and mk_expr_wp (e0:t_b0_expr) : Easy_format.t =
  match e0.exp0_desc with
  | B0_Builtin_1 _ | B0_Builtin_2 _ -> add_par (mk_expr e0)
  | _ -> mk_expr e0

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

(*
let rec pp_type out = function
  | T_Int -> Printf.fprintf out "Int"
  | T_Bool -> Printf.fprintf out "Bool"
  | T_String -> Printf.fprintf out "String"
  | T_Abstract id -> Printf.fprintf out "Abstract(%s)" (Codegen.Ada_ident.to_string id.q_id)
  | T_Array (sz,A_Int) -> Printf.fprintf out "Int[%i]" sz
  | T_Array (sz,A_Bool) -> Printf.fprintf out "Bool[%i]" sz
  | T_Array (sz,A_String) -> Printf.fprintf out "String[%i]" sz
  | T_Function (t_args,t_res) ->
    begin
      Printf.fprintf out "fun(%a" pp_type (Nlist.hd t_args);
      List.iter (Printf.fprintf out ",%a" pp_type) (Nlist.tl t_args);
      Printf.fprintf out ") return %a" pp_type t_res
    end

let print_type (ty:t_b0_type) : unit =
  Printf.fprintf stdout "%a\n" pp_type ty
*)

let b0_type_to_string : t_b0_type -> string = function
  | T_Int -> "Types.Int"
  | T_Bool -> "Types.Bool"
  | T_String -> "string"
  | T_Abstract ts -> t_symb_to_string ts
  | T_Array (sz,A_Int) -> "Types.T_Int_Array_" ^ string_of_int sz
  | T_Array (sz,A_Bool) -> "Types.T_Bool_Array_" ^ string_of_int sz
  | T_Array (sz,A_String) -> "Types.T_String_Array_" ^ string_of_int sz

let rec mk_subst (s0:t_b0_subst) : Easy_format.t =
  match s0.sub0_desc with
  | B0_Null -> mk_atom "null;"
  | B0_Affectation nle ->
    let st = Easy_format.list in
    let vars = mk_list "" "," "" st (List.map (fun (x,_) -> mk_atom (Codegen.Ada_ident.to_string x)) (Nlist.to_list nle)) in
    let defs = mk_list "" "," "" st (List.map (fun (_,e) -> mk_expr e) (Nlist.to_list nle)) in
    let st = Easy_format.list in
    mk_list "" ":=" ";" st [vars;defs]
  | B0_Array_Affectation (f,args,e) ->
    let st = Easy_format.list in
    let args = mk_list "(" "," ")" st (List.map mk_expr (Nlist.to_list args)) in
    let arr = mk_label 2 false `Auto (mk_atom (Codegen.Ada_ident.to_string f)) args in
    let def = mk_expr e in
    let st = Easy_format.list in
    mk_list "" ":=" ";" st [arr;def]
  | B0_IfThenElse (cases,def) ->
    let (p,s) = Nlist.hd cases in
    let st = Easy_format.list in
    let ifthen = mk_list "if" "" "then" st [mk_expr p] in
    let ifthen_s = mk_label 2 false `Always_rec ifthen (mk_subst s) in
    let aux (p,s) =
      let elsifthen = mk_list "elsif" "" "then" st [mk_expr p] in
      mk_label 2 false `Always_rec elsifthen (mk_subst s)
    in
    let clst = List.map aux (Nlist.tl cases) in
    let endif = mk_atom "end if;" in
    let lst =
      begin match def with
        | None -> ifthen_s::(clst@[endif])
        | Some s ->
          let els = mk_label 2 true `Always_rec (mk_atom "else") (mk_subst s) in
          ifthen_s::(clst@[els;endif])
      end
    in
    mk_sequence_nl lst
  | B0_Case (e,cases,def) ->
    let st = Easy_format.list in
    let case = mk_list "case" "" "is" st [mk_expr e] in
    let aux (lst,s) =
      let lst = List.map (fun e -> mk_atom (string_of_int e)) (Nlist.to_list lst) in
      let st = Easy_format.list in
      let whn = mk_list "  when" "," "=>" st lst in
      mk_label 4 true `Auto whn (mk_subst s)
    in
    let clst = List.map aux (Nlist.to_list cases) in
    let lst = match def with
      | None -> case::(clst@[mk_atom "end case;"])
      | Some s ->
        let others = mk_label 4 true `Auto (mk_atom "  when others =>") (mk_subst s) in
        case::(clst@[others;mk_atom "end case;"])
    in
    mk_sequence_nl lst
  | B0_Var (vars,s) ->
    let aux (id,ty) = mk_atom (Codegen.Ada_ident.to_string id ^ ": " ^ b0_type_to_string ty ^ ";") in
    let st = Easy_format.list in
    let vars = mk_list "" "" "" st (List.map aux (Nlist.to_list vars)) in
    let decl = mk_label 2 true `Always_rec (mk_atom "declare") vars in
    let st = Easy_format.list in
    let block = mk_list "begin" "" "end;" st [mk_subst s] in
    mk_sequence_nl [decl;block]
  | B0_While (cond,s) ->
    mk_sequence_nl [ mk_label 2 true `Always_rec (mk_atom "while") (mk_expr cond);
                     mk_label 2 true `Always_rec (mk_atom "loop") (mk_subst s);
                     mk_atom "end loop;"]
  | B0_CallUp ([],f,[]) -> mk_atom (t_symb_to_string f ^ ";")
  | B0_CallUp (out,f,args) ->
    let args = (List.map mk_expr args)@
               (List.map (fun x -> mk_atom (Codegen.Ada_ident.to_string x)) out) in
    mk_proc_call (t_symb_to_string f) args
  | B0_Sequencement (s1,s2) ->
    begin match get_seq_list s0 with
      | [] -> mk_atom "null;"
      | seqs ->
        let seqs = List.map (fun s -> mk_subst s) seqs in
        let st = Easy_format.list in
        mk_list "" "" "" st seqs
    end

let mk_dep (dep:Codegen.Ada_ident.t_pkg_id) =
  mk_atom ("with " ^ Codegen.Ada_ident.pkg_to_string dep ^ ";")

let mk_type (ty:t_type) : Easy_format.t =
  match ty.ty_def with
  | D_Enumerate lst ->
    let st = { Easy_format.list with
               Easy_format.space_after_opening = false;
               stick_to_label = false;
               space_before_separator = false;
               space_after_separator = true;
               separators_stick_left = true;
               space_before_closing = false;
               align_closing = false;
               wrap_body = `Wrap_atoms;
               indent_body = 3 }
    in
    let elts = List.map (fun x -> mk_atom (Codegen.Ada_ident.to_string x)) lst in
    mk_label 2 true `Auto (mk_atom ("type " ^ Codegen.Ada_ident.to_string ty.ty_name ^ " is"))
      (mk_list "(" "," ");" st elts)
  | D_Alias ts -> mk_atom ("subtype " ^ Codegen.Ada_ident.to_string ty.ty_name ^ " is " ^ t_symb_to_string ts ^ ";")
  | D_Int -> mk_atom ("subtype " ^ Codegen.Ada_ident.to_string ty.ty_name ^ " is Integer;")

let mk_arg2 (i:int) (ty:t_b0_type) : Easy_format.t =
  mk_label 2 true `Auto (mk_atom ("Arg"^string_of_int i^":"))
    (mk_atom ("in "^ b0_type_to_string ty))

let mk_func_spec (fname:Codegen.Ada.t_id) (args:Codegen.Ada.t_b0_type list) (tg:Codegen.Ada.t_b0_type) : Easy_format.t option =
  let fn = mk_atom ("function " ^ Codegen.Ada_ident.to_string fname) in
  let args = List.mapi mk_arg2 args in
  let st = Easy_format.list in
  let fn =
    if args = [] then fn
    else
      let args = mk_list "(" ";" ")" st args in
      mk_label 2 false `Auto fn args
  in
  let ret = mk_atom ("return "^b0_type_to_string tg^";") in
  Some (mk_sequence_nl [fn;ret])

let mk_const (c:t_constant) : Easy_format.t option =
  match c.c_type with
  | Fun (args,tg) -> mk_func_spec c.c_name (Nlist.to_list args) tg
  | Cst ty ->
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
    let ty = mk_atom (Codegen.Ada_ident.to_string c.c_name ^ ": constant "
                      ^ b0_type_to_string ty ^ " :=")
    in
    Some (mk_label 2 true `Auto ty (mk_list "" "" ";" st [mk_expr c.c_init]))

let mk_var (v:t_variable) : Easy_format.t =
  mk_atom (Codegen.Ada_ident.to_string v.v_name ^ ": " ^ b0_type_to_string v.v_type ^ ";")

let t_mode_to_string b = if b then "out" else "in"

let mk_arg (is_out:bool) (a:t_arg) : Easy_format.t =
  mk_label 2 true `Auto (mk_atom (Codegen.Ada_ident.to_string a.arg_name^":"))
    (mk_atom (t_mode_to_string is_out^" "^ b0_type_to_string a.arg_type))

let mk_proc_spec (p:t_procedure) : Easy_format.t =
  let args = (List.map (mk_arg false) p.p_args_in)@(List.map (mk_arg true) p.p_args_out) in
  match p.p_body with
  | Body _ ->
    begin match args with
      | [] -> mk_atom ("procedure " ^ Codegen.Ada_ident.to_string p.p_name ^ ";")
      | _ ->
        let lb = mk_atom ("procedure " ^ Codegen.Ada_ident.to_string p.p_name) in
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
        let args = mk_list "(" ";" ");" st args in
        mk_label 2 false `Always_rec lb args
    end
  | Renames ts ->
    begin match args with
      | [] ->
        let lb = mk_atom ("procedure " ^ Codegen.Ada_ident.to_string p.p_name) in
        let rn = mk_atom ("renames " ^ t_symb_to_string ts ^ ";") in
        mk_label 2 true `Auto lb rn
      | _ ->
        let lb = mk_atom ("procedure " ^ Codegen.Ada_ident.to_string p.p_name) in
        let st = Easy_format.list in
        let args = mk_list "(" ";" ")" st args in
        let proc = mk_label 2 false `Always_rec lb args in
        let rn = mk_atom ("renames " ^ t_symb_to_string ts ^ ";") in
        mk_sequence_nl [proc;rn]
    end

let mk_proc_spec_if_global (p:t_procedure) : Easy_format.t option =
  if p.p_is_local then None
  else Some (mk_proc_spec p)

let package_to_format_spec (pkg:t_package) : Easy_format.t =
  let deps = List.rev_map mk_dep ((match Codegen.Ada_ident.make_pkg_id "Types" with None -> assert false | Some id -> id)::pkg.pkg_dependencies) in
  let types = List.map mk_type pkg.pkg_types in
  let consts = Utils.filter_map mk_const pkg.pkg_constants in
  let vars = List.map mk_var pkg.pkg_variables in
  let procs = Utils.filter_map mk_proc_spec_if_global pkg.pkg_procedures in
  let st_body = { Easy_format.list with
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
  let body = mk_list ("package "^Codegen.Ada_ident.pkg_to_string pkg.pkg_name^" is") ""
      ("end "^Codegen.Ada_ident.pkg_to_string pkg.pkg_name^";") st_body
      (types@consts@vars@procs)
  in
  if deps = [] then body
  else
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
    mk_list "" "" "" st (List.rev (body::(mk_atom "")::deps))

let mk_proc_body (p:t_procedure) : Easy_format.t option =
  let pr = mk_atom ("procedure " ^ Codegen.Ada_ident.to_string p.p_name) in
  let args = (List.map (mk_arg false) p.p_args_in)@(List.map (mk_arg true) p.p_args_out) in
  match p.p_body with
  | Body s ->
    let proc =
      if args = [] then pr
      else
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
        let args = mk_list "(" ";" ")" st args in
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
    let body = mk_list "begin" "" ("end " ^ Codegen.Ada_ident.to_string p.p_name ^ ";") st [mk_subst s] in
    Some (mk_sequence_nl [proc;mk_atom "is";body])
  | Renames ts -> None

let mk_local_proc_spec (p:t_procedure) : Easy_format.t option =
  if not p.p_is_local then None
  else Some (mk_proc_spec p)

let mk_fun_body (c:t_constant) : Easy_format.t option =
  match c.c_type with
  | Fun (t_args,t_res) ->
    let fn = mk_atom ("function " ^ Codegen.Ada_ident.to_string c.c_name) in
    let args = List.mapi mk_arg2 (Nlist.to_list t_args) in
    let st = Easy_format.list in
    let fn =
      if args = [] then fn
      else
        let args = mk_list "(" ";" ")" st args in
        mk_label 2 false `Auto fn args
    in
    let ret_ty = mk_atom ("return "^b0_type_to_string t_res) in
    let st = Easy_format.list in
    let ret =  mk_list  "return" "" ";" st [mk_expr c.c_init] in
    let st = Easy_format.list in
    let body = mk_list "begin" "" ("end "^Codegen.Ada_ident.to_string c.c_name^";") st [ret] in
    Some (mk_sequence_nl [fn;ret_ty;mk_atom "is";body])
  | Cst _ -> None
 
let package_to_format_body (pkg:t_package) : Easy_format.t option =
  let lst1 = Utils.filter_map mk_local_proc_spec pkg.pkg_procedures in
  let lst2 = Utils.filter_map mk_fun_body pkg.pkg_constants in
  let lst3 = Utils.filter_map mk_proc_body pkg.pkg_procedures in
  let lst4 = match pkg.pkg_init with
    | None -> []
    | Some s -> [mk_label 2 false `Always_rec (mk_atom "begin") (mk_subst s)]
  in
  let procs = lst1@lst2@lst3@lst4 in
  match procs with
  | [] -> None
  | _ ->
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
    Some (mk_list ("package body "^Codegen.Ada_ident.pkg_to_string pkg.pkg_name^" is") ""
            ("end "^Codegen.Ada_ident.pkg_to_string pkg.pkg_name^";") st procs)

let print_package_spec (out:out_channel) (pkg:t_package) : unit Error.t_result =
  try Ok (Easy_format.Pretty.to_channel out (package_to_format_spec pkg))
  with Error.Error err -> Error err

let print_package_body (out:out_channel) (pkg:t_package) : unit Error.t_result =
  try
    begin match package_to_format_body pkg with
      | Some ef -> Ok (Easy_format.Pretty.to_channel out ef)
      | None -> Ok ()
    end
  with Error.Error err -> Error err

let some_constant_is_a_function lst =
  let aux c = match c.c_type with
    | Fun _ -> true
    | Cst _ -> false
  in
  List.exists aux lst

let is_package_body_empty pkg =
  match pkg.pkg_procedures, pkg.pkg_init with
  | [], None -> not (some_constant_is_a_function pkg.pkg_constants)
  | lst, None  ->
    not (some_constant_is_a_function pkg.pkg_constants) &&
    List.for_all (fun p -> match p.p_body with Body _ -> false | Renames _ -> true ) lst
  | _, __ -> false
