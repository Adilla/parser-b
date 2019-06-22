open SyntaxCore
open PSyntax

let is_infix = function
  | First_Projection | Second_Projection | Iteration -> false
  | Product | Difference | Addition | Division | Modulo | Power | Interval | Union
  | Intersection | Relations | Composition | Direct_Product | Parallel_Product
  | Domain_Restriction | Domain_Soustraction | Codomain_Restriction
  | Codomain_Soustraction | Surcharge | Functions _ | Concatenation | Head_Insertion
  | Tail_Insertion | Head_Restriction | Tail_Restriction | Couple _ -> true
  | Application | Image -> assert false

let pf = Format.fprintf
let str = Format.pp_print_string
let close out = Format.pp_close_box out ()
let vbox out = Format.pp_open_vbox out 0
let box out = Format.pp_open_box out 0
let break = Format.pp_print_break 

let pp_lident out (lid:lident) : unit = str out lid.lid_str

type list = {
  box : Format.formatter -> int -> unit;
  opn : string;
  clo : string;
  sep : string;
}

let list = {
  box = Format.pp_open_box;
  opn = "";
  clo = "";
  sep = "";
}

let vlist = {
  box = Format.pp_open_vbox;
  opn = "";
  clo = "";
  sep = "";
}

let hvlist = {
  box = Format.pp_open_hvbox;
  opn = "";
  clo = "";
  sep = "";
}

let pp_list list (pp:Format.formatter -> 'a -> unit)
    (out:Format.formatter) (nlst:'a Nlist.t) : unit =
  let indent = String.length list.opn in
  list.box out 0;
  str out list.opn;
  pp out (Nlist.hd nlst);
  List.iter (fun x ->
      str out list.sep;
      Format.pp_print_break out 1 indent;
      pp out x
    ) (Nlist.tl nlst);
  str out list.clo;
  Format.pp_close_box out ()

let rec get_or_nlist (p:predicate) : predicate Nlist.t =
  match p.prd_desc with
  | Binary_Prop (Disjonction,p1,p2) ->
    Nlist.concat (get_or_nlist p1) (Nlist.make1 p2)
  | _ -> Nlist.make1 p

let rec get_and_nlist (p:predicate) : predicate Nlist.t =
  match p.prd_desc with
  | Binary_Prop (Conjonction,p1,p2) ->
    Nlist.concat (get_and_nlist p1) (Nlist.make1 p2)
  | _ -> Nlist.make1 p

let rec get_seq_nlist (s:substitution) : substitution Nlist.t =
  match s.sub_desc with
  | Sequencement (s1,s2) -> Nlist.concat (get_seq_nlist s1) (Nlist.make1 s2)
  | _ -> Nlist.make1 s

let rec get_par_nlist (s:substitution) : substitution Nlist.t =
  match s.sub_desc with
  | Parallel (s1,s2) -> Nlist.concat (get_par_nlist s1) (Nlist.make1 s2)
  | _ -> Nlist.make1 s

let rec pp_expr out (e:expression) : unit =
  match e.exp_desc with
  | Ident id -> str out id
  | Dollar id -> pf out "%s$0" id
  | Builtin_0 bi -> str out (builtin0_to_string bi)
  | Builtin_1 (Inverse_Relation,e) ->
    pf out "%a~"  pp_expr_wp e
  | Builtin_1 (Unary_Minus,e) ->
    pf out "-%a" pp_expr_wp e
  | Builtin_1 (bi,e) ->
    pp_list { list with opn=(builtin1_to_string bi^"("); clo=")"} pp_expr out (Nlist.make1 e)
  | Builtin_2 (Application,f,a) ->
    pf out "@[%a@;<0 4>(%a)@]" pp_expr_wp f pp_expr a
  | Builtin_2 (Image,e1,e2) ->
    pf out "@[%a@;<0 4>[%a]@]" pp_expr_wp e1 pp_expr e2
  | Builtin_2 (Couple Comma,e1,e2) ->
    pf out "@[(%a,@;<1 1>%a)@]" pp_expr_wp e1 pp_expr_wp e2
  | Builtin_2 (Composition,e1,e2) ->
    pf out "@[(%a;@;<0 4>%a)@]" pp_expr_wp e1 pp_expr_wp e2
  | Builtin_2 (Parallel_Product,e1,e2) ->
    pf out "@[(%a ||@;<1 1>%a)@]" pp_expr_wp e1 pp_expr_wp e2
  | Builtin_2 (bi,e1,e2) ->
    if is_infix bi then
      pf out "@[%a@;<1 0>%s@;<1 0>%a@]" pp_expr_wp e1 (builtin2_to_string bi) pp_expr_wp e2
    else
      pp_list { list with opn=(builtin2_to_string bi^"("); clo=")"; sep=","} pp_expr_wp out (Nlist.make e1 [e2])
  | Pbool p ->
    pp_list { list with opn="bool("; clo=")"} pp_pred out (Nlist.make1 p)
  | Comprehension (xlst,p) ->
    pf out "@[{ %a |@;<1 2>%a }@]" (pp_list { list with sep="," } pp_lident) xlst pp_pred p
  | Binder (bi,xlst,p,e) ->
    begin
      let bi = binder_to_string bi in
      box out;
      pf out "%s(%a)."  bi (pp_list { list with sep="," } pp_lident) xlst;
      break out 0 (String.length bi);
      pf out "(%a |" pp_pred p;
      break out 1 (String.length bi);
      pf out "%a)" pp_expr e;
      close out;
    end
  | Sequence nlst ->
    pp_list { list with opn="["; clo="]"; sep="," } pp_expr_wp out nlst
  | Extension nlst ->
    pp_list { list with opn="{"; clo="}"; sep="," } pp_expr_wp out nlst
  | Record_Field_Access (e,fd) ->
    pf out "@[%a@;<0 4>'%s@]" pp_expr_wp e fd.lid_str
  | Record nlst ->
    pf out "@[rec@;<0 4>%a@]" (pp_list { hvlist with opn="("; clo=")"; sep="," } pp_field) nlst
  | Record_Type nlst ->
    pf out "@[struct@;<0 4>%a@]" (pp_list { hvlist with opn="("; clo=")"; sep="," } pp_field) nlst

and pp_expr_wp out (e:expression) : unit =
  match e.exp_desc with
  | Builtin_2 (Composition,_,_)
  | Builtin_2 (Couple Comma,_,_)
  | Builtin_2 (Parallel_Product,_,_)
  | Builtin_2 (Image,_,_)
  | Builtin_2 (Application,_,_) -> pp_expr out e

  | Builtin_1 (Unary_Minus,_)
  | Builtin_1 (Inverse_Relation,_) -> pf out "(%a)" pp_expr e

  | Builtin_2 (bi,_,_) when is_infix bi -> pf out "(%a)" pp_expr e

  | Builtin_1 _ | Builtin_2 _ | Record_Field_Access _ | Ident _ | Dollar _
  | Pbool _ | Builtin_0 _ | Comprehension _ | Binder _ | Sequence _
  | Extension _ | Record _ | Record_Type _ -> pp_expr out e

and pp_field out (fd,e:lident*expression) : unit =
  pf out "@[%s:@;<1 4>%a@]" fd.lid_str pp_expr_wp e

and pp_pred out (p:predicate) : unit =
  match p.prd_desc with
  | P_Builtin Btrue -> str out "btrue"
  | P_Builtin Bfalse -> str out "bfalse"
  | Binary_Prop (Conjonction,_,_) ->
    let nlst = get_and_nlist p in
    pp_list { vlist with sep=" &" } pp_pred_wp out nlst
  | Binary_Prop (Disjonction,_,_) ->
    let nlst = get_or_nlist p in
    pp_list { vlist with sep=" or" } pp_pred_wp out nlst
  | Binary_Prop (bop,p1,p2) ->
    pf out "@[%a %s@;<1 4>%a@]" pp_pred_wp p1 (prop_bop_to_string bop) pp_pred_wp p2
  | Binary_Pred (bop,e1,e2) ->
    pf out "@[%a %s@;<1 4>%a@]" pp_expr_wp e1 (pred_bop_to_string bop) pp_expr_wp e2
  | Negation p ->
    pp_list { list with opn="not("; clo=")" } pp_pred out (Nlist.make1 p)
  | Universal_Q (xlst,p) ->
    pf out "@[!(%a).@;<0 4>(%a)@]" (pp_list { list with sep="," } pp_lident) xlst pp_pred p
  | Existential_Q (xlst,p) ->
    pf out "@[#(%a).@;<0 4>(%a)@]" (pp_list { list with sep="," } pp_lident) xlst pp_pred p

and pp_pred_wp out (p:predicate) : unit =
  match p.prd_desc with
  | P_Builtin _ | Negation _ | Universal_Q _
  | Existential_Q _ | Binary_Pred _-> pp_pred out p
  | Binary_Prop _ -> pf out "(%a)" pp_pred p

let rec pp_subst out (s:substitution) : unit =
  match s.sub_desc with
  | Skip -> str out "skip"

  | Affectation (Tuple xlst,e) ->
    pf out "@[%a :=@;<1 4>%a@]" (pp_list { list with sep="," } pp_lident) xlst pp_expr e

  | Affectation (Function(f,alst),e) ->
    let aux out e = pf out "(%a)" pp_expr e in
    pf out "@[%s@,%a :=@;<1 4>%a@]" f.lid_str (pp_list { list with sep="" } aux) alst pp_expr e

  | Affectation (Record(rf,fd),e) ->
    pf out "@[%s@,'%s :=@;<1 4>%a@]" rf.lid_str fd.lid_str pp_expr e

  | Pre (p,s) ->
    begin
      vbox out;
      str out "PRE"; break out 0 4;
      pp_pred out p; break out 0 0;
      str out "THEN"; break out 0 4;
      pp_subst out s; break out 0 0;
      str out "END";
      close out;
    end

  | Assert (p,s) ->
    begin
      vbox out;
      str out "ASSERT"; break out 0 4;
      pp_pred out p; break out 0 0;
      str out "THEN"; break out 0 4;
      pp_subst out s; break out 0 0;
      str out "END";
      close out;
    end

  | Choice slst ->
    begin
      vbox out;
      str out "CHOICE"; break out 0 4;
      pp_subst out (Nlist.hd slst); break out 0 0;
      List.iter (fun s ->
          str out "OR"; break out 0 4;
          pp_subst out s; break out 0 0;
      ) (Nlist.tl slst);
      str out "END";
      close out;
    end

  | IfThenElse (pslst,opt) ->
    begin
      let (p,s) = Nlist.hd pslst in
      vbox out;
      str out "IF"; break out 0 4;
      pp_pred out p; break out 0 0;
      str out "THEN"; break out 0 4;
      pp_subst out s; break out 0 0;
      List.iter (fun (p,s) ->
          str out "ELSIF"; break out 0 4;
          pp_pred out p; break out 0 0;
          str out "THEN"; break out 0 4;
          pp_subst out s; break out 0 0;
      ) (Nlist.tl pslst);
      (match opt with
       | None -> ()
       | Some s ->
         (str out "ELSE"; break out 0 4;
          pp_subst out s; break out 0 0)
      );
      str out "END";
      close out;
    end

  | Select (pslst,opt) ->
    begin
      let (p,s) = Nlist.hd pslst in
      vbox out;
      str out "SELECT"; break out 0 4;
      pp_pred out p; break out 0 0;
      str out "THEN"; break out 0 4;
      pp_subst out s; break out 0 0;
      List.iter (fun (p,s) ->
          str out "WHEN"; break out 0 4;
          pp_pred out p; break out 0 0;
          str out "THEN"; break out 0 4;
          pp_subst out s; break out 0 0;
      ) (Nlist.tl pslst);
      (match opt with
       | None -> ()
       | Some s ->
         (str out "ELSE"; break out 0 4;
          pp_subst out s; break out 0 0)
      );
      str out "END";
      close out;
    end
    
  | Case (c,eslst,opt) ->
    begin
      let (nlst,s) = Nlist.hd eslst in
      vbox out;
      str out "CASE"; break out 0 4;
      pp_expr out c; break out 0 0;
      str out "OF"; break out 0 4;
      str out "EITHER";break out 0 8;
      pp_list { list with sep="," } pp_expr_wp out nlst; break out 0 4;
      str out "THEN";break out 0 8;
      pp_subst out s; break out 0 4;
      List.iter (fun (nlst,s) ->
          str out "OR"; break out 0 8;
          pp_list { list with sep="," } pp_expr_wp out nlst; break out 0 4;
          str out "THEN"; break out 0 8;
          pp_subst out s; break out 0 4;
      ) (Nlist.tl eslst);
      (match opt with
       | None -> ()
       | Some s ->
         (str out "ELSE"; break out 0 8;
          pp_subst out s; break out 0 4)
      );
      str out "END";
      break out 0 0;
      str out "END";
      close out;
    end

  | Any (xlst,p,s) ->
    begin
      vbox out;
      str out "ANY"; break out 0 4;
      pp_list { list with sep="," } pp_lident out xlst; break out 0 0;
      str out "WHERE"; break out 0 4;
      pp_pred out p; break out 0 0;
      str out "THEN"; break out 0 4;
      pp_subst out s; break out 0 0;
      str out "END";
      close out;
    end

  | Let (xlst,ielst,s) ->
    begin
      let aux (id,e) = pf out "@[%s =@;<1 4>%a@]" id.lid_str pp_expr e in
      vbox out;
      str out "LET"; break out 0 4;
      pp_list { list with sep="," } pp_lident out xlst; break out 0 0;
      str out "BE"; break out 0 4;
      aux (Nlist.hd ielst);
      List.iter (fun x ->
          str out " &"; break out 0 4;
          aux x;
        ) (Nlist.tl ielst);
      break out 0 0;
      str out "IN"; break out 0 4;
      pp_subst out s; break out 0 0;
      str out "END";
      close out;
    end

  | BecomesElt (xlst,e) ->
    pf out "@[%a@ ::@;<1 4>%a@]" (pp_list { list with sep="," } pp_lident) xlst pp_expr e

  | BecomesSuch (xlst,p) ->
    pf out "@[%a@ :(@;<1 4>%a )@]" (pp_list { list with sep="," } pp_lident) xlst pp_pred p

  | Var (xlst,s) ->
    begin
      vbox out;
      str out "VAR"; break out 0 4;
      pp_list { list with sep="," } pp_lident out xlst; break out 0 0;
      str out "IN"; break out 0 4;
      pp_subst out s; break out 0 0;
      str out "END";
      close out;
    end

  | CallUp ([],f,[]) -> str out f.lid_str
  | CallUp ([],f,hd::tl) ->
    pf out "@[%s@,(%a)@]" f.lid_str (pp_list { list with sep="," } pp_expr_wp) (Nlist.make hd tl)
  | CallUp (hd::tl,f,[]) ->
    pf out "@[%a@ <--@ %s@]" (pp_list { list with sep="," } pp_lident) (Nlist.make hd tl) f.lid_str 
  | CallUp (ohd::otl,f,ihd::itl) ->
    pf out "@[%a@ <--@ %s@,(%a)@]"
      (pp_list { list with sep="," } pp_lident) (Nlist.make ohd otl)
      f.lid_str 
      (pp_list { list with sep="," } pp_expr_wp) (Nlist.make ihd itl)

  | While (p,s,q,e) ->
    begin
      vbox out;
      str out "WHILE"; break out 0 4;
      pp_pred out p; break out 0 0;
      str out "DO"; break out 0 4;
      pp_subst out s; break out 0 0;
      str out "INVARIANT"; break out 0 4;
      pp_pred out q; break out 0 0;
      str out "VARIANT"; break out 0 4;
      pp_expr out e; break out 0 0;
      str out "END";
      close out;
    end

  | Sequencement _ ->
    begin
      let nlst = get_seq_nlist s in
      vbox out;
      pp_subst_wp out (Nlist.hd nlst);
      List.iter (fun s ->
          str out ";"; break out 0 0;
          pp_subst_wp out s;
        ) (Nlist.tl nlst);
      close out;
    end

  | Parallel _ ->
    begin
      let nlst = get_par_nlist s in
      vbox out;
      pp_subst_wp out (Nlist.hd nlst);
      List.iter (fun s ->
          str out " ||"; break out 0 0;
          pp_subst_wp out s;
        ) (Nlist.tl nlst);
      close out;
    end

and pp_subst_wp out (s:substitution) : unit =
  match s.sub_desc with
  | Skip | Affectation _ | Pre _ | Assert _ | Choice _ | IfThenElse _ | Select _
  | Case _ | Any _ | Let _ | BecomesElt _ | BecomesSuch _ | Var _ | CallUp _
  | While _ -> pp_subst out s
  | Sequencement _ | Parallel _ ->
    begin
      vbox out;
      str out "BEGIN";
      break out 0 4;
      pp_subst out s;
      break out 0 0;
      str out "END";
      close out
    end
  
let pp_minst out (mi:machine_instanciation) : unit =
  match mi.mi_params with
  | [] -> str out mi.mi_mch.lid_str
  | hd::tl ->
    pf out "@[%s(%a)@]" mi.mi_mch.lid_str (pp_list { list with sep="," } pp_expr_wp) (Nlist.make hd tl)

let pp_set out (x:set) : unit =
  match x with
  | Abstract_Set s -> str out s.lid_str
  | Concrete_Set (s,[]) -> pf out "@[%s =@ {}@]" s.lid_str
  | Concrete_Set (s,hd::tl) ->
    pf out "@[%s =@ {%a}@]" s.lid_str (pp_list { list with sep="," } pp_lident) (Nlist.make hd tl)

let pp_value out (v,e) : unit =
  pf out "@[%s =@ %a@]" v.lid_str pp_expr e

let pp_op_header out (op:operation) : unit =
  match op.op_out, op.op_in with
  | [], [] -> str out op.op_name.lid_str
  | [], hd::tl ->
    pf out "@[%s@,(%a)@]" op.op_name.lid_str (pp_list { list with sep="," } pp_lident) (Nlist.make hd tl)
  | hd::tl, [] ->
    pf out "@[%a@ <--@ %s@]" (pp_list { list with sep="," } pp_lident) (Nlist.make hd tl) op.op_name.lid_str 
  | ohd::otl, ihd::itl ->
    pf out "@[%a@ <--@ %s@,(%a)@]"
      (pp_list { list with sep="," } pp_lident) (Nlist.make ohd otl)
      op.op_name.lid_str 
      (pp_list { list with sep="," } pp_lident) (Nlist.make ihd itl)

let pp_op out (op:operation) : unit =
  pf out "@[%a =@ %a@]" pp_op_header op pp_subst_wp op.op_body
  
let pp_clause out : clause -> unit = function
  | Constraints p -> ( str out "CONSTRAINTS"; break out 0 4; pp_pred out p )
  | Imports lst -> ( str out "IMPORTS"; break out 0 4; pp_list { vlist with sep="," } pp_minst out lst )
  | Includes lst -> ( str out "INCLUDES"; break out 0 4; pp_list { vlist with sep="," } pp_minst out lst )
  | Extends lst -> ( str out "EXTENDS"; break out 0 4; pp_list { vlist with sep="," } pp_minst out lst )
  | Properties p -> ( str out "PROPERTIES"; break out 0 4; pp_pred out p )
  | Invariant p -> ( str out "INVARIANT"; break out 0 4; pp_pred out p )
  | Assertions lst -> ( str out "ASSERTIONS"; break out 0 4; pp_list { vlist with sep=";" } pp_pred out lst )
  | Initialization s -> ( str out "INITIALISATION"; break out 0 4; pp_subst out s )
  | Operations lst -> ( str out "OPERATIONS"; break out 0 4; pp_list { vlist with sep=";" } pp_op out lst )
  | Local_Operations lst -> ( str out "LOCAL_OPERATIONS"; break out 0 4; pp_list { vlist with sep=";" } pp_op out lst )
  | Values lst -> ( str out "VALUES"; break out 0 4; pp_list { vlist with sep=";" } pp_value out lst )
  | Sees lst -> ( str out "SEES"; break out 0 4; pp_list { vlist with sep="," } pp_lident out lst )
  | Promotes lst -> ( str out "PROMOTES"; break out 0 4; pp_list { vlist with sep="," } pp_lident out lst )
  | Uses lst -> ( str out "USES"; break out 0 4; pp_list { vlist with sep="," } pp_lident out lst )
  | Sets lst -> ( str out "SETS"; break out 0 4; pp_list { vlist with sep=";" } pp_set out lst )
  | Constants lst -> ( str out "CONSTANTS"; break out 0 4; pp_list { vlist with sep="," } pp_lident out lst )
  | Abstract_constants lst -> ( str out "ABSTRACT_CONSTANTS"; break out 0 4; pp_list { vlist with sep="," } pp_lident out lst )
  | Concrete_variables lst -> ( str out "CONCRETE_VARIABLES"; break out 0 4; pp_list { vlist with sep="," } pp_lident out lst )
  | Variables lst -> ( str out "VARIABLES"; break out 0 4; pp_list { vlist with sep="," } pp_lident out lst )
  | Refines mch -> ( str out "REFINES"; break out 0 4; pp_lident out mch )

let pp_machine out name params clauses : unit =
  vbox out;
  str out "MACHINE"; break out 0 4;
  (match params with
   | [] -> str out name.lid_str
   | hd::tl -> pf out "@[%s(%a)@]" name.lid_str (pp_list { list with sep="," } pp_lident) (Nlist.make hd tl) );
  break out 0 0;
  List.iter (fun cl -> break out 0 0; pp_clause out cl; break out 0 0) clauses;
  str out "END";
  Format.pp_print_flush out ();
  close out

let pp_refinement out name params clauses =
  vbox out;
  str out "REFINEMENT"; break out 0 4;
  (match params with
   | [] -> str out name.lid_str
   | hd::tl -> pf out "@[%s(%a)@]" name.lid_str (pp_list { list with sep="," } pp_lident) (Nlist.make hd tl) );
  break out 0 0; break out 0 0;
  List.iter (fun cl -> break out 0 0; pp_clause out cl; break out 0 0) clauses;
  str out "END";
  Format.pp_print_flush out ();
  close out

let pp_implementation out name params clauses =
  vbox out;
  str out "IMPLEMENTATION"; break out 0 4;
  (match params with
   | [] -> str out name.lid_str
   | hd::tl -> pf out "@[%s(%a)@]" name.lid_str (pp_list { list with sep="," } pp_lident) (Nlist.make hd tl) );
  break out 0 0; break out 0 0;
  List.iter (fun cl -> break out 0 0;pp_clause out cl; break out 0 0) clauses;
  str out "END";
  Format.pp_print_flush out ();
  close out

let pp_component out co : unit =
  match co.co_desc with
  | Machine _ ->
    pp_machine out co.co_name co.co_parameters (get_clauses co)
  | Refinement _ ->
    pp_refinement out co.co_name co.co_parameters (get_clauses co)
  | Implementation _ ->
    pp_implementation out co.co_name co.co_parameters (get_clauses co)

let print_expression out = pp_expr (Format.formatter_of_out_channel out)
let print_predicate out = pp_pred (Format.formatter_of_out_channel out)
let print_substitution out = pp_subst (Format.formatter_of_out_channel out)
let print_component out = pp_component (Format.formatter_of_out_channel out)

let expression_to_string e =
  let bf = Buffer.create 447 in
  pp_expr (Format.formatter_of_buffer bf) e;
  Buffer.contents bf

let predicate_to_string p =
  let bf = Buffer.create 447 in
  pp_pred (Format.formatter_of_buffer bf) p;
  Buffer.contents bf

let substitution_to_string s =
  let bf = Buffer.create 447 in
  pp_subst (Format.formatter_of_buffer bf) s;
  Buffer.contents bf

let component_to_string c =
  let bf = Buffer.create 447 in
  let out = Format.formatter_of_buffer bf in
  pp_component out c;
  Format.pp_print_flush out ();
  Buffer.contents bf
