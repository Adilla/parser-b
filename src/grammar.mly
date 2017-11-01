%{
open Utils
open Syntax

let mk_builtin exp_loc bi = { exp_loc; exp_typ=(); exp_desc=(Builtin bi) }
let mk_var var_loc var_id : p_var = { var_loc; var_typ=(); var_id } 
let mk_minst mi_mch mi_params : p_machine_instanciation = { mi_mch; mi_params }
let mk_expr exp_loc exp_desc : p_expression = { exp_loc; exp_desc; exp_typ=() }
let mk_pred prd_loc prd_desc : p_predicate = { prd_loc; prd_desc }
let mk_subst sub_loc sub_desc : p_substitution = { sub_loc; sub_desc }
let mk_clause cl_loc cl_desc : p_clause = { cl_loc; cl_desc }
let mk_lident lid_loc lid_str : p_lident = { lid_loc; lid_str }
let mk_operation op_out op_name op_in op_body : p_operation = { op_out; op_name; op_in; op_body } 

let mk_infix lc lc_bi bi a1 a2 =
  let f = mk_builtin lc_bi bi in
  mk_expr lc (Application (f,mk_expr lc (Couple(Infix,a1,a2))))

let mk_prefix exp_loc bi e =
  let f = mk_builtin exp_loc bi in
  { exp_loc; exp_typ=(); exp_desc=Application(f,e) }

let mk_binder exp_loc bi xlst p e =
  { exp_loc; exp_typ=(); exp_desc=Binder(bi,xlst,p,e) }

let expr_to_list (e:p_expression) : p_expression list =
  let rec aux lst e =
    match e.exp_desc with
    | Couple (Comma b,e1,e2) ->
      if b then e::lst else aux (e2::lst) e1
    | _ -> e::lst
  in
  aux [] e

let rec expr_to_nonempty_list e = Nlist.from_list_exn (expr_to_list e)

let set_true (e:p_expression) : p_expression =
  match e.exp_desc with
  | Couple (Comma _,a,b) -> { e with exp_desc=(Couple(Comma true,a,b)) }
  | _ -> e

let mk_machine_exn id params clauses =
  match mk_machine id params clauses with
  | Ok c -> c
  | Error err -> raise (Error.Error err)

let mk_refinement_exn id params ref clauses =
  match mk_refinement id params ref clauses with
  | Ok c -> c
  | Error err -> raise (Error.Error err)

let mk_implementation_exn id params ref clauses =
  match mk_implementation id params ref clauses with
  | Ok c -> c
  | Error err -> raise (Error.Error err)
%}

%token EOF
%token DEFINITIONS
%token EQUALEQUAL
%token <string> DEF_FILE
%token WHERE
%token THEN
%token SELECT
%token <string> STRING
%token AFFECTATION
%token COMMA
%token <string> IDENT
%token DOLLAR_ZERO
%token RPAR
%token LPAR
%token TRUE
%token FALSE
%token CBOOL
%token <int> INTEGER
%token MAXINT
%token MININT
%token PLUS
%token MINUS
%token STAR
%token DIV
%token MOD
%token POWER
%token SUCC
%token PRED
%token MAX
%token MIN
%token CARD
%token SIGMA
%token DOT
%token BAR
%token PI
%token MAPLET
%token EMPTY_SET
%token Z_SET
%token N_SET
%token N1_SET
%token NAT_SET
%token NAT1_SET
%token INT_SET
%token BOOL_SET
%token STRING_SET
%token LBRA
%token LBRA_COMP
%token RBRA
%token POW
%token POW1
%token FPOW
%token FPOW1
%token DOTDOT
%token B_UNION
%token B_INTER
%token G_UNION
%token G_INTER
%token Q_UNION
%token Q_INTER
%token RELATION
%token ID
%token TILDE
%token PROJ1
%token PROJ2
%token SEMICOLON
%token DPRODUCT
%token PARALLEL
%token ITERATION
%token CLOSURE
%token CLOSURE1
%token DOM
%token RAN
%token LSQU
%token RSQU
%token RESTRICTION_D
%token SOUSTRACTION_D
%token RESTRICTION_CO
%token SOUSTRACTION_CO
%token SURCHARGE
%token PARTIELLE
%token TOTALE
%token P_INJECTION
%token T_INJECTION
%token S_PARTIELLE
%token S_TOTALE
%token B_TOTALE
%token LAMBDA
%token FNC
%token REL
%token AND
%token NOT
%token OR
%token IMPLY
%token EQUIV
%token FORALL
%token EXISTS
%token EQUAL
%token NOT_EQUAL
%token MEMBER_OF
%token NOT_MEMBER_OF
%token INCLUDED
%token S_INCLUDED
%token NOT_INCLUDED
%token NOT_S_INCLUDED
%token SMALLER_OR_EQUAL
%token S_SMALLER
%token GREATER_OR_EQUAL
%token S_GREATER
%token STRUCT
%token REC
%token SQUOTE
%token SEQ
%token SEQ1
%token ISEQ
%token ISEQ1
%token PERM
%token EMPTY_SEQ
%token SIZE
%token FIRST
%token LAST
%token FRONT
%token TAIL
%token REV
%token CIRC
%token INSERTION_T
%token INSERTION_Q
%token RESTRICTION_T
%token RESTRICTION_Q
%token CONC
%token TREE
%token BTREE
%token CONST
%token TOP
%token SONS
%token PREFIX
%token POSTFIX
%token SIZET
%token MIRROR
%token RANK
%token FATHER
%token SON
%token ARITY
%token BIN
%token LEFT
%token RIGHT
%token INFIX
%token SUBTREE
%token ELSIF
%token ELSE
%token WHEN
%token CASE_OR
%token BEGIN
%token END
%token SKIP
%token PRE
%token ASSERT
%token CHOICE
%token IF
%token CASE
%token OF
%token EITHER
%token ANY
%token LET
%token BE
%token BECOMES_ELT
%token VAR
%token WHILE
%token DO
%token INVARIANT
%token VARIANT
%token MACHINE
%token CONSTRAINTS
%token SEES
%token INCLUDES
%token PROMOTES
%token EXTENDS
%token USES
%token SETS
%token CONCRETE_CONSTANTS
%token CONSTANTS
%token ABSTRACT_CONSTANTS
%token PROPERTIES
%token CONCRETE_VARIABLES
%token ABSTRACT_VARIABLES
%token VARIABLES
%token ASSERTIONS
%token INITIALISATION
%token OPERATIONS
%token LOCAL_OPERATIONS
%token IN
%token LEFTARROW
%token REFINEMENT
%token IMPLEMENTATION
%token REFINES
%token IMPORTS
%token VALUES

%start component_eof
%start no_warning
%type <Syntax.p_component> component_eof
%type <Syntax.p_expression> expression
%type <Syntax.p_predicate> predicate
%type <Syntax.p_substitution> substitution
%type <Syntax.p_substitution> level1_substitution
%type <Syntax.p_clause> clause
%type <Syntax.p_operation> operation
%type <unit> no_warning

/* 10 */
/* 20 */
%left SEMICOLON PARALLEL
/* 30 */
%left IMPLY
/* 40 */
%left AND OR
/* 60 */
%left EQUIV
/* 110 */
/* 115 */
%left COMMA
/* 120 */
/* 125 */
%left PARTIELLE S_PARTIELLE TOTALE S_TOTALE RELATION P_INJECTION T_INJECTION B_TOTALE
/* 160 */
%left B_INTER SURCHARGE SOUSTRACTION_D RESTRICTION_D DPRODUCT B_UNION MAPLET RESTRICTION_CO SOUSTRACTION_CO INSERTION_T RESTRICTION_T INSERTION_Q RESTRICTION_Q CIRC EQUAL S_SMALLER S_GREATER SMALLER_OR_EQUAL GREATER_OR_EQUAL INCLUDED NOT_EQUAL MEMBER_OF NOT_INCLUDED NOT_MEMBER_OF NOT_S_INCLUDED S_INCLUDED BECOMES_ELT AFFECTATION
/* 170 */
%left DOTDOT
/* 180 */
%left PLUS MINUS
/* 190 */
%left STAR DIV MOD
/* 200 */
%right POWER
/* 210 */
%nonassoc unary_minus
/* 220 */
/* 230 */
%left TILDE
/* 250 */
%left LSQU LPAR SQUOTE

%%

(* *****************************************************************************
 * ***** EXPRESSIONS
 * ************************************************************************** *)

var_list_comma:
        | id=IDENT { [mk_var $startpos(id) id] }
        | id=IDENT COMMA lst=var_list_comma { (mk_var $startpos(id) id)::lst }

var_nelist_comma:
| id=IDENT { Nlist.make1 (mk_var $startpos(id) id) }
| id=IDENT COMMA lst=var_list_comma { Nlist.make (mk_var $startpos(id) id) lst }

fields:
| id=IDENT MEMBER_OF e=expression { Nlist.make1 (mk_lident $startpos(id) id,e) }
| id=IDENT MEMBER_OF e=expression COMMA lst=fields { Nlist.cons (mk_lident $startpos(id) id,e) lst }

constant:
| s=STRING { String s }
| TRUE { TRUE }
| FALSE { FALSE }
| i=INTEGER { Integer i }
| MAXINT { MaxInt }
| MININT { MinInt }
| EMPTY_SET { Empty_Set }
| Z_SET { INTEGER }
| N_SET { NATURAL }
| N1_SET { NATURAL1 }
| NAT_SET { NAT }
| NAT1_SET { NAT1 }
| INT_SET { INT }
| BOOL_SET { BOOLEANS }
| STRING_SET { STRINGS }
| EMPTY_SEQ { Empty_Seq }

%inline infix_op:
| PLUS { Addition }
| MINUS { Difference }
| STAR { Product }
| DIV { Division }
| MOD { Modulo }
| POWER { Power }
| SEMICOLON { Composition  }
| DPRODUCT { Direct_Product }
| PARALLEL { Parallel_Product }
| RESTRICTION_D { Domain_Restriction }
| SOUSTRACTION_D { Domain_Soustraction }
| RESTRICTION_CO { Codomain_Restriction }
| SOUSTRACTION_CO { Codomain_Soustraction }
| SURCHARGE { Surcharge }
| PARTIELLE   { Functions Partial_Functions }
| TOTALE      { Functions Total_Functions }
| P_INJECTION { Functions Partial_Injections }
| T_INJECTION { Functions Total_Injections }
| S_PARTIELLE { Functions Partial_Surjections }
| S_TOTALE    { Functions Total_Surjections }
| B_TOTALE    { Functions Bijections }
| DOTDOT { Interval }
| B_UNION { Union }
| B_INTER { Intersection }
| RELATION { Relations }
| CIRC { Concatenation }
| INSERTION_T { Head_Insertion }
| INSERTION_Q { Tail_Insertion }
| RESTRICTION_T { Head_Restriction }
| RESTRICTION_Q { Tail_Restriction }

prefix_op:
| SUCC { Successor }
| PRED { Predecessor }
| MAX { Max }
| MIN { Min }
| ITERATION { Iteration }
| CLOSURE { Closure }
| CLOSURE1 { Transitive_Closure }
| DOM { Domain }
| RAN { Range }
| POW { Power_Set Full }
| POW1 { Power_Set Non_Empty }
| FPOW { Power_Set Finite }
| FPOW1 { Power_Set Finite_Non_Empty }
| ID { Identity_Relation }
| PROJ1 { First_Projection }
| PROJ2 { Second_Projection }
| FNC { Fnc }
| REL { Rel }
| SEQ   { Sequence_Set All_Seq }
| SEQ1  { Sequence_Set Non_Empty_Seq }
| ISEQ  { Sequence_Set Injective_Seq }
| ISEQ1 { Sequence_Set Injective_Non_Empty_Seq }
| PERM  { Sequence_Set Permutations }
| TREE { Tree }
| BTREE { Btree }
| CONST { Const }
| TOP { Top }
| SONS { Sons }
| PREFIX { Prefix }
| POSTFIX { Postfix }
| SIZET { SizeT }
| MIRROR { Mirror }
| RANK { Rank }
| FATHER { Father }
| SON { Son }
| SUBTREE { Subtree }
| ARITY { Arity }
| BIN { Bin }
| LEFT { Left }
| RIGHT { Right }
| INFIX { Infix }
| CARD { Cardinal }
| SIZE { Size }
| FIRST { First }
| LAST { Last }
| FRONT { Front }
| TAIL { Tail }
| REV { Reverse }
| CONC { G_Concatenation }
| G_UNION { G_Union }
| G_INTER { G_Intersection }

%inline binder:
| SIGMA { Sum }
| PI { Prod }
| Q_UNION { Q_Union }
| Q_INTER { Q_Intersection }
| LAMBDA { Lambda }

expression:
| c=constant { mk_expr $startpos (Builtin c) }
| id=IDENT { mk_expr $startpos (Ident id) }
| id=IDENT DOLLAR_ZERO { mk_expr $startpos (Dollar id) }
| LPAR e=expression RPAR { set_true e }
| CBOOL LPAR p=predicate RPAR { mk_expr $startpos (Pbool p) }
| MINUS e=expression { mk_prefix $startpos Unary_Minus e } %prec unary_minus
| e=expression TILDE { mk_expr $startpos (Application (mk_expr $startpos($2) (Builtin Inverse_Relation),e)) }
| e1=expression op=infix_op e2=expression { mk_infix $startpos $startpos(op) op e1 e2 }
| e1=expression LSQU e2=expression RSQU { mk_infix $startpos $startpos Image e1 e2 }
| e1=expression MAPLET e2=expression { mk_expr $startpos (Couple (Maplet,e1,e2)) }
| e1=expression COMMA e2=expression { mk_expr $startpos (Couple (Comma false,e1,e2)) }
| LBRA e=expression RBRA { mk_expr $startpos (Extension (expr_to_nonempty_list e)) } 
| f=expression LPAR a=expression RPAR { mk_expr $startpos (Application (f,a)) }
| LSQU e=expression RSQU { mk_expr $startpos (Sequence(expr_to_nonempty_list e)) }
| op=prefix_op LPAR e=expression RPAR { mk_prefix $startpos op e }
| b=binder id=IDENT DOT LPAR p=predicate BAR e=expression RPAR { mk_binder $startpos b (Nlist.make1 (mk_var $startpos(id) id)) p e }
| b=binder LPAR ids=var_nelist_comma RPAR DOT LPAR p=predicate BAR e=expression RPAR { mk_binder $startpos b ids p e }
| LBRA_COMP ids=var_nelist_comma BAR p=predicate RBRA { mk_expr $startpos (Comprehension (ids,p)) }
| STRUCT LPAR lst=fields RPAR { mk_expr $startpos (Record_Type lst)  }
| REC LPAR lst=fields RPAR { mk_expr $startpos (Record lst) }
| e=expression SQUOTE id=IDENT { mk_expr $startpos (Record_Field_Access (e,mk_lident $startpos(id) id)) }

(* *****************************************************************************
 * ***** PREDICATES
 * ************************************************************************** *)
%inline b_prop:
| AND { Conjonction }
| OR { Disjonction }
| IMPLY { Implication }
| EQUIV { Equivalence }

%inline b_pred:
| EQUAL { Equality }
| NOT_EQUAL { Disequality }
| MEMBER_OF { Membership }
| NOT_MEMBER_OF { Non_Membership }
| INCLUDED { Inclusion Not_Strict }
| S_INCLUDED { Inclusion Strict }
| NOT_INCLUDED { Inclusion Non_Inclusion }
| NOT_S_INCLUDED { Inclusion Non_Strict_Inclusion }
| SMALLER_OR_EQUAL { Inequality Smaller_or_Equal }
| S_SMALLER { Inequality Strictly_Smaller }
| GREATER_OR_EQUAL { Inequality Greater_or_Equal }
| S_GREATER { Inequality Strictly_Greater }

predicate:
  LPAR p=predicate RPAR { p }
| NOT LPAR p=predicate RPAR { mk_pred $startpos (Negation p) }
| p=predicate op=b_prop q=predicate { mk_pred $startpos (Binary_Prop (op,p,q)) }
| e1=expression op=b_pred e2=expression { mk_pred $startpos (Binary_Pred (op,e1,e2)) }
| FORALL id=IDENT DOT LPAR p=predicate RPAR { mk_pred $startpos (Universal_Q (Nlist.make1 (mk_var $startpos(id) id),p)) }
| FORALL LPAR ids=var_nelist_comma RPAR DOT LPAR p=predicate RPAR { mk_pred $startpos (Universal_Q (ids,p)) }
| EXISTS id=IDENT DOT LPAR p=predicate RPAR { mk_pred $startpos (Existential_Q (Nlist.make1 (mk_var $startpos(id) id),p)) }
| EXISTS LPAR ids=var_nelist_comma RPAR DOT LPAR p=predicate RPAR { mk_pred $startpos (Existential_Q (ids,p)) }


(* *****************************************************************************
 * ***** SUBSTITUTIONS
 * ************************************************************************** *)

substitution:
  s=level1_substitution { s }
| s1=substitution SEMICOLON s2=substitution { mk_subst $startpos (Sequencement (s1,s2)) }
| s1=substitution PARALLEL s2=substitution { mk_subst $startpos (Parallel (s1,s2)) }

elsif: ELSIF p=predicate THEN s=substitution { (p,s) }

els: ELSE s=substitution { s }

whn: WHEN p=predicate THEN s=substitution { (p,s) }

case_or: CASE_OR e=expression THEN s=substitution { (e,s) }

id_eq_expr: id=IDENT EQUAL e=expression { (mk_var $startpos(id) id,e) }

callup_subst:
| id=IDENT
 { mk_subst $startpos (CallUp ([],mk_lident $startpos(id) id,[])) }
| id=IDENT LPAR e=expression RPAR
 { mk_subst $startpos (CallUp ([],mk_lident $startpos(id) id,expr_to_list e)) }
| ids=var_list_comma LEFTARROW id=IDENT
 { mk_subst $startpos (CallUp (ids,mk_lident $startpos(id) id,[])) }
| ids=var_list_comma LEFTARROW id=IDENT LPAR e=expression RPAR
 { mk_subst $startpos (CallUp (ids,mk_lident $startpos(id) id,expr_to_list e)) }

level1_substitution:
  BEGIN s=substitution END { s }

| SKIP { mk_subst $startpos Skip }

| ids=var_nelist_comma AFFECTATION e=expression { mk_subst $startpos (Affectation (ids,e)) }

| id=IDENT LPAR e1=expression RPAR lst=list(LPAR e=expression RPAR {e}) AFFECTATION e2=expression
     { mk_subst $startpos (Function_Affectation (mk_var $startpos(id) id,Nlist.make e1 lst,e2)) }

| id=IDENT SQUOTE fi=IDENT AFFECTATION e=expression
{ mk_subst $startpos (Record_Affectation (mk_var $startpos(id) id,mk_lident $startpos(fi) fi,e)) }

| PRE p=predicate THEN s=substitution END { mk_subst $startpos (Pre (p,s)) }

| ASSERT p=predicate THEN s=substitution END { mk_subst $startpos (Assert (p,s)) }

| CHOICE lst=separated_nonempty_list(CASE_OR,substitution) END { mk_subst $startpos (Choice (Nlist.from_list_exn lst)) }

| IF p=predicate THEN s=substitution ei=elsif* e=option(els) END { mk_subst $startpos (IfThenElse (Nlist.make (p,s) ei,e)) }

| SELECT p=predicate THEN s=substitution w=whn* e=option(els) END { mk_subst $startpos (Select (Nlist.make (p,s) w,e)) }

| CASE exp=expression OF
        EITHER e=expression THEN s=substitution
        ors=case_or*
        opt=option(els)
  END END { mk_subst $startpos (Case (exp,Nlist.make (e,s) ors,opt)) }

| ANY ids=var_nelist_comma WHERE p=predicate THEN s=substitution END { mk_subst $startpos (Any (ids,p,s)) }

| LET ids=var_nelist_comma BE eqs=separated_nonempty_list(AND,id_eq_expr) IN s=substitution END
 { mk_subst $startpos (Let (ids,Nlist.from_list_exn eqs,s)) }

| ids=var_nelist_comma BECOMES_ELT e=expression { mk_subst $startpos (BecomesElt (ids,e)) }

| ids=var_nelist_comma MEMBER_OF LPAR p=predicate RPAR { mk_subst $startpos (BecomesSuch (ids,p)) }

| VAR ids=var_nelist_comma IN s=substitution END { mk_subst $startpos (Var (ids,s)) }

| c=callup_subst { c }

| WHILE cond=predicate DO s=substitution INVARIANT inv=predicate VARIANT var=expression END
 { mk_subst $startpos (While (cond,s,inv,var)) }

(* *****************************************************************************
 * ***** COMPONENTS
 * ************************************************************************** *)

mch_name_nelist:
        | id=IDENT { Nlist.make1 (mk_lident $startpos(id) id) }
        | id=IDENT COMMA lst=mch_name_nelist { Nlist.cons (mk_lident $startpos(id) id) lst }

op_name_nelist:
        | id=IDENT { Nlist.make1 (mk_lident $startpos(id) id) }
        | id=IDENT COMMA lst=op_name_nelist { Nlist.cons (mk_lident $startpos(id) id) lst }

component_eof: a=component EOF { a }

component:
| MACHINE h=machine_header lst=clause* END { let (id,params) = h in mk_machine_exn id params lst }
| REFINEMENT h=machine_header REFINES abs=IDENT lst=clause* END
  { let (id,params) = h in mk_refinement_exn id params (mk_lident $startpos(abs) abs) lst }
| IMPLEMENTATION h=machine_header REFINES abs=IDENT lst=clause* END
  { let (id,params) = h in mk_implementation_exn id params (mk_lident $startpos(abs) abs) lst }

machine_header:
  id=IDENT { (mk_lident $startpos(id) id,[]) }
| id=IDENT LPAR lst=var_list_comma RPAR { (mk_lident $startpos(id) id,lst) }

machine_instanciation:
| id=IDENT { mk_minst (mk_lident $startpos(id) id) [] }
| id=IDENT LPAR e=expression RPAR { mk_minst (mk_lident $startpos(id) id) (expr_to_list e) }

set :
| id=IDENT { Abstract_Set (mk_var $startpos(id) id) }
| id=IDENT EQUAL LBRA elts=var_list_comma RBRA { Concrete_Set (mk_var $startpos(id) id,elts) }

operation :
| id=IDENT EQUAL s=level1_substitution
 { mk_operation [] (mk_lident $startpos(id) id) [] s }
| id=IDENT LPAR lst=var_nelist_comma RPAR EQUAL s=level1_substitution
 { mk_operation [] (mk_lident $startpos(id) id) (Nlist.to_list lst) s }
| ids=var_nelist_comma LEFTARROW id=IDENT LPAR lst=var_nelist_comma RPAR EQUAL s=level1_substitution
 { mk_operation (Nlist.to_list ids) (mk_lident $startpos(id) id) (Nlist.to_list lst) s }
| ids=var_nelist_comma LEFTARROW id=IDENT EQUAL s=level1_substitution
 { mk_operation (Nlist.to_list ids) (mk_lident $startpos(id) id) [] s }

semicolon_pred_lst:
| p=predicate { [p] }
| p=predicate SEMICOLON lst=semicolon_pred_lst { p::lst }

valuation:
| id=IDENT EQUAL e=expression { (mk_var $startpos(id) id,e) }

clause:
  CONSTRAINTS p=predicate { mk_clause $startpos (Constraints p) }
| SEES lst=mch_name_nelist { mk_clause $startpos (Sees lst) }
| INCLUDES lst=separated_nonempty_list ( COMMA, machine_instanciation ) { mk_clause $startpos (Includes (Nlist.from_list_exn lst)) }
| EXTENDS lst=separated_nonempty_list ( COMMA, machine_instanciation ) { mk_clause $startpos (Extends (Nlist.from_list_exn lst)) }
| PROMOTES lst=op_name_nelist { mk_clause $startpos (Promotes lst) }
| USES lst=mch_name_nelist { mk_clause $startpos (Uses lst) }
| SETS lst=separated_nonempty_list( SEMICOLON, set ) { mk_clause $startpos (Sets (Nlist.from_list_exn lst)) }
| CONCRETE_CONSTANTS lst=var_nelist_comma { mk_clause $startpos (Constants lst) }
| CONSTANTS lst=var_nelist_comma { mk_clause $startpos (Constants lst) }
| ABSTRACT_CONSTANTS lst=var_nelist_comma { mk_clause $startpos (Abstract_constants lst) }
| PROPERTIES p=predicate { mk_clause $startpos (Properties p) }
| CONCRETE_VARIABLES lst=var_nelist_comma { mk_clause $startpos (Concrete_variables lst) }
| ABSTRACT_VARIABLES lst=var_nelist_comma { mk_clause $startpos (Variables lst) }
| VARIABLES lst=var_nelist_comma { mk_clause $startpos (Variables lst) }
| INVARIANT p=predicate { mk_clause $startpos (Invariant p) }
| ASSERTIONS lst=semicolon_pred_lst { mk_clause $startpos (Assertions (Nlist.from_list_exn lst)) }
| INITIALISATION s=substitution { mk_clause $startpos (Initialization s) }
| OPERATIONS lst=separated_nonempty_list( SEMICOLON, operation ) { mk_clause $startpos (Operations (Nlist.from_list_exn lst)) }
| LOCAL_OPERATIONS lst=separated_nonempty_list( SEMICOLON, operation ) { mk_clause $startpos (Local_Operations (Nlist.from_list_exn lst)) }
| IMPORTS lst=separated_nonempty_list(COMMA,machine_instanciation) { mk_clause $startpos (Imports (Nlist.from_list_exn lst)) }
| VALUES lst=separated_nonempty_list(SEMICOLON,valuation) { mk_clause $startpos (Values (Nlist.from_list_exn lst)) }

no_warning: DEFINITIONS EQUALEQUAL DEF_FILE { () }
%%
