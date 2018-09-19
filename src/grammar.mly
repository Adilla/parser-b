%{
open Utils
open SyntaxCore
open PSyntax

let mk_builtin exp_loc bi : expression = { exp_loc; exp_desc=(Builtin bi); exp_par=false }
let mk_minst mi_mch mi_params : machine_instanciation = { mi_mch; mi_params }
let mk_expr exp_loc exp_desc : expression = { exp_loc; exp_desc; exp_par=false }
let mk_pred prd_loc prd_desc : predicate = { prd_loc; prd_desc; prd_par=false }
let mk_subst sub_loc sub_desc : substitution = { sub_loc; sub_desc; sub_be=false }
let mk_clause cl_loc cl_desc : Utils.loc*clause  = (cl_loc, cl_desc)
let mk_lident lid_loc lid_str : lident = { lid_loc; lid_str }
let mk_operation op_out op_name op_in op_body : operation = { op_out; op_name; op_in; op_body } 

let mk_infix lc lc_bi bi a1 a2 =
  let f = mk_builtin lc_bi bi in
  mk_expr lc (Application (f,mk_expr lc (Couple(Infix,a1,a2))))

let mk_prefix exp_loc bi e =
  let f = mk_builtin exp_loc bi in
  { exp_loc; exp_desc=Application(f,e); exp_par=false }

let mk_binder exp_loc bi xlst p e =
  { exp_loc; exp_desc=Binder(bi,xlst,p,e); exp_par=false }

let expr_to_list (e:expression) : expression list =
  let rec aux lst e =
    match e.exp_desc with
    | Couple (Comma,e1,e2) ->
      if e.exp_par then e::lst else aux (e2::lst) e1
    | _ -> e::lst
  in
  aux [] e

let rec expr_to_nonempty_list e = Nlist.from_list_exn (expr_to_list e)

let mk_par (e:expression) : expression = { e with exp_par=true }
let mk_par_p (p:predicate) : predicate = { p with prd_par=true }
let mk_be (s:substitution) : substitution = { s with sub_be=true }
%}

%token EOF
%token DEFINITIONS
%token EQUALEQUAL
%token <string> DEF_FILE
%token WHERE
%token THEN
%token SELECT
%token <SyntaxCore.e_builtin> CONSTANT 
%token <SyntaxCore.e_builtin> E_PREFIX
%token <SyntaxCore.pred_bop> PREDICATE
%token <SyntaxCore.expr_binder> E_BINDER
%token <SyntaxCore.e_builtin> E_INFIX_125
%token <SyntaxCore.e_builtin> E_INFIX_160
%token <SyntaxCore.e_builtin> E_INFIX_170
%token <SyntaxCore.e_builtin> E_INFIX_180
%token <SyntaxCore.e_builtin> E_INFIX_190
%token <SyntaxCore.e_builtin> E_INFIX_200
%token <string> STRING
%token AFFECTATION
%token COMMA
%token <string> IDENT
%token DOLLAR_ZERO
%token RPAR
%token LPAR
%token CBOOL
%token MINUS
%token DOT
%token BAR
%token MAPLET
%token LBRA
%token LBRA_COMP
%token RBRA
%token TILDE
%token SEMICOLON
%token PARALLEL
%token LSQU
%token RSQU
%token AND
%token NOT
%token OR
%token IMPLY
%token EQUIV
%token FORALL
%token EXISTS
%token EQUAL
%token MEMBER_OF
%token STRUCT
%token REC
%token SQUOTE
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
%type <PSyntax.component> component_eof
%type <PSyntax.expression> expression
%type <PSyntax.predicate> predicate
%type <PSyntax.substitution> substitution
%type <PSyntax.substitution> level1_substitution
%type <PSyntax.operation> operation
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
%left E_INFIX_125
/* 160 */
%left E_INFIX_160 MAPLET PREDICATE EQUAL MEMBER_OF BECOMES_ELT AFFECTATION
/* 170 */
%left E_INFIX_170
/* 180 */
%left E_INFIX_180 MINUS
/* 190 */
%left E_INFIX_190
/* 200 */
%right E_INFIX_200 
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

ident_list_comma:
        | id=IDENT { [mk_lident $startpos(id) id] }
        | id=IDENT COMMA lst=ident_list_comma { (mk_lident $startpos(id) id)::lst }

ident_nelist_comma:
| id=IDENT { Nlist.make1 (mk_lident $startpos(id) id) }
| id=IDENT COMMA lst=ident_list_comma { Nlist.make (mk_lident $startpos(id) id) lst }

fields:
| id=IDENT MEMBER_OF e=expression { Nlist.make1 (mk_lident $startpos(id) id,e) }
| id=IDENT MEMBER_OF e=expression COMMA lst=fields { Nlist.cons (mk_lident $startpos(id) id,e) lst }

%inline infix_op:
  | i=E_INFIX_125 { i }
  | i=E_INFIX_160 { i }
  | i=E_INFIX_170 { i }
  | i=E_INFIX_180 { i }
  | i=E_INFIX_190 { i }
  | i=E_INFIX_200 { i }
  | MINUS { Difference }
  | SEMICOLON { Composition  }
  | PARALLEL { Parallel_Product }

expression:
  c=CONSTANT { mk_expr $startpos (Builtin c) }
| s=STRING { mk_expr $startpos (Builtin (String s)) }
| id=IDENT { mk_expr $startpos (Ident id) }
| id=IDENT DOLLAR_ZERO { mk_expr $startpos (Dollar id) }
| LPAR e=expression RPAR { mk_par e }
| CBOOL LPAR p=predicate RPAR { mk_expr $startpos (Pbool p) }
| MINUS e=expression { mk_prefix $startpos Unary_Minus e } %prec unary_minus
| e=expression TILDE { mk_expr $startpos (Application (mk_expr $startpos($2) (Builtin Inverse_Relation),e)) }
| e1=expression op=infix_op e2=expression { mk_infix $startpos $startpos(op) op e1 e2 }
| e1=expression LSQU e2=expression RSQU { mk_infix $startpos $startpos Image e1 e2 }
| e1=expression MAPLET e2=expression { mk_expr $startpos (Couple (Maplet,e1,e2)) }
| e1=expression COMMA e2=expression { mk_expr $startpos (Couple (Comma,e1,e2)) }
| LBRA e=expression RBRA { mk_expr $startpos (Extension (expr_to_nonempty_list e)) } 
| f=expression LPAR a=expression RPAR { mk_expr $startpos (Application (f,a)) }
| LSQU e=expression RSQU { mk_expr $startpos (Sequence(expr_to_nonempty_list e)) }
| op=E_PREFIX LPAR e=expression RPAR { mk_prefix $startpos op e }
| b=E_BINDER id=IDENT DOT LPAR p=predicate BAR e=expression RPAR { mk_binder $startpos b (Nlist.make1 (mk_lident $startpos(id) id)) p e }
| b=E_BINDER LPAR ids=ident_nelist_comma RPAR DOT LPAR p=predicate BAR e=expression RPAR { mk_binder $startpos b ids p e }
| LBRA_COMP ids=ident_nelist_comma BAR p=predicate RBRA { mk_expr $startpos (Comprehension (ids,p)) }
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
| MEMBER_OF { Membership }
| EQUAL { Equality }
| p=PREDICATE { p }

predicate:
  LPAR p=predicate RPAR { mk_par_p p }
| NOT LPAR p=predicate RPAR { mk_pred $startpos (Negation p) }
| p=predicate op=b_prop q=predicate { mk_pred $startpos (Binary_Prop (op,p,q)) }
| e1=expression op=b_pred e2=expression { mk_pred $startpos (Binary_Pred (op,e1,e2)) }
| FORALL id=IDENT DOT LPAR p=predicate RPAR { mk_pred $startpos (Universal_Q (Nlist.make1 (mk_lident $startpos(id) id),p)) }
| FORALL LPAR ids=ident_nelist_comma RPAR DOT LPAR p=predicate RPAR { mk_pred $startpos (Universal_Q (ids,p)) }
| EXISTS id=IDENT DOT LPAR p=predicate RPAR { mk_pred $startpos (Existential_Q (Nlist.make1 (mk_lident $startpos(id) id),p)) }
| EXISTS LPAR ids=ident_nelist_comma RPAR DOT LPAR p=predicate RPAR { mk_pred $startpos (Existential_Q (ids,p)) }

(* *****************************************************************************
 * ***** SUBSTITUTIONS
 * ************************************************************************** *)

substitution:
  s=level1_substitution { s }
| s1=substitution SEMICOLON s2=substitution { mk_subst $startpos($2) (Sequencement (s1,s2)) }
| s1=substitution PARALLEL s2=substitution { mk_subst $startpos($2) (Parallel (s1,s2)) }

elsif: ELSIF p=predicate THEN s=substitution { (p,s) }

els: ELSE s=substitution { s }

whn: WHEN p=predicate THEN s=substitution { (p,s) }

case_or: CASE_OR e=expression THEN s=substitution { (expr_to_nonempty_list e,s) }

id_eq_expr: id=IDENT EQUAL e=expression { (mk_lident $startpos(id) id,e) }

callup_subst:
| id=IDENT
 { mk_subst $startpos (CallUp ([],mk_lident $startpos(id) id,[])) }
| id=IDENT LPAR e=expression RPAR
 { mk_subst $startpos (CallUp ([],mk_lident $startpos(id) id,expr_to_list e)) }
| ids=ident_list_comma LEFTARROW id=IDENT
 { mk_subst $startpos (CallUp (ids,mk_lident $startpos(id) id,[])) }
| ids=ident_list_comma LEFTARROW id=IDENT LPAR e=expression RPAR
 { mk_subst $startpos (CallUp (ids,mk_lident $startpos(id) id,expr_to_list e)) }

level1_substitution:
  BEGIN s=substitution END { mk_be s }

| SKIP { mk_subst $startpos Skip }

| ids=ident_nelist_comma AFFECTATION e=expression { mk_subst $startpos (Affectation (Tuple ids,e)) }

| id=IDENT LPAR e1=expression RPAR lst=list(LPAR e=expression RPAR {e}) AFFECTATION e2=expression
     { mk_subst $startpos (Affectation (Function(mk_lident $startpos(id) id,Nlist.make e1 lst),e2)) }

| id=IDENT SQUOTE fi=IDENT AFFECTATION e=expression
{ mk_subst $startpos (Affectation (Record (mk_lident $startpos(id) id,mk_lident $startpos(fi) fi),e)) }

| PRE p=predicate THEN s=substitution END { mk_subst $startpos (Pre (p,s)) }

| ASSERT p=predicate THEN s=substitution END { mk_subst $startpos (Assert (p,s)) }

| CHOICE lst=separated_nonempty_list(CASE_OR,substitution) END { mk_subst $startpos (Choice (Nlist.from_list_exn lst)) }

| IF p=predicate THEN s=substitution ei=elsif* e=option(els) END { mk_subst $startpos (IfThenElse (Nlist.make (p,s) ei,e)) }

| SELECT p=predicate THEN s=substitution w=whn* e=option(els) END { mk_subst $startpos (Select (Nlist.make (p,s) w,e)) }

| CASE exp=expression OF
        EITHER e=expression THEN s=substitution
        ors=case_or*
        opt=option(els)
  END END { mk_subst $startpos (Case (exp,Nlist.make (expr_to_nonempty_list e,s) ors,opt)) }

| ANY ids=ident_nelist_comma WHERE p=predicate THEN s=substitution END { mk_subst $startpos (Any (ids,p,s)) }

| LET ids=ident_nelist_comma BE eqs=separated_nonempty_list(AND,id_eq_expr) IN s=substitution END
 { mk_subst $startpos (Let (ids,Nlist.from_list_exn eqs,s)) }

| ids=ident_nelist_comma BECOMES_ELT e=expression { mk_subst $startpos (BecomesElt (ids,e)) }

| ids=ident_nelist_comma MEMBER_OF LPAR p=predicate RPAR { mk_subst $startpos (BecomesSuch (ids,p)) }

| VAR ids=ident_nelist_comma IN s=substitution END { mk_subst $startpos (Var (ids,s)) }

| c=callup_subst { c }

| WHILE cond=predicate DO s=substitution INVARIANT inv=predicate VARIANT var=expression END
 { mk_subst $startpos (While (cond,s,inv,var)) }

(* *****************************************************************************
 * ***** COMPONENTS
 * ************************************************************************** *)

component_eof: a=component EOF { a }

component:
| MACHINE h=machine_header lst=clause* END { let (id,params) = h in mk_machine_exn id params lst }
| REFINEMENT h=machine_header REFINES abs=IDENT lst=clause* END
  { let (id,params) = h in mk_refinement_exn id params (mk_lident $startpos(abs) abs) lst }
| IMPLEMENTATION h=machine_header REFINES abs=IDENT lst=clause* END
  { let (id,params) = h in mk_implementation_exn id params (mk_lident $startpos(abs) abs) lst }

machine_header:
  id=IDENT { (mk_lident $startpos(id) id,[]) }
| id=IDENT LPAR lst=ident_list_comma RPAR { (mk_lident $startpos(id) id,lst) }

machine_instanciation:
| id=IDENT { mk_minst (mk_lident $startpos(id) id) [] }
| id=IDENT LPAR e=expression RPAR { mk_minst (mk_lident $startpos(id) id) (expr_to_list e) }

set :
| id=IDENT { Abstract_Set (mk_lident $startpos(id) id) }
| id=IDENT EQUAL LBRA elts=ident_list_comma RBRA { Concrete_Set (mk_lident $startpos(id) id,elts) }

operation :
| id=IDENT EQUAL s=level1_substitution
 { mk_operation [] (mk_lident $startpos(id) id) [] s }
| id=IDENT LPAR lst=ident_nelist_comma RPAR EQUAL s=level1_substitution
 { mk_operation [] (mk_lident $startpos(id) id) (Nlist.to_list lst) s }
| ids=ident_nelist_comma LEFTARROW id=IDENT LPAR lst=ident_nelist_comma RPAR EQUAL s=level1_substitution
 { mk_operation (Nlist.to_list ids) (mk_lident $startpos(id) id) (Nlist.to_list lst) s }
| ids=ident_nelist_comma LEFTARROW id=IDENT EQUAL s=level1_substitution
 { mk_operation (Nlist.to_list ids) (mk_lident $startpos(id) id) [] s }

semicolon_pred_lst:
| p=predicate { [p] }
| p=predicate SEMICOLON lst=semicolon_pred_lst { p::lst }

valuation:
| id=IDENT EQUAL e=expression { (mk_lident $startpos(id) id,e) }

clause:
  CONSTRAINTS p=predicate { mk_clause $startpos (Constraints p) }
| SEES lst=ident_nelist_comma { mk_clause $startpos (Sees lst) }
| INCLUDES lst=separated_nonempty_list ( COMMA, machine_instanciation ) { mk_clause $startpos (Includes (Nlist.from_list_exn lst)) }
| EXTENDS lst=separated_nonempty_list ( COMMA, machine_instanciation ) { mk_clause $startpos (Extends (Nlist.from_list_exn lst)) }
| PROMOTES lst=ident_nelist_comma { mk_clause $startpos (Promotes lst) }
| USES lst=ident_nelist_comma { mk_clause $startpos (Uses lst) }
| SETS lst=separated_nonempty_list( SEMICOLON, set ) { mk_clause $startpos (Sets (Nlist.from_list_exn lst)) }
| CONCRETE_CONSTANTS lst=ident_nelist_comma { mk_clause $startpos (Constants lst) }
| CONSTANTS lst=ident_nelist_comma { mk_clause $startpos (Constants lst) }
| ABSTRACT_CONSTANTS lst=ident_nelist_comma { mk_clause $startpos (Abstract_constants lst) }
| PROPERTIES p=predicate { mk_clause $startpos (Properties p) }
| CONCRETE_VARIABLES lst=ident_nelist_comma { mk_clause $startpos (Concrete_variables lst) }
| ABSTRACT_VARIABLES lst=ident_nelist_comma { mk_clause $startpos (Variables lst) }
| VARIABLES lst=ident_nelist_comma { mk_clause $startpos (Variables lst) }
| INVARIANT p=predicate { mk_clause $startpos (Invariant p) }
| ASSERTIONS lst=semicolon_pred_lst { mk_clause $startpos (Assertions (Nlist.from_list_exn lst)) }
| INITIALISATION s=substitution { mk_clause $startpos (Initialization s) }
| OPERATIONS lst=separated_nonempty_list( SEMICOLON, operation ) { mk_clause $startpos (Operations (Nlist.from_list_exn lst)) }
| LOCAL_OPERATIONS lst=separated_nonempty_list( SEMICOLON, operation ) { mk_clause $startpos (Local_Operations (Nlist.from_list_exn lst)) }
| IMPORTS lst=separated_nonempty_list(COMMA,machine_instanciation) { mk_clause $startpos (Imports (Nlist.from_list_exn lst)) }
| VALUES lst=separated_nonempty_list(SEMICOLON,valuation) { mk_clause $startpos (Values (Nlist.from_list_exn lst)) }

no_warning: DEFINITIONS EQUALEQUAL DEF_FILE { () }
%%
