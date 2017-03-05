%{
open Utils
open Expression
open Substitution
open Component

let mk_infix_app lc (f:expression) (a1:expression) (a2:expression) : expression =
  Application (lc,f,Couple(lc,Infix,a1,a2))

let expr_to_list (e:expression) : expression list =
  let rec aux lst = function
    | Couple (_,Comma,e1,e2) -> aux (e2::lst) e1
    | e -> e::lst
  in
  aux [] e

let rec expr_to_nonempty_list (e:expression): expression non_empty_list =
  let lst = expr_to_list e in
  (List.hd lst,List.tl lst)

let expr_to_rfields (e:expression): (ident option * expression) non_empty_list =
  let rec aux lst = function
  | Couple (_,Comma,e1,e2) -> aux e1 ((None,e2)::lst)
  | e -> [(None,e)]
  in
  let lst = aux e in
  (List.hd lst,List.tl lst)

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
(* %token BECOMES_SUCH *)
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
%type <Component.component> component_eof
%type <Expression.expression> expression
%type <Expression.predicate> predicate
%type <Substitution.substitution> substitution
%type <Component.clause> clause

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

(* MISC *)

ident_lst_comma:
        | id=IDENT { [($startpos(id),id)] }
        | id=IDENT COMMA lst=ident_lst_comma { ($startpos(id),id)::lst }

liste_ident:
  id=IDENT { [($startpos(id),id)] }
| LPAR lst=ident_lst_comma RPAR { lst }

fields:
| id=IDENT MEMBER_OF e=expression { [(($startpos(id),id),e)] }
| id=IDENT MEMBER_OF e=expression COMMA lst=fields { (($startpos(id),id),e)::lst }

rec_fields:
(* | e=expression { [None,e] } *)
(* | e=expression COMMA lst=rec_fields { (None,e)::lst } *)
| id=IDENT MEMBER_OF e=expression { [(Some ($startpos(id),id),e)] }
| id=IDENT MEMBER_OF e=expression COMMA lst=rec_fields { (Some ($startpos(id),id),e)::lst }

expression:
(* expression_primaire: *)
| id=IDENT { Ident ($startpos(id),id) }
| id=IDENT DOLLAR_ZERO { Dollar ($startpos(id),id) }
| LPAR e=expression RPAR { Parentheses ($startpos,e) }
| s=STRING { Builtin ($startpos(s),String s) }
(* expression_booleenne: *)
| TRUE { Builtin ($startpos,TRUE) }
| FALSE { Builtin ($startpos,FALSE) }
| CBOOL LPAR p=predicate RPAR { Pbool ($startpos,p) }
(* entier_lit: *)
| i=INTEGER { Builtin ($startpos(i),Integer i) }
| MAXINT { Builtin ($startpos,MaxInt) }
| MININT { Builtin ($startpos,MinInt) }
(* expression_arithmetique: *)
| e1=expression PLUS e2=expression { mk_infix_app $startpos (Builtin ($startpos($2),Addition)) e1 e2 }
| e1=expression MINUS e2=expression { mk_infix_app $startpos (Builtin ($startpos($2),Difference)) e1 e2 }
| MINUS e=expression { Application ($startpos,Builtin($startpos,Unary_Minus),e) } %prec unary_minus
| e1=expression STAR e2=expression { mk_infix_app $startpos (Builtin ($startpos($2),Product)) e1 e2 }
| e1=expression DIV e2=expression { mk_infix_app $startpos (Builtin ($startpos($2),Division)) e1 e2 }
| e1=expression MOD e2=expression { mk_infix_app $startpos (Builtin ($startpos($2),Modulo)) e1 e2 }
| e1=expression POWER e2=expression { mk_infix_app $startpos (Builtin ($startpos($2),Power)) e1 e2 }
| SUCC LPAR e=expression RPAR { Application ($startpos,Builtin($startpos,Successor),e) }
| PRED LPAR e=expression RPAR { Application ($startpos,Builtin($startpos,Predecessor),e) }
| MAX LPAR e=expression RPAR { Application ($startpos,Builtin($startpos,Max),e) }
| MIN LPAR e=expression RPAR { Application ($startpos,Builtin($startpos,Min),e) }
| CARD LPAR e=expression RPAR { Application ($startpos,Builtin($startpos,Cardinal),e) }
| SIGMA ids=liste_ident DOT LPAR p=predicate BAR e=expression RPAR { Binder ($startpos,Sum,(List.hd ids,List.tl ids),p,e) }
| PI ids=liste_ident DOT LPAR p=predicate BAR e=expression RPAR { Binder ($startpos,Prod,(List.hd ids,List.tl ids),p,e) }
(* expression_de_couples: *)
| e1=expression MAPLET e2=expression { Couple ($startpos,Maplet,e1,e2) }
| e1=expression COMMA e2=expression { Couple ($startpos,Comma,e1,e2) }
(* expression_d_ensembles: *)
| EMPTY_SET { Builtin ($startpos,Empty_Set) }
| Z_SET { Builtin ($startpos,INTEGER) }
| N_SET { Builtin ($startpos,NATURAL) }
| N1_SET { Builtin ($startpos,NATURAL1) }
| NAT_SET { Builtin ($startpos,NAT) }
| NAT1_SET { Builtin ($startpos,NAT1) }
| INT_SET { Builtin ($startpos,INT) }
| BOOL_SET { Builtin ($startpos,BOOLEANS) }
| STRING_SET { Builtin($startpos,STRINGS) }
(* construction_d_ensembles: *)
| LBRA_COMP ids=ident_lst_comma BAR p=predicate RBRA  { Comprehension ($startpos,(List.hd ids,List.tl ids),p) }
| POW LPAR e=expression RPAR { Application($startpos,Builtin ($startpos,Power_Set Full),e) }
| POW1 LPAR e=expression RPAR { Application($startpos,Builtin($startpos,Power_Set Non_Empty),e) }
| FPOW LPAR e=expression RPAR { Application($startpos,Builtin($startpos,Power_Set Finite),e) }
| FPOW1 LPAR e=expression RPAR { Application($startpos,Builtin($startpos,Power_Set Finite_Non_Empty),e) }
| LBRA e=expression RBRA { Extension ($startpos,expr_to_nonempty_list e) } 
| e1=expression DOTDOT e2=expression { mk_infix_app $startpos (Builtin ($startpos($2),Interval)) e1 e2 }
| e1=expression B_UNION e2=expression { mk_infix_app $startpos (Builtin ($startpos($2),Union)) e1 e2 }
| e1=expression B_INTER e2=expression { mk_infix_app $startpos (Builtin ($startpos($2),Intersection)) e1 e2 }
| G_UNION LPAR e=expression RPAR { Application ($startpos,Builtin($startpos,G_Union),e) }
| G_INTER LPAR e=expression RPAR { Application ($startpos,Builtin($startpos,G_Intersection),e) }
| Q_UNION ids=liste_ident DOT LPAR p=predicate BAR e=expression RPAR { Binder ($startpos,Q_Union,(List.hd ids,List.tl ids),p,e) }
| Q_INTER ids=liste_ident DOT LPAR p=predicate BAR e=expression RPAR { Binder ($startpos,Q_Intersection,(List.hd ids,List.tl ids),p,e) }
(* expression_de_records: *)
| STRUCT LPAR lst=fields RPAR   { Record_Type ($startpos,(List.hd lst,List.tl lst))  }
| REC LPAR lst=rec_fields RPAR  { Record ($startpos,(List.hd lst,List.tl lst)) }
| REC LPAR e=expression RPAR  { Record ($startpos,expr_to_rfields e) }
| e=expression SQUOTE id=IDENT   { Record_Field_Access ($startpos,e,($startpos(id),id)) }
(* expression_de_relations: *)
| e1=expression RELATION e2=expression { mk_infix_app $startpos (Builtin ($startpos($2),Relations)) e1 e2 }
| ID LPAR e=expression RPAR { Application ($startpos,Builtin ($startpos,Identity_Relation),e) }
| e=expression TILDE { Application ($startpos,Builtin ($startpos($2),Inverse_Relation),e) }
| PROJ1 LPAR e=expression RPAR { Application ($startpos,Builtin($startpos,First_Projection),e) }
| PROJ2 LPAR e=expression RPAR { Application ($startpos,Builtin($startpos,Second_Projection),e) }
| e1=expression SEMICOLON e2=expression { mk_infix_app $startpos (Builtin ($startpos($2),Composition))  e1 e2  }
| e1=expression DPRODUCT e2=expression { mk_infix_app $startpos  (Builtin ($startpos($2),Direct_Product))  e1 e2 }
| e1=expression PARALLEL e2=expression { mk_infix_app $startpos (Builtin ($startpos($2),Parallel_Product)) e1 e2 }
| ITERATION LPAR e=expression RPAR { Application ($startpos,Builtin($startpos,Iteration),e) }
| CLOSURE LPAR e=expression RPAR { Application ($startpos,Builtin($startpos,Closure),e) }
| CLOSURE1 LPAR e=expression RPAR { Application ($startpos,Builtin($startpos,Transitive_Closure),e) }
| DOM LPAR e=expression RPAR { Application ($startpos,Builtin($startpos,Domain),e) }
| RAN LPAR e=expression RPAR { Application ($startpos,Builtin($startpos,Range),e) }
| e1=expression LSQU e2=expression RSQU { mk_infix_app $startpos (Builtin ($startpos($2),Image)) e1 e2 }
| e1=expression RESTRICTION_D e2=expression  { mk_infix_app $startpos (Builtin ($startpos($2),Domain_Restriction)) e1 e2 }
| e1=expression SOUSTRACTION_D e2=expression { mk_infix_app $startpos (Builtin ($startpos($2),Domain_Soustraction)) e1 e2 }
| e1=expression RESTRICTION_CO e2=expression { mk_infix_app $startpos (Builtin ($startpos($2),Codomain_Restriction)) e1 e2 }
| e1=expression SOUSTRACTION_CO e2=expression { mk_infix_app $startpos (Builtin ($startpos($2),Codomain_Soustraction)) e1 e2 }
| e1=expression SURCHARGE e2=expression { mk_infix_app $startpos (Builtin ($startpos($2),Surcharge)) e1 e2 }
(* expression_de_fonctions: *)
| e1=expression PARTIELLE e2=expression   { mk_infix_app $startpos (Builtin ($startpos($2),Functions Partial_Functions)) e1 e2 }
| e1=expression TOTALE e2=expression      { mk_infix_app $startpos (Builtin ($startpos($2),Functions Total_Functions)) e1 e2 }
| e1=expression P_INJECTION e2=expression { mk_infix_app $startpos (Builtin ($startpos($2),Functions Partial_Injections)) e1 e2 }
| e1=expression T_INJECTION e2=expression { mk_infix_app $startpos (Builtin ($startpos($2),Functions Total_Injections)) e1 e2 }
| e1=expression S_PARTIELLE e2=expression { mk_infix_app $startpos (Builtin ($startpos($2),Functions Partial_Surjections)) e1 e2 }
| e1=expression S_TOTALE e2=expression    { mk_infix_app $startpos (Builtin ($startpos($2),Functions Total_Surjections)) e1 e2 }
| e1=expression B_TOTALE e2=expression    { mk_infix_app $startpos (Builtin ($startpos($2),Functions Bijections)) e1 e2 }
(* construction_de_fonctions: *)
| LAMBDA ids=liste_ident DOT LPAR p=predicate BAR e=expression RPAR { Binder ($startpos,Lambda,(List.hd ids,List.tl ids),p,e) }
| f=expression LPAR a=expression RPAR { Application ($startpos,f,a) }
| FNC LPAR e=expression RPAR { Application ($startpos,Builtin($startpos,Fnc),e) }
| REL LPAR e=expression RPAR { Application ($startpos,Builtin($startpos,Rel),e) }
(* expression_de_suites: *)
| SEQ LPAR e=expression RPAR   { Application ($startpos,Builtin($startpos,Sequence_Set All_Seq),e) }
| SEQ1 LPAR e=expression RPAR  { Application ($startpos,Builtin($startpos,Sequence_Set Non_Empty_Seq),e) }
| ISEQ LPAR e=expression RPAR  { Application ($startpos,Builtin($startpos,Sequence_Set Injective_Seq),e) }
| ISEQ1 LPAR e=expression RPAR { Application ($startpos,Builtin($startpos,Sequence_Set Injective_Non_Empty_Seq),e) }
| PERM LPAR e=expression RPAR  { Application ($startpos,Builtin($startpos,Sequence_Set Permutations),e) }
| EMPTY_SEQ { Builtin($startpos,Empty_Seq) }
| LSQU e=expression RSQU { Sequence($startpos,expr_to_nonempty_list e)  }
(* construction_de_suites: *)
| SIZE LPAR e=expression RPAR { Application($startpos,Builtin($startpos,Size),e) }
| FIRST LPAR e=expression RPAR { Application($startpos,Builtin($startpos,First),e) }
| LAST LPAR e=expression RPAR { Application($startpos,Builtin($startpos,Last),e) }
| FRONT LPAR e=expression RPAR { Application($startpos,Builtin($startpos,Front),e) }
| TAIL LPAR e=expression RPAR { Application($startpos,Builtin($startpos,Tail),e) }
| REV LPAR e=expression RPAR { Application($startpos,Builtin($startpos,Reverse),e) }
| e1=expression CIRC e2=expression { mk_infix_app $startpos (Builtin($startpos($2),Concatenation)) e1 e2 }
| e1=expression INSERTION_T e2=expression { mk_infix_app $startpos (Builtin($startpos($2),Head_Insertion)) e1 e2 }
| e1=expression INSERTION_Q e2=expression { mk_infix_app $startpos (Builtin($startpos($2),Tail_Insertion)) e1 e2 }
| e1=expression RESTRICTION_T e2=expression { mk_infix_app $startpos (Builtin($startpos($2),Head_Restriction)) e1 e2 }
| e1=expression RESTRICTION_Q e2=expression { mk_infix_app $startpos (Builtin($startpos($2),Tail_Restriction)) e1 e2 }
| CONC LPAR e=expression RPAR { Application($startpos,Builtin($startpos,G_Concatenation),e) }
(* expression_d_arbres: *)
| TREE LPAR e=expression RPAR { Application($startpos,Builtin($startpos,Tree),e) }
| BTREE LPAR e=expression RPAR { Application($startpos,Builtin($startpos,Btree),e) }
| CONST LPAR e=expression RPAR { Application ($startpos,Builtin($startpos,Const),e) }
| TOP LPAR e=expression RPAR { Application($startpos,Builtin($startpos,Top),e) }
| SONS LPAR e=expression RPAR { Application($startpos,Builtin($startpos,Sons),e) }
| PREFIX LPAR e=expression RPAR { Application($startpos,Builtin($startpos,Prefix),e) }
| POSTFIX LPAR e=expression RPAR { Application($startpos,Builtin($startpos,Postfix),e) }
| SIZET LPAR e=expression RPAR { Application($startpos,Builtin($startpos,SizeT),e) }
| MIRROR LPAR e=expression RPAR { Application($startpos,Builtin($startpos,Mirror),e) }
| RANK LPAR e=expression RPAR { Application ($startpos,Builtin($startpos,Rank),e) }
| FATHER LPAR e=expression RPAR { Application ($startpos,Builtin($startpos,Father),e) }
| SON LPAR e=expression RPAR { Application ($startpos,Builtin($startpos,Son),e) }
| SUBTREE LPAR e=expression RPAR { Application ($startpos,Builtin($startpos,Subtree),e) }
| ARITY LPAR e=expression RPAR { Application ($startpos,Builtin($startpos,Arity),e) }
| BIN LPAR e=expression RPAR { Application($startpos,Builtin($startpos,Bin),e) }
| LEFT LPAR e=expression RPAR { Application($startpos,Builtin($startpos,Left),e) }
| RIGHT LPAR e=expression RPAR { Application($startpos,Builtin($startpos,Right),e) }
| INFIX LPAR e=expression RPAR { Application($startpos,Builtin($startpos,Infix),e) }

(* PREDICATES *)

predicate:
  LPAR p=predicate RPAR { Pparentheses ($startpos,p) }
| p=predicate AND q=predicate { Binary_Prop ($startpos,Conjonction,p,q) }
| NOT LPAR p=predicate RPAR { Negation ($startpos,p) }
| p=predicate OR q=predicate { Binary_Prop ($startpos,Disjonction,p,q) }
| p=predicate IMPLY q=predicate { Binary_Prop ($startpos,Implication,p,q) }
| p=predicate EQUIV q=predicate { Binary_Prop ($startpos,Equivalence,p,q) }
| FORALL ids=liste_ident DOT LPAR p=predicate RPAR { Universal_Q ($startpos,(List.hd ids,List.tl ids),p) }
| EXISTS ids=liste_ident DOT LPAR p=predicate RPAR { Existential_Q ($startpos,(List.hd ids,List.tl ids),p) }
| e1=expression EQUAL e2=expression { Binary_Pred ($startpos,Equality,e1,e2) }
| e1=expression NOT_EQUAL e2=expression { Binary_Pred ($startpos,Disequality,e1,e2) }
| e1=expression MEMBER_OF e2=expression { Binary_Pred ($startpos,Membership,e1,e2) }
| e1=expression NOT_MEMBER_OF e2=expression { Binary_Pred ($startpos,Non_Membership,e1,e2) }
| e1=expression INCLUDED e2=expression { Binary_Pred ($startpos,Inclusion Not_Strict,e1,e2) }
| e1=expression S_INCLUDED e2=expression { Binary_Pred ($startpos,Inclusion Strict,e1,e2) }
| e1=expression NOT_INCLUDED e2=expression { Binary_Pred ($startpos,Inclusion Non_Inclusion,e1,e2) }
| e1=expression NOT_S_INCLUDED e2=expression { Binary_Pred ($startpos,Inclusion Non_Strict_Inclusion,e1,e2) }
| e1=expression SMALLER_OR_EQUAL e2=expression { Binary_Pred ($startpos,Inequality Smaller_or_Equal,e1,e2) }
| e1=expression S_SMALLER e2=expression { Binary_Pred ($startpos,Inequality Strictly_Smaller,e1,e2) }
| e1=expression GREATER_OR_EQUAL e2=expression { Binary_Pred ($startpos,Inequality Greater_or_Equal,e1,e2) }
| e1=expression S_GREATER e2=expression { Binary_Pred ($startpos,Inequality Strictly_Greater,e1,e2) }

(* GENERALIZED SUBSTITUTIONS *)

substitution:
  s=level1_substitution { s }
| s1=substitution SEMICOLON s2=substitution { Sequencement (s1,s2) }
| s1=substitution PARALLEL s2=substitution { Parallel (s1,s2) }

elsif: ELSIF p=predicate THEN s=substitution { (p,s) }

els: ELSE s=substitution { s }

whn: WHEN p=predicate THEN s=substitution { (p,s) }

case_or: CASE_OR e=expression THEN s=substitution { (e,s) }

id_eq_expr: id=IDENT EQUAL e=expression { (($startpos(id),id),e) }

callup_subst:
| id=IDENT { CallUp ([],($startpos(id),id),[]) }
| id=IDENT LPAR e=expression RPAR { CallUp ([],($startpos(id),id),expr_to_list e) }
| ids=ident_lst_comma LEFTARROW id=IDENT { CallUp (ids,($startpos(id),id),[]) }
| ids=ident_lst_comma LEFTARROW id=IDENT LPAR e=expression RPAR { CallUp (ids,($startpos(id),id),expr_to_list e) }

level1_substitution:
  BEGIN s=substitution END { s }
| SKIP { Skip }
| ids=ident_lst_comma AFFECTATION e=expression { Affectation ((List.hd ids,List.tl ids),expr_to_nonempty_list e) }
| id=IDENT LPAR e1=expression RPAR lst=list(LPAR e=expression RPAR {e}) AFFECTATION e2=expression
     { Function_Affectation (($startpos(id),id),(e1,lst),e2) }
| id=IDENT SQUOTE fi=IDENT AFFECTATION e=expression { Record_Affectation (($startpos(id),id),($startpos(fi),fi),e) }
| PRE p=predicate THEN s=substitution END { Pre (p,s) }
| ASSERT p=predicate THEN s=substitution END { Assert (p,s) }
| CHOICE lst=separated_nonempty_list(CASE_OR,substitution) END { Choice (List.hd lst,List.tl lst) }
| IF p=predicate THEN s=substitution ei=elsif* e=option(els) END { IfThenElse (((p,s),ei),e) }
| SELECT p=predicate THEN s=substitution w=whn* e=option(els) END { Select (((p,s),w),e) }
| CASE exp=expression OF
        EITHER e=expression THEN s=substitution
        ors=case_or*
        opt=option(els)
  END END { Case (exp,((e,s),ors),opt) }
| ANY ids=ident_lst_comma WHERE p=predicate THEN s=substitution END { Any ((List.hd ids,List.tl ids),p,s) }
| LET ids=ident_lst_comma BE eqs=separated_nonempty_list(AND,id_eq_expr) IN s=substitution END { Let ((List.hd ids,List.tl ids),(List.hd eqs,List.tl eqs),s) }
| ids=ident_lst_comma BECOMES_ELT e=expression { BecomesElt ((List.hd ids,List.tl ids),e) }
| ids=ident_lst_comma MEMBER_OF LPAR p=predicate RPAR { BecomesSuch ((List.hd ids,List.tl ids),p) }
| VAR ids=ident_lst_comma IN s=substitution END { Var ((List.hd ids,List.tl ids),s) }
| c=callup_subst { c }
| WHILE cond=predicate DO s=substitution INVARIANT inv=predicate VARIANT var=expression END { While (cond,s,inv,var) }

(* ABSTRACT MACHINES *)

component_eof: a=component EOF { a }

component:
| MACHINE h=machine_header lst=clause* END { let (id,params) = h in Abstract_machine (mk_machine_exn id params lst) }
| REFINEMENT h=machine_header REFINES abs=IDENT lst=clause* END
  { let (id,params) = h in Refinement (mk_refinement_exn id params ($startpos(abs),abs) lst) }
| IMPLEMENTATION h=machine_header REFINES abs=IDENT lst=clause* END
  { let (id,params) = h in Implementation (mk_implementation_exn id params ($startpos(abs),abs) lst) }

machine_header:
  id=IDENT { (($startpos(id),id),[]) }
| id=IDENT LPAR lst=ident_lst_comma RPAR { (($startpos(id),id),lst) }

machine_instanciation:
| id=IDENT { (($startpos(id),id),[]) }
| id=IDENT LPAR e=expression RPAR { (($startpos(id),id),expr_to_list e) }

set :
| id=IDENT { Abstract_Set ($startpos(id),id) }
| id=IDENT EQUAL LBRA elts=ident_lst_comma RBRA { Concrete_Set (($startpos(id),id),elts) }

operation :
| id=IDENT EQUAL s=level1_substitution { ([],($startpos(id),id),[],s) }
| id=IDENT LPAR lst=ident_lst_comma RPAR EQUAL s=level1_substitution { ([],($startpos(id),id),lst,s) }
| ids=ident_lst_comma LEFTARROW id=IDENT LPAR lst=ident_lst_comma RPAR EQUAL s=level1_substitution { (ids,($startpos(id),id),lst,s) }
| ids=ident_lst_comma LEFTARROW id=IDENT EQUAL s=level1_substitution { (ids,($startpos(id),id),[],s) }

semicolon_pred_lst:
| p=predicate { [p] }
| p=predicate SEMICOLON lst=semicolon_pred_lst { p::lst }

import:
| id=IDENT { (($startpos(id),id),[]) }
| id=IDENT LPAR e=expression RPAR { (($startpos(id),id),expr_to_list e) }

valuation:
| id=IDENT EQUAL e=expression { (($startpos(id),id),e) }

clause:
  CONSTRAINTS p=predicate { Constraints($startpos,p) }
| SEES lst=ident_lst_comma { Sees($startpos,lst) }
| INCLUDES lst=separated_nonempty_list ( COMMA, machine_instanciation ) { Includes($startpos,lst) }
| EXTENDS lst=separated_nonempty_list ( COMMA, machine_instanciation ) { Extends($startpos,lst) }
| PROMOTES lst=ident_lst_comma { Promotes($startpos,lst) }
| USES lst=ident_lst_comma { Uses($startpos,lst) }
| SETS lst=separated_nonempty_list( SEMICOLON, set ) { Sets ($startpos,lst) }
| CONCRETE_CONSTANTS lst=ident_lst_comma { Constants ($startpos,lst) }
| CONSTANTS lst=ident_lst_comma { Constants ($startpos,lst) }
| ABSTRACT_CONSTANTS lst=ident_lst_comma { Abstract_constants ($startpos,lst) }
| PROPERTIES p=predicate { Properties ($startpos,p) }
| CONCRETE_VARIABLES lst=ident_lst_comma { Concrete_variables ($startpos,lst) }
| ABSTRACT_VARIABLES lst=ident_lst_comma { Variables ($startpos,lst) }
| VARIABLES lst=ident_lst_comma { Variables ($startpos,lst) }
| INVARIANT p=predicate { Invariant ($startpos,p) }
| ASSERTIONS lst=semicolon_pred_lst { Assertions ($startpos,lst) }
| INITIALISATION s=substitution { Initialization ($startpos,s) }
| OPERATIONS lst=separated_nonempty_list( SEMICOLON, operation ) { Operations ($startpos,lst) }
| LOCAL_OPERATIONS lst=separated_nonempty_list( SEMICOLON, operation ) { Local_Operations ($startpos,lst) }
| IMPORTS lst=separated_nonempty_list(COMMA,import) { Imports ($startpos,lst) }
| VALUES lst=separated_nonempty_list(SEMICOLON,valuation) { Values($startpos,lst) }
%%
