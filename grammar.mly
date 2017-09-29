%{
open Utils
open Expression
open Substitution
open Component

let mk_infix_app lc (f:u_expr) (a1:u_expr) (a2:u_expr) : u_expr =
  Application (lc,false,f,Couple(lc,false,Infix,a1,a2))

let expr_to_list (e:u_expr) : u_expr list =
  let rec aux lst = function
    | Couple (_,b,Comma,e1,e2) as e ->
      if b then e::lst else aux (e2::lst) e1
    | e -> e::lst
  in
  aux [] e

let rec expr_to_nonempty_list (e:u_expr): u_expr non_empty_list =
  let lst = expr_to_list e in (List.hd lst,List.tl lst)

let set_true = function
  | Couple (l,_,cm,a,b) -> Couple(l,true,cm,a,b)
  | e -> e

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
%type <Component.u_comp> component_eof
%type <Expression.u_expr> expression
%type <Expression.u_pred> predicate
%type <Substitution.u_subst> substitution
%type <Substitution.u_subst> level1_substitution
%type <Component.u_clause> clause
%type <Component.u_operation> operation

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

ident_lst_comma2:
        | id=IDENT { [(false,($startpos(id),id))] }
        | id=IDENT COMMA lst=ident_lst_comma2 { (false,($startpos(id),id))::lst }

liste_ident:
  id=IDENT { [(false,($startpos(id),id))] }
| LPAR lst=ident_lst_comma2 RPAR { lst }

fields:
| id=IDENT MEMBER_OF e=expression { [(($startpos(id),id),e)] }
| id=IDENT MEMBER_OF e=expression COMMA lst=fields { (($startpos(id),id),e)::lst }

expression:
(* expression_primaire: *)
| id=IDENT { Ident (false,($startpos(id),id)) }
| id=IDENT DOLLAR_ZERO { Dollar (false,($startpos(id),id)) }
| LPAR e=expression RPAR { set_true e }
| s=STRING { Builtin ($startpos(s),false,String s) }
(* expression_booleenne: *)
| TRUE { Builtin ($startpos,false,TRUE) }
| FALSE { Builtin ($startpos,false,FALSE) }
| CBOOL LPAR p=predicate RPAR { Pbool ($startpos,false,p) }
(* entier_lit: *)
| i=INTEGER { Builtin ($startpos(i),false,Integer i) }
| MAXINT { Builtin ($startpos,false,MaxInt) }
| MININT { Builtin ($startpos,false,MinInt) }
(* expression_arithmetique: *)
| e1=expression PLUS e2=expression { mk_infix_app $startpos (Builtin ($startpos($2),false,Addition)) e1 e2 }
| e1=expression MINUS e2=expression { mk_infix_app $startpos (Builtin ($startpos($2),false,Difference)) e1 e2 }
| MINUS e=expression { Application ($startpos,false,Builtin($startpos,false,Unary_Minus),e) } %prec unary_minus
| e1=expression STAR e2=expression { mk_infix_app $startpos (Builtin ($startpos($2),false,Product)) e1 e2 }
| e1=expression DIV e2=expression { mk_infix_app $startpos (Builtin ($startpos($2),false,Division)) e1 e2 }
| e1=expression MOD e2=expression { mk_infix_app $startpos (Builtin ($startpos($2),false,Modulo)) e1 e2 }
| e1=expression POWER e2=expression { mk_infix_app $startpos (Builtin ($startpos($2),false,Power)) e1 e2 }
| SUCC LPAR e=expression RPAR { Application ($startpos,false,Builtin($startpos,false,Successor),e) }
| PRED LPAR e=expression RPAR { Application ($startpos,false,Builtin($startpos,false,Predecessor),e) }
| MAX LPAR e=expression RPAR { Application ($startpos,false,Builtin($startpos,false,Max),e) }
| MIN LPAR e=expression RPAR { Application ($startpos,false,Builtin($startpos,false,Min),e) }
| CARD LPAR e=expression RPAR { Application ($startpos,false,Builtin($startpos,false,Cardinal),e) }
| SIGMA ids=liste_ident DOT LPAR p=predicate BAR e=expression RPAR { Binder ($startpos,false,Sum,(List.hd ids,List.tl ids),p,e) }
| PI ids=liste_ident DOT LPAR p=predicate BAR e=expression RPAR { Binder ($startpos,false,Prod,(List.hd ids,List.tl ids),p,e) }
(* expression_de_couples: *)
| e1=expression MAPLET e2=expression { Couple ($startpos,false,Maplet,e1,e2) }
| e1=expression COMMA e2=expression { Couple ($startpos,false,Comma,e1,e2) }
(* expression_d_ensembles: *)
| EMPTY_SET { Builtin ($startpos,false,Empty_Set) }
| Z_SET { Builtin ($startpos,false,INTEGER) }
| N_SET { Builtin ($startpos,false,NATURAL) }
| N1_SET { Builtin ($startpos,false,NATURAL1) }
| NAT_SET { Builtin ($startpos,false,NAT) }
| NAT1_SET { Builtin ($startpos,false,NAT1) }
| INT_SET { Builtin ($startpos,false,INT) }
| BOOL_SET { Builtin ($startpos,false,BOOLEANS) }
| STRING_SET { Builtin($startpos,false,STRINGS) }
(* construction_d_ensembles: *)
| LBRA_COMP ids=ident_lst_comma2 BAR p=predicate RBRA  { Comprehension ($startpos,false,(List.hd ids,List.tl ids),p) }
| POW LPAR e=expression RPAR { Application($startpos,false,Builtin ($startpos,false,Power_Set Full),e) }
| POW1 LPAR e=expression RPAR { Application($startpos,false,Builtin($startpos,false,Power_Set Non_Empty),e) }
| FPOW LPAR e=expression RPAR { Application($startpos,false,Builtin($startpos,false,Power_Set Finite),e) }
| FPOW1 LPAR e=expression RPAR { Application($startpos,false,Builtin($startpos,false,Power_Set Finite_Non_Empty),e) }
| LBRA e=expression RBRA { Extension ($startpos,false,expr_to_nonempty_list e) } 
| e1=expression DOTDOT e2=expression { mk_infix_app $startpos (Builtin ($startpos($2),false,Interval)) e1 e2 }
| e1=expression B_UNION e2=expression { mk_infix_app $startpos (Builtin ($startpos($2),false,Union)) e1 e2 }
| e1=expression B_INTER e2=expression { mk_infix_app $startpos (Builtin ($startpos($2),false,Intersection)) e1 e2 }
| G_UNION LPAR e=expression RPAR { Application ($startpos,false,Builtin($startpos,false,G_Union),e) }
| G_INTER LPAR e=expression RPAR { Application ($startpos,false,Builtin($startpos,false,G_Intersection),e) }
| Q_UNION ids=liste_ident DOT LPAR p=predicate BAR e=expression RPAR { Binder ($startpos,false,Q_Union,(List.hd ids,List.tl ids),p,e) }
| Q_INTER ids=liste_ident DOT LPAR p=predicate BAR e=expression RPAR { Binder ($startpos,false,Q_Intersection,(List.hd ids,List.tl ids),p,e) }
(* expression_de_records: *)
| STRUCT LPAR lst=fields RPAR   { Record_Type ($startpos,false,(List.hd lst,List.tl lst))  }
| REC LPAR lst=fields RPAR  { Record ($startpos,false,(List.hd lst,List.tl lst)) }
(* | REC LPAR e=expression RPAR  { Record ($startpos,expr_to_rfields e) } *)
| e=expression SQUOTE id=IDENT   { Record_Field_Access ($startpos,false,e,($startpos(id),id)) }
(* expression_de_relations: *)
| e1=expression RELATION e2=expression { mk_infix_app $startpos (Builtin ($startpos($2),false,Relations)) e1 e2 }
| ID LPAR e=expression RPAR { Application ($startpos,false,Builtin ($startpos,false,Identity_Relation),e) }
| e=expression TILDE { Application ($startpos,false,Builtin ($startpos($2),false,Inverse_Relation),e) }
| PROJ1 LPAR e=expression RPAR { Application ($startpos,false,Builtin($startpos,false,First_Projection),e) }
| PROJ2 LPAR e=expression RPAR { Application ($startpos,false,Builtin($startpos,false,Second_Projection),e) }
| e1=expression SEMICOLON e2=expression { mk_infix_app $startpos (Builtin ($startpos($2),false,Composition))  e1 e2  }
| e1=expression DPRODUCT e2=expression { mk_infix_app $startpos  (Builtin ($startpos($2),false,Direct_Product))  e1 e2 }
| e1=expression PARALLEL e2=expression { mk_infix_app $startpos (Builtin ($startpos($2),false,Parallel_Product)) e1 e2 }
| ITERATION LPAR e=expression RPAR { Application ($startpos,false,Builtin($startpos,false,Iteration),e) }
| CLOSURE LPAR e=expression RPAR { Application ($startpos,false,Builtin($startpos,false,Closure),e) }
| CLOSURE1 LPAR e=expression RPAR { Application ($startpos,false,Builtin($startpos,false,Transitive_Closure),e) }
| DOM LPAR e=expression RPAR { Application ($startpos,false,Builtin($startpos,false,Domain),e) }
| RAN LPAR e=expression RPAR { Application ($startpos,false,Builtin($startpos,false,Range),e) }
| e1=expression LSQU e2=expression RSQU { mk_infix_app $startpos (Builtin ($startpos($2),false,Image)) e1 e2 }
| e1=expression RESTRICTION_D e2=expression  { mk_infix_app $startpos (Builtin ($startpos($2),false,Domain_Restriction)) e1 e2 }
| e1=expression SOUSTRACTION_D e2=expression { mk_infix_app $startpos (Builtin ($startpos($2),false,Domain_Soustraction)) e1 e2 }
| e1=expression RESTRICTION_CO e2=expression { mk_infix_app $startpos (Builtin ($startpos($2),false,Codomain_Restriction)) e1 e2 }
| e1=expression SOUSTRACTION_CO e2=expression { mk_infix_app $startpos (Builtin ($startpos($2),false,Codomain_Soustraction)) e1 e2 }
| e1=expression SURCHARGE e2=expression { mk_infix_app $startpos (Builtin ($startpos($2),false,Surcharge)) e1 e2 }
(* expression_de_fonctions: *)
| e1=expression PARTIELLE e2=expression   { mk_infix_app $startpos (Builtin ($startpos($2),false,Functions Partial_Functions)) e1 e2 }
| e1=expression TOTALE e2=expression      { mk_infix_app $startpos (Builtin ($startpos($2),false,Functions Total_Functions)) e1 e2 }
| e1=expression P_INJECTION e2=expression { mk_infix_app $startpos (Builtin ($startpos($2),false,Functions Partial_Injections)) e1 e2 }
| e1=expression T_INJECTION e2=expression { mk_infix_app $startpos (Builtin ($startpos($2),false,Functions Total_Injections)) e1 e2 }
| e1=expression S_PARTIELLE e2=expression { mk_infix_app $startpos (Builtin ($startpos($2),false,Functions Partial_Surjections)) e1 e2 }
| e1=expression S_TOTALE e2=expression    { mk_infix_app $startpos (Builtin ($startpos($2),false,Functions Total_Surjections)) e1 e2 }
| e1=expression B_TOTALE e2=expression    { mk_infix_app $startpos (Builtin ($startpos($2),false,Functions Bijections)) e1 e2 }
(* construction_de_fonctions: *)
| LAMBDA ids=liste_ident DOT LPAR p=predicate BAR e=expression RPAR { Binder ($startpos,false,Lambda,(List.hd ids,List.tl ids),p,e) }
| f=expression LPAR a=expression RPAR { Application ($startpos,false,f,a) }
| FNC LPAR e=expression RPAR { Application ($startpos,false,Builtin($startpos,false,Fnc),e) }
| REL LPAR e=expression RPAR { Application ($startpos,false,Builtin($startpos,false,Rel),e) }
(* expression_de_suites: *)
| SEQ LPAR e=expression RPAR   { Application ($startpos,false,Builtin($startpos,false,Sequence_Set All_Seq),e) }
| SEQ1 LPAR e=expression RPAR  { Application ($startpos,false,Builtin($startpos,false,Sequence_Set Non_Empty_Seq),e) }
| ISEQ LPAR e=expression RPAR  { Application ($startpos,false,Builtin($startpos,false,Sequence_Set Injective_Seq),e) }
| ISEQ1 LPAR e=expression RPAR { Application ($startpos,false,Builtin($startpos,false,Sequence_Set Injective_Non_Empty_Seq),e) }
| PERM LPAR e=expression RPAR  { Application ($startpos,false,Builtin($startpos,false,Sequence_Set Permutations),e) }
| EMPTY_SEQ { Builtin($startpos,false,Empty_Seq) }
| LSQU e=expression RSQU { Sequence($startpos,false,expr_to_nonempty_list e)  }
(* construction_de_suites: *)
| SIZE LPAR e=expression RPAR { Application($startpos,false,Builtin($startpos,false,Size),e) }
| FIRST LPAR e=expression RPAR { Application($startpos,false,Builtin($startpos,false,First),e) }
| LAST LPAR e=expression RPAR { Application($startpos,false,Builtin($startpos,false,Last),e) }
| FRONT LPAR e=expression RPAR { Application($startpos,false,Builtin($startpos,false,Front),e) }
| TAIL LPAR e=expression RPAR { Application($startpos,false,Builtin($startpos,false,Tail),e) }
| REV LPAR e=expression RPAR { Application($startpos,false,Builtin($startpos,false,Reverse),e) }
| e1=expression CIRC e2=expression { mk_infix_app $startpos (Builtin($startpos($2),false,Concatenation)) e1 e2 }
| e1=expression INSERTION_T e2=expression { mk_infix_app $startpos (Builtin($startpos($2),false,Head_Insertion)) e1 e2 }
| e1=expression INSERTION_Q e2=expression { mk_infix_app $startpos (Builtin($startpos($2),false,Tail_Insertion)) e1 e2 }
| e1=expression RESTRICTION_T e2=expression { mk_infix_app $startpos (Builtin($startpos($2),false,Head_Restriction)) e1 e2 }
| e1=expression RESTRICTION_Q e2=expression { mk_infix_app $startpos (Builtin($startpos($2),false,Tail_Restriction)) e1 e2 }
| CONC LPAR e=expression RPAR { Application($startpos,false,Builtin($startpos,false,G_Concatenation),e) }
(* expression_d_arbres: *)
| TREE LPAR e=expression RPAR { Application($startpos,false,Builtin($startpos,false,Tree),e) }
| BTREE LPAR e=expression RPAR { Application($startpos,false,Builtin($startpos,false,Btree),e) }
| CONST LPAR e=expression RPAR { Application ($startpos,false,Builtin($startpos,false,Const),e) }
| TOP LPAR e=expression RPAR { Application($startpos,false,Builtin($startpos,false,Top),e) }
| SONS LPAR e=expression RPAR { Application($startpos,false,Builtin($startpos,false,Sons),e) }
| PREFIX LPAR e=expression RPAR { Application($startpos,false,Builtin($startpos,false,Prefix),e) }
| POSTFIX LPAR e=expression RPAR { Application($startpos,false,Builtin($startpos,false,Postfix),e) }
| SIZET LPAR e=expression RPAR { Application($startpos,false,Builtin($startpos,false,SizeT),e) }
| MIRROR LPAR e=expression RPAR { Application($startpos,false,Builtin($startpos,false,Mirror),e) }
| RANK LPAR e=expression RPAR { Application ($startpos,false,Builtin($startpos,false,Rank),e) }
| FATHER LPAR e=expression RPAR { Application ($startpos,false,Builtin($startpos,false,Father),e) }
| SON LPAR e=expression RPAR { Application ($startpos,false,Builtin($startpos,false,Son),e) }
| SUBTREE LPAR e=expression RPAR { Application ($startpos,false,Builtin($startpos,false,Subtree),e) }
| ARITY LPAR e=expression RPAR { Application ($startpos,false,Builtin($startpos,false,Arity),e) }
| BIN LPAR e=expression RPAR { Application($startpos,false,Builtin($startpos,false,Bin),e) }
| LEFT LPAR e=expression RPAR { Application($startpos,false,Builtin($startpos,false,Left),e) }
| RIGHT LPAR e=expression RPAR { Application($startpos,false,Builtin($startpos,false,Right),e) }
| INFIX LPAR e=expression RPAR { Application($startpos,false,Builtin($startpos,false,Infix),e) }

(* PREDICATES *)

predicate:
  LPAR p=predicate RPAR { p }
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
| s1=substitution SEMICOLON s2=substitution { Sequencement ($startpos,s1,s2) }
| s1=substitution PARALLEL s2=substitution { Parallel ($startpos,s1,s2) }

elsif: ELSIF p=predicate THEN s=substitution { (p,s) }

els: ELSE s=substitution { s }

whn: WHEN p=predicate THEN s=substitution { (p,s) }

case_or: CASE_OR e=expression THEN s=substitution { (e,s) }

id_eq_expr: id=IDENT EQUAL e=expression { (($startpos(id),id),e) }

callup_subst:
| id=IDENT { CallUp ($startpos,[],($startpos(id),id),[]) }
| id=IDENT LPAR e=expression RPAR { CallUp ($startpos,[],($startpos(id),id),expr_to_list e) }
| ids=ident_lst_comma LEFTARROW id=IDENT { CallUp ($startpos,ids,($startpos(id),id),[]) }
| ids=ident_lst_comma LEFTARROW id=IDENT LPAR e=expression RPAR { CallUp ($startpos,ids,($startpos(id),id),expr_to_list e) }

level1_substitution:
  BEGIN s=substitution END { s }
| SKIP { Skip $startpos }
| ids=ident_lst_comma AFFECTATION e=expression { Affectation ($startpos,(List.hd ids,List.tl ids),e) }
| id=IDENT LPAR e1=expression RPAR lst=list(LPAR e=expression RPAR {e}) AFFECTATION e2=expression
     { Function_Affectation ($startpos,($startpos(id),id),(e1,lst),e2) }
| id=IDENT SQUOTE fi=IDENT AFFECTATION e=expression { Record_Affectation ($startpos,($startpos(id),id),($startpos(fi),fi),e) }
| PRE p=predicate THEN s=substitution END { Pre ($startpos,p,s) }
| ASSERT p=predicate THEN s=substitution END { Assert ($startpos,p,s) }
| CHOICE lst=separated_nonempty_list(CASE_OR,substitution) END { Choice ($startpos,(List.hd lst,List.tl lst)) }
| IF p=predicate THEN s=substitution ei=elsif* e=option(els) END { IfThenElse ($startpos,((p,s),ei),e) }
| SELECT p=predicate THEN s=substitution w=whn* e=option(els) END { Select ($startpos,((p,s),w),e) }
| CASE exp=expression OF
        EITHER e=expression THEN s=substitution
        ors=case_or*
        opt=option(els)
  END END { Case ($startpos,exp,((e,s),ors),opt) }
| ANY ids=ident_lst_comma WHERE p=predicate THEN s=substitution END { Any ($startpos,(List.hd ids,List.tl ids),p,s) }
| LET ids=ident_lst_comma BE eqs=separated_nonempty_list(AND,id_eq_expr) IN s=substitution END
         { Let ($startpos,(List.hd ids,List.tl ids),(List.hd eqs,List.tl eqs),s) }
| ids=ident_lst_comma BECOMES_ELT e=expression { BecomesElt ($startpos,(List.hd ids,List.tl ids),e) }
| ids=ident_lst_comma MEMBER_OF LPAR p=predicate RPAR { BecomesSuch ($startpos,(List.hd ids,List.tl ids),p) }
| VAR ids=ident_lst_comma IN s=substitution END { Var ($startpos,(List.hd ids,List.tl ids),s) }
| c=callup_subst { c }
| WHILE cond=predicate DO s=substitution INVARIANT inv=predicate VARIANT var=expression END
         { While ($startpos,cond,s,inv,var) }

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
| id=IDENT LPAR lst=ident_lst_comma2 RPAR EQUAL s=level1_substitution { ([],($startpos(id),id),lst,s) }
| ids=ident_lst_comma2 LEFTARROW id=IDENT LPAR lst=ident_lst_comma2 RPAR EQUAL s=level1_substitution { (ids,($startpos(id),id),lst,s) }
| ids=ident_lst_comma2 LEFTARROW id=IDENT EQUAL s=level1_substitution { (ids,($startpos(id),id),[],s) }

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
