open Utils
open Expression

type ('lc,'ty) substitution =
  | Skip of 'lc
  | Affectation of 'lc * 'lc ident non_empty_list * ('lc,'ty) expression
  | Function_Affectation of 'lc * 'lc ident * ('lc,'ty) expression non_empty_list * ('lc,'ty) expression
  | Record_Affectation of 'lc * 'lc ident * 'lc ident * ('lc,'ty) expression (** record'field := e *)
  | Pre of 'lc * ('lc,'ty) predicate * ('lc,'ty) substitution
  | Assert of 'lc * ('lc,'ty) predicate * ('lc,'ty) substitution
  | Choice of 'lc * ('lc,'ty) substitution non_empty_list
  | IfThenElse of 'lc * (('lc,'ty) predicate * ('lc,'ty) substitution) non_empty_list * ('lc,'ty) substitution option
  | Select of 'lc * (('lc,'ty) predicate * ('lc,'ty) substitution) non_empty_list * ('lc,'ty) substitution option
  | Case of 'lc * ('lc,'ty) expression * (('lc,'ty) expression * ('lc,'ty) substitution) non_empty_list * ('lc,'ty) substitution option
  | Any of 'lc * 'lc ident non_empty_list * ('lc,'ty) predicate * ('lc,'ty) substitution
  | Let of 'lc * 'lc ident non_empty_list * ('lc ident * ('lc,'ty) expression) non_empty_list * ('lc,'ty) substitution
  | BecomesElt of 'lc * 'lc ident non_empty_list * ('lc,'ty) expression
  | BecomesSuch of 'lc * 'lc ident non_empty_list * ('lc,'ty) predicate
  | Var of 'lc * 'lc ident non_empty_list * ('lc,'ty) substitution
  | CallUp of 'lc * 'lc ident list * 'lc ident * ('lc,'ty) expression list
  | While of 'lc * ('lc,'ty) predicate * ('lc,'ty) substitution * ('lc,'ty) predicate * ('lc,'ty) expression
  | Sequencement of 'lc * ('lc,'ty) substitution * ('lc,'ty) substitution
  | Parallel of 'lc * ('lc,'ty) substitution * ('lc,'ty) substitution

type u_subst = (loc,bool) substitution

val subst_eq : ('lc,'ty) substitution -> ('lc2,'ty2) substitution -> bool

val subst_list_eq : ('lc2,'ty2) substitution list -> ('lc2,'ty2) substitution list -> bool
