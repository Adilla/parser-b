open Utils
open Expression

type substitution =
  | Skip
  | Affectation1 of ident list * expression list
  | Affectation2 of ident * expression list * expression
  | Affectation3 of ident * expression list * expression list * expression
  | Affectation4 of ident * ident * expression
  | Pre of predicate * substitution
  | Assert of predicate * substitution
  | Choice of substitution list
  | IfThenElse of predicate * substitution * (predicate*substitution) list * substitution option
  | Select of predicate * substitution * (predicate*substitution) list * substitution option
  | Case of expression * expression list * substitution * (expression list*substitution) list * substitution option
  | Any of ident list * predicate * substitution
  | Let of ident list * (ident*expression) list * substitution
  | BecomesElt of ident list * expression
  | BecomesSuch of ident list * predicate
  | Var of ident list * substitution
  | CallUp of ident list * ident * expression list
  | While of predicate * substitution * predicate * expression
  | Sequencement of substitution * substitution
  | Parallel of substitution * substitution
