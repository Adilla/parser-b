type t_token = Grammar.token * Lexing.position * Lexing.position
val token_to_string : Grammar.token -> string

module Base_Lexer : sig
  type state
  val mk_state: filename:string -> Lexing.lexbuf -> state
  val get_token_exn: state -> t_token
end

type tree
val tree_subst : (string*t_token list) list -> t_token list -> tree
val tree_pop : tree -> (t_token*tree) option
val tree_top : tree -> t_token option
val token_to_tree : t_token -> tree
