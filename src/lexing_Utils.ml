type t_token = Grammar.token * Lexing.position * Lexing.position

let token_to_string tk =
  let open Grammar in
  match tk with
    | CONSTANT x -> SyntaxCore.builtin_to_string x
    | E_PREFIX x -> SyntaxCore.builtin_to_string x
    | PREDICATE x -> SyntaxCore.pred_bop_to_string x
    | E_BINDER x -> SyntaxCore.binder_to_string x
    | E_INFIX_125 x -> SyntaxCore.builtin_to_string x
    | E_INFIX_160 x -> SyntaxCore.builtin_to_string x
    | E_INFIX_170 x -> SyntaxCore.builtin_to_string x
    | E_INFIX_180 x -> SyntaxCore.builtin_to_string x
    | E_INFIX_190 x -> SyntaxCore.builtin_to_string x
    | E_INFIX_200 x -> SyntaxCore.builtin_to_string x
    | PROPERTIES -> "PROPERTIES"
    | OF -> "OF"
    | LBRA_COMP -> "{"
    | CONCRETE_VARIABLES -> "CONCRETE_VARIABLES"
    | ABSTRACT_CONSTANTS -> "ABSTRACT_CONSTANTS"
    | WHERE ->      "WHERE"
    | THEN ->       "THEN"
    | SELECT ->     "SELECT"
    | DEFINITIONS    -> "DEFINITIONS"
    | ELSIF          -> "ELSIF"
    | ELSE           -> "ELSE"
    | WHEN           -> "WHEN"
    | CASE_OR        -> "OR"
    | BEGIN          -> "BEGIN"
    | END            -> "END"
    | SKIP           -> "skip"
    | PRE            -> "PRE"
    | ASSERT         -> "ASSERT"
    | CHOICE         -> "CHOICE"
    | IF             -> "IF"
    | CASE           -> "CASE"
    | OR             -> "or"
    | EITHER         -> "EITHER"
    | ANY            -> "ANY"
    | LET            -> "LET"
    | BE             -> "BE"
    | VAR            -> "VAR"
    | WHILE          -> "WHILE"
    | DO             -> "DO"
    | INVARIANT      -> "INVARIANT"
    | VARIANT        -> "VARIANT"
    | MACHINE        -> "MACHINE"
    | CONSTRAINTS    -> "CONSTRAINTS"
    | SEES           -> "SEES"
    | INCLUDES       -> "INCLUDES"
    | PROMOTES       -> "PROMOTES"
    | EXTENDS        -> "EXTENDS"
    | USES           -> "USES"
    | SETS           -> "SETS"
    | CONSTANTS      -> "CONSTANTS"
    | VARIABLES      -> "VARIABLES"
    | ASSERTIONS     -> "ASSERTIONS"
    | OPERATIONS     -> "OPERATIONS"
    | LOCAL_OPERATIONS     -> "LOCAL_OPERATIONS"
    | IN             -> "IN"
    | REFINEMENT     -> "REFINEMENT"
    | REFINES        -> "REFINES"
    | IMPORTS        -> "IMPORTS"
    | VALUES         -> "VALUES"
    | IMPLEMENTATION -> "IMPLEMENTATION"
    | INITIALISATION -> "INITIALISATION"
    | CONCRETE_CONSTANTS -> "CONCRETE_CONSTANTS"
    | ABSTRACT_VARIABLES -> "ABSTRACT_VARIABLES"
    | CBOOL  -> "bool"
    | NOT     -> "not"
    | REC     -> "rec"
    | STRUCT  -> "struct"
    | MAPLET          -> "|->"
    | LEFTARROW       -> "<--"
    | EQUIV           -> "<=>"
    | PARALLEL        -> "||"
    | IMPLY           -> "=>"
    | AFFECTATION     -> ":="
    | BECOMES_ELT     -> "::"
    | EQUALEQUAL      -> "=="
    | DOLLAR_ZERO     -> "$0"
    | DOT             -> "."
    | SQUOTE          -> "\'"
    | BAR             -> "|"
    | LBRA            -> "{"
    | RBRA            -> "}"
    | TILDE           -> "~"
    | SEMICOLON       -> ";"
    | LSQU            -> "["
    | RSQU            -> "]"
    | AND             -> "&"
    | FORALL          -> "!"
    | EXISTS          -> "#"
    | EQUAL           -> "="
    | MEMBER_OF       -> ":"
    | MINUS           -> "-"
    | COMMA           -> ","
    | RPAR            -> ")"
    | LPAR            -> "("
    | DEF_FILE id -> Printf.sprintf "<%s>" id
    | IDENT id -> Printf.sprintf "identifier(%s)" id
    | EOF -> "__EOF__"
    | STRING s -> Printf.sprintf "\"%s\"" s

module Base_Lexer : sig
  type state
  val mk_state: filename:string -> Lexing.lexbuf -> state
  val get_token_exn: state -> t_token
end =
struct
  type state = Lexing.lexbuf

  let mk_state ~(filename:string) (lb:Lexing.lexbuf) : state =
    let open Lexing in
    lb.lex_curr_p <- { lb.lex_curr_p with pos_fname = filename };
    lb

  let get_token_exn (lexbuf:state) : t_token =
    let open Lexing in
    let token = Lexer_base.token lexbuf in
    let startp = lexbuf.lex_start_p
    and endp = lexbuf.lex_curr_p in
    token, startp, endp
end

type tk_or_tk_list = 
  | Token of t_token
  | List of t_token list

type tree = tk_or_tk_list list

let tree_subst (params:(string*t_token list) list) : t_token list -> tree =
  List.map (function
  | (Grammar.IDENT id,_,_) as tk ->
    begin match List.assoc_opt id params with
      | None -> Token tk
      | Some tks -> List tks
    end
  | tk -> Token tk)

let tree_subst_loc st ed (params:(string*t_token list) list) : t_token list -> tree =
  List.map (function
  | (Grammar.IDENT id as tk,_,_) ->
    begin match List.assoc_opt id params with
      | None -> Token (tk,st,ed)
      | Some tks -> List tks
    end
  | (tk,_,_) -> Token (tk,st,ed))

let rec tree_pop : tree -> (t_token*tree) option = function
  | [] -> None
  | (Token tk)::lst -> Some (tk,lst)
  | (List [])::lst -> tree_pop lst
  | (List (hd::tl))::lst -> Some (hd,(List tl)::lst)

let rec tree_top : tree -> t_token option = function
  | [] -> None
  | (Token tk)::_ -> Some tk
  | (List [])::lst -> tree_top lst
  | (List (hd::_))::_ -> Some hd

let token_to_tree tk = [Token tk]
