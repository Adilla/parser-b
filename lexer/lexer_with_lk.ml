open Grammar

type t_token = token * Lexing.position * Lexing.position

let dummy = ( EOF, Lexing.dummy_pos, Lexing.dummy_pos )

type state = { lb:Lexing.lexbuf;
               mutable queue: t_token Queue.t;
               mutable last: t_token; }

let get_token lexbuf =
  let token = Lexer_base.token lexbuf in
  let startp = lexbuf.Lexing.lex_start_p
  and endp = lexbuf.Lexing.lex_curr_p in
  token, startp, endp

let mk_state filename input =
  let lb = Lexing.from_channel input in
  lb.Lexing.lex_curr_p <- { lb.Lexing.lex_curr_p with Lexing.pos_fname = filename; };
  { lb; queue=Queue.create (); last=dummy; }

let get_next (state:state) : t_token =
  let next =
    if Queue.is_empty state.queue then get_token state.lb
    else Queue.pop state.queue
  in state.last <- next; next

let prepend_queue (state:state) (queue:t_token Queue.t) : unit =
  Queue.transfer state.queue queue;
  state.queue <- queue

(* ----- *)

let token_to_string = function
  | SIZE -> "size"
  | REV -> "rev"
  | RAN -> "ran"
  | PROPERTIES -> "PROPERTIES"
  | OF -> "OF"
  | LBRA_COMP -> "LBRA_COMP({)"
  | CONCRETE_VARIABLES -> "CONCRETE_VARIABLES"
  | ABSTRACT_CONSTANTS -> "ABSTRACT_CONSTANTS"
  | CONC -> "conc"
  | WHERE ->      "WHERE"
  | THEN ->       "THEN"
  | SELECT ->     "SELECT"
  | TRUE ->       "TRUE"
  | FALSE ->      "FALSE"
  | MAXINT ->     "MAXINT"
  | MININT ->     "MININT"
  | SIGMA ->      "SIGMA"
  | Z_SET ->      "INTEGER"
  | N1_SET ->     "NATURAL1"
  | N_SET ->      "NATURAL"
  | NAT1_SET ->   "NAT1"
  | NAT_SET ->    "NAT"
  | INT_SET ->    "INT"
  | BOOL_SET ->   "BOOL"
  | STRING_SET -> "STRING"
  | PI ->         "PI"
  | POW1 ->       "POW1"
  | POW ->        "POW"
  | FPOW1 ->      "FIN1"
  | FPOW ->       "FIN"
  | Q_UNION ->    "UNION"
  | Q_INTER ->    "INTER"
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
  | MOD    -> "mod"
  | SUCC   -> "succ"
  | PRED   -> "pred"
  | MAX    -> "max"
  | MIN    -> "min"
  | CARD   -> "card"
  | G_UNION -> "union"
  | G_INTER -> "inter"
  | PROJ1     -> "prj1"
  | PROJ2     -> "prj2"
  | ITERATION -> "iterate"
  | CLOSURE   -> "closure"
  | CLOSURE1  -> "closure1"
  | DOM     -> "dom"
  | FNC     -> "fnc"
  | REL     -> "rel"
  | NOT     -> "not"
  | ID      -> "id"
  | REC     -> "rec"
  | STRUCT  -> "struct"
  | SEQ1    -> "seq1"
  | SEQ     -> "seq"
  | ISEQ1   -> "iseq1"
  | ISEQ    -> "iseq"
  | PERM    -> "perm"
  | FIRST   -> "first"
  | LAST    -> "last"
  | TAIL    -> "tail"
  | S_PARTIELLE     -> "+->>"
  | S_TOTALE        -> "-->>"
  | B_TOTALE        -> ">->>"
  | NOT_S_INCLUDED  -> "/<<:"
  | MAPLET          -> "|->"
  | RELATION        -> "<->"
  | SOUSTRACTION_D  -> "<<|"
  | SOUSTRACTION_CO -> "|>>"
  | PARTIELLE       -> "+->"
  | TOTALE          -> "-->"
  | LEFTARROW       -> "<--"
  | P_INJECTION     -> ">+>"
  | T_INJECTION     -> ">->"
  | EQUIV           -> "<=>"
  | S_INCLUDED      -> "<<:"
  | NOT_INCLUDED    -> "/<:"
  | RESTRICTION_Q   -> "\\|/"
  | RESTRICTION_T   -> "/|\\"
  | INSERTION_Q     -> "<-"
  | INSERTION_T     -> "->"
  | RESTRICTION_CO  -> "|>"
  | POWER           -> "**"
  | EMPTY_SET       -> "{}"
  | EMPTY_SEQ       -> "[]"
  | DOTDOT          -> ".."
  | B_UNION         -> "\\/"
  | B_INTER         -> "/\\"
  | DPRODUCT        -> "><"
  | PARALLEL        -> "||"
  | RESTRICTION_D   -> "<|"
  | SURCHARGE       -> "<+"
  | IMPLY           -> "=>"
  | NOT_EQUAL       -> "/="
  | NOT_MEMBER_OF   -> "/:"
  | INCLUDED        -> "<:"
  | SMALLER_OR_EQUAL -> "<="
  | GREATER_OR_EQUAL -> ">="
  | AFFECTATION     -> ":="
  | BECOMES_ELT     -> "::"
(*   | BECOMES_SUCH    -> ":(" *)
  | EQUALEQUAL      -> "=="
  | DOLLAR_ZERO     -> "$0"
  | DIV             -> "/"
  | DOT             -> "."
  | SQUOTE          -> "\'"
  | BAR             -> "|"
  | LBRA            -> "{"
  | RBRA            -> "}"
  | TILDE           -> "~"
  | SEMICOLON       -> ";"
  | LSQU            -> "["
  | RSQU            -> "]"
  | LAMBDA          -> "%"
  | AND             -> "&"
  | FORALL          -> "!"
  | EXISTS          -> "#"
  | EQUAL           -> "="
  | MEMBER_OF       -> ":"
  | S_SMALLER       -> "<"
  | S_GREATER       -> ">"
  | PLUS            -> "+"
  | MINUS           -> "-"
  | STAR            -> "*"
  | COMMA           -> ","
  | RPAR            -> ")"
  | LPAR            -> "("
  | CIRC            -> "^"
  | FRONT           -> "front"

  | TREE -> "tree"
  | BTREE -> "btree"
  | CONST -> "const"
  | TOP -> "top"
  | SONS -> "sons"
  | PREFIX -> "prefix"
  | POSTFIX -> "postfix"
  | SIZET -> "sizet"
  | MIRROR -> "mirror"
  | RANK -> "rank"
  | FATHER -> "father"
  | SON -> "son"
  | ARITY -> "arity"
  | BIN -> "bin"
  | LEFT -> "left"
  | RIGHT -> "right"
  | INFIX -> "infix"
  | SUBTREE -> "subtree"

  | DEF_FILE id -> Printf.sprintf "FILE(%s)" id
  | IDENT id -> Printf.sprintf "IDENT(%s)" id
  | INTEGER i -> string_of_int i
  | EOF -> "__EOF__"
  | STRING s -> Printf.sprintf "STRING(%s)" s

let get_last_token_str state =
  let (tk,_,_) = state.last in
  token_to_string tk

let get_current_pos state = state.lb.Lexing.lex_curr_p
