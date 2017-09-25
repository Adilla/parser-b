{
  open Lexing
  open Grammar

  let chars_read = ref ""
  let string_loc = ref (0,0)
  let add_char c = chars_read := Printf.sprintf "%s%c" !chars_read c

  let flush () =
    chars_read := ""
(*     string_loc := loc *)

  let int_of_int_lit (s:string) : int =
    if String.get s 0 = '-' then
      let l=String.length s in
      - (int_of_string (String.sub s 1 (l-1)))
    else int_of_string s

let keywords = Hashtbl.create 137

let _ = List.iter (fun (name, keyword) ->
    Hashtbl.add keywords name keyword) [
    "WHERE"     , WHERE;
   "THEN"      , THEN;
   "SELECT"    , SELECT;
   "TRUE"      , TRUE;
   "FALSE"     , FALSE;
   "MAXINT"    , MAXINT;
   "MININT"    , MININT;
   "SIGMA"     , SIGMA;
   "INTEGER"   , Z_SET;
   "NATURAL1"  , N1_SET;
   "NATURAL"   , N_SET;
   "NAT1"      , NAT1_SET;
   "NAT"       , NAT_SET;
   "INT"       , INT_SET;
   "BOOL"      , BOOL_SET;
   "STRING"    , STRING_SET;
   "PI"        , PI;
   "POW1"      , POW1;
   "POW"       , POW;
   "FIN1"      , FPOW1;
   "FIN"       , FPOW;
   "UNION"     , Q_UNION;
   "INTER"     , Q_INTER;
   "DEFINITIONS"        , DEFINITIONS;
   "ELSIF"              , ELSIF;
   "ELSE"               , ELSE;
   "WHEN"               , WHEN;
   "OR"                 , CASE_OR;
   "BEGIN"              , BEGIN;
   "END"                , END;
   "skip"               , SKIP;
   "PRE"                , PRE;
   "ASSERT"             , ASSERT;
   "CHOICE"             , CHOICE;
   "IF"                 , IF;
   "CASE"               , CASE;
   "EITHER"             , EITHER;
   "ANY"                , ANY;
   "LET"                , LET;
   "BE"                 , BE;
   "VAR"                , VAR;
   "WHILE"              , WHILE;
   "DO"                 , DO;
   "INVARIANT"          , INVARIANT;
   "VARIANT"            , VARIANT;
   "MACHINE"            , MACHINE;
   "CONSTRAINTS"        , CONSTRAINTS;
   "SEES"               , SEES;
   "INCLUDES"           , INCLUDES;
   "PROMOTES"           , PROMOTES;
   "EXTENDS"            , EXTENDS;
   "USES"               , USES;
   "SETS"               , SETS;
   "CONCRETE_CONSTANTS" , CONCRETE_CONSTANTS;
   "CONSTANTS"          , CONSTANTS;
   "ABSTRACT_VARIABLES" , ABSTRACT_VARIABLES;
   "VARIABLES"          , VARIABLES;
   "ASSERTIONS"         , ASSERTIONS;
   "INITIALISATION"     , INITIALISATION;
   "OPERATIONS"         , OPERATIONS;
   "LOCAL_OPERATIONS"   , LOCAL_OPERATIONS;
   "IN"                 , IN;
   "REFINEMENT"         , REFINEMENT;
   "IMPLEMENTATION"     , IMPLEMENTATION;
   "REFINES"            , REFINES;
   "IMPORTS"            , IMPORTS;
   "VALUES"             , VALUES;
   "PROPERTIES"         , PROPERTIES;
   "OF"                 , OF;
   "CONCRETE_VARIABLES" , CONCRETE_VARIABLES;
   "ABSTRACT_CONSTANTS" , ABSTRACT_CONSTANTS;

   "bool"      , CBOOL;
   "mod"       , MOD;
   "succ"      , SUCC;
   "pred"      , PRED;
   "max"       , MAX;
   "min"       , MIN;
   "card"      , CARD;
   "union"     , G_UNION;
   "inter"     , G_INTER;
   "prj1"      , PROJ1;
   "prj2"      , PROJ2;
   "iterate"   , ITERATION;
   "closure"   , CLOSURE;
   "closure1"  , CLOSURE1;
   "dom"       , DOM;
   "fnc"       , FNC;
   "rel"       , REL;
   "not"       , NOT;
   "or"        , OR;
   "id"        , ID;
   "rec"       , REC;
   "struct"    , STRUCT;
   "seq1"      , SEQ1;
   "seq"       , SEQ;
   "iseq1"     , ISEQ1;
   "iseq"      , ISEQ;
   "perm"      , PERM;
   "tree"      , TREE;
   "btree"     , BTREE;
   "const"     , CONST;
   "top"       , TOP;
   "sons"      , SONS;
   "prefix"    , PREFIX;
   "postfix"   , POSTFIX;
   "sizet"     , SIZET;
   "size"      , SIZE;
   "mirror"    , MIRROR;
   "rank"      , RANK;
   "ran"       , RAN;
   "father"    , FATHER;
   "son"       , SON;
   "arity"     , ARITY;
   "bin"       , BIN;
   "left"      , LEFT;
   "right"     , RIGHT;
   "infix"     , INFIX;
   "subtree"   , SUBTREE;
   "first"      , FIRST;
   "front"      , FRONT;
   "last"       , LAST;
   "tail"       , TAIL;
   "conc"       , CONC;
   "size"       , SIZE;
   "rev"        , REV;
   "ran"        , RAN;
  ]

let ident_to_token loc id =
  try Hashtbl.find keywords id
  with Not_found -> IDENT id

}

let space   = [' ' '\t']
let ident   = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let int_lit = ['0'-'9']+
let commented_line = "//" [^'\n']*
(* let ren_ident = ident ( '.' ident )+ *)
let def_file = '<' ['a'-'z' 'A'-'Z' '0'-'9' '_' '.']+ '>'

rule token = parse
  | space       { token lexbuf }
  | '\n'        { new_line lexbuf ; token lexbuf }
  | '\r'        { token lexbuf }
  | "/*"        { comment lexbuf }
  | commented_line     { token lexbuf }
  | '"'         { flush (); string lexbuf }
  
  | "+->>"      { S_PARTIELLE  }
  | "-->>"      { S_TOTALE  }
  | ">->>"      { B_TOTALE  }
  | "/<<:"      { NOT_S_INCLUDED  }

  | "|->"       { MAPLET  }
  | "<->"       { RELATION  }
  | "<<|"       { SOUSTRACTION_D  }
  | "|>>"       { SOUSTRACTION_CO  }
  | "+->"       { PARTIELLE  }
  | "-->"       { TOTALE  }
  | "<--"       { LEFTARROW  }
  | ">+>"       { P_INJECTION  }
  | ">->"       { T_INJECTION  }
  | "<=>"       { EQUIV  }
  | "<<:"       { S_INCLUDED  }
  | "/<:"       { NOT_INCLUDED  }
  | "\\|/"      { RESTRICTION_Q  }
  | "/|\\"      { RESTRICTION_T  }

  | "<-"        { INSERTION_Q  }
  | "->"        { INSERTION_T  }
  | "|>"        { RESTRICTION_CO  }
  | "**"        { POWER  }
  | "{}"        { EMPTY_SET  }
  | "[]"        { EMPTY_SEQ  }
  | ".."        { DOTDOT  }
  | "\\/"       { B_UNION  }
  | "/\\"       { B_INTER  }
  | "><"        { DPRODUCT  }
  | "||"        { PARALLEL  }
  | "<|"        { RESTRICTION_D  }
  | "<+"        { SURCHARGE  }
  | "=>"        { IMPLY  }
  | "/="        { NOT_EQUAL  }
  | "/:"        { NOT_MEMBER_OF }
  | "<:"        { INCLUDED  }
  | "<="        { SMALLER_OR_EQUAL  }
  | ">="        { GREATER_OR_EQUAL  }
  | ":="        { AFFECTATION  }
  | "::"        { BECOMES_ELT  }
(*   | ":("        { BECOMES_SUCH  } *)
  | "=="        { EQUALEQUAL  }
  | "$0"        { DOLLAR_ZERO  }

  | '/'         { DIV  }
  | '.'         { DOT  }
  | '\''        { SQUOTE  }
  | '|'         { BAR  }
  | '{'         { LBRA  }
  | '}'         { RBRA   }
  | '~'         { TILDE  }
  | ';'         { SEMICOLON  }
  | '['         { LSQU  }
  | ']'         { RSQU  }
  | '%'         { LAMBDA  }
  | '&'         { AND  }
  | '!'         { FORALL  }
  | '#'         { EXISTS  }
  | '='         { EQUAL  }
  | ':'         { MEMBER_OF  }
  | '<'         { S_SMALLER  }
  | '>'         { S_GREATER  }
  | '+'         { PLUS  }
  | '-'         { MINUS  }
  | '*'         { STAR  }
  | ','         { COMMA  }
  | ')'         { RPAR  }
  | '('         { LPAR  }
  | '^'         { CIRC  }

  | def_file as id {
      DEF_FILE ( String.sub id 1 ((String.length id)-2) ) }
  | ident as id { ident_to_token lexbuf.Lexing.lex_start_p id }
 
(*   | ren_ident as id { REN_IDENT ( get_loc lexbuf , id ) } *)
  | int_lit as i  { INTEGER ( int_of_int_lit i ) }
  | _   as c    { raise (Error.Error (lexbuf.Lexing.lex_start_p, "Unexpected character '" ^ String.make 1 c ^ "'.")) }
  | eof         { EOF }

 and comment = parse
  | "*/" { token lexbuf          }
  | '\n' { new_line lexbuf ; comment lexbuf }
  | _    { comment lexbuf        }
  | eof	 { raise (Error.Error (lexbuf.Lexing.lex_start_p, "Unexpected end of file")) }

and string = parse
  | '\\' '"' { add_char '"'; string lexbuf }
  | '\\' (_ as c) { add_char '\\'; add_char c; string lexbuf }
  | '\n' { Lexing.new_line lexbuf ; add_char '\n'; string lexbuf }
  | '"'  { STRING !chars_read }
  | _ as c { add_char c; string lexbuf }
  | eof	 { raise (Error.Error (lexbuf.Lexing.lex_start_p, "Unexpected end of file.")) }
