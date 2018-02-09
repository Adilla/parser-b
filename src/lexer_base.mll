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
    try
    if String.get s 0 = '-' then
      let l=String.length s in
      - (int_of_string (String.sub s 1 (l-1)))
    else int_of_string s
    with Failure _ -> (print_endline s; 0)

let err lexbuf err_txt =
  let open Error in
  raise (Error { err_loc=lexbuf.lex_start_p; err_txt })

let keywords = Hashtbl.create 137

let _ = List.iter (fun (name, keyword) ->
    Hashtbl.add keywords name keyword) [
    "WHERE"     , WHERE;
   "THEN"      , THEN;
   "SELECT"    , SELECT;
   "TRUE"      , CONSTANT Syntax.TRUE;
   "FALSE"     , CONSTANT Syntax.FALSE;
   "MAXINT"    , CONSTANT Syntax.MaxInt;
   "MININT"    , CONSTANT Syntax.MinInt;
   "SIGMA"     , E_BINDER Syntax.Sum;
   "INTEGER"   , CONSTANT Syntax.INTEGER;
   "NATURAL1"  , CONSTANT Syntax.NATURAL1;
   "NATURAL"   , CONSTANT Syntax.NATURAL;
   "NAT1"      , CONSTANT Syntax.NAT1;
   "NAT"       , CONSTANT Syntax.NAT;
   "INT"       , CONSTANT Syntax.INT;
   "BOOL"      , CONSTANT Syntax.BOOLEANS;
   "STRING"    , CONSTANT Syntax.STRINGS;
   "PI"        , E_BINDER Syntax.Prod;
   "POW1"      , E_PREFIX (Syntax.Power_Set Syntax.Non_Empty);
   "POW"       , E_PREFIX (Syntax.Power_Set Syntax.Full);
   "FIN1"      , E_PREFIX (Syntax.Power_Set Syntax.Finite_Non_Empty);
   "FIN"       , E_PREFIX (Syntax.Power_Set Syntax.Finite);
   "UNION"     , E_BINDER Syntax.Q_Union;
   "INTER"     , E_BINDER Syntax.Q_Intersection;
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
   "VISIBLE_VARIABLES" , CONCRETE_VARIABLES;
   "ABSTRACT_CONSTANTS" , ABSTRACT_CONSTANTS;
   "HIDDEN_CONSTANTS" , ABSTRACT_CONSTANTS;

   "bool"      , CBOOL;
   "mod"       , E_INFIX_190 Syntax.Modulo;
   "succ"      , E_PREFIX Syntax.Successor;
   "pred"      , E_PREFIX Syntax.Predecessor;
   "max"       , E_PREFIX Syntax.Max;
   "min"       , E_PREFIX Syntax.Min;
   "card"      , E_PREFIX Syntax.Cardinal;
   "union"     , E_PREFIX Syntax.G_Union;
   "inter"     , E_PREFIX Syntax.G_Intersection;
   "prj1"      , E_PREFIX Syntax.First_Projection;
   "prj2"      , E_PREFIX Syntax.Second_Projection;
   "iterate"   , E_PREFIX Syntax.Iteration;
   "closure"   , E_PREFIX Syntax.Closure;
   "closure1"  , E_PREFIX Syntax.Transitive_Closure;
   "dom"       , E_PREFIX Syntax.Domain;
   "fnc"       , E_PREFIX Syntax.Fnc;
   "rel"       , E_PREFIX Syntax.Rel;
   "not"       , NOT;
   "or"        , OR;
    "id"        , E_PREFIX Syntax.Identity_Relation;
   "rec"       , REC;
   "struct"    , STRUCT;
   "seq1"       , E_PREFIX (Syntax.Sequence_Set Syntax.Non_Empty_Seq);
   "seq"        , E_PREFIX (Syntax.Sequence_Set Syntax.All_Seq);
   "iseq1"      , E_PREFIX (Syntax.Sequence_Set Syntax.Injective_Non_Empty_Seq);
   "iseq"       , E_PREFIX (Syntax.Sequence_Set Syntax.Injective_Seq);
   "perm"       , E_PREFIX (Syntax.Sequence_Set Syntax.Permutations);
   "tree"       , E_PREFIX Syntax.Tree;
   "btree"      , E_PREFIX Syntax.Btree;
   "const"      , E_PREFIX Syntax.Const;
   "top"        , E_PREFIX Syntax.Top;
   "sons"       , E_PREFIX Syntax.Sons;
   "prefix"     , E_PREFIX Syntax.Prefix;
   "postfix"    , E_PREFIX Syntax.Postfix;
   "sizet"      , E_PREFIX Syntax.SizeT;
   "size"       , E_PREFIX Syntax.Size;
   "mirror"     , E_PREFIX Syntax.Mirror;
   "rank"       , E_PREFIX Syntax.Rank;
   "ran"        , E_PREFIX Syntax.Range;
   "father"     , E_PREFIX Syntax.Father;
   "son"        , E_PREFIX Syntax.Son;
   "arity"      , E_PREFIX Syntax.Arity;
   "bin"        , E_PREFIX Syntax.Bin;
   "left"       , E_PREFIX Syntax.Left;
   "right"      , E_PREFIX Syntax.Right;
   "infix"      , E_PREFIX Syntax.Infix;
   "subtree"    , E_PREFIX Syntax.Subtree;
   "first"      , E_PREFIX Syntax.First;
   "front"      , E_PREFIX Syntax.Front;
   "last"       , E_PREFIX Syntax.Last;
   "tail"       , E_PREFIX Syntax.Tail;
   "conc"       , E_PREFIX Syntax.G_Concatenation;
   "size"       , E_PREFIX Syntax.Size;
   "rev"        , E_PREFIX Syntax.Reverse;
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
  
  | "+->>"      { E_INFIX_125 (Syntax.Functions Syntax.Partial_Surjections) }
  | "-->>"      { E_INFIX_125 (Syntax.Functions Syntax.Total_Surjections)  }
  | ">->>"      { E_INFIX_125 (Syntax.Functions  Syntax.Bijections) }
  | "/<<:"      { PREDICATE (Syntax.Inclusion Syntax.Non_Strict_Inclusion) }

  | "|->"       { MAPLET  }
  | "<->"       { E_INFIX_125 Syntax.Relations  }
  | "<<|"       { E_INFIX_160 Syntax.Domain_Soustraction  }
  | "|>>"       { E_INFIX_160 Syntax.Codomain_Soustraction }
  | "+->"       { E_INFIX_125 (Syntax.Functions Syntax.Partial_Functions)  }
  | "-->"       { E_INFIX_125 (Syntax.Functions Syntax.Total_Functions) }
  | "<--"       { LEFTARROW  }
  | ">+>"       { E_INFIX_125 (Syntax.Functions Syntax.Partial_Injections)  }
  | ">->"       { E_INFIX_125 (Syntax.Functions Syntax.Total_Injections) }
  | "<=>"       { EQUIV  }
  | "<<:"       { PREDICATE (Syntax.Inclusion Syntax.Strict) }
  | "/<:"       { PREDICATE (Syntax.Inclusion Syntax.Non_Inclusion) }
  | "\\|/"      { E_INFIX_160 Syntax.Tail_Restriction }
  | "/|\\"      { E_INFIX_160 Syntax.Head_Restriction }

  | "<-"        { E_INFIX_160 Syntax.Tail_Insertion }
  | "->"        { E_INFIX_160 Syntax.Head_Insertion }
  | "|>"        { E_INFIX_160 Syntax.Codomain_Restriction }
  | "**"        { E_INFIX_200 Syntax.Power  }
  | "{}"        { CONSTANT Syntax.Empty_Set  }
  | "[]"        { CONSTANT Syntax.Empty_Seq  }
  | "<>"        { CONSTANT Syntax.Empty_Seq  }
  | ".."        { E_INFIX_170 Syntax.Interval  }
  | "\\/"       { E_INFIX_160 Syntax.Union }
  | "/\\"       { E_INFIX_160 Syntax.Intersection }
  | "><"        { E_INFIX_160 Syntax.Direct_Product }
  | "||"        { PARALLEL  }
  | "<|"        { E_INFIX_160 Syntax.Domain_Restriction }
  | "<+"        { E_INFIX_160 Syntax.Surcharge }
  | "=>"        { IMPLY  }
  | "/="        { PREDICATE Syntax.Disequality }
  | "/:"        { PREDICATE Syntax.Non_Membership }
  | "<:"        { PREDICATE (Syntax.Inclusion Syntax.Not_Strict) }
  | "<="        { PREDICATE (Syntax.Inequality Syntax.Smaller_or_Equal) }
  | ">="        { PREDICATE (Syntax.Inequality Syntax.Greater_or_Equal) }
  | ":="        { AFFECTATION  }
  | "::"        { BECOMES_ELT  }
(*   | ":("        { BECOMES_SUCH  } *)
  | "=="        { EQUALEQUAL  }
  | "$0"        { DOLLAR_ZERO  }

  | '/'         { E_INFIX_190 Syntax.Division }
  | '.'         { DOT  }
  | '\''        { SQUOTE  }
  | '|'         { BAR  }
  | '{'         { LBRA  }
  | '}'         { RBRA   }
  | '~'         { TILDE  }
  | ';'         { SEMICOLON  }
  | '['         { LSQU  }
  | ']'         { RSQU  }
  | '%'         { E_BINDER Syntax.Lambda  }
  | '&'         { AND  }
  | '!'         { FORALL  }
  | '#'         { EXISTS  }
  | '='         { EQUAL  }
  | ':'         { MEMBER_OF  }
  | '<'         { PREDICATE (Syntax.Inequality Syntax.Strictly_Smaller) }
  | '>'         { PREDICATE (Syntax.Inequality Syntax.Strictly_Greater) }
  | '+'         { E_INFIX_180 Syntax.Addition  }
  | '-'         { MINUS  }
  | '*'         { E_INFIX_190 Syntax.Product }
  | ','         { COMMA  }
  | ')'         { RPAR  }
  | '('         { LPAR  }
  | '^'         { E_INFIX_160 Syntax.Concatenation }

  | def_file as id {
      DEF_FILE ( String.sub id 1 ((String.length id)-2) ) }
  | ident as id { ident_to_token lexbuf.Lexing.lex_start_p id }
 
(*   | ren_ident as id { REN_IDENT ( get_loc lexbuf , id ) } *)
  | int_lit as i  { CONSTANT (Syntax.Integer ( int_of_int_lit i )) }
  | _   as c    { err lexbuf ("Unexpected character '" ^ String.make 1 c ^ "'.") }
  | eof         { EOF }

 and comment = parse
  | "*/" { token lexbuf          }
  | '\n' { new_line lexbuf ; comment lexbuf }
  | _    { comment lexbuf        }
  | eof	 { err lexbuf "Unexpected end of file" }

and string = parse
  | '\\' '"' { add_char '"'; string lexbuf }
  | '\\' (_ as c) { add_char '\\'; add_char c; string lexbuf }
  | '\n' { Lexing.new_line lexbuf ; add_char '\n'; string lexbuf }
  | '"'  {  STRING !chars_read }
  | _ as c { add_char c; string lexbuf }
  | eof	 { err lexbuf "Unexpected end of file." }
